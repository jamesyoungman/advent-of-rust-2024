use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;
use std::fmt::{Display, Write};
use std::str;

use lib::grid::Position;

struct World {
    w: i64,
    h: i64,
    antennas: HashMap<char, Vec<Position>>,
}

fn is_antenna_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric()
}

fn parse_input(input: &str) -> World {
    let mut antennas = HashMap::new();
    let mut ymax: usize = 0;
    let mut xmax: usize = 0;
    for (y, line) in input.lines().enumerate() {
        ymax = y;
        for (x, ch) in line.chars().enumerate() {
            xmax = max(xmax, x);
            if is_antenna_char(ch) {
                let pos = Position {
                    x: x as i64,
                    y: y as i64,
                };
                antennas
                    .entry(ch)
                    .and_modify(|positions: &mut Vec<Position>| {
                        positions.push(pos);
                    })
                    .or_insert_with(|| vec![pos]);
            }
        }
    }
    World {
        w: (xmax + 1) as i64,
        h: (ymax + 1) as i64,
        antennas,
    }
}

fn all_pairs<T>(v: &[T]) -> impl Iterator<Item = (T, T)> + use<'_, T>
where
    T: Copy,
{
    v[0..]
        .iter()
        .enumerate()
        .flat_map(|(i, right)| v[0..i].iter().map(|left| (*left, *right)))
}

#[test]
fn test_all_pairs() {
    assert_eq!(all_pairs::<char>(&[]).collect::<Vec<_>>(), Vec::new());
    assert_eq!(all_pairs::<Position>(&[]).collect::<Vec<_>>(), Vec::new());

    assert_eq!(all_pairs(&['a', 'b']).collect::<Vec<_>>(), vec![('a', 'b')]);
    assert_eq!(
        all_pairs(&['a', 'b', 'c']).collect::<Vec<_>>(),
        vec![('a', 'b'), ('a', 'c'), ('b', 'c')]
    );
}

impl World {
    fn markers(&self) -> impl Iterator<Item = (Position, char)> + use<'_> {
        self.antennas
            .iter()
            .flat_map(|(marker, locations)| locations.iter().map(|loc| (*loc, *marker)))
    }

    fn inbounds(&self, pos: &Position) -> bool {
        pos.x >= 0 && pos.x < self.w && pos.y >= 0 && pos.y < self.h
    }

    fn antinodes(&self) -> impl Iterator<Item = Antinode> + use<'_> {
        self.antennas
            .iter()
            .flat_map(|(label, positions)| {
                all_pairs(positions).flat_map(|(p1, p2)| compute_antinodes(*label, &p1, &p2))
            })
            .filter(|antinode| self.inbounds(&antinode.location))
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct Antinode {
    label: char,
    location: Position,
}

fn compute_antinodes(label: char, p1: &Position, p2: &Position) -> [Antinode; 2] {
    assert!(p1 != p2);
    let delta = p2 - p1;
    [
        Antinode {
            label,
            location: p1 - &delta,
        },
        Antinode {
            label,
            location: p2 + &delta,
        },
    ]
}

#[test]
fn test_compute_antinodes() {
    let computed_antinodes: HashSet<Antinode> =
        compute_antinodes('0', &Position { x: 8, y: 1 }, &Position { x: 5, y: 2 }).into();
    let expected_antinoides: HashSet<Antinode> = [
        Antinode {
            label: '0',
            location: Position { x: 11, y: 0 },
        },
        Antinode {
            label: '0',
            location: Position { x: 2, y: 3 },
        },
    ]
    .into();
    assert_eq!(computed_antinodes, expected_antinoides);
}

impl Display for World {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let markers: HashMap<Position, char> = self.markers().collect();
        for y in 0..self.h {
            for x in 0..self.w {
                let here = Position { x, y };
                f.write_char(*markers.get(&here).unwrap_or(&'.'))?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "............\n",
        "........0...\n",
        ".....0......\n",
        ".......0....\n",
        "....0.......\n",
        "......A.....\n",
        "............\n",
        "............\n",
        "........A...\n",
        ".........A..\n",
        "............\n",
        "............\n"
    )
}

#[cfg(test)]
fn antinodes_for_sample_input() -> &'static str {
    concat!(
        "......#....#\n",
        "...#....0...\n",
        "....#0....#.\n",
        "..#....0....\n",
        "....0....#..\n",
        ".#....A.....\n",
        "...#........\n",
        "#......#....\n",
        "........A...\n",
        ".........A..\n",
        "..........#.\n",
        "..........#.\n"
    )
}

#[test]
fn test_parse_and_display() {
    let input = sample_input();
    let world = parse_input(input);
    let displayed = world.to_string();
    assert_eq!(input, displayed);
}

#[cfg(test)]
fn display_antinodes<W: Write>(
    mut writer: W,
    world: &World,
    antinodes: &HashSet<Antinode>,
) -> Result<(), std::fmt::Error> {
    let base_markers: HashMap<Position, char> = world.markers().collect();
    let antinode_locations: HashSet<Position> =
        antinodes.iter().map(|antinode| antinode.location).collect();
    for y in 0..world.h {
        for x in 0..world.w {
            let here = Position { x, y };
            writer.write_char(match base_markers.get(&here) {
                Some(marker) => {
                    // This location may also be an antinode location,
                    // but since there is an antenna here, we show the
                    // antenna label (following the example on the AoC
                    // site).
                    *marker
                }
                None => {
                    if antinode_locations.contains(&here) {
                        '#'
                    } else {
                        '.'
                    }
                }
            })?;
        }
        writer.write_char('\n')?;
    }
    Ok(())
}

#[test]
fn test_display_antinodes() {
    let world = parse_input(sample_input());
    let antinodes: HashSet<Antinode> = world.antinodes().collect();
    let mut displayed_antinodes = String::new();
    display_antinodes(&mut displayed_antinodes, &world, &antinodes)
        .expect("writes to String should always succeed");
    let expected = antinodes_for_sample_input();
    println!("expected:\n{expected}\ngot:\n{displayed_antinodes}");
    assert_eq!(displayed_antinodes, expected);
}

fn part1(world: &World) -> usize {
    let distinct_positions: HashSet<Position> = world
        .antinodes()
        .map(|antinode| antinode.location)
        .collect();
    distinct_positions.len()
}

#[test]
fn test_part1() {
    let world = parse_input(sample_input());
    assert_eq!(part1(&world), 14);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let world = parse_input(input);
    println!("day 08 part 1: {}", part1(&world));
}
