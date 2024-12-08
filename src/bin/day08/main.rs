use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::fmt::Formatter;
use std::fmt::{Display, Write};
use std::str;

use lib::grid::{BoundingBox, Movement, Position};

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

    fn bounding_box(&self) -> BoundingBox {
        let result = BoundingBox::from_corners(
            &Position { x: 0, y: 0 },
            &Position {
                x: self.w - 1,
                y: self.h - 1,
            },
        );
        assert_eq!(result.width(), self.w);
        assert_eq!(result.height(), self.h);
        result
    }

    fn antinodes(
        &self,
        min_multiplier: i64,
        max_multiplier: Option<i64>,
    ) -> impl Iterator<Item = Antinode> + use<'_> {
        let bounds = self.bounding_box();
        self.antennas.iter().flat_map(move |(label, positions)| {
            all_pairs(positions).flat_map(move |(p1, p2)| {
                compute_antinodes(*label, p1, p2, min_multiplier, max_multiplier, bounds)
            })
        })
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
struct Antinode {
    label: char,
    location: Position,
}

fn in_bounds_antinodes<'a>(
    label: char,
    p1: Position,
    delta: Movement,
    bounds: BoundingBox,
    sign: i64,
    min_multiplier: i64,
    max_multiplier: Option<i64>,
) -> impl Iterator<Item = Antinode> + use<'a> {
    (min_multiplier..)
        .take_while(move |multiplier| match max_multiplier {
            None => true,
            Some(n) => *multiplier <= n,
        })
        .map(move |multiplier| {
            let m: i64 = multiplier * sign;
            Antinode {
                label,
                location: p1 - (delta * m),
            }
        })
        .take_while(move |antinode| bounds.contains(&antinode.location))
}

fn compute_antinodes<'a>(
    label: char,
    p1: Position,
    p2: Position,
    min_multiplier: i64,
    max_multiplier: Option<i64>,
    bounds: BoundingBox,
) -> impl Iterator<Item = Antinode> + 'a {
    assert!(p1 != p2);
    let delta = p2 - p1;
    in_bounds_antinodes(label, p1, delta, bounds, 1, min_multiplier, max_multiplier).chain(
        in_bounds_antinodes(label, p2, delta, bounds, -1, min_multiplier, max_multiplier),
    )
}

#[test]
fn test_compute_antinodes_part1() {
    let bounds = BoundingBox::from_corners(&Position { x: 0, y: 0 }, &Position { x: 12, y: 5 });
    let got: HashSet<Antinode> = compute_antinodes(
        '0',
        Position { x: 8, y: 1 },
        Position { x: 5, y: 2 },
        1,
        Some(1),
        bounds,
    )
    .map(|a: Antinode| a)
    .collect();
    dbg!(&got);
    assert_eq!(got.len(), 2);
    assert!(got.contains(&Antinode {
        label: '0',
        location: Position { x: 11, y: 0 },
    }));
    assert!(got.contains(&Antinode {
        label: '0',
        location: Position { x: 2, y: 3 },
    }));
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
fn part1_antinodes_for_sample_input() -> &'static str {
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

#[cfg(test)]
fn part2_antinodes_for_sample_input() -> &'static str {
    concat!(
        "##....#....#\n",
        ".#.#....0...\n",
        "..#.#0....#.\n",
        "..##...0....\n",
        "....0....#..\n",
        ".#...#A....#\n",
        "...#..#.....\n",
        "#....#.#....\n",
        "..#.....A...\n",
        "....#....A..\n",
        ".#........#.\n",
        "...#......##\n",
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
fn test_display_antinodes_part1() {
    let world = parse_input(sample_input());
    let antinodes: HashSet<Antinode> = world.antinodes(1, Some(1)).collect();
    let mut displayed_antinodes = String::new();
    display_antinodes(&mut displayed_antinodes, &world, &antinodes)
        .expect("writes to String should always succeed");
    let expected = part1_antinodes_for_sample_input();
    println!("expected:\n{expected}\ngot:\n{displayed_antinodes}");
    assert_eq!(displayed_antinodes, expected);
}

fn solve(world: &World, min_multiplier: i64, max_multiplier: Option<i64>) -> usize {
    let distinct_positions: HashSet<Position> = world
        .antinodes(min_multiplier, max_multiplier)
        .map(|antinode| antinode.location)
        .collect();
    distinct_positions.len()
}

fn part1(world: &World) -> usize {
    solve(world, 1, Some(1))
}

#[test]
fn test_part1() {
    let world = parse_input(sample_input());
    assert_eq!(part1(&world), 14);
}

#[test]
fn test_display_antinodes_part2() {
    let world = parse_input(sample_input());
    let antinodes: HashSet<Antinode> = world.antinodes(0, None).collect();
    let mut displayed_antinodes = String::new();
    display_antinodes(&mut displayed_antinodes, &world, &antinodes)
        .expect("writes to String should always succeed");
    let expected = part2_antinodes_for_sample_input();
    println!("expected:\n{expected}\ngot:\n{displayed_antinodes}");
    assert_eq!(displayed_antinodes, expected);
}

fn part2(world: &World) -> usize {
    solve(world, 0, None)
}

#[test]
fn test_part2() {
    let world = parse_input(sample_input());
    assert_eq!(part2(&world), 34);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let world = parse_input(input);
    println!("day 08 part 1: {}", part1(&world));
    println!("day 08 part 2: {}", part2(&world));
}
