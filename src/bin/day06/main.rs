use lib::grid::{CompassDirection, Position};

use std::collections::HashSet;
use std::fmt::{Display, Write};
use std::str;

#[derive(Debug)]
struct Grid {
    cells: Vec<Vec<char>>,
    visited: HashSet<Position>,
    guard_orientation: CompassDirection,
    guard_pos: Position,
}

impl Grid {
    fn contains(&self, pos: &Position) -> bool {
        self.get(pos).is_some()
    }

    fn get(&self, pos: &Position) -> Option<char> {
        if pos.y >= 0 && pos.x >= 0 && pos.y < self.cells.len() as i64 {
            let row = &self.cells[pos.y as usize];
            if pos.x < row.len() as i64 {
                return Some(row[pos.x as usize]);
            }
        }
        None
    }

    fn patrol(&mut self) {
        let mut limit = 1_000_000;
        while self.contains(&self.guard_pos) {
            if limit == 0 {
                panic!("infinite patrol\n{}\n{:?}", self, self);
            } else {
                limit -= 1;
            }

            self.visited.insert(self.guard_pos);
            let next = self.guard_pos.move_direction(&self.guard_orientation);
            match self.get(&next) {
                Some('#') => {
                    self.guard_orientation = self.guard_orientation.rotated_clockwise();
                }
                _ => {
                    self.guard_pos = next;
                }
            }
        }
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn guard_indicator(orientation: CompassDirection) -> char {
            use CompassDirection::*;
            match orientation {
                North => '^',
                East => '>',
                South => 'v',
                West => '<',
            }
        }

        for (y, row) in self.cells.iter().enumerate() {
            for (x, ch) in row.iter().enumerate() {
                let here = Position {
                    x: x as i64,
                    y: y as i64,
                };
                if self.guard_pos == here {
                    f.write_char(guard_indicator(self.guard_orientation))?;
                } else if self.visited.contains(&here) {
                    f.write_char('X')?;
                } else {
                    f.write_char(*ch)?;
                }
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl TryFrom<&str> for Grid {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut cells: Vec<Vec<char>> = s.lines().map(|line| line.chars().collect()).collect();
        let mut guard: Option<(Position, CompassDirection)> = None;
        for (y, row) in cells.iter().enumerate() {
            for (x, ch) in row.iter().enumerate() {
                let here = Position {
                    x: x as i64,
                    y: y as i64,
                };
                let orientation = match ch {
                    '>' => CompassDirection::East,
                    '<' => CompassDirection::West,
                    '^' => CompassDirection::North,
                    'v' => CompassDirection::South,
                    _ => {
                        continue;
                    }
                };
                guard = Some((here, orientation));
            }
        }
        match guard {
            Some((guard_pos, guard_orientation)) => {
                cells[guard_pos.y as usize][guard_pos.x as usize] = '.';
                Ok(Grid {
                    cells,
                    visited: HashSet::new(),
                    guard_orientation,
                    guard_pos,
                })
            }
            None => Err("guard not found".to_string()),
        }
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "....#.....\n",
        ".........#\n",
        "..........\n",
        "..#.......\n",
        ".......#..\n",
        "..........\n",
        ".#..^.....\n",
        "........#.\n",
        "#.........\n",
        "......#...\n",
    )
}

#[test]
fn test_parse_input() {
    let grid = Grid::try_from(concat!(".>.\n", "#..\n")).expect("grid should be valid");
    assert_eq!(grid.cells, vec![vec!['.', '.', '.'], vec!['#', '.', '.']]);
}

#[test]
fn test_grid_display() {
    let mut grid = Grid::try_from(concat!(".>.\n", "#..\n")).expect("grid should be valid");
    grid.guard_pos.x = 0;
    assert_eq!(&grid.to_string(), ">..\n#..\n");
}

#[test]
fn test_patrol() {
    let mut grid = Grid::try_from(sample_input()).expect("sample input should be valid");
    grid.patrol();
    let got = grid.to_string();
    let expected = concat!(
        "....#.....\n",
        "....XXXXX#\n",
        "....X...X.\n",
        "..#.X...X.\n",
        "..XXXXX#X.\n",
        "..X.X.X.X.\n",
        ".#XXXXXXX.\n",
        ".XXXXXXX#.\n",
        "#XXXXXXX..\n",
        "......#X..\n",
    );
    if got != expected {
        eprintln!("got:\n{got}\nexpected:\n{expected}\n");
    }
    assert_eq!(&got, expected);
}

fn part1(s: &str) -> usize {
    let mut grid = Grid::try_from(s).expect("part 1 input should be valid");
    grid.patrol();
    grid.visited.len()
}

#[test]
fn test_part1() {
    assert_eq!(part1(sample_input()), 41);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 05 part 1: {}", part1(input));
}
