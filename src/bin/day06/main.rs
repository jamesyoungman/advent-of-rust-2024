use lib::grid::{CompassDirection, Position};

use std::collections::HashSet;
use std::fmt::{Display, Write};
use std::str;

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct GuardState {
    pos: Position,
    orientation: CompassDirection,
}

#[derive(Debug, Clone)]
struct Grid {
    cells: Vec<Vec<char>>,
    obstruction: Option<Position>,
    visited: HashSet<GuardState>,
    guard: GuardState,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
enum PatrolOutcome {
    WentAway,
    InfiniteLoop,
}

impl Grid {
    fn contains(&self, pos: &Position) -> bool {
        self.get(pos).is_some()
    }

    fn bounds(&self) -> Position {
        if self.cells.is_empty() {
            Position { x: 0, y: 0 }
        } else {
            Position {
                x: self.cells[0].len() as i64,
                y: self.cells.len() as i64,
            }
        }
    }

    fn add_obstruction(&mut self, pos: Position) -> bool {
        match self.get(&pos) {
            Some('#' | '>' | '<' | 'v' | '^') => false,
            Some('O') => {
                panic!("only one obstruction is supported");
            }
            Some('.') => {
                self.obstruction = Some(pos);
                true
            }
            Some(other) => {
                panic!("unexpected character {other} in grid");
            }
            None => {
                panic!("obstructions can only be placed inside the grid");
            }
        }
    }

    fn have_visited(&self, pos: &Position) -> bool {
        use CompassDirection::*;
        [North, East, South, West].iter().any(|orientation| {
            self.visited.contains(&GuardState {
                pos: *pos,
                orientation: *orientation,
            })
        })
    }

    fn distinct_position_count(&self) -> usize {
        self.visited
            .iter()
            .map(|state| state.pos)
            .collect::<HashSet<_>>()
            .len()
    }

    fn get(&self, pos: &Position) -> Option<char> {
        if let Some(opos) = self.obstruction.as_ref() {
            if opos == pos {
                return Some('O');
            }
        }
        if pos.y >= 0 && pos.x >= 0 && pos.y < self.cells.len() as i64 {
            let row = &self.cells[pos.y as usize];
            if pos.x < row.len() as i64 {
                return Some(row[pos.x as usize]);
            }
        }
        None
    }

    fn patrol(&mut self) -> PatrolOutcome {
        while self.contains(&self.guard.pos) {
            if self.visited.contains(&self.guard) {
                return PatrolOutcome::InfiniteLoop;
            }
            self.visited.insert(self.guard);
            let next = self.guard.pos.move_direction(&self.guard.orientation);
            match self.get(&next) {
                Some('#' | 'O') => {
                    self.guard.orientation = self.guard.orientation.rotated_clockwise();
                }
                _ => {
                    self.guard.pos = next;
                }
            }
        }
        PatrolOutcome::WentAway
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
                if let Some(op) = self.obstruction {
                    if op == here {
                        f.write_char('O')?;
                        continue;
                    }
                }
                if self.guard.pos == here {
                    f.write_char(guard_indicator(self.guard.orientation))?;
                } else if self.have_visited(&here) {
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
        let mut guard: Option<GuardState> = None;
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
                guard = Some(GuardState {
                    pos: here,
                    orientation,
                });
            }
        }
        match guard {
            Some(g) => {
                cells[g.pos.y as usize][g.pos.x as usize] = '.';
                Ok(Grid {
                    cells,
                    obstruction: None,
                    visited: HashSet::new(),
                    guard: g,
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
    grid.guard.pos.x = 0;
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
    grid.distinct_position_count()
}

#[test]
fn test_part1() {
    assert_eq!(part1(sample_input()), 41);
}

#[test]
fn test_part2() {
    assert_eq!(part2(sample_input()), 6);
}

fn part2(s: &str) -> usize {
    let original_grid = Grid::try_from(s).expect("part 2 input should be valid");
    let bbox = original_grid.bounds();
    let mut result = 0;
    for obstruction_x in 0..bbox.x {
        for obstruction_y in 0..bbox.y {
            let mut grid = original_grid.clone();
            let obstruction = Position {
                x: obstruction_x,
                y: obstruction_y,
            };
            if grid.add_obstruction(obstruction) {
                match grid.patrol() {
                    PatrolOutcome::WentAway => (),
                    PatrolOutcome::InfiniteLoop => {
                        result += 1;
                    }
                }
            }
        }
    }
    result
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 05 part 1: {}", part1(input));
    println!("day 05 part 2: {}", part2(input));
}
