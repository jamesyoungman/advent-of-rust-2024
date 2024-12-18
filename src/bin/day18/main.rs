#[cfg(test)]
use std::cmp::min;
use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;
use std::str;

use rustc_hash::FxHashMap;

#[cfg(test)]
use lib::grid::CompassDirection;
use lib::grid::{BoundingBox, Position, ALL_MOVE_OPTIONS};

use lib::parse::parse_number;

#[derive(Debug, PartialEq, Eq, Clone)]
struct Memory {
    bbox: BoundingBox,
    fallen_bytes: HashSet<Position>,
}

fn parse_fallen_bytes(s: &str) -> Vec<Position> {
    fn parse_line(line: &str) -> Position {
        match line.split_once(',') {
            Some((x, y)) => Position {
                x: parse_number(x),
                y: parse_number(y),
            },
            None => {
                panic!("line '{line}' should contain coordinates but does not");
            }
        }
    }
    s.lines().map(parse_line).collect()
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "5,4\n", "4,2\n", "4,5\n", "3,0\n", "2,1\n", "6,3\n", "2,4\n", "1,5\n", "0,6\n", "3,3\n",
        "2,6\n", "5,1\n", "1,2\n", "5,5\n", "2,5\n", "6,5\n", "1,4\n", "0,4\n", "6,4\n", "1,1\n",
        "6,1\n", "1,0\n", "0,5\n", "1,6\n", "2,0\n",
    )
}

#[cfg(test)]
fn parse_input(input: &str, xmax: i64, ymax: i64, limit: usize) -> Memory {
    let fallen_bytes = parse_fallen_bytes(input);
    let n = min(limit, fallen_bytes.len());
    Memory::new(&fallen_bytes[..n], xmax, ymax)
}

#[test]
fn test_parse_all_input() {
    let input_str = sample_input();
    // We use bogus bounds here, the key idea is that x!=y.
    let input = parse_input(input_str, 600, 900, usize::MAX);
    for pos in &[
        Position { x: 5, y: 4 },
        Position { x: 4, y: 2 },
        Position { x: 4, y: 5 },
        Position { x: 3, y: 0 },
    ] {
        assert!(input.fallen_bytes.contains(pos));
    }

    assert_eq!(
        input.fallen_bytes.len(),
        input_str.chars().filter(|ch| *ch == '\n').count()
    );
    assert_eq!(&input.bbox.top_left, &Position { x: 0, y: 0 });
    assert_eq!(&input.bbox.bottom_right, &Position { x: 600, y: 900 });
}

#[test]
fn test_parse_input_prefix() {
    let input_str = sample_input();
    let input = parse_input(input_str, 6, 6, 3);
    assert_eq!(input.fallen_bytes.len(), 3);
    for pos in &[
        Position { x: 5, y: 4 },
        Position { x: 4, y: 2 },
        Position { x: 4, y: 5 },
    ] {
        assert!(input.fallen_bytes.contains(pos));
    }
    assert!(!input.fallen_bytes.contains(&Position { x: 3, y: 0 }));
    assert_eq!(&input.bbox.top_left, &Position { x: 0, y: 0 });
    assert_eq!(&input.bbox.bottom_right, &Position { x: 6, y: 6 });
}

impl Memory {
    fn in_bounds(&self, pos: &Position) -> bool {
        self.bbox.contains(pos)
    }

    fn insert_fallen_byte(&mut self, pos: Position) -> bool {
        self.fallen_bytes.insert(pos)
    }

    fn new(fallen: &[Position], xmax: i64, ymax: i64) -> Memory {
        let bbox: BoundingBox = BoundingBox::containing(
            [&Position { x: 0, y: 0 }, &Position { x: xmax, y: ymax }].into_iter(),
        )
        .unwrap();
        let mut fallen_bytes: HashSet<Position> = HashSet::new();
        for p in fallen {
            fallen_bytes.insert(*p);
        }
        Memory { bbox, fallen_bytes }
    }
}

fn bfs<FN, FB, IN>(
    start: &Position,
    mut neighbours: FN,
    visited: &mut FxHashMap<Position, usize>,
    mut goal: FB,
) -> Option<usize>
where
    FN: FnMut(Position) -> IN,           // enumerates neighbours
    IN: Iterator<Item = Position>,       // iterates over neighbours
    FB: FnMut(usize, &Position) -> bool, // are we at a goal?
{
    let mut queue: VecDeque<(usize, Position)> = VecDeque::from([(0, *start)]);
    while let Some((steps, here)) = queue.pop_front() {
        if goal(steps, &here) {
            return Some(steps);
        }
        visited.entry(here).or_insert_with(|| {
            for neighbour in neighbours(here) {
                queue.push_back((steps + 1, neighbour));
            }
            steps
        });
    }
    None
}

trait Graph {
    type Node;
    fn neighbours(&self, n: &Self::Node) -> Vec<Self::Node>;
}

impl Graph for Memory {
    type Node = Position;

    fn neighbours(&self, start: &Self::Node) -> Vec<Self::Node> {
        ALL_MOVE_OPTIONS
            .iter()
            .map(|direction| start.move_direction(direction))
            .filter(|pos| self.in_bounds(pos))
            .filter(|pos| !self.fallen_bytes.contains(pos))
            .collect()
    }
}

#[test]
fn test_neighbours() {
    let input_str = sample_input();
    let mem: Memory = parse_input(input_str, 6, 6, 12);
    use CompassDirection::*;
    let route: Vec<(Position, CompassDirection)> = vec![
        (Position { x: 0, y: 0 }, East),  // 0
        (Position { x: 1, y: 0 }, South), // 1
        (Position { x: 1, y: 1 }, South), // 2
        (Position { x: 1, y: 2 }, East),  // 3
        (Position { x: 2, y: 2 }, East),  // 4
        (Position { x: 3, y: 2 }, North), // 5
        (Position { x: 3, y: 1 }, East),  // 6
        (Position { x: 4, y: 1 }, North), // 7
        (Position { x: 4, y: 0 }, East),  // 8
        (Position { x: 5, y: 0 }, East),  // 9
        (Position { x: 6, y: 0 }, South), // 10
        (Position { x: 6, y: 1 }, South), // 11
        (Position { x: 6, y: 2 }, West),  // 12
        (Position { x: 5, y: 2 }, South), // 13
        (Position { x: 5, y: 3 }, West),  // 14
        (Position { x: 4, y: 3 }, South), // 15
        (Position { x: 4, y: 4 }, West),  // 16
        (Position { x: 3, y: 4 }, South), // 17
        (Position { x: 3, y: 5 }, South), // 18
        (Position { x: 3, y: 6 }, East),  // 19
        (Position { x: 4, y: 6 }, East),  // 20
        (Position { x: 5, y: 6 }, East),  // 21
                                          // Final position is 6,6
    ];
    for i in 1..route.len() {
        let from = route[i - 1].0;
        let to = route[i].0;
        dbg!(&from);
        dbg!(&route[i - 1].1);
        dbg!(&to);
        assert_eq!(
            from.move_direction(&route[i - 1].1),
            to,
            "inconsistent at i={i}"
        );
    }
    for i in 0..route.len() {
        assert!(!mem.fallen_bytes.contains(&route[i].0));
    }
}

fn part1_bfs(
    mem: &Memory,
    start: &Position,
    goal: &Position,
    visited: &mut FxHashMap<Position, usize>,
) -> Option<usize> {
    let mut steps = None;
    let atgoal = |i: usize, pos: &Position| -> bool {
        if pos == goal {
            steps = Some(i);
            true
        } else {
            false
        }
    };
    let neighbours = |here: Position| mem.neighbours(&here).into_iter();
    bfs(start, neighbours, visited, atgoal);
    steps
}

fn part2(
    fallen_bytes: &[Position],
    start: &Position,
    goal: &Position,
    limit: usize,
    xmax: i64,
    ymax: i64,
) -> Option<Position> {
    let mut mem = Memory::new(&fallen_bytes[..limit], xmax, ymax);
    let mut visited: FxHashMap<Position, usize> = Default::default();
    part1_bfs(&mem, start, goal, &mut visited);

    for faller in &fallen_bytes[limit..] {
        if mem.insert_fallen_byte(*faller) && visited.contains_key(faller) {
            // Previously unoccupied positino, check for blockage.
            visited.clear();
            if part1_bfs(&mem, start, goal, &mut visited).is_none() {
                return Some(*faller);
            }
        }
    }
    None
}

#[test]
fn test_part1_bfs() {
    let mem = parse_input(sample_input(), 6, 6, 12);
    let start = Position { x: 0, y: 0 };
    let goal = Position { x: 6, y: 6 };
    let mut visited = Default::default();
    let steps = part1_bfs(&mem, &start, &goal, &mut visited);
    assert_eq!(steps, Some(22));
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let fallen_bytes = parse_fallen_bytes(input_str);
    let mem = Memory::new(&fallen_bytes[..1024], 70, 70);
    let origin = Position { x: 0, y: 0 };
    let goal = Position { x: 70, y: 70 };
    let mut visited: FxHashMap<Position, usize> = Default::default();
    match part1_bfs(&mem, &origin, &goal, &mut visited) {
        Some(steps) => {
            println!("Day 18 part 1: {}", steps);
            if let Some(blocker) = part2(&fallen_bytes, &origin, &goal, 1024, 70, 70) {
                println!("Day 18 part 2: {}", blocker);
            } else {
                println!("Day 18 part 2: found no blocker");
            }
        }
        None => {
            panic!("Day 18: cannot solve part 1");
        }
    }
}
