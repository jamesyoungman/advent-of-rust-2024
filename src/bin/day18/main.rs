#[cfg(test)]
use std::cmp::min;
use std::collections::{HashSet, VecDeque};
use std::fmt::Debug;
use std::str;

use disjoint::DisjointSet;
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

fn part1(mem: &Memory, start: &Position, goal: &Position) -> Option<usize> {
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
    let mut visited: FxHashMap<Position, usize> = Default::default();
    bfs(start, neighbours, &mut visited, atgoal);
    steps
}

fn part2(
    fallen_bytes: &[Position],
    start: &Position,
    goal: &Position,
    xmax: i64,
    ymax: i64,
) -> Option<Position> {
    let mem = Memory::new(fallen_bytes, xmax, ymax);

    // Create a Position->usize mapping so that we can use the usize
    // values with the Disjoint Set.
    let idmap: FxHashMap<Position, usize> = mem
        .bbox
        .surface()
        .enumerate()
        .map(|(i, p)| (p, i))
        .collect();

    // We have established an invariant that every Position within the
    // area to be handled has an associated id.  So our lookup
    // function can never fail, as long as the positin is within
    // bounds.
    let find = |pos: &Position| -> usize {
        *idmap
            .get(pos)
            .expect("every position should already be included")
    };

    // Scan the map, joining every pair of adjacent cells when neither
    // of them is a wall.
    let mut rooms = DisjointSet::with_len(idmap.len());
    for (id, pos) in mem
        .bbox
        .surface()
        .filter(|pos| !mem.fallen_bytes.contains(pos))
        .map(|pos| (find(&pos), pos))
    {
        // This is not a wall.
        for neighbourid in mem.neighbours(&pos).into_iter().map(|p| find(&p)) {
            // neighbourid is also not a wall (because
            // Memory::neighbours() does not return walls).  We
            // know that these cells are part of the same room
            // because they are adjoining.
            rooms.join(id, neighbourid);
        }
    }
    // At this point we have a disjoint set identifying all the
    // "rooms" in the map after all bytes have fallen.
    let startid = find(start);
    let goalid = find(goal);
    if rooms.is_joined(startid, goalid) {
        panic!("after all bytes have falled, {goal} is still reachable from {start}");
    }

    // Remove all the falled bytes - importantly, in reverse order.
    for faller in fallen_bytes.iter().rev() {
        let fallerid = find(faller);
        // When `faller` turns from a wall into an empty square, this
        // has the effect of merging the newly empty cell with any
        // empty squares around it.
        for n in mem.neighbours(faller) {
            rooms.join(fallerid, find(&n));
        }
        // If, as a result, `start` and `goal` are part of the same
        // room, then the falling byte we just deleted must have been
        // the first (considering the order they actually fell, not
        // the reversed order) falling byte that caused `start` and
        // `goal` to be disconnected.
        if rooms.is_joined(startid, goalid) {
            return Some(*faller);
        }
    }
    None
}

#[test]
fn test_part1() {
    let start = Position { x: 0, y: 0 };
    let goal = Position { x: 6, y: 6 };
    let mem = parse_input(sample_input(), 6, 6, 12);
    let steps = part1(&mem, &start, &goal);
    assert_eq!(steps, Some(22));
}

#[test]
fn test_part2() {
    let fallen_bytes = parse_fallen_bytes(sample_input());
    let start = Position { x: 0, y: 0 };
    let goal = Position { x: 6, y: 6 };
    assert_eq!(
        part2(&fallen_bytes, &start, &goal, 6, 6),
        Some(Position { x: 6, y: 1 })
    );
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let fallen_bytes = parse_fallen_bytes(input_str);
    let xmax = 70;
    let ymax = 70;
    let mem = Memory::new(&fallen_bytes[..1024], xmax, ymax);
    let origin = Position { x: 0, y: 0 };
    let goal = Position { x: xmax, y: ymax };
    if let Some(steps) = part1(&mem, &origin, &goal) {
        println!("Day 18 part 1: {}", steps);
    } else {
        panic!("Day 18: cannot solve part 1");
    }
    if let Some(blocker) = part2(&fallen_bytes, &origin, &goal, xmax, ymax) {
        println!("Day 18 part 2: {}", blocker);
    } else {
        println!("Day 18 part 2: found no blocker");
    }
}
