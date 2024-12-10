use std::collections::{HashMap, HashSet, VecDeque};
use std::str;

use lib::grid::{Position, ALL_MOVE_OPTIONS};

#[derive(Eq, PartialEq, Debug, Hash)]
struct World {
    w: i64,
    h: i64,
    height: Vec<Vec<u8>>,
}

fn parse_input(input: &str) -> World {
    fn convert_char(ch: char) -> u8 {
        u8::try_from(
            ch.to_digit(10)
                .expect("topography should contain only decimal digits"),
        )
        .expect("decimal digits shoudl all fit into a u8")
    }

    fn convert_row(row: &str) -> Vec<u8> {
        row.chars().map(convert_char).collect()
    }

    let height: Vec<Vec<u8>> = input.lines().map(convert_row).collect();
    World {
        w: height.iter().map(|row| row.len()).max().unwrap_or(0) as i64,
        h: height.len() as i64,
        height,
    }
}

#[cfg(test)]
fn sample_input_1() -> &'static str {
    concat!(
        "0123\n", // don't join these lines
        "1234\n", // don't join these lines
        "8765\n", // don't join these lines
        "9876\n",
    )
}

#[cfg(test)]
fn sample_input_2() -> &'static str {
    concat!(
        "89010123\n",
        "78121874\n",
        "87430965\n",
        "96549874\n",
        "45678903\n",
        "32019012\n",
        "01329801\n",
        "10456732\n",
    )
}

#[test]
fn test_parse_world_1() {
    assert_eq!(
        parse_input(sample_input_1()),
        World {
            w: 4,
            h: 4,
            height: vec![
                vec![0, 1, 2, 3,],
                vec![1, 2, 3, 4,],
                vec![8, 7, 6, 5,],
                vec![9, 8, 7, 6,],
            ]
        }
    );
}

impl World {
    fn height_of(&self, pos: &Position) -> u8 {
        self.height[pos.y as usize][pos.x as usize]
    }

    fn downhill_neighbours_of(&self, pos: Position) -> impl Iterator<Item = Position> + use<'_> {
        let h: u8 = self.height_of(&pos);
        let height_wanted: u8 = h.wrapping_sub(1);
        ALL_MOVE_OPTIONS
            .iter()
            .filter(move |_| h > 0)
            .map(move |direction| pos.move_direction(direction))
            .filter(|pos| self.inbounds(&pos))
            .filter(move |neighbour_pos| {
                let neighbour_height = self.height_of(neighbour_pos);
                neighbour_height == height_wanted
            })
    }

    fn inbounds(&self, pos: &Position) -> bool {
        pos.x >= 0 && pos.y >= 0 && pos.y < self.h && pos.x < self.w
    }

    fn scan(&self) -> impl Iterator<Item = Position> + use<'_> {
        (0..self.h).flat_map(|y| {
            (0..self.w).map(move |x| Position {
                x: x as i64,
                y: y as i64,
            })
        })
    }

    fn summits(&self) -> Vec<Position> {
        self.scan().filter(|pos| self.height_of(pos) == 9).collect()
    }
}

#[test]
fn test_downhill_neighbours() {
    let world1 = parse_input(sample_input_1());
    let neighbour_count = |pos| world1.downhill_neighbours_of(pos).count();
    assert_eq!(neighbour_count(Position { x: 0, y: 0 }), 0);
    assert_eq!(neighbour_count(Position { x: 1, y: 1 }), 2);
}

#[test]
fn test_scan() {
    let world1 = parse_input(sample_input_1());
    let mut seen = HashSet::new();
    for pos in world1.scan() {
        assert!(pos.x >= 0);
        assert!(pos.x < 4);
        assert!(pos.y >= 0);
        assert!(pos.y < 4);
        if !seen.insert(pos) {
            panic!("scan visited {pos:?} twice");
        }
    }
    assert_eq!(seen.len(), 16);
}

#[test]
fn test_summits() {
    let world1 = parse_input(sample_input_1());
    assert_eq!(world1.summits(), vec![Position { x: 0, y: 3 }])
}

fn bfs<FN, FB, IN>(start: &Position, neighbours: FN, goal: FB) -> Vec<Position>
where
    FN: Fn(Position) -> IN,        // enumerates neighbours
    IN: Iterator<Item = Position>, // iterates over neighbours
    FB: Fn(&Position) -> bool,     // are we at a goal?
{
    let mut result: Vec<Position> = Vec::new();
    let mut visited: HashSet<Position> = HashSet::new();
    let mut queue: VecDeque<Position> = VecDeque::from([*start]);
    while let Some(here) = queue.pop_front() {
        if goal(&here) {
            result.push(here);
        }
        for neighbour in neighbours(here) {
            if visited.insert(neighbour) {
                // this neighbour not previously visited.
                queue.push_back(neighbour);
            }
        }
    }
    result
}

fn find_trailheads_reaching_summit(world: &World, summit: &Position) -> Vec<Position> {
    assert_eq!(world.height_of(summit), 9);
    let is_trailhead = |pos: &Position| world.height_of(pos) == 0;
    let neighbours = |pos: Position| world.downhill_neighbours_of(pos);
    bfs(summit, neighbours, is_trailhead)
}

#[test]
fn test_find_trailheads_reaching_summit() {
    let world = parse_input(sample_input_1());
    let summit = Position { x: 0, y: 3 };
    let trailheads = find_trailheads_reaching_summit(&world, &summit);
    assert_eq!(trailheads, vec![Position { x: 0, y: 0 }]);
}

fn score_all_trailheads(world: &World) -> HashMap<Position, usize> {
    let mut result: HashMap<Position, usize> = HashMap::new();
    let summits = world.summits();
    for summit in summits.iter() {
        for trailhead in find_trailheads_reaching_summit(world, summit) {
            result
                .entry(trailhead)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }
    }
    result
}

#[test]
fn test_score_all_trailheads() {
    let world = parse_input(sample_input_1());
    let scores: HashMap<Position, usize> = score_all_trailheads(&world);
    assert_eq!(scores.len(), 1);
    assert_eq!(scores.get(&Position { x: 0, y: 0 }), Some(&1));
}

fn part1(world: &World) -> usize {
    score_all_trailheads(world).values().sum()
}

#[test]
fn test_part1() {
    let world1 = parse_input(sample_input_1());
    assert_eq!(part1(&world1), 1);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let world = parse_input(input);
    println!("day 10 part 1: {}", part1(&world));
}
