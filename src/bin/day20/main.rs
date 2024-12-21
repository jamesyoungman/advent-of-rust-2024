use std::collections::{BTreeSet, HashSet};
use std::fmt::{Debug, Display, Write};
use std::hash::Hash;
use std::str;

use rustc_hash::FxHashMap;

use lib::grid::{manhattan, BoundingBox, Position, ALL_MOVE_OPTIONS};
use lib::minheap::MinHeap;

type Node = Position;

type GenericMap<K, V> = FxHashMap<K, V>;
type Distance = i64;

#[derive(Debug, Eq, PartialEq)]
struct World {
    bbox: BoundingBox,
    walls: HashSet<Position>,
    origin: Position,
    goal: Position,
}

impl World {
    fn start_node(&self) -> Position {
        self.origin
    }

    fn at_goal(&self, n: &Node) -> bool {
        &self.goal == n
    }

    pub fn passable_neighbours(&self, current: &Node) -> impl Iterator<Item = Position> + use<'_> {
        self.neighbours_in_bbox(current)
            .filter(|pos| !self.walls.contains(pos))
    }

    fn neighbours_in_bbox(&self, current: &Node) -> impl Iterator<Item = Position> + use<'_> {
        let curr = *current;
        ALL_MOVE_OPTIONS.iter().filter_map(move |d| {
            let p = curr.move_direction(&d);
            if self.bbox.contains(&p) {
                Some(p)
            } else {
                None
            }
        })
    }

    fn distances_from_origin(&self) -> GenericMap<Position, Distance> {
        let neighbours = |n: &Node, _step: Distance| self.passable_neighbours(n).into_iter();
        let at_goal = |_, n: &Node| self.at_goal(n);

        fn fixed_edge_cost<'a, 'b>(n1: &'a Node, n2: &'b Node) -> Distance {
            assert!(n1.is_neighbour_of(&n2));
            1
        }
        dijkstra(self.start_node(), neighbours, fixed_edge_cost, at_goal)
    }
}

impl Display for World {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in self.bbox.rows() {
            for x in self.bbox.columns() {
                let here = Position { x, y };
                let marker: char = if self.origin == here {
                    'S'
                } else if self.goal == here {
                    'E'
                } else if self.walls.contains(&here) {
                    '#'
                } else {
                    '.'
                };
                f.write_char(marker)?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl From<&str> for World {
    fn from(value: &str) -> Self {
        let mut goal: Option<Position> = None;
        let mut origin: Option<Position> = None;
        let mut bbox = BoundingBox::new(&Position { x: 0, y: 0 });
        let mut walls: HashSet<_> = Default::default();
        for (y, line) in value.lines().enumerate() {
            for (x, ch) in line.chars().enumerate() {
                let here = Position {
                    x: x as i64,
                    y: y as i64,
                };
                bbox.update(&here);
                match ch {
                    'S' => {
                        origin = Some(here);
                    }
                    'E' => {
                        goal = Some(here);
                    }
                    '#' => {
                        walls.insert(here);
                    }
                    '.' => (),
                    other => {
                        panic!("unexpected character '{other}' in input");
                    }
                }
            }
        }
        match (goal, origin) {
            (Some(goal), Some(origin)) => World {
                goal,
                origin,
                walls,
                bbox,
            },
            (None, _) => {
                panic!("input contained no end position");
            }
            (_, None) => {
                panic!("input contained no start position");
            }
        }
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "###############\n",
        "#...#...#.....#\n",
        "#.#.#.#.#.###.#\n",
        "#S#...#.#.#...#\n",
        "#######.#.#.###\n",
        "#######.#.#...#\n",
        "#######.#.###.#\n",
        "###..E#...#...#\n",
        "###.#######.###\n",
        "#...###...#...#\n",
        "#.#####.#.###.#\n",
        "#.#...#.#.#...#\n",
        "#.#.#.#.#.#.###\n",
        "#...#...#...###\n",
        "###############\n",
    )
}

#[test]
fn test_parse_roundtrip() {
    let world = World::from(sample_input());
    let output = world.to_string();
    assert_eq!(output, sample_input());
}

#[test]
fn test_goal_dist_sample_input() {
    let world = World::from(sample_input());
    let distance_from_origin: GenericMap<Position, Distance> = world.distances_from_origin();
    assert_eq!(distance_from_origin.get(&world.start_node()), Some(&0));
    assert_eq!(distance_from_origin.get(&world.goal), Some(&84));
}

fn dijkstra<N, E, FN, FB, IN>(
    source: N,
    mut neighbours: FN,
    edge_cost: E,
    _goal: FB,
) -> GenericMap<N, Distance>
where
    N: Hash + Eq + PartialEq + Ord + Clone,
    FN: for<'a> FnMut(&'a N, Distance) -> IN, // enumerates neighbours
    E: for<'b> Fn(&'b N, &'b N) -> Distance,  // edge cost
    IN: Iterator<Item = N>,                   // iterates over neighbours
    FB: FnMut(usize, &N) -> bool,             // are we at a goal?
{
    let mut q: MinHeap<(Distance, N)> = MinHeap::new();
    let mut prev: GenericMap<N, BTreeSet<N>> = GenericMap::default();
    let mut dist: GenericMap<N, Distance> = GenericMap::default();

    dist.insert(source.clone(), 0);
    q.push((0, source));

    //let mut count = 0;
    while let Some((upri, u)) = q.pop() {
        //if count % 1000 == 0 {
        //    println!("iteration {count:6}: PQ length is {0:6}", q.len());
        //}
        //count += 1;
        for v in neighbours(&u, upri) {
            let cost = edge_cost(&u, &v);
            let alt: Distance = dist.get(&u).unwrap_or(&Distance::MAX).saturating_add(cost);
            let dist_v: Distance = *dist.get(&v).unwrap_or(&Distance::MAX);
            if alt <= dist_v {
                prev.entry(v.clone())
                    .and_modify(|entry| {
                        entry.insert(u.clone());
                    })
                    .or_insert_with(|| {
                        let mut s = BTreeSet::new();
                        s.insert(u.clone());
                        s
                    });
                dist.insert(v.clone(), alt);
                q.push((alt, v));
            }
        }
    }
    dist
}

fn all_points_within_manhattan_dist_of<'a>(
    pos: Position,
    dist: i64,
    bbox: &'a BoundingBox,
) -> impl Iterator<Item = Position> + use<'a> {
    assert!(dist >= 0);
    (0..=dist).flat_map(move |ytravel| {
        let remaining = dist - ytravel;
        (0..=remaining).flat_map(move |xtravel| {
            // We Use a set here to avoid duplication if xtravel or
            // ytravel is 0.
            let candidates: HashSet<Position> = HashSet::from([
                Position {
                    x: pos.x + xtravel,
                    y: pos.y + ytravel,
                },
                Position {
                    x: pos.x + xtravel,
                    y: pos.y - ytravel,
                },
                Position {
                    x: pos.x - xtravel,
                    y: pos.y + ytravel,
                },
                Position {
                    x: pos.x - xtravel,
                    y: pos.y - ytravel,
                },
            ]);
            candidates.into_iter().filter(|pos| bbox.contains(&pos))
        })
    })
}

#[test]
fn test_all_points_within_manhattan_dist_0_of() {
    let here = Position { x: 10, y: 4 };
    let limit = BoundingBox::from_corners(&Position { x: 0, y: 0 }, &Position { x: 100, y: 100 });
    let reachable: Vec<Position> = all_points_within_manhattan_dist_of(here, 0, &limit).collect();
    assert_eq!(1, reachable.len());
    assert_eq!(reachable.as_slice(), [here]);
}

#[test]
fn test_all_points_within_manhattan_dist_1_of() {
    let here = Position { x: 10, y: 4 };
    let limit = BoundingBox::from_corners(&Position { x: 0, y: 0 }, &Position { x: 100, y: 100 });
    let reachable: HashSet<Position> =
        all_points_within_manhattan_dist_of(here, 1, &limit).collect();
    assert_eq!(5, reachable.len());
    assert!(reachable.contains(&here));
    assert!(reachable.contains(&Position { x: 10, y: 3 })); // moved north
    assert!(reachable.contains(&Position { x: 11, y: 4 })); // moved east
    assert!(reachable.contains(&Position { x: 10, y: 5 })); // moved south
    assert!(reachable.contains(&Position { x: 9, y: 4 })); // moved west
}

#[test]
fn test_all_points_within_manhattan_dist_2_of() {
    let here = Position { x: 0, y: 0 };
    // Limit results to the +x, +y quadrant only
    let limit = BoundingBox::from_corners(&Position { x: 0, y: 0 }, &Position { x: 10, y: 10 });
    let reachable: HashSet<Position> =
        all_points_within_manhattan_dist_of(here, 2, &limit).collect();
    assert_eq!(reachable.len(), 6);
    assert!(reachable.contains(&Position { x: 0, y: 0 }));

    assert!(reachable.contains(&Position { x: 1, y: 0 }));
    assert!(reachable.contains(&Position { x: 2, y: 0 }));

    assert!(reachable.contains(&Position { x: 0, y: 1 }));
    assert!(reachable.contains(&Position { x: 0, y: 2 }));

    assert!(reachable.contains(&Position { x: 1, y: 1 }));
}

fn find_number_of_cheats(world: &World, min_saving: i64, max_cheat: i64) -> usize {
    assert!(min_saving >= 0);
    assert!(max_cheat >= 0);
    let distance_from_origin: GenericMap<Position, Distance> = world.distances_from_origin();

    // We now need to enumerate all pairs of points which aren't walls
    // and between which we can move within the cheat time, where the
    // saving with the cheat would be at least 100 picoseconds.
    //
    // Let's denote a candidate pair as (s, d).  For this pair to have
    // a saving of at least 100 picoseconds, the distance for (s,
    // goal) must itself be < 100 picoseconds.  Similarly, if the
    // distance for (origin, d) is less than 100 picoseconds, we can't
    // save 100.
    assert_eq!(distance_from_origin.get(&world.origin), Some(&0));

    let distance_between_points = |from: &Position, to: &Position| -> Option<i64> {
        match (distance_from_origin.get(from), distance_from_origin.get(to)) {
            (Some(a), Some(b)) => {
                // This takes advantage of the fact that there is only
                // one route through the maze, meaning that two points
                // having the same distance from the origin must be
                // the same point.
                Some(b - a)
            }
            (None, _) | (_, None) => None,
        }
    };

    let possible_cheat_endpoints: Vec<Position> = distance_from_origin
        .iter()
        .filter_map(|(pos, dist_from_origin)| {
            if *dist_from_origin > min_saving {
                Some(pos)
            } else {
                None
            }
        })
        .copied()
        .collect();
    let possible_cheats: Vec<(Position, Position, i64)> = possible_cheat_endpoints
        .iter()
        .flat_map(|end| {
            all_points_within_manhattan_dist_of(*end, max_cheat, &world.bbox)
                .map(move |start| (start, end))
        })
        .filter_map(|(start, end)| {
            let cheat_travel_time = manhattan(&start, &end);
            match distance_between_points(&start, end) {
                Some(long_way_around) => {
                    let saving = long_way_around - cheat_travel_time;
                    if saving >= min_saving {
                        Some((start, *end, saving))
                    } else {
                        None
                    }
                }
                None => None,
            }
        })
        .collect();
    possible_cheats.len()
}

#[test]
fn test_find_number_of_cheats() {
    let world = World::from(sample_input());

    // According to the text of the question, there is one cheat that
    // saves 64 picoseconds.
    assert_eq!(find_number_of_cheats(&world, 64, 2), 1);

    // There is 1 cheat that saves exactly 40, hence 2 that save at
    // least 40 (the one that saves 40 and the one that saves 64).
    assert_eq!(find_number_of_cheats(&world, 40, 2), 2);

    // There is 1 cheat that saves exactly 38, hence 3 that save at
    // least 38.
    assert_eq!(find_number_of_cheats(&world, 38, 2), 3);

    // There is 1 cheat that saves exactly 36, hence 4 that save at
    // least 36.
    assert_eq!(find_number_of_cheats(&world, 36, 2), 4);

    // There is 1 cheat that saves exactly 20, hence 5 that save at
    // least 20.
    assert_eq!(find_number_of_cheats(&world, 20, 2), 5);

    // There are 3 cheats that save ewxactly 12, hence 8 that save at
    // least 12.
    assert_eq!(find_number_of_cheats(&world, 12, 2), 8);
}

fn part1(world: &World) -> usize {
    find_number_of_cheats(world, 100, 2)
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let world = World::from(input_str);
    println!("Day 20 part 1: {}", part1(&world));
}
