use lib::grid::{BoundingBox, CompassDirection, Position};
use lib::minheap::MinHeap;
use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fmt::{Display, Formatter, Write};
use std::str;

type Distance = i64;

const TURN_COST: i64 = 1000;
const STEP_COST: i64 = 1;

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
struct Node {
    pos: Position,
    orientation: CompassDirection,
}

impl Node {
    fn edge_cost(&self, n: &Node) -> i64 {
        use CompassDirection::*;
        if self.pos == n.pos {
            match (self.orientation, n.orientation) {
                (begin, end) if begin == end => 0,
                (North | South, East | West) | (East | West, North | South) => TURN_COST,
                (begin, end) => {
                    panic!(
                        "{0:?}->{1:?}: cannot turn from facing {begin} to facing {end} in one move",
                        self, n
                    );
                }
            }
        } else if self.orientation == n.orientation {
            if self.pos.move_direction(&self.orientation) == n.pos {
                STEP_COST
            } else {
                panic!(
                    "can't get from {0} to {1} in one move if you are currently facing {2}",
                    self.pos, n.pos, self.orientation
                );
            }
        } else {
            panic!("cannot change both position and direction in a single move");
        }
    }
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Copy)]
struct PrioritisedNode {
    priority: Distance,
    node: Node,
}

#[derive(Debug)]
struct World {
    start: Position,
    exit: Position,
    bbox: BoundingBox,
    empty_tiles: HashSet<Position>,
}

impl Display for World {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for y in self.bbox.rows() {
            for x in self.bbox.columns() {
                let here = Position { x, y };
                f.write_char(if here == self.start {
                    'S'
                } else if here == self.exit {
                    'E'
                } else if self.empty_tiles.contains(&here) {
                    '.'
                } else {
                    '#'
                })?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl From<&str> for World {
    fn from(value: &str) -> Self {
        let mut empty_tiles = HashSet::new();
        let mut bbox = BoundingBox::new(&Position { x: 0, y: 0 });
        let mut start = None;
        let mut exit = None;
        for (y, line) in value.lines().enumerate() {
            for (x, ch) in line.chars().enumerate() {
                let here = Position {
                    x: x as i64,
                    y: y as i64,
                };
                bbox.update(&here);
                match ch {
                    '.' => {
                        empty_tiles.insert(here);
                    }
                    '#' => (),
                    'S' => {
                        empty_tiles.insert(here);
                        start = Some(here);
                    }
                    'E' => {
                        empty_tiles.insert(here);
                        exit = Some(here);
                    }
                    other => {
                        panic!("unrecognised character '{other}'");
                    }
                }
            }
        }
        match (start, exit) {
            (Some(start), Some(exit)) => World {
                start,
                exit,
                bbox,
                empty_tiles,
            },
            (None, _) => {
                panic!("map has no start");
            }
            (_, None) => {
                panic!("map has no exit");
            }
        }
    }
}

#[cfg(test)]
fn first_sample_input() -> &'static str {
    concat!(
        "###############\n",
        "#.......#....E#\n",
        "#.#.###.#.###.#\n",
        "#.....#.#...#.#\n",
        "#.###.#####.#.#\n",
        "#.#.#.......#.#\n",
        "#.#.#####.###.#\n",
        "#...........#.#\n",
        "###.#.#####.#.#\n",
        "#...#.....#.#.#\n",
        "#.#.#.###.#.#.#\n",
        "#.....#...#.#.#\n",
        "#.###.#.#.#.#.#\n",
        "#S..#.....#...#\n",
        "###############\n",
    )
}

#[cfg(test)]
fn second_sample_input() -> &'static str {
    concat!(
        "#################\n",
        "#...#...#...#..E#\n",
        "#.#.#.#.#.#.#.#.#\n",
        "#.#.#.#...#...#.#\n",
        "#.#.#.#.###.#.#.#\n",
        "#...#.#.#.....#.#\n",
        "#.#.#.#.#.#####.#\n",
        "#.#...#.#.#.....#\n",
        "#.#.#####.#.###.#\n",
        "#.#.#.......#...#\n",
        "#.#.###.#####.###\n",
        "#.#.#...#.....#.#\n",
        "#.#.#.#####.###.#\n",
        "#.#.#.........#.#\n",
        "#.#.#.#########.#\n",
        "#S#.............#\n",
        "#################\n",
    )
}

#[test]
fn parse_and_display() {
    let world: World = World::from(first_sample_input());
    let shown = world.to_string();
    assert_eq!(shown, first_sample_input());
}

#[derive(Debug)]
struct Graph<'a> {
    world: &'a World,
}

fn is_right_angle(a: &CompassDirection, b: &CompassDirection) -> bool {
    use CompassDirection::*;
    matches!(
        (a, b),
        (North | South, East | West) | (East | West, North | South)
    )
}

const ALL_DIRECTIONS: [CompassDirection; 4] = [
    CompassDirection::North,
    CompassDirection::East,
    CompassDirection::South,
    CompassDirection::West,
];

impl Graph<'_> {
    fn neighbours(&self, n: &Node) -> Vec<Node> {
        let mut result = Vec::with_capacity(5);
        let pos_after_step = n.pos.move_direction(&n.orientation);
        if self.world.empty_tiles.contains(&pos_after_step) {
            result.push(Node {
                pos: pos_after_step,
                orientation: n.orientation,
            })
        }
        result.extend(
            ALL_DIRECTIONS
                .iter()
                .filter(|orientation| is_right_angle(orientation, &n.orientation))
                .map(move |orientation| Node {
                    pos: n.pos,
                    orientation: *orientation,
                }),
        );
        //println!("neighbours of {n:?} are {result:?}");
        result
    }

    fn edge_cost(&self, n1: &Node, n2: &Node) -> i64 {
        n1.edge_cost(n2)
    }
}

struct ShortestPaths {
    distances: BTreeMap<Node, Distance>,
}

fn dijkstra(g: &Graph, source: Node) -> ShortestPaths {
    let mut q: MinHeap<PrioritisedNode> = MinHeap::new();
    let mut prev: BTreeMap<Node, BTreeSet<Node>> = BTreeMap::new();
    let mut dist: BTreeMap<Node, Distance> = BTreeMap::new();

    dist.insert(source, 0);
    q.push(PrioritisedNode {
        priority: 0,
        node: source,
    });

    let mut count = 0;
    while let Some(u) = q.pop() {
        if count % 1000 == 0 {
            println!("iteration {count:6}: PQ length is {0:6}", q.len());
        }
        count += 1;
        for v in g.neighbours(&u.node) {
            let cost = g.edge_cost(&u.node, &v);
            let alt: Distance = dist
                .get(&u.node)
                .unwrap_or(&Distance::MAX)
                .saturating_add(cost);
            let dist_v: Distance = *dist.get(&v).unwrap_or(&Distance::MAX);
            if alt <= dist_v {
                prev.entry(v)
                    .and_modify(|entry| {
                        entry.insert(u.node);
                    })
                    .or_insert_with(|| {
                        let mut s = BTreeSet::new();
                        s.insert(u.node);
                        s
                    });
                dist.insert(v, alt);
                q.push(PrioritisedNode {
                    priority: alt,
                    node: v,
                });
            }
        }
    }
    ShortestPaths { distances: dist }
}

fn find_shortest_paths(world: &World) -> (i64, Vec<Node>, ShortestPaths) {
    let start = Node {
        pos: world.start,
        orientation: CompassDirection::East,
    };
    let graph = Graph { world };
    let shortest_paths = dijkstra(&graph, start);

    assert!(shortest_paths.distances.contains_key(&start));

    let (final_nodes, mindist): (Vec<Node>, Distance) = {
        let mut nodes_by_dist: Vec<(Distance, Node)> = shortest_paths
            .distances
            .iter()
            .filter(|(node, _)| node.pos == world.exit)
            .map(|(node, dist)| (*dist, *node))
            .collect();
        nodes_by_dist.sort();
        match nodes_by_dist.iter().min() {
            Some(&(mindist, _)) => (
                nodes_by_dist
                    .iter()
                    .take_while(|(dist, _)| *dist == mindist)
                    .map(|(_, node)| *node)
                    .collect(),
                mindist,
            ),
            None => {
                panic!("did not find solution");
            }
        }
    };
    (mindist, final_nodes, shortest_paths)
}

fn find_tiles_on_best_route(
    world: &World,
    final_nodes: &[Node],
    forward_cost: Distance,
    shortest_paths: &ShortestPaths,
) -> BTreeSet<Position> {
    assert_eq!(final_nodes.len(), 1);
    let final_node = final_nodes
        .first()
        .expect("should be at least one final node");
    let reverse_start_node = Node {
        pos: final_node.pos,
        orientation: final_node.orientation.reversed(),
    };
    let shortest_reverse_paths = dijkstra(&Graph { world }, reverse_start_node);
    let best_path: BTreeSet<Position> = shortest_paths
        .distances
        .iter()
        .filter(|(forward_node, forward_dist)| {
            match shortest_reverse_paths.distances.get(&Node {
                pos: forward_node.pos,
                orientation: forward_node.orientation.reversed(),
            }) {
                Some(&reverse_dist) => reverse_dist + **forward_dist == forward_cost,
                None => false,
            }
        })
        .map(|(node, _)| node.pos)
        .collect();
    best_path
}

fn display_best_route<W: std::fmt::Write>(
    mut writer: W,
    world: &World,
    best: &BTreeSet<Position>,
) -> std::fmt::Result {
    for y in world.bbox.rows() {
        for x in world.bbox.columns() {
            let here = Position { x, y };
            writer.write_char(if best.contains(&here) {
                'O'
            } else if here == world.start {
                'S'
            } else if here == world.exit {
                'E'
            } else if world.empty_tiles.contains(&here) {
                '.'
            } else {
                '#'
            })?;
        }
        writer.write_char('\n')?;
    }
    Ok(())
}

fn part2(
    world: &World,
    final_nodes: &[Node],
    forward_cost: Distance,
    shortest_paths: &ShortestPaths,
) -> usize {
    let best = find_tiles_on_best_route(world, final_nodes, forward_cost, shortest_paths);
    let mut s = String::new();
    let _ = display_best_route(&mut s, world, &best);
    println!("{s}");
    best.len()
}

#[test]
fn test_find_shortest_paths_simplest_possible() {
    let world = World::from(concat!("####\n", "#SE#\n", "####\n",));
    let (distance, _final_node, _shortest_paths) = find_shortest_paths(&world);
    assert_eq!(distance, 1);
}

#[test]
fn test_find_shortest_paths_first_sample_input() {
    let world = World::from(first_sample_input());
    let (cost, _final_node, _shortest_paths) = find_shortest_paths(&world);
    assert_eq!(cost, 7036);
}

#[test]
fn test_find_shortest_paths_second_sample_input() {
    let world = World::from(second_sample_input());
    let (cost, _final_node, _shortest_paths) = find_shortest_paths(&world);
    assert_eq!(cost, 11048);
}

#[test]
fn test_part2_first_sample_input() {
    let world = World::from(first_sample_input());
    let (cost, final_nodes, shortest_paths) = find_shortest_paths(&world);
    let count = part2(&world, &final_nodes, cost, &shortest_paths);
    assert_eq!(count, 45);
}

#[test]
fn test_part2_second_sample_input() {
    let world = World::from(second_sample_input());
    let (cost, final_nodes, shortest_paths) = find_shortest_paths(&world);
    let count = part2(&world, &final_nodes, cost, &shortest_paths);
    assert_eq!(count, 64);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let world = World::from(input_str);
    let (cost, final_nodes, shortest_paths) = find_shortest_paths(&world);
    println!("day 16 part 1: {}", cost);
    let count = part2(&world, &final_nodes, cost, &shortest_paths);
    println!("day 16 part 1: {}", count);
}
