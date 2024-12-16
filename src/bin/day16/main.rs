use lib::grid::{BoundingBox, CompassDirection, Position};
use lib::minheap::MinHeap;
use std::collections::{BTreeMap, HashSet};
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
fn sample_input() -> &'static str {
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

#[test]
fn parse_and_display() {
    let world: World = World::from(sample_input());
    let shown = world.to_string();
    assert_eq!(shown, sample_input());
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

impl<'a> Graph<'a> {
    fn vertices(&self) -> impl Iterator<Item = Node> + use<'_> {
        self.world.empty_tiles.iter().flat_map(|&pos| {
            ALL_DIRECTIONS.iter().map(move |orientation| Node {
                pos,
                orientation: *orientation,
            })
        })
    }

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
                .filter(|orientation| is_right_angle(*orientation, &n.orientation))
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

fn dijkstra(g: &Graph, source: Node) -> (BTreeMap<Node, Distance>, BTreeMap<Node, Node>) {
    let mut q: MinHeap<PrioritisedNode> = MinHeap::new();
    let mut prev: BTreeMap<Node, Node> = BTreeMap::new();
    let mut dist: BTreeMap<Node, Distance> = BTreeMap::new();

    dist.insert(source, 0);
    q.push(PrioritisedNode {
        priority: 0,
        node: source,
    });

    for vertex in g.vertices() {
        if vertex != source {
            q.push(PrioritisedNode {
                priority: Distance::MAX,
                node: vertex,
            });
        }
    }

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
            if alt < dist_v {
                prev.insert(v, u.node);
                dist.insert(v, alt);
                decrease_priority(&mut q, &v, alt);
            }
        }
    }
    (dist, prev)
}

fn decrease_priority(q: &mut MinHeap<PrioritisedNode>, v: &Node, distance: i64) {
    let original_len = q.len();
    let mut keep: MinHeap<PrioritisedNode> = MinHeap::with_capacity(q.capacity());
    let mut found = false;
    while let Some(n) = q.pop() {
        if &n.node == v {
            found = true;
            break;
        } else {
            keep.push(n);
        }
    }
    q.push(PrioritisedNode {
        priority: distance,
        node: *v,
    });
    q.append(&mut keep);
    assert!(keep.is_empty());
    if found {
        assert_eq!(q.len(), original_len);
    } else {
        assert_eq!(q.len(), original_len + 1);
    }
}

fn retrace_path<'a>(finish: &Node, start: &Node, prev: &'a BTreeMap<Node, Node>) -> Vec<&'a Node> {
    let mut result = Vec::new();
    let mut current: &Node = finish;
    while let Some(p) = prev.get(current) {
        result.push(p);
        if p == start {
            return result;
        }
        current = p;
    }
    panic!("cannot find path back to start");
}

fn part1(world: &World) -> i64 {
    let start = Node {
        pos: world.start,
        orientation: CompassDirection::East,
    };
    let graph = Graph { world };
    let (dists, preds) = dijkstra(&graph, start);

    assert!(dists.contains_key(&start));

    let final_node: &Node = dists
        .iter()
        .filter(|(node, _)| node.pos == world.exit)
        .map(|(node, dist)| (dist, node))
        .min()
        .map(|(_, node)| node)
        .expect("did not find solution");
    let distance = match dists.get(&final_node) {
        Some(d) => d,
        None => {
            panic!("don't know distance beween start and finish {final_node:?}");
        }
    };
    //let mut path = retrace_path(final_node, &start, &preds);
    //path.reverse();
    *distance
}

#[test]
fn test_part1_simplest_possible() {
    let world = World::from(concat!("####\n", "#SE#\n", "####\n",));
    assert_eq!(part1(&world), 1);
}

#[test]
fn test_part1() {
    let world = World::from(sample_input());
    let cost = part1(&world);
    assert_eq!(cost, 7036);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let world = World::from(input_str);
    println!("day 16 part 1: {}", part1(&world));
}
