use lib::grid::{BoundingBox, Position, ALL_MOVE_OPTIONS};
use std::collections::{HashMap, HashSet, VecDeque};
use std::str;

#[derive(Debug, PartialEq, Eq)]
struct Measurements {
    area: usize,
    perimeter: usize,
}

impl Measurements {
    fn total_price(&self) -> usize {
        self.area * self.perimeter
    }
}

fn count_true_transitions<I>(items: I) -> usize
where
    I: Iterator<Item = bool>,
{
    fn f((count, prev): (usize, bool), current: bool) -> (usize, bool) {
        let transition: usize = if current && !prev { 1 } else { 0 };
        (count + transition, current)
    }
    items.fold((0, false), f).0
}

#[derive(Debug, PartialEq, Eq)]
struct Plot {
    label: char,
    squares: HashSet<Position>,
    bbox: BoundingBox,
}

impl Plot {
    pub fn new(label: char, squares: HashSet<Position>) -> Plot {
        assert!(!squares.is_empty());
        if let Some(bbox) = make_bbox(squares.iter()) {
            Plot {
                label,
                squares,
                bbox,
            }
        } else {
            panic!("empty plot");
        }
    }

    pub fn analyze(&self) -> Measurements {
        let mut area = 0;
        let mut perimeter = 0;
        for pos in self.squares.iter() {
            area += 1;
            let neighbour_count = neighbours(pos, &self.squares).len();
            perimeter += 4 - neighbour_count;
        }
        let sides = self.count_sides();
        Measurements {
            area,
            perimeter,
            sides,
        }
    }

    fn attached_edge_count(&self, _pos: &Position) -> usize {
        let b = self.bbox.inflated_by(1, 1);
        let western_edges = count_true_transitions(b.columns().map(|x| {
            b.rows().map(|y| {
                let left = Position { x, y };
                let right = Position { x: x + 1, y };
                !self.squares.contains(&left) && self.squares.contains(&right)
            })
        }));
        let northern_edges = count_true_transitions(b.rows().map(|y| {
            b.cols().map(|x| {
                let upper = Position { x, y };
                let lower = Position { x, y: y + 1 };
                !self.squares.contains(&upper) && self.squares.contains(&lower)
            })
        }));
        western_edges * 2 + northern_edges * 2
    }

    fn count_sides(&self) -> usize {
        self.squares
            .iter()
            .map(|pos| self.attached_edge_count(pos))
            .sum()
>>>>>>> 1f25d84 (Day 12: still not working.)
    }
}

fn make_bbox<'a, I: Iterator<Item = &'a Position>>(squares: I) -> Option<BoundingBox> {
    squares.fold(None, |bbox, pos| match bbox {
        Some(mut b) => {
            b.update(pos);
            Some(b)
        }
        None => Some(BoundingBox::new(pos)),
    })
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Garden {
    layout: Vec<Vec<char>>,
}

fn bfs<FN, FB, IN>(
    start: &Position,
    mut neighbours: FN,
    visited: &mut HashSet<Position>,
    goal: FB,
) -> Vec<Position>
where
    FN: FnMut(Position) -> IN,     // enumerates neighbours
    IN: Iterator<Item = Position>, // iterates over neighbours
    FB: Fn(&Position) -> bool,     // are we at a goal?
{
    let mut result: Vec<Position> = Vec::new();
    let mut queue: VecDeque<Position> = VecDeque::from([*start]);
    while let Some(here) = queue.pop_front() {
        if !visited.insert(here) {
            continue;
        }
        if goal(&here) {
            result.push(here);
        }
        for neighbour in neighbours(here) {
            queue.push_back(neighbour);
        }
    }
    result
}

fn parse_input(input: &str) -> Garden {
    Garden {
        layout: input.lines().map(|line| line.chars().collect()).collect(),
    }
}

impl Garden {
    fn identify_plots(&self) -> Vec<Plot> {
        let mut affiliations: HashMap<char, HashSet<Position>> = HashMap::new();
        for (ch, pos) in self.layout.iter().enumerate().flat_map(|(y, row)| {
            row.iter().enumerate().map(move |(x, label)| {
                (
                    *label,
                    Position {
                        x: x as i64,
                        y: y as i64,
                    },
                )
            })
        }) {
            affiliations
                .entry(ch)
                .and_modify(|positions| {
                    positions.insert(pos);
                })
                .or_insert_with(|| {
                    let mut positions = HashSet::new();
                    positions.insert(pos);
                    positions
                });
        }

        let mut visited: HashSet<Position> = HashSet::new();
        let mut result = Vec::new();
        fn keep_all(_: &Position) -> bool {
            true
        }
        for (label, squares) in affiliations.into_iter() {
            if squares.is_empty() {
                panic!("unexpectedly, there are no squares with label {label}");
            }
            let todo: Vec<Position> = squares.iter().copied().collect();
            for here in todo {
                if !visited.contains(&here) {
                    //eprintln!("Label {label}: running BFS on {squares:?}...");
                    let get_neighbours = |pos: Position| compute_neighbours(pos, &squares);
                    let plot_squares: HashSet<Position> =
                        bfs(&here, get_neighbours, &mut visited, keep_all)
                            .into_iter()
                            .collect();
                    if plot_squares.is_empty() {
                        panic!("inconsistency with label {label}: bfs on position {here:?} yielded no squares at all");
                    }
                    result.push(Plot::new(label, plot_squares));
                }
            }
        }
        result
    }
}

#[cfg(test)]
fn sample_input_small() -> &'static str {
    concat!(
        "AAAA\n", // linebreak
        "BBCD\n", // linebreak
        "BBCC\n", // linebreak
        "EEEC\n",
    )
}

#[test]
fn test_parse_input() {
    assert_eq!(
        parse_input(sample_input_small()),
        Garden {
            layout: vec![
                vec!['A', 'A', 'A', 'A'],
                vec!['B', 'B', 'C', 'D'],
                vec!['B', 'B', 'C', 'C'],
                vec!['E', 'E', 'E', 'C'],
            ]
        }
    );
}

fn compute_neighbours(
    here: Position,
    squares: &HashSet<Position>,
) -> impl Iterator<Item = Position> + use<'_> {
    ALL_MOVE_OPTIONS
        .iter()
        .copied()
        .map(move |direction| {
            let newpos: Position = here.move_direction(&direction);
            newpos
        })
        .filter(|there| squares.contains(there))
}

fn neighbours(here: &Position, squares: &HashSet<Position>) -> Vec<Position> {
    compute_neighbours(*here, squares).collect()
}

#[test]
fn test_analyze_plot() {
    let plot = Plot::new(
        'x',
        [
            Position { x: 0, y: 0 },
            Position { x: 1, y: 0 },
            Position { x: 0, y: 1 },
            Position { x: 1, y: 1 },
        ]
        .into(),
    );
    assert_eq!(
        plot.analyze(),
        Measurements {
            area: 4,
            perimeter: 8,
        }
    );

    let plot = Plot::new(
        'Z',
        [Position { x: 0, y: 0 }, Position { x: 1, y: 0 }].into(),
    );
    assert_eq!(
        plot.analyze(),
        Measurements {
            area: 2,
            perimeter: 6,
        }
    );
}

fn part1(g: &Garden) -> usize {
    let plots = g.identify_plots();
    plots
        .iter()
        .map(|plot| plot.analyze())
        .map(|measurements| measurements.total_price())
        .sum()
}

#[test]
fn test_part1() {
    let garden: Garden = parse_input(sample_input_small());
    assert_eq!(part1(&garden), 140);
}

fn main() {
    let input: Garden = parse_input(str::from_utf8(include_bytes!("input.txt")).unwrap());
    println!("day 12 part 1: {}", part1(&input));
}
