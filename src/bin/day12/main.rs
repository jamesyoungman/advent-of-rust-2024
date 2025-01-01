use lib::grid::{BoundingBox, Position, ALL_MOVE_OPTIONS};
use std::collections::{HashMap, HashSet, VecDeque};
use std::str;

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

    fn neighbours(&self, here: Position) -> impl Iterator<Item = Position> + use<'_> {
        compute_neighbours(here, &self.squares)
    }

    pub fn area(&self) -> usize {
        self.squares.len()
    }

    pub fn perimeter(&self) -> usize {
        self.squares
            .iter()
            .map(|pos| 4 - self.neighbours(*pos).count())
            .sum()
    }

    fn count_corners_here(&self, here: &Position) -> usize {
        let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
            let there = Position {
                x: here.x + dx,
                y: here.y + dy,
            };
            self.squares.contains(&there)
        };
        [
            is_northeast_corner(*here, same_crop),
            is_southeast_corner(*here, same_crop),
            is_southwest_corner(*here, same_crop),
            is_northwest_corner(*here, same_crop),
        ]
        .into_iter()
        .filter(|x| *x)
        .count()
    }

    pub fn total_sides(&self) -> usize {
        self.squares
            .iter()
            .map(|pos| self.count_corners_here(pos))
            .sum()
    }

    pub fn total_discounted_price(&self) -> usize {
        self.area() * self.total_sides()
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
            for here in squares.iter() {
                if !visited.contains(here) {
                    let get_neighbours = |pos: Position| compute_neighbours(pos, &squares);
                    let plot_squares: HashSet<Position> =
                        bfs(here, get_neighbours, &mut visited, keep_all)
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
    assert_eq!(plot.area(), 4);
    assert_eq!(plot.perimeter(), 8);

    let plot = Plot::new(
        'Z',
        [Position { x: 0, y: 0 }, Position { x: 1, y: 0 }].into(),
    );
    assert_eq!(plot.area(), 2);
    assert_eq!(plot.perimeter(), 6);
}

fn part1(g: &Garden) -> usize {
    let plots = g.identify_plots();
    plots
        .iter()
        .map(|plot| plot.area() * plot.perimeter())
        .sum()
}

#[test]
fn test_part1() {
    let garden: Garden = parse_input(sample_input_small());
    assert_eq!(part1(&garden), 140);
}

fn is_northeast_corner<F>(here: Position, same_crop: F) -> bool
where
    F: Fn(Position, i64, i64) -> bool,
{
    // Cases:
    //
    // Here, we consider the "O" at the bottom-left of the example.  Other
    // squares marked "O" have the same crop, and squares marked "x"
    // have a crop different to O (and not necessarily the same as
    // each other).  The contents of squares marked "."  are not
    // material.
    //
    // Inside corner:
    //
    // Ox
    // OO
    //
    // Outside corner:
    //
    // x.
    // Ox
    //
    // We start not knowing the state of any surrounding squares:
    //
    // ??
    // O?
    //
    let north_same = || same_crop(here, 0, -1);
    let north_east_same = || same_crop(here, 1, -1);
    let east_same = || same_crop(here, 1, 0);

    if north_same() {
        // O?
        // O?
        if east_same() {
            // O?
            // OO
            if north_east_same() {
                // OO
                // OO
                false
            } else {
                // Ox
                // OO
                true // inside corner
            }
        } else {
            // O?
            // Ox
            false
        }
    } else {
        // x?
        // O?
        if east_same() {
            // x?
            // OO
            false
        } else {
            // x?
            // Ox
            true
        }
    }
}

fn is_southeast_corner<F>(here: Position, same_crop: F) -> bool
where
    F: Fn(Position, i64, i64) -> bool,
{
    let same = |here: Position, dx: i64, dy: i64| -> bool { same_crop(here, -dy, dx) };
    is_northeast_corner(here, same)
}

fn is_southwest_corner<F>(here: Position, same_crop: F) -> bool
where
    F: Fn(Position, i64, i64) -> bool,
{
    let same = |here: Position, dx: i64, dy: i64| -> bool { same_crop(here, -dx, -dy) };
    is_northeast_corner(here, same)
}

fn is_northwest_corner<F>(here: Position, same_crop: F) -> bool
where
    F: Fn(Position, i64, i64) -> bool,
{
    let same = |here: Position, dx: i64, dy: i64| -> bool { same_crop(here, -dx, dy) };
    is_northeast_corner(here, same)
}

#[test]
fn test_is_ne_corner() {
    let garden = parse_input(sample_input_small());
    let plots: Vec<Plot> = garden.identify_plots();
    for plot in plots {
        let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
            let there = Position {
                x: here.x + dx,
                y: here.y + dy,
            };
            plot.squares.contains(&there)
        };
        match plot.label {
            'A' => {
                assert!(!is_northeast_corner(Position { x: 0, y: 0 }, same_crop));
            }
            'B' => {
                assert!(is_northeast_corner(Position { x: 1, y: 1 }, same_crop));
                // B (outside)
            }
            'C' => {
                assert!(is_northeast_corner(Position { x: 2, y: 1 }, same_crop)); // C (outside)
                assert!(is_northeast_corner(Position { x: 3, y: 2 }, same_crop)); // C (outside)
                assert!(is_northeast_corner(Position { x: 2, y: 2 }, same_crop)); // C (inside)
                assert!(!is_northeast_corner(Position { x: 3, y: 3 }, same_crop));
                // it's a se+sw corner but not ne
            }
            'D' => {
                assert!(is_northeast_corner(Position { x: 3, y: 1 }, same_crop));
                // D (outside)
            }
            'E' => (),
            other => {
                panic!("unexpected plot label {other}'");
            }
        }
    }
}

#[test]
fn test_is_nw_corner() {
    let garden = parse_input(sample_input_small());
    let plots: Vec<Plot> = garden.identify_plots();
    for plot in plots {
        let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
            let there = Position {
                x: here.x + dx,
                y: here.y + dy,
            };
            plot.squares.contains(&there)
        };
        match plot.label {
            'A' => {
                assert!(
                    is_northwest_corner(Position { x: 0, y: 0 }, same_crop),
                    "0,0 should be a NW corner for A"
                );
            }
            'B' => {
                assert!(
                    is_northwest_corner(Position { x: 0, y: 1 }, same_crop),
                    "0,1 should be a NW corner for B"
                );
                assert!(
                    !is_northwest_corner(Position { x: 0, y: 2 }, same_crop),
                    "0,2 should not be a NW corner"
                );
            }
            'C' | 'D' | 'E' => (),
            other => {
                panic!("unexpected plot label {other}'");
            }
        }
    }
}

#[test]
fn test_isolated_square_corners() {
    let garden = parse_input(concat!(
        "AAA\n", // do not eliminate line break
        "ABA\n", // do not eliminate line break
        "AAA\n",
    ));
    let plots: Vec<Plot> = garden.identify_plots();
    for plot in plots {
        let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
            let there = Position {
                x: here.x + dx,
                y: here.y + dy,
            };
            plot.squares.contains(&there)
        };
        match plot.label {
            'B' => {
                let centre = Position { x: 1, y: 1 };
                assert!(is_northwest_corner(centre, same_crop));
                assert!(is_northeast_corner(centre, same_crop));
                assert!(is_southeast_corner(centre, same_crop));
                assert!(is_southwest_corner(centre, same_crop));
            }
            _ => (),
        }
    }
}

#[test]
fn test_inside_corners() {
    let garden = parse_input(concat!(
        "AAA\n", // do not eliminate line break
        "ABA\n", // do not eliminate line break
        "AAA\n",
    ));
    let plots: Vec<Plot> = garden.identify_plots();
    for plot in plots {
        let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
            let there = Position {
                x: here.x + dx,
                y: here.y + dy,
            };
            plot.squares.contains(&there)
        };
        match plot.label {
            'A' => {
                let ne_pos = Position { x: 2, y: 0 };
                assert!(is_southwest_corner(ne_pos, same_crop));
                assert!(is_northeast_corner(ne_pos, same_crop));

                let se_pos = Position { x: 2, y: 2 };
                assert!(is_northwest_corner(se_pos, same_crop));
                assert!(is_southeast_corner(se_pos, same_crop));

                let sw_pos = Position { x: 0, y: 2 };
                assert!(is_northeast_corner(sw_pos, same_crop));
                assert!(is_southwest_corner(sw_pos, same_crop));

                let nw_pos = Position { x: 0, y: 0 };
                assert!(is_southeast_corner(nw_pos, same_crop));
                assert!(is_northwest_corner(nw_pos, same_crop));
            }
            'B' => (),
            other => {
                panic!("unexpected plot label {other}'");
            }
        }
    }
}

#[test]
fn test_is_se_corner() {
    let garden = parse_input(sample_input_small());
    let plot_a: Plot = garden
        .identify_plots()
        .into_iter()
        .find(|plot| plot.label == 'A')
        .expect("there is a plot A");
    let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
        let there = Position {
            x: here.x + dx,
            y: here.y + dy,
        };
        plot_a.squares.contains(&there)
    };
    assert!(!is_southeast_corner(Position { x: 0, y: 0 }, same_crop));
    assert!(!is_southeast_corner(Position { x: 1, y: 0 }, same_crop));
    assert!(!is_southeast_corner(Position { x: 2, y: 0 }, same_crop));
    assert!(is_southeast_corner(Position { x: 3, y: 0 }, same_crop));
}

#[test]
fn test_is_sw_corner() {
    let garden = parse_input(sample_input_small());
    let plots: Vec<Plot> = garden.identify_plots();
    assert_eq!(plots.len(), 5);
    for plot in plots {
        let same_crop = |here: Position, dx: i64, dy: i64| -> bool {
            let there = Position {
                x: here.x + dx,
                y: here.y + dy,
            };
            plot.squares.contains(&there)
        };
        match plot.label {
            'A' => {
                assert!(is_southwest_corner(Position { x: 0, y: 0 }, same_crop));
                // A (outside)
            }
            'B' => {
                assert!(is_southwest_corner(Position { x: 0, y: 2 }, same_crop)); // B (outside)
                assert!(!is_southwest_corner(Position { x: 0, y: 1 }, same_crop));
            }
            'C' | 'D' => (),
            'E' => {
                assert!(is_southwest_corner(Position { x: 0, y: 3 }, same_crop));
                // E (outside)
            }
            other => {
                panic!("unexpected plot label {other}'");
            }
        }
    }
}

#[test]
fn test_edge_noncorners() {
    let garden = parse_input(concat!(
        "AAA\n", // do not eliminate line break
        "ABA\n", // do not eliminate line break
        "AAA\n",
    ));
    let plots: Vec<Plot> = garden.identify_plots();
    assert_eq!(plots.len(), 2);
    for plot in plots {
        match plot.label {
            'A' => {
                assert_eq!(plot.count_corners_here(&Position { x: 0, y: 1 }), 0); // west centre
                assert_eq!(plot.count_corners_here(&Position { x: 2, y: 1 }), 0); // east centre
                assert_eq!(plot.count_corners_here(&Position { x: 1, y: 0 }), 0); // north centre
                assert_eq!(plot.count_corners_here(&Position { x: 1, y: 2 }), 0);
                // south centre
            }
            'B' => (),
            other => {
                panic!("unexpected plot label {other}'");
            }
        }
    }
}

#[test]
fn test_eeeee_count_sides() {
    let garden: Garden = parse_input(concat!(
        "EEEEE\n", // retain line break
        "EXXXX\n", // retain line break
        "EEEEE\n", // retain line break
        "EXXXX\n", // retain line break
        "EEEEE\n",
    ));
    let plots: Vec<Plot> = garden.identify_plots();
    for plot in plots {
        if plot.label == 'E' {
            assert_eq!(plot.total_sides(), 12);
            assert_eq!(plot.area(), 17);
        } else {
            assert_eq!(plot.label, 'X');
        }
    }
}

#[test]
fn test_part2_e() {
    let garden: Garden = parse_input(concat!(
        "EEEEE\n", // retain line break
        "EXXXX\n", // retain line break
        "EEEEE\n", // retain line break
        "EXXXX\n", // retain line break
        "EEEEE\n",
    ));
    assert_eq!(part2(&garden), 236);
}

#[test]
fn test_part2_aaaaaa() {
    let garden: Garden = parse_input(concat!(
        "AAAAAA\n", // retain line break
        "AAABBA\n", // retain line break
        "AAABBA\n", // retain line break
        "ABBAAA\n", // retain line break
        "ABBAAA\n", // retain line break
        "AAAAAA\n", // retain line break
    ));
    assert_eq!(part2(&garden), 368);
}

#[test]
fn test_count_sides() {
    let garden: Garden = parse_input(concat!(
        "AAAA\n", // retain line break
        "BBCD\n", // retain line break
        "BBCC\n", // retain line break
        "EEEC\n", // retain line break
    ));
    let plots = garden.identify_plots();
    assert_eq!(plots.len(), 5);
    for plot in plots {
        match plot.label {
            'A' | 'B' | 'D' | 'E' => {
                assert_eq!(
                    plot.total_sides(),
                    4,
                    "wrong number of sides for plot {}",
                    plot.label
                );
            }
            'C' => {
                assert_eq!(plot.total_sides(), 8);
            }
            other => {
                panic!("unexpected plot label {other}'");
            }
        }
    }
}

fn part2(garden: &Garden) -> usize {
    let plots = garden.identify_plots();
    plots.iter().map(|plot| plot.total_discounted_price()).sum()
}

fn main() {
    let input: Garden = parse_input(str::from_utf8(include_bytes!("input.txt")).unwrap());
    println!("day 12 part 1: {}", part1(&input));
    println!("day 12 part 2: {}", part2(&input));
}
