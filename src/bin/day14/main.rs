use std::collections::{HashMap, HashSet};
use std::hash::Hash;
use std::str;

use regex::Regex;

use lib::grid::{BoundingBox, Movement, Position, ALL_MOVE_OPTIONS};
use lib::parse::parse_number;

fn count_things_hm<I, T>(things: I) -> HashMap<T, usize>
where
    I: Iterator<Item = T>,
    T: Hash + PartialEq + Eq,
{
    let mut counts = HashMap::new();
    for thing in things {
        counts
            .entry(thing)
            .and_modify(|count| *count += 1)
            .or_insert(1);
    }
    counts
}

#[derive(Debug, PartialEq, Eq)]
struct Robot {
    // In this puzzle, or at least in the examples, the +y direction
    // is down the screen, meaning that Position{x:0,y:0} is the
    // top-left.
    initial_position: Position,
    velocity: Movement,
}

#[derive(Debug, PartialEq, Eq, Hash)]
enum Quadrant {
    NE,
    SE,
    SW,
    NW,
}

fn quadrant(pos: &Position, width: i64, height: i64) -> Option<Quadrant> {
    use Quadrant::*;
    let centre = Position {
        x: width / 2,
        y: height / 2,
    };
    let y_sign = pos.y - centre.y;
    let x_sign = pos.x - centre.x;
    if x_sign == 0 || y_sign == 0 {
        return None;
    }
    if x_sign == 0 || y_sign == 0 {
        return None;
    }

    // Remember, (0,0) is the top-left.  So y-values for the NE, NW
    // quadrants are smaller than those for the SE,SW quadrants.
    #[allow(clippy::collapsible_else_if)]
    let q: Quadrant = if x_sign < 0 {
        if y_sign < 0 {
            NW
        } else {
            SW
        }
    } else {
        if y_sign < 0 {
            NE
        } else {
            SE
        }
    };
    Some(q)
}

#[test]
fn test_quadrant() {
    use Quadrant::*;
    assert_eq!(quadrant(&Position { x: 0, y: 0 }, 3, 3), Some(NW));
    assert_eq!(quadrant(&Position { x: 2, y: 0 }, 3, 3), Some(NE));
    assert_eq!(quadrant(&Position { x: 2, y: 2 }, 3, 3), Some(SE));
    assert_eq!(quadrant(&Position { x: 0, y: 2 }, 3, 3), Some(SW));
    assert_eq!(quadrant(&Position { x: 0, y: 1 }, 3, 3), None);
    assert_eq!(quadrant(&Position { x: 1, y: 0 }, 3, 3), None);
}

fn wrapped(mut val: i64, limit: i64) -> i64 {
    while val < 0 {
        val += limit * 1000; // ugh, low tech, FIXME
    }
    let result = val % limit;
    assert!(result >= 0);
    assert!(result < limit);
    result
}

#[test]
fn test_wrapped() {
    assert_eq!(wrapped(0, 1), 0);
    assert_eq!(wrapped(1, 1), 0);
    assert_eq!(wrapped(-1, 1), 0);
    assert_eq!(wrapped(0, 2), 0);
    assert_eq!(wrapped(1, 2), 1);
    assert_eq!(wrapped(2, 2), 0);
}

impl Robot {
    fn position_at(&self, time: i64, width: i64, height: i64) -> Position {
        let position = self.initial_position + (self.velocity * time);
        Position {
            x: wrapped(position.x, width),
            y: wrapped(position.y, height),
        }
    }

    fn next_position(&self, current: &Position, width: i64, height: i64) -> Position {
        Position {
            x: wrapped(current.x + self.velocity.dx, width),
            y: wrapped(current.y + self.velocity.dy, height),
        }
    }
}

#[test]
fn test_position_at() {
    let w = 11;
    let h = 7;
    let robot = Robot {
        initial_position: Position { x: 2, y: 4 },
        velocity: Movement { dx: 2, dy: -3 },
    };
    assert_eq!(
        robot.position_at(0, w, h),
        Position { x: 2, y: 4 },
        "wrong position at t=0"
    );
    assert_eq!(
        robot.position_at(1, w, h),
        Position { x: 4, y: 1 },
        "wrong position at t=1"
    );
    assert_eq!(
        robot.position_at(2, w, h),
        Position { x: 6, y: 5 },
        "wrong position at t=2"
    );
    assert_eq!(
        robot.position_at(3, w, h),
        Position { x: 8, y: 2 },
        "wrong position at t=3"
    );
    assert_eq!(
        robot.position_at(4, w, h),
        Position { x: 10, y: 6 },
        "wrong position at t=4"
    );
    assert_eq!(
        robot.position_at(5, w, h),
        Position { x: 1, y: 3 },
        "wrong position at t=5"
    );
}

fn robot_positions(
    robots: &[Robot],
    time: i64,
    width: i64,
    height: i64,
) -> impl Iterator<Item = Position> + use<'_> {
    robots
        .iter()
        .map(move |robot| robot.position_at(time, width, height))
}

fn next_robot_positions(
    robots: &[Robot],
    positions: &[Position],
    width: i64,
    height: i64,
) -> Vec<Position> {
    robots
        .iter()
        .zip(positions.iter())
        .map(|(r, pos)| r.next_position(pos, width, height))
        .collect()
}

fn part1(robots: &[Robot], width: i64, height: i64, verbose: bool) -> usize {
    fn reducer(
        mut accumulator: HashMap<Option<Quadrant>, usize>,
        quadrant: Option<Quadrant>,
    ) -> HashMap<Option<Quadrant>, usize> {
        accumulator
            .entry(quadrant)
            .and_modify(|count| {
                *count += 1;
            })
            .or_insert(1);
        accumulator
    }
    let positions: Vec<Position> = robot_positions(robots, 100, width, height).collect();
    if verbose {
        eprintln!(
            "positions at time 100:\n{}",
            draw_positions(positions.iter().copied())
        );
    }
    let counts_by_quadrant: HashMap<Option<Quadrant>, usize> = positions
        .iter()
        .map(|pos| quadrant(pos, width, height))
        .fold(HashMap::new(), reducer);
    assert_eq!(
        counts_by_quadrant.values().sum::<usize>(),
        robots.len(),
        "we should not gain or lose robots"
    );
    counts_by_quadrant
        .iter()
        // Drop the counts for quadrant=None.
        .filter_map(|(q, count)| if q.is_some() { Some(count) } else { None })
        .product()
}

fn draw_positions<I>(positions: I) -> String
where
    I: Iterator<Item = Position>,
{
    let positions = count_things_hm(positions);
    match BoundingBox::containing(positions.keys()) {
        None => "".to_string(),
        Some(bbox) => {
            let mut output = String::new();
            for y in bbox.rows() {
                for x in bbox.columns() {
                    let here = Position { x, y };
                    match positions.get(&here) {
                        None | Some(0) => {
                            output.push('.');
                        }
                        Some(n) => {
                            let s = n.to_string();
                            if s.len() > 1 {
                                output.push('!');
                            } else {
                                output.push_str(&s);
                            }
                        }
                    }
                }
                output.push('\n');
            }
            output
        }
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "p=0,4 v=3,-3\n",
        "p=6,3 v=-1,-3\n",
        "p=10,3 v=-1,2\n",
        "p=2,0 v=2,-1\n",
        "p=0,0 v=1,3\n",
        "p=3,0 v=-2,-2\n",
        "p=7,6 v=-1,-3\n",
        "p=3,0 v=-1,-2\n",
        "p=9,3 v=2,3\n",
        "p=7,3 v=-1,2\n",
        "p=2,4 v=2,-3\n",
        "p=9,5 v=-3,-3\n",
    )
}

struct Parser {
    line_re: Regex,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            line_re: Regex::new(r"^p=(-?\d+),(-?\d+) v=(-?\d+),(-?\d+)$").unwrap(),
        }
    }

    fn parse_line(&self, line: &str) -> Robot {
        if let Some(m) = self.line_re.captures(line) {
            let (_, [x, y, dx, dy]) = m.extract();
            Robot {
                initial_position: Position {
                    x: parse_number(x),
                    y: parse_number(y),
                },
                velocity: Movement {
                    dx: parse_number(dx),
                    dy: parse_number(dy),
                },
            }
        } else {
            panic!("unrecognised input line {line}");
        }
    }

    pub fn parse_input(&self, input: &str) -> Vec<Robot> {
        input.lines().map(|line| self.parse_line(line)).collect()
    }
}

#[test]
fn test_parse_input() {
    let parser = Parser::new();
    let got = parser.parse_input(sample_input());
    assert_eq!(
        got[0],
        Robot {
            initial_position: Position { x: 0, y: 4 },
            velocity: Movement { dx: 3, dy: -3 }
        }
    );
}

#[test]
fn test_part1() {
    let parser = Parser::new();
    let robots = parser.parse_input(sample_input());
    assert_eq!(part1(&robots, 11, 7, true), 12);
}

fn part2(robots: &[Robot], w: i64, h: i64) {
    let mut positions: Vec<Position> = robots.iter().map(|r| r.initial_position).collect();
    for time in 0..(w * h + 1) {
        let updated_positions: Vec<Position> = next_robot_positions(robots, &positions, w, h);
        let hit = maybe_xmas_tree(updated_positions.iter().copied());
        if hit {
            let pic = draw_positions(updated_positions.iter().copied());
            println!("possible tree at time {time}:\n{pic}");
        }
        positions = updated_positions;
    }
}

fn maybe_xmas_tree<I: Iterator<Item = Position>>(positions: I) -> bool {
    let occupied: HashSet<Position> = positions.collect();

    fn has_at_least_n_neighbours(
        pos: &Position,
        min_neighbours: usize,
        occupied: &HashSet<Position>,
    ) -> bool {
        min_neighbours == 0
            || ALL_MOVE_OPTIONS
                .iter()
                .map(|direction| pos.move_direction(direction))
                .filter(|npos| occupied.contains(npos))
                .count()
                >= min_neighbours
    }

    let total_occupied_squares = occupied.len();
    let squares_with_neighbours = occupied
        .iter()
        .filter(|pos| has_at_least_n_neighbours(pos, 1, &occupied))
        .count();
    squares_with_neighbours * 2 > total_occupied_squares
}

#[cfg(test)]
fn pic_to_positions(pic: &str) -> Vec<Position> {
    pic.lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, ch)| {
                if ch == '.' {
                    None
                } else {
                    Some(Position {
                        x: x as i64,
                        y: y as i64,
                    })
                }
            })
        })
        .collect()
}

#[test]
fn test_pic_to_positions() {
    assert_eq!(
        pic_to_positions(concat!(
            ".x.\n", // do not join lines please
            "xxx\n"
        )),
        vec![
            Position { x: 1, y: 0 },
            Position { x: 0, y: 1 },
            Position { x: 1, y: 1 },
            Position { x: 2, y: 1 }
        ]
    );
}

#[test]
fn test_maybe_xmas_tree_nope_big() {
    assert!(!maybe_xmas_tree(
        pic_to_positions(concat!(
	    ".....1..1......1........................................11...........................................\n",
	    "......1.........................1....................................................................\n",
	    "...1.....1..............1...................................................1...................1....\n",
	    "......1..............................1......1...................................1...........1........\n",
	    "........1.........................1...............1......111...............................1.........\n",
	    "....1..........................................1...............1..2......1...........................\n",
	    "......................................1.1...............11...............111.......................11\n",
	    "..2................1......1..1............1....11..1........1........1.......11......................\n",
	    ".................1...........1...........................................1....................1......\n",
	    "............................1........................................................................\n",
	    ".....................1..........1................................1...................................\n",
	    "..........................1.............1.............................1.............1..1...........1.\n",
	    ".............................................................................................1.......\n",
	    "..1.....1...........1..1............................1...............................................1\n",
	    "..........1.....................................................1..........................1.........\n",
	    "..................1.............................1................1..............1..........1.........\n",
	    "...................................................................1............1.1..1....1..........\n",
	    "..................................................1...................................1..............\n",
	    "..................1......1...................1........1.........1............1...........1...........\n",
	    "...1............1..................1...............................................................11\n",
	    "............1..................1....1..................1.....1.............1.....................1...\n",
	    "........1........................1......1..............................................1............1\n",
	    ".............1........................................1.............................1................\n",
	    "..............1................1...............11.........1.............1................1...........\n",
	    ".1..1............1.1...............1..1.................................2.....1......................\n",
	    "...........................................1...............1.............1..................1........\n",
	    "........1....1.1....1..........1....1.......................1........................................\n",
	    "...............................1.................1.......................................1...........\n",
	    "......1...........1........................................................1........................1\n",
	    "........1...............1................1............1......1.......................................\n",
	    "..........................1....1.1.................................1.................................\n",
	    "................................................................................1....................\n",
	    "...............1............1..........1.............1..1........................1..............1....\n",
	    ".......1.1........1.......................................1..1...........1...........................\n",
	    "...............1.....................................................................................\n",
	    ".........1.........................1.............1..............1....................................\n",
	    "...........1...............1........1..................................1.............................\n",
	    ".........................................................1.......1........1..1.........1.............\n",
	    ".......................................1..............................1.......11................1....\n",
	    "....1..........................................................................1.....................\n",
	    "....1......................................1..............................1........................1.\n",
	    "................................1..................1..............................1..................\n",
	    ".................1...............1............1.........1................1........1........1.1.......\n",
	    "..................................1..........1.1...........................................1.........\n",
	    ".......1..........................1.......................1...................1......................\n",
	    ".....................................................................................................\n",
	    "....................1........1...1.1...............1...........1.............1..........1........1...\n",
	    ".............................................1........................1..1.................1.........\n",
	    ".......1............1................................................................................\n",
	    ".1.......................1..............1.1.....1...1....1............1...........1.1................\n",
	    "..............................1..1.........1..............................1...........1..............\n",
	    "..................................11................1.....................1........1.................\n",
	    "......................1................1....1........1.....1..............................1..........\n",
	    "............1...................................1.1............1.................1...................\n",
	    "...1........11................................1.1...................1.........................1......\n",
	    "...........................1..............1........1................1.1...........1.........1........\n",
	    "....................1...........1....1...............1...1...............................1........1..\n",
	    "....1....................1...1.........................1....1........................................\n",
	    "............1.................1..................1.........1..1......1...1...........................\n",
	    "...............11.....................1..........................1..111...........................1..\n",
	    ".................1....................1........................................................1.....\n",
	    "..11.........................1...................................................1...................\n",
	    "..................................1.........1.1.......................................1...1..........\n",
	    "1....1....................................1......1........................................1..........\n",
	    "..........................1.....1.......................................................1....1.......\n",
	    "...............1...........................................1.........................................\n",
	    "...................1.................................................................................\n",
	    "......................................1...1.............1.......................1....................\n",
	    "..................2..............1...................................................................\n",
	    "........1........1.1...2........................................................1...........1........\n",
	    "...11............1..........................1........................................................\n",
	    ".............................................................1.......................................\n",
	    "....1....1..................1...1....................................................................\n",
	    "...................................................1..1....................1..............1..........\n",
	    ".1...........1...........................1............................1.......................1......\n",
	    ".......1.......................1........1....1..................1.....1......1...1...................\n",
	    "....1...1......................11..........................1....................1.....1...........1..\n",
	    ".................................................................1........1..........................\n",
	    "............1........1.........................1..1..........................................1.......\n",
	    "...........................1..........1....2..........................1..............................\n",
	    "1..........................................1..........................1.........1....................\n",
	    "..........1...1...........................1......1......................................1............\n",
	    "..1............1...................1..................................................1..............\n",
	    ".................................................1...................................................\n",
	    "............1.....1............1.........................1......1............1.......................\n",
	    "..........................1........1.................1.............1............1.1.........1........\n",
	    "1................1...............................................................................1...\n",
	    ".................1..........1.....................1.1.........1.............................1........\n",
	    "....1............................1.......1......1..........................1..................1......\n",
	    "...................1.11.......1...............................1..................11..1...............\n",
	    "................................................1..................1......1..........................\n",
	    "......................................1......................................1.......................\n",
	    "..................1.................1.............1..........1.....................1..1.1...1.1......\n",
	    "..................1....................................1.............1....1..........................\n",
	    "...1.................................1.........................1.............1.......................\n",
	    ".................................1.....1.................1............1..............................\n",
	    ".......................1.............1........1....1.................1..1..1....................1....\n",
	    "...................1................1....1.............................1..1..............1.....1.1...\n",
	    "...............1................................1................1...................................\n",
	    ".........................................1.1........................1................................\n",
	    "......................11............................1................................................\n",
	    "1.............................1........1..............................1..1......1....................\n",
	    "..1....................................1.................1..1..1...............................1.....\n",
	)).into_iter()));
}

#[test]
fn test_maybe_xmas_tree_yes() {
    assert!(maybe_xmas_tree(
        pic_to_positions(concat!(
            ".....\n", // do not join lines
            "..x..\n", // do not join lines
            ".xxx.\n", // do not join lines
            "xxxxx\n", // do not join lines
            "xxxxx\n", // do not join lines
            "..x..\n", // do not join lines
            "..x..\n", // do not join lines
        ))
        .into_iter(),
    ));
    assert!(maybe_xmas_tree(
        pic_to_positions(concat!(
            ".....x.\n", // do not join lines
            "...x...\n", // do not join lines
            "..xxx..\n", // do not join lines
            ".x.xxx.\n", // do not join lines
            ".xxxxx.\n", // do not join lines
            "...x...\n", // do not join lines
        ))
        .into_iter(),
    ));
    assert!(maybe_xmas_tree(
        pic_to_positions(concat!(
            ".....x.\n", // do not join lines
            "...x...\n", // do not join lines
            "..xxx..\n", // do not join lines
            ".x.xxx.\n", // do not join lines
            ".xxxxx.\n", // do not join lines
            "...x...\n", // do not join lines
            "..xxx..\n", // do not join lines
            "..xxx..\n", // do not join lines
        ))
        .into_iter(),
    ));
}

fn main() {
    const WIDTH: i64 = 101;
    const HEIGHT: i64 = 103;
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let parser = Parser::new();
    let robots = parser.parse_input(input_str);
    println!("day 14 part 1: {}", part1(&robots, WIDTH, HEIGHT, false));
    println!("part2:");
    // t=6354 looks like a christmas tree, but the website sayd 6354 is too low.
    // apparently 6355 is correct.
    part2(&robots, WIDTH, HEIGHT);
}
