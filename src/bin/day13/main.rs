use regex::Regex;
use std::str;

use lib::parse::parse_number;

#[derive(Debug, Clone, Eq, PartialEq)]
struct Puzzle {
    ax: i64,
    ay: i64,
    bx: i64,
    by: i64,
    tx: i64,
    ty: i64,
}

fn is_satisfying_combination(a: i64, b: i64, t: i64, na: i64, nb: i64) -> bool {
    a * na + b * nb == t
}

/// We want solutions of the form (na)*a+(nb)*b = t
/// where all variables are integers.
fn cheapest_single_axis_satisfying_combination<F>(
    a: i64,
    b: i64,
    t: i64,
    filter: F,
) -> Option<(i64, i64)>
where
    F: Fn(i64, i64) -> bool,
{
    // Button A is more expensive, so we want to minimise nb (the
    // number of times we press button A).
    for na in 0..=100 {
        let a_contrib = na * a;
        if a_contrib > t {
            break;
        }
        let b_contrib = t - a_contrib;
        if b_contrib % b != 0 {
            continue;
        }
        let nb = b_contrib / b;
        if filter(na, nb) {
            return Some((na, nb));
        }
    }
    None
}

fn cheapest_two_axis_satisfying_combination(p: &Puzzle) -> Option<(i64, i64)> {
    let y_axis_is_good =
        |na: i64, nb: i64| -> bool { is_satisfying_combination(p.ay, p.by, p.ty, na, nb) };
    cheapest_single_axis_satisfying_combination(p.ax, p.bx, p.tx, y_axis_is_good)
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "Button A: X+94, Y+34\n",
        "Button B: X+22, Y+67\n",
        "Prize: X=8400, Y=5400\n",
        "\n",
        "Button A: X+26, Y+66\n",
        "Button B: X+67, Y+21\n",
        "Prize: X=12748, Y=12176\n",
        "\n",
        "Button A: X+17, Y+86\n",
        "Button B: X+84, Y+37\n",
        "Prize: X=7870, Y=6450\n",
        "\n",
        "Button A: X+69, Y+23\n",
        "Button B: X+27, Y+71\n",
        "Prize: X=18641, Y=10279\n",
    )
}

#[cfg(test)]
fn sample_puzzles() -> [Puzzle; 4] {
    [
        Puzzle {
            ax: 94,
            ay: 34,
            bx: 22,
            by: 67,
            tx: 8400,
            ty: 5400,
        },
        Puzzle {
            ax: 26,
            ay: 66,
            bx: 67,
            by: 21,
            tx: 12748,
            ty: 12176,
        },
        Puzzle {
            ax: 17,
            ay: 86,
            bx: 84,
            by: 37,
            tx: 7870,
            ty: 6450,
        },
        Puzzle {
            ax: 69,
            ay: 23,
            bx: 27,
            by: 71,
            tx: 18641,
            ty: 10279,
        },
    ]
}

#[test]
fn test_cheapest_satisfying_combination() {
    let samples = sample_puzzles();
    // Puzzle 1
    assert_eq!(
        cheapest_two_axis_satisfying_combination(&samples[0]),
        Some((80, 40))
    );
    // Puzzle 2
    assert_eq!(cheapest_two_axis_satisfying_combination(&samples[1]), None);
    // Puzzle 3
    assert_eq!(
        cheapest_two_axis_satisfying_combination(&samples[2]),
        Some((38, 86))
    );
    // Puzzle 4
    assert_eq!(cheapest_two_axis_satisfying_combination(&samples[3]), None);
}

fn puzzle_cost(na: i64, nb: i64) -> i64 {
    na * 3 + nb
}

#[test]
fn test_puzzle_cost() {
    assert_eq!(puzzle_cost(38, 86), 200);
}

fn part1(puzzles: &[Puzzle]) -> i64 {
    puzzles
        .iter()
        .filter_map(cheapest_two_axis_satisfying_combination)
        .map(|(na, nb)| puzzle_cost(na, nb))
        .sum()
}

struct Parser {
    button_re: Regex,
    prize_re: Regex,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            button_re: Regex::new(r"^Button (.): X[+](\d+), Y[+](\d+)$").unwrap(),
            prize_re: Regex::new(r"^Prize: X=(\d+), Y=(\d+)$").unwrap(),
        }
    }

    pub fn parse_puzzle(&self, s: &str) -> Puzzle {
        let lines: Vec<&str> = s.lines().map(|s| s.trim()).collect();
        if lines.len() != 3 {
            panic!("wrong number of lines");
        }
        let a_matches = match self.button_re.captures(lines[0]) {
            Some(m) => m,
            None => {
                panic!("no match for button A: {}", lines[0]);
            }
        };
        let b_matches = match self.button_re.captures(lines[1]) {
            Some(m) => m,
            None => {
                panic!("no match for button B: {}", lines[1]);
            }
        };
        let prize_matches = match self.prize_re.captures(lines[2]) {
            Some(m) => m,
            None => {
                panic!("no match for prize line: {}", lines[2]);
            }
        };
        let info = (
            a_matches.extract(),
            b_matches.extract(),
            prize_matches.extract(),
        );
        match info {
            ((_, ["A", ax, ay]), (_, ["B", bx, by]), (_, [tx, ty])) => Puzzle {
                ax: parse_number(ax),
                ay: parse_number(ay),
                bx: parse_number(bx),
                by: parse_number(by),
                tx: parse_number(tx),
                ty: parse_number(ty),
            },
            _ => {
                panic!("no pattern match for {info:?}");
            }
        }
    }
}

fn parse_input(input: &str) -> Vec<Puzzle> {
    let parser = Parser::new();
    input
        .split("\n\n")
        .map(|desc: &str| parser.parse_puzzle(desc))
        .collect()
}

#[test]
fn test_parse_input() {
    let got = parse_input(sample_input());
    let expected = sample_puzzles().to_vec();
    for (g, e) in got.iter().zip(expected.iter()) {
        assert_eq!(g, e);
    }
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let input = parse_input(input_str);
    println!("day 13 part 1: {}", part1(&input));
}
