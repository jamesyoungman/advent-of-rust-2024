use regex::Regex;
use std::str;

use lib::parse::parse_number;

#[derive(Debug, Clone, Eq, PartialEq)]
struct AxisPuzzle {
    a: i64, // position cheange when pressing button A
    b: i64, // position cheange when pressing button B
    t: i64, // the prize's location
}

impl AxisPuzzle {
    fn is_satisfying_combination(&self, na: i64, nb: i64) -> bool {
        self.a * na + self.b * nb == self.t
    }

    /// We want solutions of the form (na)*a+(nb)*b = t
    /// where all variables are integers.
    fn cheapest_single_axis_satisfying_combination<F>(&self, filter: F) -> Option<(i64, i64)>
    where
        F: Fn(i64, i64) -> bool,
    {
        // Button A is more expensive, so we want to minimise nb (the
        // number of times we press button A).
        for na in 0..=100 {
            let a_contrib = na * self.a;
            if a_contrib > self.t {
                break;
            }
            let b_contrib = self.t - a_contrib;
            if b_contrib % self.b != 0 {
                continue;
            }
            let nb = b_contrib / self.b;
            if filter(na, nb) {
                return Some((na, nb));
            }
        }
        None
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Puzzle {
    x: AxisPuzzle,
    y: AxisPuzzle,
}

fn puzzle_cost(na: i64, nb: i64) -> i64 {
    na * 3 + nb
}

#[test]
fn test_puzzle_cost() {
    assert_eq!(puzzle_cost(38, 86), 200);
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
                x: AxisPuzzle {
                    a: parse_number(ax),
                    b: parse_number(bx),
                    t: parse_number(tx),
                },
                y: AxisPuzzle {
                    a: parse_number(ay),
                    b: parse_number(by),
                    t: parse_number(ty),
                },
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

mod part1 {
    use super::*;

    fn cheapest_two_axis_satisfying_combination(p: &Puzzle) -> Option<(i64, i64)> {
        let y_axis_is_good = |na: i64, nb: i64| -> bool { p.y.is_satisfying_combination(na, nb) };
        p.x.cheapest_single_axis_satisfying_combination(y_axis_is_good)
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

    pub fn solve(puzzles: &[Puzzle]) -> i64 {
        puzzles
            .iter()
            .filter_map(cheapest_two_axis_satisfying_combination)
            .map(|(na, nb)| puzzle_cost(na, nb))
            .sum()
    }

    #[cfg(test)]
    fn sample_puzzles() -> [Puzzle; 4] {
        [
            Puzzle {
                x: AxisPuzzle {
                    a: 94,
                    b: 22,
                    t: 8400,
                },
                y: AxisPuzzle {
                    a: 34,
                    b: 67,
                    t: 5400,
                },
            },
            Puzzle {
                x: AxisPuzzle {
                    a: 26,
                    b: 67,
                    t: 12748,
                },
                y: AxisPuzzle {
                    a: 66,
                    b: 21,
                    t: 12176,
                },
            },
            Puzzle {
                x: AxisPuzzle {
                    a: 17,
                    b: 84,
                    t: 7870,
                },
                y: AxisPuzzle {
                    a: 86,
                    b: 37,
                    t: 6450,
                },
            },
            Puzzle {
                x: AxisPuzzle {
                    a: 69,
                    b: 27,
                    t: 18641,
                },
                y: AxisPuzzle {
                    a: 23,
                    b: 71,
                    t: 10279,
                },
            },
        ]
    }

    #[test]
    fn test_parse_input() {
        let got = parse_input(sample_input());
        let expected = sample_puzzles().to_vec();
        for (g, e) in got.iter().zip(expected.iter()) {
            assert_eq!(g, e);
        }
    }
}

mod part2 {
    // The grabber starts at (0,0).  We want to get to the prize
    // which is at location T.
    //
    // If we can get the grabber to T, then there is some
    // (na, nb) such that na*A + nb*B = T.
    //
    // In other words,
    //
    // 1. the span of vectors A and B includes T.
    // 2. the scalars are integers
    //
    // If either condition is false, we can't get the grabber to T (as
    // in the second and third examples).
    //
    // Idea: supposing A,B are basis vectors (i.e. are linearly
    // independent) then we can perform a change of basis on T.
    // Initially T is expressed in terms of ([1,0], [0,1]) (being the
    // +x and +y vectors respectively).  If we transform its
    // coordinates to use (A, B) as the basis vectors, then we can
    // determine na,nb.
    //
    // If A and B are NOT linearly independent, this means that A and
    // B are colinear.  Hence for our optimal solution either na=0 or
    // nb=0 and we can choose which one.  If 3*|B| < |A| then we
    // should use only button B (i.e. na=0) and otherwise we should
    // use only button A (i.e. nb=0).
    use super::*;

    #[cfg(test)]
    fn sample_puzzles() -> [Puzzle; 4] {
        [
            Puzzle {
                x: AxisPuzzle {
                    a: 94,
                    b: 22,
                    t: 10000000008400,
                },
                y: AxisPuzzle {
                    a: 34,
                    b: 67,
                    t: 10000000005400,
                },
            },
            Puzzle {
                x: AxisPuzzle {
                    a: 26,
                    b: 67,
                    t: 10000000012748,
                },
                y: AxisPuzzle {
                    a: 66,
                    b: 21,
                    t: 10000000012176,
                },
            },
            Puzzle {
                x: AxisPuzzle {
                    a: 17,
                    b: 84,
                    t: 10000000007870,
                },
                y: AxisPuzzle {
                    a: 86,
                    b: 37,
                    t: 10000000006450,
                },
            },
            Puzzle {
                x: AxisPuzzle {
                    a: 69,
                    b: 27,
                    t: 10000000018641,
                },
                y: AxisPuzzle {
                    a: 23,
                    b: 71,
                    t: 10000000010279,
                },
            },
        ]
    }

    pub fn apply_part2_correction(mut puzzles: Vec<Puzzle>) -> Vec<Puzzle> {
        fn apply_correction(p: &mut AxisPuzzle) {
            p.t += 10000000000000;
        }
        for p in puzzles.iter_mut() {
            apply_correction(&mut p.x);
            apply_correction(&mut p.y);
        }
        puzzles
    }

    #[test]
    fn test_parse_input() {
        let got = apply_part2_correction(parse_input(sample_input()));
        let expected = sample_puzzles().to_vec();
        for (g, e) in got.iter().zip(expected.iter()) {
            assert_eq!(g, e);
        }
    }
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let part1_input = parse_input(input_str);
    println!("day 13 part 1: {}", part1::solve(&part1_input));
    let _part2_input = part2::apply_part2_correction(part1_input.clone());
}
