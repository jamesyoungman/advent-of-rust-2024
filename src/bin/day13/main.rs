use regex::Regex;
use std::str;

use lib::parse::parse_number;

#[derive(Debug, Clone, Eq, PartialEq)]
struct AxisPuzzle {
    a: i64, // position cheange when pressing button A
    b: i64, // position cheange when pressing button B
    t: i64, // the prize's location
}

#[derive(Debug, Clone, Eq, PartialEq)]
struct Puzzle {
    x: AxisPuzzle,
    y: AxisPuzzle,
}

impl Puzzle {
    // Suppose we have a system of equations:
    //
    // a1*x + b1*y = c1
    // a2*x + b2*y = c2
    //
    // Then by Cramer's rule,
    //
    // x=(c1*b2-b1*c2)/(a1*b2-b1*a2)
    // y=(a1*c2-c1*a1)/(a1*b2-b1*a2)
    //
    // We can use this to solve an instance of `Puzzle`.
    //
    fn solve(&self) -> Option<(i64, i64)> {
        fn solve_by_cramer_method(
            a1: i64,
            b1: i64,
            c1: i64,
            a2: i64,
            b2: i64,
            c2: i64,
        ) -> Option<(i64, i64)> {
            let denominator = a1 * b2 - b1 * a2;
            if denominator == 0 {
                None
            } else {
                let x = (c1 * b2 - b1 * c2) / denominator;
                let y = (a1 * c2 - c1 * a2) / denominator;
                if a1 * x + b1 * y == c1 && a2 * x + b2 * y == c2 {
                    Some((x, y))
                } else {
                    None
                }
            }
        }

        // In our case, our system of equations is:
        //
        // self.x.a*na + self.x.b*nb = self.x.t
        // self.y.a*na + self.y.b*nb = self.y.t
        //
        // Hence the solution is returned as (na, nb).
        solve_by_cramer_method(self.x.a, self.x.b, self.x.t, self.y.a, self.y.b, self.y.t)
    }
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
    #[cfg(test)]
    use super::*;
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

fn solve(puzzles: &[Puzzle]) -> i64 {
    puzzles
        .iter()
        .filter_map(|puzzle| puzzle.solve())
        .map(|(na, nb)| puzzle_cost(na, nb))
        .sum()
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let part1_input = parse_input(input_str);
    println!("day 13 part 1: {}", solve(&part1_input));

    let part2_input = part2::apply_part2_correction(part1_input.clone());
    println!("day 13 part 2: {}", solve(&part2_input));
}
