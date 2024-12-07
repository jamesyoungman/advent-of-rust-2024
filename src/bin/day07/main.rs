use lib::parse::parse_number;
use std::str;

type Number = i64;

#[derive(Debug, PartialEq, Eq)]
struct Equation {
    result: Number,
    input: Vec<Number>,
}

fn parse_equation(s: &str) -> Equation {
    match s.split_once(": ") {
        None => {
            panic!("expected ': '");
        }
        Some((left, right)) => {
            let result: Number = parse_number(left);
            let input = right.split(' ').map(parse_number).collect();
            Equation {
                result: result as Number,
                input,
            }
        }
    }
}

#[test]
fn test_parse_equation() {
    assert_eq!(
        parse_equation("156: 15 6"),
        Equation {
            result: 156,
            input: vec![15, 6],
        }
    );
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "190: 10 19\n",
        "3267: 81 40 27\n",
        "83: 17 5\n",
        "156: 15 6\n",
        "7290: 6 8 6 15\n",
        "161011: 16 10 13\n",
        "192: 17 8 14\n",
        "21037: 9 7 18 13\n",
        "292: 11 6 16 20\n"
    )
}

fn parse_input(input: &str) -> Vec<Equation> {
    input.lines().map(parse_equation).collect()
}

#[test]
fn test_parse_input() {
    fn make_equation(result: Number, input: Vec<Number>) -> Equation {
        Equation { result, input }
    }
    assert_eq!(
        parse_input(sample_input()),
        vec![
            make_equation(190, vec![10, 19]),
            make_equation(3267, vec![81, 40, 27]),
            make_equation(83, vec![17, 5]),
            make_equation(156, vec![15, 6]),
            make_equation(7290, vec![6, 8, 6, 15]),
            make_equation(161011, vec![16, 10, 13]),
            make_equation(192, vec![17, 8, 14]),
            make_equation(21037, vec![9, 7, 18, 13]),
            make_equation(292, vec![11, 6, 16, 20])
        ]
    )
}

fn eval_step(left: Number, right: Number, op: bool) -> Number {
    if op {
        left * right
    } else {
        left + right
    }
}

fn equation_is_valid_with_these_operators(eq: &Equation, mut operators: u32) -> bool {
    let mut numbers: Vec<Number> = eq.input.iter().rev().copied().collect();
    let mut accumulator = numbers
        .pop()
        .expect("equation should have at least one number");
    while let Some(n) = numbers.pop() {
        let op = operators & 1 != 0;
        operators >>= 1;
        accumulator = eval_step(accumulator, n, op);
    }
    accumulator == eq.result
}

#[test]
fn test_validity_check_with_operators() {
    let equation = Equation {
        result: 3267,
        input: vec![81, 40, 27],
    };
    assert!(equation_is_valid_with_these_operators(&equation, 0b10u32));
    assert!(equation_is_valid_with_these_operators(&equation, 0b01u32));
    assert!(!equation_is_valid_with_these_operators(&equation, 0b00u32));
    assert!(!equation_is_valid_with_these_operators(&equation, 0b11u32));
}

fn equation_is_valid(eq: &Equation) -> bool {
    let limit = 1u32 << (eq.input.len() - 1);
    (0..limit).any(|ops| equation_is_valid_with_these_operators(eq, ops))
}

#[test]
fn test_validity_check() {
    let solvable = Equation {
        result: 3267,
        input: vec![81, 40, 27],
    };
    let unsolvable = Equation {
        result: 21037,
        input: vec![9, 7, 18, 13],
    };
    assert!(equation_is_valid(&solvable));
    assert!(!equation_is_valid(&unsolvable));
}

fn part1(input: &[Equation]) -> Number {
    input
        .iter()
        .filter(|eq| equation_is_valid(eq))
        .map(|eq| eq.result)
        .sum()
}

#[test]
fn test_part1() {
    let input = parse_input(sample_input());
    assert_eq!(part1(&input), 3749);
}

fn main() {
    let input = parse_input(str::from_utf8(include_bytes!("input.txt")).unwrap());
    println!("day 07 part 1: {}", part1(&input));
}
