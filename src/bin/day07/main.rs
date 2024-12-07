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

#[derive(Debug, PartialEq, Eq, Clone, Copy, Default)]
enum Operator {
    #[default]
    Add,
    Multiply,
    Concatenate, // part 2 only
}

impl Operator {
    fn successor(&self) -> (bool, Operator) {
        match self {
            Operator::Add => (false, Operator::Multiply),
            Operator::Multiply => (false, Operator::Concatenate),
            Operator::Concatenate => (true, Operator::Add),
        }
    }
}

fn count_places(mut n: Number) -> usize {
    if n == 0 {
        1
    } else {
        let mut places = 0;
        while n > 0 {
            n /= 10;
            places += 1;
        }
        places
    }
}

#[test]
fn test_count_places() {
    assert_eq!(count_places(1), 1);
    assert_eq!(count_places(2), 1);
    assert_eq!(count_places(9), 1);
    assert_eq!(count_places(10), 2);
    assert_eq!(count_places(50), 2);
    assert_eq!(count_places(101), 3);
}

fn pow10(power: Number) -> Number {
    (0..power).fold(1, |acc, _| acc * 10)
}

#[test]
fn test_pow10() {
    assert_eq!(pow10(0), 1);
    assert_eq!(pow10(1), 10);
    assert_eq!(pow10(2), 100);
    assert_eq!(pow10(3), 1000);
}

fn concatenate(left: Number, right: Number) -> Number {
    let shift10 = count_places(right) as Number;
    left * pow10(shift10) + right
}

#[test]
fn test_concatenate() {
    assert_eq!(concatenate(0, 0), 0);
    assert_eq!(concatenate(0, 1), 1);
    assert_eq!(concatenate(0, 10), 10);
    assert_eq!(concatenate(1, 1), 11);
    assert_eq!(concatenate(1, 2), 12);
    assert_eq!(concatenate(2, 1), 21);
    assert_eq!(concatenate(9, 9), 99);
    assert_eq!(concatenate(9, 10), 910);
    assert_eq!(concatenate(10, 9), 109);
}

fn eval_step(left: Number, right: Number, op: Operator) -> Number {
    match op {
        Operator::Add => left + right,
        Operator::Multiply => left * right,
        Operator::Concatenate => concatenate(left, right),
    }
}

fn evaluate(numbers: &[Number], operators: Vec<Operator>) -> Number {
    match numbers {
        [accumulator, tail @ ..] => tail
            .iter()
            .zip(operators)
            .fold(*accumulator, |acc, (n, op)| eval_step(acc, *n, op)),
        [] => {
            panic!("empty numbers");
        }
    }
}

#[test]
fn test_evaluate_two_items() {
    assert_eq!(evaluate(&[0, 0], vec![Operator::Add]), 0);
    assert_eq!(evaluate(&[0, 0], vec![Operator::Multiply]), 0);
    assert_eq!(evaluate(&[0, 1], vec![Operator::Add]), 1);
    assert_eq!(evaluate(&[0, 1], vec![Operator::Multiply]), 0);
    assert_eq!(evaluate(&[2, 1], vec![Operator::Add]), 3);
    assert_eq!(evaluate(&[2, 3], vec![Operator::Multiply]), 6);
}

#[test]
fn test_evaluate_three_items() {
    assert_eq!(
        evaluate(&[2, 3, 10], vec![Operator::Multiply, Operator::Add]),
        16
    );
    assert_eq!(
        evaluate(&[2, 3, 4], vec![Operator::Multiply, Operator::Multiply]),
        24
    );
}

fn equation_is_valid_with_these_operators(eq: &Equation, operators: Vec<Operator>) -> bool {
    let accumulator = evaluate(&eq.input, operators);
    accumulator == eq.result
}

#[test]
fn test_validity_check_with_operators() {
    let equation = Equation {
        result: 3267,
        input: vec![81, 40, 27],
    };
    use Operator::*;
    assert!(equation_is_valid_with_these_operators(
        &equation,
        vec![Add, Multiply]
    ));
    assert!(equation_is_valid_with_these_operators(
        &equation,
        vec![Multiply, Add]
    ));
    assert!(!equation_is_valid_with_these_operators(
        &equation,
        vec![Multiply, Multiply]
    ));
    assert!(!equation_is_valid_with_these_operators(
        &equation,
        vec![Add, Add]
    ));
}

mod part1 {
    use super::*;

    fn make_ops(eq: &Equation, op_seq: u32) -> Vec<Operator> {
        (0..(eq.input.len()))
            .map(|bitpos| op_seq & (1 << bitpos) != 0)
            .map(|use_add| {
                if use_add {
                    Operator::Add
                } else {
                    Operator::Multiply
                }
            })
            .collect()
    }

    fn equation_is_valid(eq: &Equation) -> bool {
        let limit = 1u32 << (eq.input.len() - 1);
        (0..limit).any(|ops| {
            let operations = make_ops(eq, ops);
            equation_is_valid_with_these_operators(eq, operations)
        })
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

    pub fn solve(input: &[Equation]) -> Number {
        input
            .iter()
            .filter(|eq| equation_is_valid(eq))
            .map(|eq| eq.result)
            .sum()
    }

    #[test]
    fn test_solve() {
        let input = parse_input(sample_input());
        assert_eq!(solve(&input), 3749);
    }
}

mod part2 {
    use super::*;

    struct OpIterator {
        ops: Vec<Operator>,
        saturated: bool,
    }

    impl OpIterator {
        pub fn new(places: usize) -> OpIterator {
            let mut ops = Vec::with_capacity(places);
            ops.resize(places, Operator::Add);
            OpIterator {
                ops,
                saturated: false,
            }
        }

        fn increment(&mut self) {
            assert!(!self.saturated);
            for (revplace, digitval) in self.ops.iter_mut().enumerate().rev() {
                let (carry, nextval) = digitval.successor();
                *digitval = nextval;
                if carry {
                    if revplace == 0 {
                        self.saturated = true;
                        break;
                    }
                } else {
                    break;
                }
            }
        }
    }

    impl Iterator for OpIterator {
        type Item = Vec<Operator>;

        fn next(&mut self) -> Option<Vec<Operator>> {
            if self.saturated {
                None
            } else {
                let result = self.ops.clone();
                self.increment();
                Some(result)
            }
        }
    }

    #[test]
    fn test_op_iterator() {
        use Operator::*;
        let mut it = OpIterator::new(2);
        assert_eq!(it.next(), Some(vec![Add, Add]));
        assert_eq!(it.next(), Some(vec![Add, Multiply]));
        assert_eq!(it.next(), Some(vec![Add, Concatenate]));
        assert_eq!(it.next(), Some(vec![Multiply, Add]));
        assert_eq!(it.next(), Some(vec![Multiply, Multiply]));
        assert_eq!(it.next(), Some(vec![Multiply, Concatenate]));
        assert_eq!(it.next(), Some(vec![Concatenate, Add]));
        assert_eq!(it.next(), Some(vec![Concatenate, Multiply]));
        assert_eq!(it.next(), Some(vec![Concatenate, Concatenate]));
        assert_eq!(it.next(), None);
    }

    fn equation_is_valid(eq: &Equation) -> bool {
        let mut op_it = OpIterator::new(eq.input.len() - 1);
        op_it.any(|operations| equation_is_valid_with_these_operators(eq, operations))
    }

    #[test]
    fn test_validity_check() {
        let solvable = Equation {
            result: 3267,
            input: vec![81, 40, 27],
        };
        let solvable_with_cat = Equation {
            result: 156,
            input: vec![15, 6],
        };
        let unsolvable = Equation {
            result: 21037,
            input: vec![9, 7, 18, 13],
        };
        assert!(equation_is_valid(&solvable));
        assert!(equation_is_valid(&solvable_with_cat));
        assert!(!equation_is_valid(&unsolvable));
    }

    pub fn solve(input: &[Equation]) -> Number {
        input
            .iter()
            .filter(|eq| equation_is_valid(eq))
            .map(|eq| eq.result)
            .sum()
    }

    #[test]
    fn test_solve() {
        let input = parse_input(sample_input());
        assert_eq!(solve(&input), 11387);
    }
}

fn main() {
    let input = parse_input(str::from_utf8(include_bytes!("input.txt")).unwrap());
    println!("day 07 part 1: {}", part1::solve(&input));
    println!("day 07 part 2: {}", part2::solve(&input));
}
