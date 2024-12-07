use lib::parse::parse_number;
use regex::Regex;
use std::str;

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    let re = Regex::new(r"mul\((\d+),(\d+)\)").unwrap();
    re.captures_iter(input)
        .map(|c| c.extract())
        .map(|(_, [first, second])| (parse_number(first), parse_number(second)))
        .collect()
}

#[test]
fn test_parse_input() {
    assert_eq!(parse_input("mul(1,2)"), vec![(1, 2)]);
    assert_eq!(
        parse_input("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"),
        vec![(2, 4), (5, 5), (11, 8), (8, 5)]
    );
}

fn compute(input: &str) -> i32 {
    parse_input(input).iter().map(|(a, b)| a * b).sum()
}

#[test]
fn test_compute() {
    assert_eq!(
        compute("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"),
        161
    );
}

mod part1 {
    pub fn part1(input: &str) -> i32 {
        super::compute(input)
    }
}

mod part2 {
    use regex::{NoExpand, Regex};

    fn implement_conditionals(input: &str) -> String {
        let re_y = Regex::new(r"do\(\)").unwrap();
        let re_n = Regex::new(r"don't\(\)").unwrap();
        let munged = re_y
            .replace_all(
                re_n.replace_all(input, NoExpand("N")).as_ref(),
                NoExpand("Y"),
            )
            .to_string();
        let mut result = String::new();
        let mut accept = true;
        for ch in munged.chars() {
            match (accept, ch) {
                (_, 'Y') => {
                    accept = true;
                }
                (false, _) => (),
                (true, 'N') => {
                    accept = false;
                }
                (true, c) => {
                    result.push(c);
                }
            }
        }
        result
    }

    #[test]
    fn test_implement_conditionals() {
        assert_eq!(
            implement_conditionals(
                "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
            ),
            "xmul(2,4)&mul[3,7]!^?mul(8,5))"
        );
    }

    pub fn part2(input: &str) -> i32 {
        let preprocessed_input = implement_conditionals(input);
        super::compute(&preprocessed_input)
    }

    #[test]
    fn test_part2() {
        assert_eq!(
            part2("xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"),
            48
        );
    }
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 03 part 1: {}", part1::part1(input));
    // not 134969308
    println!("day 03 part 2: {}", part2::part2(input));
}
