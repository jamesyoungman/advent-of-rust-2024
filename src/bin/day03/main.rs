use regex::Regex;
use std::str;

fn parse_number(s: &str) -> i32 {
    match s.trim().parse() {
        Ok(n) => n,
        Err(e) => {
            panic!("failed to parse '{s}' as a number: {e}");
        }
    }
}

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

fn part1(input: &str) -> i32 {
    parse_input(input).iter().map(|(a, b)| a * b).sum()
}

#[test]
fn test_part1() {
    assert_eq!(
        part1("xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"),
        161
    );
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 03 part 1: {}", part1(input));
}
