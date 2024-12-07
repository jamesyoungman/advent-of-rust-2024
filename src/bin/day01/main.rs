use lib::parse::parse_number;
use lib::multiset::MultiSet;
use std::str;

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    input
        .lines()
        .map(|line| match line.split_once(" ") {
            Some((l, r)) => (parse_number(l), parse_number(r)),
            None => {
                panic!("expected 2 numbers");
            }
        })
        .collect()
}

#[cfg(test)]
fn test_input() -> &'static str {
    concat!("3   4\n", "4   3\n", "2   5\n", "1   3\n", "3   9\n", "3   3\n")
}

#[test]
fn test_part1() {
    assert_eq!(part1(test_input()), 11);
}

fn part1(input: &str) -> i32 {
    let data = parse_input(input);
    let mut left: Vec<i32> = data.iter().map(|(l, _)| l).copied().collect();
    let mut right: Vec<i32> = data.iter().map(|(_, r)| r).copied().collect();
    left.sort();
    right.sort();
    left.iter()
        .zip(right.iter())
        .map(|(l, r)| (l - r).abs())
        .sum()
}

fn part2(input: &str) -> i32 {
    let data = parse_input(input);
    let right_elems: MultiSet<i32> = data.iter().map(|(_, r)| r).copied().collect();
    data.iter()
        .map(|(l, _)| l * *right_elems.get(l).unwrap_or(&0) as i32)
        .sum()
}

#[test]
fn test_part2() {
    assert_eq!(part2(test_input()), 31);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("part 1: {}", part1(input));
    println!("part 2: {}", part2(input));
}
