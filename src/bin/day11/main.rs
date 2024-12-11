use std::str;

use lib::parse::parse_number;

type Value = u64;

fn pow10(power: usize) -> Value {
    // taken from day07
    (0..power).fold(1, |acc, _| acc * 10)
}

#[test]
fn test_pow10() {
    assert_eq!(pow10(0), 1);
    assert_eq!(pow10(1), 10);
    assert_eq!(pow10(2), 100);
    assert_eq!(pow10(3), 1000);
}

fn make_two_stones(val: Value, ndigits: usize) -> [Value; 2] {
    let divisor = pow10(ndigits / 2);
    [val / divisor, val % divisor]
}

fn test_make_two_stones() {
    assert_eq!(make_two_stones(10, 2), [1, 0]);
    assert_eq!(make_two_stones(21, 2), [2, 1]);
    assert_eq!(make_two_stones(1000, 4), [10, 0]);
    assert_eq!(make_two_stones(1001, 4), [10, 1]);
    assert_eq!(make_two_stones(1234, 4), [12, 34]);
}

fn number_of_digits(val: Value) -> usize {
    let mut bound = 10;
    for count in 1.. {
        if bound > val {
            return count;
        }
        bound = bound.checked_mul(10).expect("should not overflow")
    }
    unreachable!()
}

#[test]
fn test_number_of_digits() {
    assert_eq!(number_of_digits(1), 1);
    assert_eq!(number_of_digits(2), 1);
    assert_eq!(number_of_digits(9), 1);
    assert_eq!(number_of_digits(0), 1);
    assert_eq!(number_of_digits(10), 2);
    assert_eq!(number_of_digits(99), 2);
    assert_eq!(number_of_digits(100), 3);
    assert_eq!(number_of_digits(500), 3);
    assert_eq!(number_of_digits(999), 3);
    assert_eq!(number_of_digits(1000), 4);
}

fn consider_stone(val: Value, output: &mut Vec<Value>) {
    if val == 0 {
        output.push(1);
    } else {
        let ndigits = number_of_digits(val);
        if ndigits % 2 == 0 {
            output.extend(make_two_stones(val, ndigits));
        } else {
            match val.checked_mul(2024) {
                Some(newval) => output.push(newval),
                None => {
                    panic!("{val}*2048 is out-of-range!");
                }
            }
        }
    }
}

#[test]
fn test_consider_stone() {
    fn consider(val: Value) -> Vec<Value> {
        let mut result = Vec::new();
        consider_stone(val, &mut result);
        result
    }
    assert_eq!(consider(0), vec![1]);
    assert_eq!(consider(10), vec![1, 0]);
    assert_eq!(consider(63), vec![6, 3]);
    assert_eq!(consider(1234), vec![12, 34]);
    assert_eq!(consider(3), vec![3 as Value * 2024]);
}

fn blink(input: Vec<Value>) -> Vec<Value> {
    let mut result = Vec::with_capacity(input.len());
    for stone in input {
        consider_stone(stone, &mut result);
    }
    result
}

#[test]
fn test_blink() {
    let expected = vec![
        vec![125, 17],
        vec![253000, 1, 7],
        vec![253, 0, 2024, 14168],
        vec![512072, 1, 20, 24, 28676032],
        vec![512, 72, 2024, 2, 0, 2, 4, 2867, 6032],
        vec![1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32],
        vec![
            2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8, 6, 7, 6, 0, 3,
            2,
        ],
    ];
    let mut current = expected[0].clone();
    for expect in &expected[1..] {
        current = blink(current);
        assert_eq!(&current, expect);
    }
}

fn part1(mut stones: Vec<Value>) -> usize {
    for _ in 0..25 {
        stones = blink(stones);
    }
    stones.len()
}

fn parse_input(s: &str) -> Vec<Value> {
    s.split_whitespace()
        .map(|label| parse_number(label))
        .collect()
}

#[test]
fn test_parse_input() {
    assert_eq!(parse_input("125 17"), vec![125, 17]);
}

fn main() {
    let input = parse_input(str::from_utf8(include_bytes!("input.txt")).unwrap());
    println!("day 11 part 1: {}", part1(input));
}
