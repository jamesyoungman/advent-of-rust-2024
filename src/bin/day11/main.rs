use std::collections::HashMap;
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

#[test]
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

fn consider_stone_n_times(
    val: Value,
    times: usize,
    memo: &mut HashMap<(Value, usize), usize>,
) -> usize {
    if times == 0 {
        return 1;
    }
    let memo_key = (val, times);
    if let Some(&stone_count) = memo.get(&memo_key) {
        return stone_count;
    }

    let stone_count = if val == 0 {
        consider_stone_n_times(1, times - 1, memo)
    } else {
        let ndigits = number_of_digits(val);
        if ndigits % 2 == 0 {
            let stones = make_two_stones(val, ndigits);
            consider_stone_n_times(stones[0], times - 1, memo)
                .checked_add(consider_stone_n_times(stones[1], times - 1, memo))
                .expect("addition should not overflow")
        } else {
            match val.checked_mul(2024) {
                Some(newval) => consider_stone_n_times(newval, times - 1, memo),
                None => {
                    panic!("{val}*2048 is out-of-range!");
                }
            }
        }
    };
    memo.insert(memo_key, stone_count);
    stone_count
}

#[test]
fn test_iterate_stone_once() {
    fn iterate_stone_once(val: Value) -> usize {
        let mut memo = HashMap::new();
        consider_stone_n_times(val, 1, &mut memo)
    }
    assert_eq!(iterate_stone_once(0), 1);
    assert_eq!(iterate_stone_once(10), 2);
    assert_eq!(iterate_stone_once(63), 2);
    assert_eq!(iterate_stone_once(1234), 2);
    assert_eq!(iterate_stone_once(3), 1);
}

fn blink(input: &[Value], times: usize) -> usize {
    let mut memo = HashMap::new();
    input
        .iter()
        .map(|val| consider_stone_n_times(*val, times, &mut memo))
        .sum()
}

#[test]
fn test_blink_1() {
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
    for i in 1..expected.len() {
        let got = blink(&expected[i - 1], 1);
        let expect = expected[i].len();
        if got != expect {
            panic!(
                "wrong stone count for input {:?}: expected {expect}, got {got}",
                expected[i - 1]
            );
        }
    }
}

fn part1(stones: &[Value]) -> usize {
    blink(stones, 25)
}

fn part2(stones: &[Value]) -> usize {
    blink(stones, 75)
}

fn parse_input(s: &str) -> Vec<Value> {
    s.split_whitespace().map(parse_number).collect()
}

#[test]
fn test_parse_input() {
    assert_eq!(parse_input("125 17"), vec![125, 17]);
}

fn main() {
    let input = parse_input(str::from_utf8(include_bytes!("input.txt")).unwrap());
    println!("day 11 part 1: {}", part1(&input));
    println!("day 11 part 2: {}", part2(&input));
}
