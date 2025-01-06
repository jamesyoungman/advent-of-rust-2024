use std::collections::BTreeMap;
use std::str;

use lib::parse::parse_number;

fn generate(mut n: u64) -> u64 {
    fn prune(n: u64) -> u64 {
        n % 16777216
    }

    fn mix(n: u64, m: u64) -> u64 {
        n ^ m
    }

    n = prune(mix(n, n * 64)); // first step
    n = prune(mix(n, n / 32)); // second step
    n = prune(mix(n, n * 2048)); // third step
    n
}

#[test]
fn test_generate() {
    assert_eq!(generate(123), 15887950);
    assert_eq!(generate(15887950), 16495136);
}

#[cfg(test)]
fn sample_123() -> Vec<u64> {
    vec![
        15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254,
    ]
}

#[cfg(test)]
fn sample_input() -> Vec<u64> {
    vec![1, 10, 100, 2024]
}

#[test]
fn test_generate_123() {
    let mut n = 123;
    for expected in sample_123() {
        let got: u64 = generate(n);
        assert_eq!(got, expected);
        n = got;
    }
}

fn generate_steps(initial: u64, steps: usize) -> u64 {
    (0..steps).fold(initial, |acc, _step| generate(acc))
}

fn generate_2k(initial: u64) -> u64 {
    generate_steps(initial, 2000)
}
fn part1(input: &[u64]) -> u64 {
    input.iter().copied().map(generate_2k).sum()
}

#[test]
fn test_part1() {
    let input: Vec<u64> = sample_input();
    assert_eq!(part1(&input), 37327623);
}

fn generate_and_remember_2k_final_digits(initial: u64) -> [u8; 2000] {
    let mut result = [0; 2000];
    result[0] = (initial % 10) as u8;
    let mut n = initial;
    for r in result.iter_mut().skip(1) {
        n = generate(n);
        *r = (n % 10) as u8;
    }
    result
}

#[derive(Debug, PartialEq, Eq, Default, Clone, Copy)]
struct Delta {
    units_digit: u8,
    change: i8,
}

fn compute_deltas(values: &[u8; 2000]) -> [Delta; 1999] {
    let mut result = [Default::default(); 1999];
    for i in 1..values.len() {
        let prev: i8 = values[i - 1] as i8;
        let curr: i8 = values[i] as i8;
        let change = curr - prev;
        result[i - 1] = Delta {
            units_digit: values[i],
            change,
        };
    }
    result
}

#[test]
fn test_compute_deltas() {
    let actuals = generate_and_remember_2k_final_digits(123);
    assert_eq!(&actuals[0..4], &[3, 0, 6, 5]);
    let changes: [Delta; 1999] = compute_deltas(&actuals);
    assert_eq!(
        &changes[0..4],
        &[
            Delta {
                units_digit: 0,
                change: -3,
            },
            Delta {
                units_digit: 6,
                change: 6,
            },
            Delta {
                units_digit: 5,
                change: -1,
            },
            Delta {
                units_digit: 4,
                change: -1,
            }
        ]
    );
}

fn first_sale_by_pattern(initial: u64) -> BTreeMap<[i8; 4], u8> {
    let prices: [u8; 2000] = generate_and_remember_2k_final_digits(initial);
    let changes: [Delta; 1999] = compute_deltas(&prices);
    changes.windows(4).fold(BTreeMap::new(), |mut acc, w| {
        let key: [i8; 4] = [w[0].change, w[1].change, w[2].change, w[3].change];
        let sale_price = w[3].units_digit;
        acc.entry(key).or_insert(sale_price);
        acc
    })
}

fn total_sales_by_pattern(initials: &[u64]) -> BTreeMap<[i8; 4], u64> {
    let mut total_sales = BTreeMap::new();
    for initial in initials.iter() {
        for (pattern, sale) in first_sale_by_pattern(*initial) {
            let sale = u64::from(sale);
            total_sales
                .entry(pattern)
                .and_modify(|tot| {
                    *tot += sale;
                })
                .or_insert(sale);
        }
    }
    total_sales
}

#[test]
fn test_first_sale_by_pattern_123() {
    let sales = first_sale_by_pattern(123);
    assert_eq!(sales.get(&[-1, -1, 0, 2]), Some(&6));
}

#[test]
fn test_first_sale_by_pattern_example() {
    let pattern = [-2, 1, -1, 3];
    for (initial, price) in [(1, Some(&7)), (2, Some(&7)), (3, None), (2024, Some(&9))] {
        let sales = first_sale_by_pattern(initial);
        assert_eq!(sales.get(&pattern), price);
    }
}

fn part2(initials: &[u64]) -> Option<u64> {
    total_sales_by_pattern(initials).values().max().copied()
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let input: Vec<u64> = input_str.lines().map(parse_number).collect();
    println!("Day 22 part 1: {}", part1(&input));
    match part2(&input) {
        Some(answer) => {
            println!("Day 22 part 2: {answer}");
        }
        None => {
            println!("Day 22 part 2: could not find a solution");
        }
    }
}
