use std::str;

use lib::parse::parse_number;

fn prune(n: u64) -> u64 {
    n % 16777216
}

fn mix(n: u64, m: u64) -> u64 {
    n ^ m
}

fn generate(mut n: u64) -> u64 {
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

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let input: Vec<u64> = input_str.lines().map(parse_number).collect();
    println!("Day 22 part 1: {}", part1(&input));
}
