use std::str;

fn parse_number(s: &str) -> i32 {
    match s.trim().parse() {
        Ok(n) => n,
        Err(e) => {
            panic!("failed to parse '{s}' as a number: {e}");
        }
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "7 6 4 2 1\n",
        "1 2 7 8 9\n",
        "9 7 6 2 1\n",
        "1 3 2 4 5\n",
        "8 6 4 4 1\n",
        "1 3 6 7 9\n",
    )
}

fn parse_input(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|line| line.split(' ').map(parse_number).collect())
        .collect()
}

#[test]
fn test_parse_input() {
    assert_eq!(
        parse_input(sample_input()),
        vec![
            vec![7, 6, 4, 2, 1],
            vec![1, 2, 7, 8, 9],
            vec![9, 7, 6, 2, 1],
            vec![1, 3, 2, 4, 5],
            vec![8, 6, 4, 4, 1],
            vec![1, 3, 6, 7, 9],
        ]
    );
}

fn diffs(report: &[i32]) -> Vec<i32> {
    report
        .windows(2)
        .map(|w| match w {
            &[l, r] => r - l,
            _ => {
                panic!("expected two window items");
            }
        })
        .collect()
}

fn small(n: &i32) -> bool {
    n.abs() <= 3
}

fn safe(report: &[i32]) -> bool {
    let d = diffs(report);
    match d.as_slice() {
        [] => false,
        [first, rest @ ..] => {
            let sign = first.signum();
            small(first) && rest.iter().all(|x| x.signum() == sign) && rest.iter().all(small)
        }
    }
}

#[test]
fn test_safe_provided_cases() {
    assert!(safe(&[7, 6, 4, 2, 1]));
    assert!(!safe(&[1, 2, 7, 8, 9]));
    assert!(!safe(&[9, 7, 6, 2, 1]));
    assert!(!safe(&[1, 3, 2, 4, 5]));
    assert!(!safe(&[8, 6, 4, 4, 1]));
    assert!(safe(&[1, 3, 6, 7, 9]));
}

#[test]
fn test_safe_extra_cases() {
    assert!(safe(&[7, 6]));
    assert!(!safe(&[7, 2, 1]));
}

fn part1(input: &str) -> usize {
    parse_input(input)
        .iter()
        .filter(|report| safe(&report))
        .count()
}

#[test]
fn test_part1() {
    assert_eq!(part1(sample_input()), 2)
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("part 1: {}", part1(input));
}
