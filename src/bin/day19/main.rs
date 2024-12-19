use std::str;

use regex::Regex;

struct Towels {
    available: Vec<String>,
    wanted: Vec<String>,
}

impl Towels {
    fn new(available: Vec<String>, wanted: Vec<String>) -> Towels {
        Towels { available, wanted }
    }

    fn available_regex(&self) -> Regex {
        let available_as_rs: Vec<String> =
            self.available.iter().map(|w| format!("({})", w)).collect();
        let rx_pattern: String = format!("^({})*$", available_as_rs.join("|"));
        dbg!(&rx_pattern);
        match Regex::new(&rx_pattern) {
            Ok(regex) => regex,
            Err(e) => {
                panic!("generates regex pattern {rx_pattern} is not valid: {e}");
            }
        }
    }
}

impl From<&str> for Towels {
    fn from(input: &str) -> Self {
        fn parse_patterns(s: &str) -> Vec<String> {
            s.split(", ").map(String::from).collect()
        }

        fn parse_wanted(s: &str) -> Vec<String> {
            s.lines()
                .filter(|line| !line.is_empty())
                .map(String::from)
                .collect()
        }

        match input.split_once("\n\n") {
            Some((available, wanted)) => {
                Towels::new(parse_patterns(available), parse_wanted(wanted))
            }
            None => {
                panic!("expected double newline in the input");
            }
        }
    }
}

fn parse_input(input: &str) -> Towels {
    Towels::from(input)
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "r, wr, b, g, bwu, rb, gb, br\n",
        "\n",
        "brwrr\n",
        "bggr\n",
        "gbbr\n",
        "rrbgbr\n",
        "ubwu\n",
        "bwurrg\n",
        "brgr\n",
        "bbrgwb\n",
        // This empty line shouldn't really be here but filtering it
        // out could prevent a mishap with handling examples or
        // inputs.
        "\n",
    )
}

#[test]
fn test_parse_input() {
    let got = parse_input(sample_input());
    assert_eq!(
        got.available
            .iter()
            .map(|s| s.as_str())
            .collect::<Vec<&str>>(),
        vec!["r", "wr", "b", "g", "bwu", "rb", "gb", "br"]
    );

    assert_eq!(
        got.wanted.iter().map(|s| s.as_str()).collect::<Vec<_>>(),
        vec!["brwrr", "bggr", "gbbr", "rrbgbr", "ubwu", "bwurrg", "brgr", "bbrgwb",]
    );
}

fn part1(t: &Towels) -> usize {
    let regex = t.available_regex();
    t.wanted.iter().filter(|w| regex.is_match(w)).count()
}

#[test]
fn test_part1() {
    let towels = parse_input(sample_input());
    assert_eq!(part1(&towels), 6);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let towels = parse_input(input_str);
    println!("Day 19 part 1: {}", part1(&towels));
}
