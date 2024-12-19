use std::str;

use patricia_tree::StringPatriciaMap;
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

fn make_atom_set(t: &Towels) -> StringPatriciaMap<()> {
    let mut result = StringPatriciaMap::new();
    for available in t.available.iter() {
        result.insert(available, ());
    }
    result
}

fn count_ways(
    wanted: &str,
    atoms: &StringPatriciaMap<()>,
    memo: &mut StringPatriciaMap<usize>,
) -> usize {
    if let Some(ways) = memo.get(wanted) {
        return *ways;
    }
    let prefixes: Vec<String> = atoms
        .common_prefixes(wanted)
        .map(|(s, _)| s.to_string())
        .collect();
    if atoms.contains_key(wanted) {
        assert!(prefixes.iter().any(|s| s == wanted));
    }
    let result = prefixes
        .iter()
        .map(|prefix| {
            if prefix.len() == wanted.len() {
                1
            } else {
                let right = &wanted[prefix.len()..];
                count_ways(right, atoms, memo)
            }
        })
        .sum();
    memo.insert(wanted, result);
    result
}

#[test]
fn test_count_ways_basics() {
    let mut memo = StringPatriciaMap::new();
    let mut atoms = StringPatriciaMap::new();
    for s in ["a", "b"] {
        atoms.insert(s, ());
    }

    assert_eq!(count_ways("a", &atoms, &mut memo), 1);
    assert_eq!(count_ways("aa", &atoms, &mut memo), 1);
    assert_eq!(count_ways("aaa", &atoms, &mut memo), 1);
    assert_eq!(count_ways("b", &atoms, &mut memo), 1);

    assert_eq!(count_ways("ab", &atoms, &mut memo), 1);
    assert_eq!(count_ways("ba", &atoms, &mut memo), 1);
    assert_eq!(count_ways("aba", &atoms, &mut memo), 1);
}

#[test]
fn test_count_ways_doubles() {
    let mut memo = StringPatriciaMap::new();
    let mut atoms = StringPatriciaMap::new();
    for s in ["a", "aa", "b", "bb"] {
        atoms.insert(s, ());
    }
    assert_eq!(count_ways("aabb", &atoms, &mut memo), 4);
}

#[test]
fn test_count_ways_mix() {
    let mut atoms = StringPatriciaMap::new();
    let mut memo = StringPatriciaMap::new();
    for s in ["a", "ab", "b"] {
        atoms.insert(s, ());
    }
    assert_eq!(count_ways("aba", &atoms, &mut memo), 2);
}

fn part2(t: &Towels) -> usize {
    let mut memo = StringPatriciaMap::new();
    let atoms = make_atom_set(t);
    t.wanted
        .iter()
        .map(|w| count_ways(w, &atoms, &mut memo))
        .sum()
}

#[test]
fn test_part2_simple() {
    let towels = Towels {
        available: vec!["b".to_string(), "r".to_string(), "br".to_string()],
        wanted: vec!["br".to_string()],
    };
    assert_eq!(part2(&towels), 2);
}

#[test]
fn test_part2() {
    let towels = parse_input(sample_input());
    assert_eq!(part2(&towels), 16);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let towels = parse_input(input_str);
    println!("Day 19 part 1: {}", part1(&towels));
    println!("Day 19 part 2: {}", part2(&towels));
}
