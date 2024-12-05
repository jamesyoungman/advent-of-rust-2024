use std::collections::{HashMap, HashSet};
use std::str;

struct OrderingRules {
    predecessors: HashMap<i32, HashSet<i32>>,
}

impl OrderingRules {
    pub fn new() -> OrderingRules {
        OrderingRules {
            predecessors: HashMap::new(),
        }
    }

    pub fn insert(&mut self, pred: i32, succ: i32) {
        self.predecessors
            .entry(succ)
            .and_modify(|entry| {
                entry.insert(pred);
            })
            .or_insert_with(|| {
                let mut x = HashSet::new();
                x.insert(pred);
                x
            });
    }

    #[cfg(test)]
    pub fn contains(&self, (left, right): (i32, i32)) -> bool {
        match self.predecessors.get(&right) {
            None => false,
            Some(items) => items.contains(&left),
        }
    }

    pub fn check_update(&self, values: &[i32]) -> bool {
        for r in 1..values.len() {
            for l in 0..r {
                let left = values[l];
                let right = values[r];
                match self.predecessors.get(&left) {
                    None => (),
                    Some(items) => {
                        if items.contains(&right) {
                            // right should be a predecessor of left, but instead it is not.
                            return false;
                        }
                    }
                }
            }
        }
        true
    }
}

fn parse_number(s: &str) -> i32 {
    match s.trim().parse() {
        Ok(n) => n,
        Err(e) => {
            panic!("failed to parse '{s}' as a number: {e}");
        }
    }
}

fn parse_ordering_rules(input: &str) -> OrderingRules {
    let mut result = OrderingRules::new();
    for line in input.lines() {
        let (pred, succ) = line
            .split_once('|')
            .expect("ordering rules should contain '|'");
        result.insert(parse_number(pred), parse_number(succ));
    }
    result
}

fn parse_updates(input: &str) -> Vec<Vec<i32>> {
    input
        .lines()
        .map(|line| line.split(',').map(parse_number).collect())
        .collect()
}

fn parse_input(input: &str) -> (OrderingRules, Vec<Vec<i32>>) {
    let (order_rules, updates) = input
        .split_once("\n\n")
        .expect("input should have two line breaks");
    (parse_ordering_rules(order_rules), parse_updates(updates))
}

#[cfg(test)]
fn example_input() -> &'static str {
    concat!(
        "47|53\n",
        "97|13\n",
        "97|61\n",
        "97|47\n",
        "75|29\n",
        "61|13\n",
        "75|53\n",
        "29|13\n",
        "97|29\n",
        "53|29\n",
        "61|53\n",
        "97|53\n",
        "61|29\n",
        "47|13\n",
        "75|47\n",
        "97|75\n",
        "47|61\n",
        "75|61\n",
        "47|29\n",
        "75|13\n",
        "53|13\n",
        "\n",
        "75,47,61,53,29\n",
        "97,61,53,29,13\n",
        "75,29,13\n",
        "75,97,47,61,53\n",
        "61,13,29\n",
        "97,13,75,29,47\n",
    )
}

#[test]
fn test_parse_input() {
    let (rules, updates) = parse_input(example_input());

    assert!(rules.contains((47, 53)));
    assert!(rules.contains((53, 13)));
    assert!(!rules.contains((47, 97)));

    assert_eq!(updates[0], vec![75, 47, 61, 53, 29]);
    assert_eq!(updates[1], vec![97, 61, 53, 29, 13]);
    assert_eq!(updates[2], vec![75, 29, 13]);
    assert_eq!(updates[3], vec![75, 97, 47, 61, 53]);
    assert_eq!(updates[4], vec![61, 13, 29]);
    assert_eq!(updates[5], vec![97, 13, 75, 29, 47]);
}

#[test]
fn test_check_update() {
    let (rules, _) = parse_input(example_input());
    assert!(rules.check_update(&[75, 47, 61, 53, 29]));
    assert!(rules.check_update(&[97, 61, 53, 29, 13]));
    assert!(rules.check_update(&[75, 29, 13]));
    assert!(!rules.check_update(&[75, 97, 47, 61, 53]));
    assert!(!rules.check_update(&[61, 13, 29]));
    assert!(!rules.check_update(&[97, 13, 75, 29, 47]));
}

fn middle(items: &[i32]) -> i32 {
    let h = items.len() / 2;
    if h * 2 == items.len() {
        panic!("did not expect an even number of items");
    }
    items[h]
}

#[test]
fn test_middle() {
    assert_eq!(middle(&[3]), 3);
    assert_eq!(middle(&[1, 2, 3]), 2);
    assert_eq!(middle(&[3, 2, 1]), 2);
    assert_eq!(middle(&[2, 3, 1]), 3);
    assert_eq!(middle(&[1, 2, 7, 4, 5]), 7);
}

fn part1(input: &str) -> i32 {
    let (rules, updates) = parse_input(input);
    updates
        .iter()
        .filter(|update| rules.check_update(update))
        .map(|u| middle(u))
        .sum()
}

#[test]
fn test_part1() {
    assert_eq!(part1(example_input()), 143);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 05 part 1: {}", part1(input));
}
