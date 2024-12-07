use std::collections::{HashMap, HashSet};
use std::str;
use lib::parse::parse_number;

enum Removal {
    Vertices(HashSet<i32>),
    Cycle,
    Empty,
}

#[derive(Debug, Clone, Default)]
struct Graph {
    vertices: HashSet<i32>,
    edges: HashMap<i32, HashSet<i32>>,
}

impl Graph {
    pub fn new() -> Graph {
        Graph::default()
    }

    pub fn insert_vertex(&mut self, item: i32) {
        self.vertices.insert(item);
    }

    pub fn insert_edge(&mut self, from: i32, to: i32) {
        assert!(self.vertices.contains(&from));
        assert!(self.vertices.contains(&to));
        self.edges
            .entry(from)
            .and_modify(|dests| {
                dests.insert(to);
            })
            .or_insert_with(|| {
                let mut d = HashSet::new();
                d.insert(to);
                d
            });
    }

    pub fn contains_edge(&self, from: &i32, to: &i32) -> bool {
        match self.edges.get(from) {
            None => false,
            Some(to_vertices) => to_vertices.contains(to),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.vertices.is_empty()
    }

    fn retain_vertices<F: Fn(&i32) -> bool>(&mut self, want: F) {
        // Remove the vertices we don't want to keep.
        self.vertices.retain(want);

        // Now re-establish the invariant that all edges are to and
        // from vertices present in self.vertices.
        self.edges.retain(|v, to_vertices| {
            if !self.vertices.contains(v) {
                return false;
            }
            to_vertices.retain(|to| {
                // There is an edge v->to, and we already know v is in retained.
                self.vertices.contains(to)
            });
            true
        });
    }

    fn remove_vertices(&mut self, goners: &HashSet<i32>) {
        self.edges.retain(|vertex, to_vertices| {
            if goners.contains(vertex) {
                to_vertices.retain(|to| !goners.contains(to));
                false
            } else {
                true
            }
        });
        self.vertices.retain(|vertex| !goners.contains(vertex));
    }

    fn remove_vertices_without_pred(&mut self) -> Removal {
        if self.is_empty() {
            Removal::Empty
        } else {
            let candidates: HashSet<i32> = self.vertices.clone();
            let mut disqualified: HashSet<i32> = HashSet::new();
            for (_from, to_vertices) in self.edges.iter() {
                for v in to_vertices {
                    disqualified.insert(*v);
                }
            }
            let result: HashSet<i32> = candidates.difference(&disqualified).copied().collect();
            if result.is_empty() {
                Removal::Cycle
            } else {
                self.remove_vertices(&result);
                Removal::Vertices(result)
            }
        }
    }

    pub fn topological_ordering(mut self) -> Vec<i32> {
        let mut result = Vec::new();
        loop {
            match self.remove_vertices_without_pred() {
                Removal::Empty => {
                    return result;
                }
                Removal::Cycle => {
                    panic!("cycle: {:?}", self);
                }
                Removal::Vertices(vs) => {
                    assert!(!vs.is_empty());
                    result.extend(vs.iter());
                }
            }
        }
    }
}

#[derive(Clone, Default, Debug)]
struct OrderingRules {
    successors: Graph,
}

impl OrderingRules {
    pub fn new() -> OrderingRules {
        OrderingRules {
            successors: Graph::new(),
        }
    }

    #[cfg(test)]
    pub fn from(vertices: &[i32], edges: &[(i32, i32)]) -> OrderingRules {
        let mut rules = OrderingRules::default();
        for v in vertices {
            rules.insert_vertex(*v);
        }
        for (from, to) in edges {
            rules.insert_edge(*from, *to);
        }
        rules
    }

    pub fn insert_vertex(&mut self, item: i32) {
        self.successors.insert_vertex(item);
    }

    pub fn insert_edge(&mut self, pred: i32, succ: i32) {
        self.successors.insert_vertex(pred);
        self.successors.insert_vertex(succ);
        self.successors.insert_edge(pred, succ);
    }

    pub fn contains_edge(&self, left: &i32, right: &i32) -> bool {
        self.successors.contains_edge(left, right)
    }

    pub fn check_update(&self, values: &[i32]) -> bool {
        for r in 1..values.len() {
            for l in 0..r {
                let left = values[l];
                let right = values[r];
                if self.contains_edge(&right, &left) {
                    return false;
                }
            }
        }
        true
    }

    fn retain_pages<F: Fn(&i32) -> bool>(&mut self, want: F) {
        self.successors.retain_vertices(want);
    }

    pub fn topological_ordering(self) -> Vec<i32> {
        self.successors.topological_ordering()
    }
}

fn parse_ordering_rules(input: &str) -> OrderingRules {
    let mut result = OrderingRules::new();
    for line in input.lines() {
        let (pred, succ) = line
            .split_once('|')
            .expect("ordering rules should contain '|'");
        result.insert_edge(parse_number(pred), parse_number(succ));
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
    let mut rules = parse_ordering_rules(order_rules);
    let update_lists = parse_updates(updates);
    let pages_present_in_updates: HashSet<i32> = update_lists.iter().flatten().copied().collect();
    for page in pages_present_in_updates {
        rules.insert_vertex(page);
    }
    (rules, update_lists)
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

    assert!(rules.contains_edge(&47, &53));
    assert!(rules.contains_edge(&53, &13));
    assert!(!rules.contains_edge(&47, &97));

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

fn fix_ordering(update: &HashSet<i32>, rules: &OrderingRules) -> Vec<i32> {
    let mut result = Vec::new();
    let mut rules_for_this_update = rules.clone();
    rules_for_this_update.retain_pages(|page| update.contains(page));
    let ordering: Vec<i32> = rules_for_this_update.topological_ordering();
    for item in ordering {
        if update.contains(&item) {
            result.push(item);
        }
    }
    result
}

#[test]
fn test_fix_ordering() {
    let sample_rules = OrderingRules::from(&[1, 2], &[(1, 2)]);
    let reversed_rules = OrderingRules::from(&[1, 2], &[(2, 1)]);
    assert_eq!(
        fix_ordering(
            &HashSet::new(), // empty update
            &sample_rules
        ),
        vec![] // empty output
    );

    dbg!(&sample_rules);
    assert_eq!(
        fix_ordering(&HashSet::from([1, 2]), &sample_rules),
        vec![1, 2]
    );

    dbg!(&reversed_rules);
    assert_eq!(
        fix_ordering(&HashSet::from([1, 2]), &reversed_rules),
        vec![2, 1]
    );
}

fn part2(input: &str) -> i32 {
    let (rules, updates) = parse_input(input);

    fn make_set(update: &[i32]) -> HashSet<i32> {
        update.iter().copied().collect()
    }

    updates
        .iter()
        .filter(|update| !rules.check_update(update))
        .map(|update| make_set(update))
        .map(|update| fix_ordering(&update, &rules))
        .map(|u| middle(&u))
        .sum()
}

#[test]
fn test_part2() {
    assert_eq!(part2(example_input()), 123);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 05 part 1: {}", part1(input));
    println!("day 05 part 2: {}", part2(input));
}
