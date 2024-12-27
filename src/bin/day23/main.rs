use std::collections::{BTreeSet, HashMap, HashSet};
use std::str;

#[derive(PartialEq, Eq, Clone, Debug, Default)]
struct Network {
    edges: HashMap<String, HashSet<String>>,
}

fn in_canonical_order<'a>(id1: &'a str, id2: &'a str) -> (&'a str, &'a str) {
    if is_in_canonical_order(id1, id2) {
        (id1, id2)
    } else {
        (id2, id1)
    }
}

fn is_t_name(s: &str) -> bool {
    s.starts_with('t')
}

fn is_in_canonical_order<'a>(id1: &'a str, id2: &'a str) -> bool {
    match (is_t_name(id1), is_t_name(id2)) {
        (true, true) | (false, false) => id1 < id2,
        (true, false) => true,
        (false, true) => false,
    }
}

#[test]
fn test_in_canonical_order() {
    assert!(is_in_canonical_order("ab", "pq"));
    assert!(!is_in_canonical_order("pq", "ab"));

    assert!(is_in_canonical_order("tz", "aa"));
    assert!(!is_in_canonical_order("aa", "tz"));

    assert!(is_in_canonical_order("ta", "tb"));
    assert!(!is_in_canonical_order("tb", "ta"));
}

impl Network {
    fn insert(&mut self, edge: (String, String)) {
        let (id1, id2) = in_canonical_order(edge.0.as_str(), edge.1.as_str());
        self.edges
            .entry(id1.to_string())
            .and_modify(|neighbours| {
                neighbours.insert(id2.to_string());
            })
            .or_insert_with(|| {
                let mut n = HashSet::new();
                n.insert(id2.to_string());
                n
            });
    }

    fn t_triples(&self) -> BTreeSet<BTreeSet<&str>> {
        let all_nodes: HashSet<&str> = self
            .edges
            .keys()
            .chain(self.edges.values().flat_map(|n| n.iter()))
            .map(|s| s.as_str())
            .collect();
        let mut result: BTreeSet<BTreeSet<&str>> = Default::default();
        for (t_name, t_neighbours) in self
            .edges
            .iter()
            .filter(|(name, _neighbours)| is_t_name(name))
        {
            for second_name in t_neighbours {
                for third_name in all_nodes.iter() {
                    if self.contains_edge((second_name, third_name))
                        && self.contains_edge((t_name, third_name))
                    {
                        let r = vec![t_name, second_name, *third_name].into_iter().collect();
                        result.insert(r);
                    }
                }
            }
        }
        result
    }

    fn contains_edge(&self, edge: (&str, &str)) -> bool {
        let (id1, id2) = in_canonical_order(edge.0, edge.1);
        match self.edges.get(id1) {
            Some(nodes) => nodes.contains(id2),
            None => false,
        }
    }
}

fn parse_input(s: &str) -> Network {
    let mut result = Network::default();
    for line in s.lines() {
        match line.split_once('-') {
            Some((left, right)) => {
                result.insert((left.to_string(), right.to_string()));
            }
            None => {
                panic!("line '{line}' should contain a '-'");
            }
        }
    }
    result
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "kh-tc\n", "qp-kh\n", "de-cg\n", "ka-co\n", "yn-aq\n", "qp-ub\n", "cg-tb\n", "vc-aq\n",
        "tb-ka\n", "wh-tc\n", "yn-cg\n", "kh-ub\n", "ta-co\n", "de-co\n", "tc-td\n", "tb-wq\n",
        "wh-td\n", "ta-ka\n", "td-qp\n", "aq-cg\n", "wq-ub\n", "ub-vc\n", "de-ta\n", "wq-aq\n",
        "wq-vc\n", "wh-yn\n", "ka-de\n", "kh-ta\n", "co-tc\n", "wh-qp\n", "tb-vc\n", "td-yn\n",
    )
}

#[test]
fn test_sample_input_connectivity_examples() {
    let net: Network = parse_input(sample_input());
    assert!(net.contains_edge(("co", "de")));
    assert!(net.contains_edge(("de", "co")));
    assert!(!net.contains_edge(("de", "de"))); // no self edges
    assert!(!net.contains_edge(("de", "qq"))); // no node qq
}

#[test]
fn test_count_t_triples() {
    let net: Network = parse_input(sample_input());
    fn make_triple<'a>(a: &'a str, b: &'a str, c: &'a str) -> BTreeSet<&'a str> {
        let mut result = BTreeSet::new();
        result.insert(a);
        result.insert(b);
        result.insert(c);
        result
    }
    let triples_expected: BTreeSet<BTreeSet<&str>> = BTreeSet::from([
        make_triple("co", "de", "ta"),
        make_triple("co", "ka", "ta"),
        make_triple("de", "ka", "ta"),
        make_triple("qp", "td", "wh"),
        make_triple("tb", "vc", "wq"),
        make_triple("tc", "td", "wh"),
        make_triple("td", "wh", "yn"),
    ]);
    let triples_got = net.t_triples();
    assert_eq!(triples_expected, triples_got);
    dbg!(&triples_got);
    assert_eq!(triples_got.len(), 7);
}

fn part1(net: &Network) -> usize {
    net.t_triples().len()
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let net: Network = parse_input(input_str);
    println!("Day 23 part 1: {}", part1(&net));
}
