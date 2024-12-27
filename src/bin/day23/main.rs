use std::str;

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "kh-tc\n", "qp-kh\n", "de-cg\n", "ka-co\n", "yn-aq\n", "qp-ub\n", "cg-tb\n", "vc-aq\n",
        "tb-ka\n", "wh-tc\n", "yn-cg\n", "kh-ub\n", "ta-co\n", "de-co\n", "tc-td\n", "tb-wq\n",
        "wh-td\n", "ta-ka\n", "td-qp\n", "aq-cg\n", "wq-ub\n", "ub-vc\n", "de-ta\n", "wq-aq\n",
        "wq-vc\n", "wh-yn\n", "ka-de\n", "kh-ta\n", "co-tc\n", "wh-qp\n", "tb-vc\n", "td-yn\n",
    )
}

fn parse_input_edges(s: &str) -> impl Iterator<Item = (String, String)> + use<'_> {
    s.lines().map(|line| match line.split_once('-') {
        Some((left, right)) => (left.to_string(), right.to_string()),
        None => {
            panic!("line '{line}' should contain a '-'");
        }
    })
}

mod part1 {
    use super::parse_input_edges;
    #[cfg(test)]
    use super::sample_input;
    use std::collections::{BTreeSet, HashMap, HashSet};

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
        for edge in parse_input_edges(s) {
            result.insert(edge);
        }
        result
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
    }

    pub fn solve(input: &str) -> usize {
        let net: Network = parse_input(input);
        net.t_triples().len()
    }

    #[test]
    fn test_solve() {
        assert_eq!(solve(sample_input()), 7);
    }
}

mod part2 {
    use super::parse_input_edges;
    #[cfg(test)]
    use super::sample_input;
    use std::collections::{HashMap, HashSet};

    /// This implementation of the Bron-Kerbosch algorithm was taken
    /// from Rosetta Code, at
    /// https://rosettacode.org/wiki/Bron%E2%80%93Kerbosch_algorithm#Rust
    /// (on 2024-12-27).
    fn bron_kerbosch_v2(
        r: &HashSet<String>,
        p: &mut HashSet<String>,
        x: &mut HashSet<String>,
        g: &HashMap<String, HashSet<String>>,
        cliques: &mut Vec<Vec<String>>,
    ) {
        if p.is_empty() && x.is_empty() {
            if r.len() > 2 {
                let mut clique: Vec<String> = r.iter().cloned().collect();
                clique.sort();
                cliques.push(clique);
            }
            return;
        }

        // Choose a pivot with the maximum degree in P ∪ X
        let pivot = p
            .union(x)
            .max_by_key(|v| g.get(*v).map_or(0, |neighbors| neighbors.len()))
            .cloned();

        if let Some(pivot_vertex) = pivot {
            let neighbors = g.get(&pivot_vertex).cloned().unwrap_or_default();
            let candidates: Vec<String> = p.difference(&neighbors).cloned().collect();

            for v in candidates {
                // New R is R ∪ {v}
                let mut new_r = r.clone();
                new_r.insert(v.clone());

                // New P is P ∩ N(v)
                let neighbors_v = g.get(&v).cloned().unwrap_or_default();
                let mut new_p = p
                    .intersection(&neighbors_v)
                    .cloned()
                    .collect::<HashSet<String>>();

                // New X is X ∩ N(v)
                let mut new_x = x
                    .intersection(&neighbors_v)
                    .cloned()
                    .collect::<HashSet<String>>();

                // Recursive call
                bron_kerbosch_v2(&new_r, &mut new_p, &mut new_x, g, cliques);

                // Move v from P to X
                p.remove(&v);
                x.insert(v);
            }
        }
    }

    fn maximal_cliques<I>(edges: I) -> Vec<Vec<String>>
    where
        I: Iterator<Item = (String, String)>,
    {
        let mut graph: HashMap<String, HashSet<String>> = HashMap::new();
        for (src, dest) in edges {
            // Graph is bidirectional, so we insert the forward edge:
            graph
                .entry(src.to_string())
                .or_default()
                .insert(dest.to_string());
            // ... and the backward edge:
            graph
                .entry(dest.to_string())
                .or_default()
                .insert(src.to_string());
        }

        let r: HashSet<String> = HashSet::new();
        let mut p: HashSet<String> = graph.keys().cloned().collect();
        let mut x: HashSet<String> = HashSet::new();

        // Collect cliques
        let mut cliques: Vec<Vec<String>> = Vec::new();
        bron_kerbosch_v2(&r, &mut p, &mut x, &graph, &mut cliques);
        cliques
    }

    #[test]
    fn test_maximal_cliques() {
        let input = vec![
            ("a", "b"),
            ("a", "c"),
            ("b", "c"),
            ("d", "e"),
            ("d", "f"),
            ("e", "f"),
        ];
        let mut cliques = maximal_cliques(
            input
                .into_iter()
                .map(|(a, b)| (a.to_string(), b.to_string())),
        );
        cliques.sort();
        assert_eq!(
            cliques,
            vec![
                vec!["a".to_string(), "b".to_string(), "c".to_string()],
                vec!["d".to_string(), "e".to_string(), "f".to_string()],
            ]
        );
    }

    pub fn solve(s: &str) -> String {
        let mut cliques = maximal_cliques(parse_input_edges(s));
        cliques.sort_by_key(|a| a.len());
        match cliques.pop() {
            Some(mut clique) => {
                clique.sort();
                clique.join(",")
            }
            None => {
                panic!("no clique");
            }
        }
    }

    #[test]
    fn test_solve() {
        assert_eq!(solve(sample_input()), "co,de,ka,ta".to_string());
    }
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("Day 23 part 1: {}", part1::solve(input_str));
    println!("Day 23 part 2: {}", part2::solve(input_str));
}
