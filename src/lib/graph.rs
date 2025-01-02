use std::collections::{BTreeSet, HashMap};
use std::hash::Hash;

use super::minheap::MinHeap;

pub type GenericMap<K, V> = HashMap<K, V>;
pub type Distance = i64;

pub fn dijkstra<N, E, FN, IN>(
    source: N,
    mut neighbours: FN,
    edge_cost: E,
) -> GenericMap<N, Distance>
where
    N: Hash + Eq + PartialEq + Ord + Clone,
    FN: for<'a> FnMut(&'a N, Distance) -> IN, // enumerates neighbours
    E: for<'b> Fn(&'b N, &'b N) -> Distance,  // edge cost
    IN: Iterator<Item = N>,                   // iterates over neighbours
{
    let mut q: MinHeap<(Distance, N)> = MinHeap::new();
    let mut prev: GenericMap<N, BTreeSet<N>> = GenericMap::default();
    let mut dist: GenericMap<N, Distance> = GenericMap::default();

    dist.insert(source.clone(), 0);
    q.push((0, source));

    while let Some((upri, u)) = q.pop() {
        for v in neighbours(&u, upri) {
            let cost = edge_cost(&u, &v);
            let alt: Distance = dist.get(&u).unwrap_or(&Distance::MAX).saturating_add(cost);
            let dist_v: Distance = *dist.get(&v).unwrap_or(&Distance::MAX);
            if alt <= dist_v {
                prev.entry(v.clone())
                    .and_modify(|entry| {
                        entry.insert(u.clone());
                    })
                    .or_insert_with(|| {
                        let mut s = BTreeSet::new();
                        s.insert(u.clone());
                        s
                    });
                dist.insert(v.clone(), alt);
                q.push((alt, v));
            }
        }
    }
    dist
}
