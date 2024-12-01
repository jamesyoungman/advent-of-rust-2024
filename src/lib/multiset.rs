use std::collections::BTreeMap;

pub struct MultiSet<T> {
    items: BTreeMap<T, usize>,
}

impl<T: Ord> FromIterator<T> for MultiSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut result = BTreeMap::new();
        for item in iter {
            result
                .entry(item)
                .and_modify(|count| *count += 1)
                .or_insert(1);
        }
        MultiSet { items: result }
    }
}

impl<T: Ord> MultiSet<T> {
    pub fn get(&self, key: &T) -> Option<&usize> {
        self.items.get(key)
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn contains_key(&self, key: &T) -> bool {
        self.items.contains_key(key)
    }
}

#[test]
fn test_counting() {
    let counts: MultiSet<i32> = vec![1, 2, 3, 4, 3, 6].iter().copied().collect();
    assert_eq!(counts.get(&1), Some(&1));
    assert_eq!(counts.get(&3), Some(&2));
}

#[test]
fn test_absent() {
    let counts: MultiSet<i32> = vec![1, 2].iter().copied().collect();
    assert_eq!(counts.get(&99), None);
}

#[test]
fn test_empty() {
    let counts: MultiSet<i32> = Vec::new().iter().copied().collect();
    assert_eq!(counts.get(&1), None);
}

#[test]
fn test_len() {
    let counts: MultiSet<i32> = vec![1, 2].iter().copied().collect();
    assert_eq!(counts.len(), 2);

    let empty: MultiSet<i32> = Vec::new().iter().copied().collect();
    assert_eq!(empty.len(), 0);
}

#[test]
fn test_contains_key() {
    let counts: MultiSet<i32> = vec![1, 2].iter().copied().collect();
    assert!(counts.contains_key(&1));
    assert!(counts.contains_key(&2));
    assert!(!counts.contains_key(&3));

    let empty: MultiSet<i32> = Vec::new().iter().copied().collect();
    assert!(!empty.contains_key(&0));
}
