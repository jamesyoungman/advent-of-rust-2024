use std::cmp::{Ord, Reverse};
use std::collections::{binary_heap, BinaryHeap};

pub struct MinHeap<T> {
    max_heap: BinaryHeap<Reverse<T>>,
}

impl<T> MinHeap<T>
where
    T: Ord,
{
    pub fn new() -> MinHeap<T> {
        MinHeap {
            max_heap: BinaryHeap::new(),
        }
    }

    pub fn with_capacity(capacity: usize) -> MinHeap<T> {
        MinHeap {
            max_heap: BinaryHeap::with_capacity(capacity),
        }
    }

    pub fn capacity(&self) -> usize {
        self.max_heap.capacity()
    }

    pub fn push(&mut self, item: T) {
        self.max_heap.push(Reverse(item))
    }

    pub fn pop(&mut self) -> Option<T> {
        self.max_heap.pop().map(unreverse)
    }

    pub fn peek(&self) -> Option<&T> {
        match self.max_heap.peek() {
            Some(peeked) => Some(&peeked.0),
            None => None,
        }
    }

    pub fn len(&self) -> usize {
        self.max_heap.len()
    }

    pub fn is_empty(&self) -> bool {
        self.max_heap.is_empty()
    }

    pub fn drain(&mut self) -> Drain<T> {
        Drain {
            max_heap_drain: self.max_heap.drain(),
        }
    }

    pub fn append(&mut self, other: &mut MinHeap<T>) {
        self.max_heap.append(&mut other.max_heap);
    }
}

fn unreverse<T>(item: Reverse<T>) -> T {
    item.0
}

pub struct Drain<'a, T> {
    max_heap_drain: binary_heap::Drain<'a, Reverse<T>>,
}

impl<T> Iterator for Drain<'_, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.max_heap_drain.next().map(unreverse)
    }
}

impl<T: Ord> From<Vec<T>> for MinHeap<T> {
    fn from(value: Vec<T>) -> Self {
        let rv: Vec<Reverse<T>> = value.into_iter().map(|item| Reverse(item)).collect();
        MinHeap {
            max_heap: BinaryHeap::from(rv),
        }
    }
}

impl<T: Ord> FromIterator<T> for MinHeap<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let rv: Vec<Reverse<T>> = iter.into_iter().map(|item| Reverse(item)).collect();
        MinHeap {
            max_heap: BinaryHeap::from(rv),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::MinHeap;

    #[test]
    fn new() {
        let mut h = MinHeap::new();
        h.push(1);
        assert_eq!(h.pop(), Some(1));
    }

    #[test]
    fn is_empty() {
        let mut h = MinHeap::new();
        assert!(h.is_empty());
        h.push(1);
        assert!(!h.is_empty());
    }

    #[test]
    fn len() {
        let mut h = MinHeap::new();
        assert_eq!(h.len(), 0);
        h.push(1);
        assert_eq!(h.len(), 1);
    }

    #[test]
    fn peek() {
        let mut h = MinHeap::new();
        h.push(1);
        h.push(2);
        assert_eq!(h.peek(), Some(&1));
        h.pop();
        assert_eq!(h.peek(), Some(&2));
        h.pop();
        assert_eq!(h.peek(), None);
    }

    #[test]
    fn pop() {
        let mut h = MinHeap::new();
        h.push(1);
        h.push(2);
        assert_eq!(h.pop(), Some(1));
        assert_eq!(h.pop(), Some(2));
        assert_eq!(h.pop(), None);

        // Same again but items originally added in the opposite order
        let mut h = MinHeap::new();
        h.push(1);
        h.push(2);
        assert_eq!(h.pop(), Some(1));
        assert_eq!(h.pop(), Some(2));
        assert_eq!(h.pop(), None);
    }

    /// Verify that we don't require any trait beyond Ord.
    #[test]
    fn ord_requirement() {
        #[derive(Ord, PartialOrd, PartialEq, Eq)]
        struct Orderable(i32);

        let mut h = MinHeap::new();
        h.push(Orderable(10));
        h.push(Orderable(20));
        match h.pop() {
            Some(got) => {
                assert_eq!(got.0, 10);
            }
            None => {
                panic!("should be able to pop from a non-empty heap");
            }
        }
    }

    #[test]
    fn append() {
        let mut h1 = MinHeap::new();
        h1.push(10);
        h1.push(20);
        assert_eq!(h1.len(), 2);

        let mut h2 = MinHeap::new();
        h2.push(1000);
        h2.push(2);
        h2.push(6);
        assert_eq!(h2.len(), 3);

        h1.append(&mut h2);
        assert_eq!(h2.len(), 0);
        assert_eq!(h1.len(), 5);

        assert_eq!(h1.pop(), Some(2));
        assert_eq!(h1.pop(), Some(6));
        assert_eq!(h1.pop(), Some(10));
        assert_eq!(h1.pop(), Some(20));
        assert_eq!(h1.pop(), Some(1000));
        assert_eq!(h1.pop(), None);
    }

    #[test]
    fn capacity() {
        let h: MinHeap<i32> = MinHeap::with_capacity(400);
        assert!(h.capacity() >= 400);
    }
}
