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

    pub fn drain(&mut self) -> Drain<T> {
        Drain {
            max_heap_drain: self.max_heap.drain(),
        }
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

mod tests {
    #[test]
    fn new() {
        let mut h = MinHeap::new();
        h.push(1);
        assert_eq!(h.pop(), Some(1));
    }

    #[test]
    fn peek() {
        let mut h = MinHeap::new();
        h.push(1);
        h.push(2);
        assert_eq!(h.peek(), Some(&1));
    }

    #[test]
    fn pop() {
        let mut h = MinHeap::new();
        h.push(1);
        h.push(2);
        assert_eq!(h.pop(), Some(1));
        assert_eq!(h.pop(), Some(2));

        // Same again but items originally added in the opposite order
        let mut h = MinHeap::new();
        h.push(1);
        h.push(2);
        assert_eq!(h.pop(), Some(1));
        assert_eq!(h.pop(), Some(2));
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
}
