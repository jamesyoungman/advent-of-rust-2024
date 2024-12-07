use lib::grid::{CompassDirection, Position};

use std::collections::HashSet;
use std::error::Error;
use std::fmt::{Debug, Display, Write};
use std::ops::{Index, IndexMut};
use std::str;

mod dense_grid_impl {
    use super::*;

    #[derive(Debug)]
    pub struct DenseGrid<T> {
        pub w: usize,
        pub h: usize,
        items: Vec<T>,
    }

    impl<T> Clone for DenseGrid<T>
    where
        T: Clone,
    {
        fn clone(&self) -> Self {
            DenseGrid {
                w: self.w,
                h: self.h,
                items: self.items.clone(),
            }
        }
    }

    #[derive(Debug)]
    pub struct GridTooBig {}
    impl Error for GridTooBig {}

    impl Display for GridTooBig {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str("grid is too big")
        }
    }

    impl<T: Clone> DenseGrid<T> {
        pub fn new(w: usize, h: usize, fill: T) -> Result<DenseGrid<T>, GridTooBig> {
            match w.checked_mul(h) {
                Some(len) => {
                    let mut items = Vec::with_capacity(len);
                    items.resize(len, fill);
                    Ok(DenseGrid { w, h, items })
                }
                None => Err(GridTooBig {}),
            }
        }
    }

    impl<T: Debug> DenseGrid<T> {
        fn pos(&self, index: (usize, usize)) -> usize {
            let (x, y) = index;
            assert!(x < self.w);
            assert!(y < self.h);
            y * self.w + x
        }

        pub fn iter(&self) -> DenseGridIterator<T> {
            DenseGridIterator {
                inner: self.items.iter(),
            }
        }
    }

    impl<T: Debug> Index<(usize, usize)> for DenseGrid<T> {
        type Output = T;

        fn index(&self, index: (usize, usize)) -> &Self::Output {
            &self.items[self.pos(index)]
        }
    }

    impl<T: Debug> IndexMut<(usize, usize)> for DenseGrid<T> {
        fn index_mut(&mut self, index: (usize, usize)) -> &mut Self::Output {
            let i = self.pos(index);
            &mut self.items[i]
        }
    }

    pub struct DenseGridIterator<'a, T> {
        inner: std::slice::Iter<'a, T>,
    }

    impl<'a, T> Iterator for DenseGridIterator<'a, T> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            self.inner.next()
        }
    }

    #[test]
    fn test_dense_grid() {
        let mut g = DenseGrid::new(1, 1, 0u8).expect("sufficiently small");
        assert_eq!(g[(0, 0)], 0);
        g[(0, 0)] = 2;
        assert_eq!(g[(0, 0)], 2);

        let v: Vec<u8> = g.iter().copied().collect();
        assert_eq!(&v, &[2]);

        let mut it = g.iter();
        assert_eq!(it.next(), Some(&2u8));
        assert_eq!(it.next(), None);
    }
}

use dense_grid_impl::DenseGrid;
use dense_grid_impl::DenseGridIterator;
use dense_grid_impl::GridTooBig;

#[derive(Debug, Clone)]
struct VisitSet {
    data: DenseGrid<u8>,
}

impl VisitSet {
    pub fn new(w: i64, h: i64) -> Result<VisitSet, GridTooBig> {
        Ok(VisitSet {
            data: DenseGrid::new(w as usize, h as usize, 0u8)?,
        })
    }

    pub fn direction_to_bit(d: CompassDirection) -> u8 {
        match d {
            CompassDirection::North => 0b0001,
            CompassDirection::East => 0b0010,
            CompassDirection::South => 0b0100,
            CompassDirection::West => 0b1000,
        }
    }

    fn bit_to_direction(x: u8) -> CompassDirection {
        match x {
            0b0001 => CompassDirection::North,
            0b0010 => CompassDirection::East,
            0b0100 => CompassDirection::South,
            0b1000 => CompassDirection::West,
            _ => unreachable!(),
        }
    }

    pub fn contains(&self, state: &GuardState) -> bool {
        let val = self.data[(state.pos.x as usize, state.pos.y as usize)];
        val & Self::direction_to_bit(state.orientation) != 0
    }

    pub fn insert(&mut self, state: GuardState) -> bool {
        assert!(state.pos.x < self.data.w as i64);
        assert!(state.pos.y < self.data.h as i64);
        let t = (state.pos.x as usize, state.pos.y as usize);
        let val: &mut u8 = &mut self.data[t];
        let bit = Self::direction_to_bit(state.orientation);
        if *val & bit != 0 {
            false
        } else {
            *val |= bit;
            true
        }
    }

    pub fn iter(&self) -> VisitSetIterator<'_> {
        VisitSetIterator {
            begin: true,
            bits: None,
            pos: 0,
            grid: &self.data,
            inner: self.data.iter(),
        }
    }
}

struct VisitSetIterator<'a> {
    begin: bool,
    bits: Option<u8>,
    pos: usize,
    grid: &'a DenseGrid<u8>,
    inner: DenseGridIterator<'a, u8>,
}

fn take_smallest_set_bit(n: &mut u8) -> Option<u8> {
    let mut mask: u8 = 1;
    while *n != 0 {
        if *n & mask != 0 {
            *n ^= mask;
            return Some(mask);
        }
        mask <<= 1;
    }
    None
}

#[test]
fn test_take_smallest_set_bit() {
    let mut val = 0;
    assert_eq!(take_smallest_set_bit(&mut val), None);
    assert_eq!(take_smallest_set_bit(&mut val), None);

    let mut val = 1;
    assert_eq!(take_smallest_set_bit(&mut val), Some(1));
    assert_eq!(take_smallest_set_bit(&mut val), None);

    let mut val = 2;
    assert_eq!(take_smallest_set_bit(&mut val), Some(2));
    assert_eq!(take_smallest_set_bit(&mut val), None);

    let mut val = 3;
    assert_eq!(take_smallest_set_bit(&mut val), Some(1));
    assert_eq!(take_smallest_set_bit(&mut val), Some(2));
    assert_eq!(take_smallest_set_bit(&mut val), None);
}

impl Iterator for VisitSetIterator<'_> {
    type Item = GuardState;

    fn next(&mut self) -> Option<Self::Item> {
        'outer: loop {
            let pos = Position {
                x: (self.pos % self.grid.w) as i64,
                y: (self.pos / self.grid.w) as i64,
            };
            if self.bits.is_none() {
                if self.pos >= self.grid.w * self.grid.h {
                    return None;
                }
                if !self.begin {
                    self.pos += 1;
                }
                self.bits = self.inner.next().copied();
                self.begin = false;
            }
            match self.bits.as_mut() {
                None => {
                    return None;
                }
                Some(b) => match take_smallest_set_bit(b) {
                    None => {
                        self.bits = None;
                        continue 'outer;
                    }
                    Some(bit) => {
                        let orientation: CompassDirection = VisitSet::bit_to_direction(bit);
                        return Some(GuardState { pos, orientation });
                    }
                },
            }
        }
    }
}

#[test]
fn test_visit_set_iteration() {
    let mut visited = VisitSet::new(1, 1).unwrap();
    let mut it = visited.iter();
    assert_eq!(it.next(), None);
    drop(it);
    let facing_east = GuardState {
        pos: Position { x: 0, y: 0 },
        orientation: CompassDirection::East,
    };
    visited.insert(facing_east);
    let mut it = visited.iter();
    assert_eq!(it.next(), Some(facing_east));
    assert_eq!(it.next(), None);
    drop(it);
    let facing_south = GuardState {
        pos: Position { x: 0, y: 0 },
        orientation: CompassDirection::South,
    };
    visited.insert(facing_south);

    let v: Vec<GuardState> = visited.iter().collect();
    assert!(v.contains(&GuardState {
        pos: Position { x: 0, y: 0 },
        orientation: CompassDirection::East
    }));
    assert!(v.contains(&GuardState {
        pos: Position { x: 0, y: 0 },
        orientation: CompassDirection::South
    }));
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
struct GuardState {
    pos: Position,
    orientation: CompassDirection,
}

#[derive(Debug, Clone)]
struct Grid {
    cells: Vec<Vec<char>>,
    obstruction: Option<Position>,
    //visited: HashSet<GuardState>,
    visited: VisitSet,
    guard: GuardState,
    w: i64,
    h: i64,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
enum PatrolOutcome {
    WentAway,
    InfiniteLoop,
}

impl Grid {
    fn inbounds(&self, pos: &Position) -> bool {
        pos.x >= 0 && pos.y >= 0 && pos.y < self.h && pos.x < self.w
    }

    fn add_obstruction(&mut self, pos: Position) -> bool {
        match self.get(&pos) {
            Some('#' | '>' | '<' | 'v' | '^') => false,
            Some('O') => {
                panic!("only one obstruction is supported");
            }
            Some('.') => {
                self.obstruction = Some(pos);
                true
            }
            Some(other) => {
                panic!("unexpected character {other} in grid");
            }
            None => {
                panic!("obstructions can only be placed inside the grid");
            }
        }
    }

    fn have_visited(&self, pos: &Position) -> bool {
        use CompassDirection::*;
        [North, East, South, West].iter().any(|orientation| {
            self.visited.contains(&GuardState {
                pos: *pos,
                orientation: *orientation,
            })
        })
    }

    fn distinct_position_count(&self) -> usize {
        self.visited
            .iter()
            .map(|state| state.pos)
            .collect::<HashSet<_>>()
            .len()
    }

    fn get(&self, pos: &Position) -> Option<char> {
        if let Some(opos) = self.obstruction.as_ref() {
            if opos == pos {
                return Some('O');
            }
        }
        if pos.y >= 0 && pos.x >= 0 && pos.y < self.cells.len() as i64 {
            let row = &self.cells[pos.y as usize];
            if pos.x < row.len() as i64 {
                return Some(row[pos.x as usize]);
            }
        }
        None
    }

    fn patrol(&mut self) -> PatrolOutcome {
        while self.inbounds(&self.guard.pos) {
            self.visited.insert(self.guard);
            let next = self.guard.pos.move_direction(&self.guard.orientation);
            match self.get(&next) {
                Some('#' | 'O') => {
                    self.guard.orientation = self.guard.orientation.rotated_clockwise();
                    if self.visited.contains(&self.guard) {
                        return PatrolOutcome::InfiniteLoop;
                    }
                }
                _ => {
                    self.guard.pos = next;
                }
            }
        }
        PatrolOutcome::WentAway
    }
}

impl Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn guard_indicator(orientation: CompassDirection) -> char {
            use CompassDirection::*;
            match orientation {
                North => '^',
                East => '>',
                South => 'v',
                West => '<',
            }
        }

        for (y, row) in self.cells.iter().enumerate() {
            for (x, ch) in row.iter().enumerate() {
                let here = Position {
                    x: x as i64,
                    y: y as i64,
                };
                if let Some(op) = self.obstruction {
                    if op == here {
                        f.write_char('O')?;
                        continue;
                    }
                }
                if self.guard.pos == here {
                    f.write_char(guard_indicator(self.guard.orientation))?;
                } else if self.have_visited(&here) {
                    f.write_char('X')?;
                } else {
                    f.write_char(*ch)?;
                }
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

impl TryFrom<&str> for Grid {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut cells: Vec<Vec<char>> = s.lines().map(|line| line.chars().collect()).collect();
        let mut guard: Option<GuardState> = None;
        for (y, row) in cells.iter().enumerate() {
            for (x, ch) in row.iter().enumerate() {
                let here = Position {
                    x: x as i64,
                    y: y as i64,
                };
                let orientation = match ch {
                    '>' => CompassDirection::East,
                    '<' => CompassDirection::West,
                    '^' => CompassDirection::North,
                    'v' => CompassDirection::South,
                    _ => {
                        continue;
                    }
                };
                guard = Some(GuardState {
                    pos: here,
                    orientation,
                });
            }
        }
        match guard {
            Some(g) => {
                let h = cells.len() as i64;
                let w = if h > 0 { cells[0].len() } else { 0 } as i64;
                cells[g.pos.y as usize][g.pos.x as usize] = '.';
                Ok(Grid {
                    cells,
                    obstruction: None,
                    visited: VisitSet::new(w, h)
                        .expect("the grid should be small enough to be representable"),
                    guard: g,
                    w,
                    h,
                })
            }
            None => Err("guard not found".to_string()),
        }
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "....#.....\n",
        ".........#\n",
        "..........\n",
        "..#.......\n",
        ".......#..\n",
        "..........\n",
        ".#..^.....\n",
        "........#.\n",
        "#.........\n",
        "......#...\n",
    )
}

#[test]
fn test_parse_input() {
    let grid = Grid::try_from(concat!(".>.\n", "#..\n")).expect("grid should be valid");
    assert_eq!(grid.cells, vec![vec!['.', '.', '.'], vec!['#', '.', '.']]);
}

#[test]
fn test_grid_display() {
    let mut grid = Grid::try_from(concat!(".>.\n", "#..\n")).expect("grid should be valid");
    grid.guard.pos.x = 0;
    assert_eq!(&grid.to_string(), ">..\n#..\n");
}

#[test]
fn test_patrol() {
    let mut grid = Grid::try_from(sample_input()).expect("sample input should be valid");
    grid.patrol();
    let got = grid.to_string();
    let expected = concat!(
        "....#.....\n",
        "....XXXXX#\n",
        "....X...X.\n",
        "..#.X...X.\n",
        "..XXXXX#X.\n",
        "..X.X.X.X.\n",
        ".#XXXXXXX.\n",
        ".XXXXXXX#.\n",
        "#XXXXXXX..\n",
        "......#X..\n",
    );
    if got != expected {
        eprintln!("got:\n{got}\nexpected:\n{expected}\n");
    }
    assert_eq!(&got, expected);
}

fn part1(s: &str) -> usize {
    let mut grid = Grid::try_from(s).expect("part 1 input should be valid");
    grid.patrol();
    grid.distinct_position_count()
}

#[test]
fn test_part1() {
    assert_eq!(part1(sample_input()), 41);
}

#[test]
fn test_part2() {
    assert_eq!(part2(sample_input()), 6);
}

fn obstacle_candidate_locations(mut grid: Grid) -> HashSet<Position> {
    // Figure out the path we take if we add no obstruction.
    grid.patrol();
    // It's useful to consider placing an obsrtruction in each square
    // we actually occupied.  This provides roughly a 3x speed-up
    // (compared to trying every square).
    grid.visited.iter().map(|state| state.pos).collect()
}

fn part2(s: &str) -> usize {
    let original_grid = Grid::try_from(s).expect("part 2 input should be valid");
    let candidates = obstacle_candidate_locations(original_grid.clone());
    let mut result = 0;
    for obstruction in candidates {
        let mut grid = original_grid.clone();
        if grid.add_obstruction(obstruction) {
            match grid.patrol() {
                PatrolOutcome::WentAway => (),
                PatrolOutcome::InfiniteLoop => {
                    result += 1;
                }
            }
        }
    }
    result
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 05 part 1: {}", part1(input));
    println!("day 05 part 2: {}", part2(input));
}
