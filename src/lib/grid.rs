use std::cmp::{max, min};
use std::fmt::{self, Debug, Display, Formatter, Write};

use itertools::Itertools;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum CompassDirection {
    North,
    South,
    West,
    East,
}

impl CompassDirection {
    pub fn reversed(&self) -> CompassDirection {
        use CompassDirection::*;
        match self {
            North => South,
            South => North,
            East => West,
            West => East,
        }
    }
}

impl Display for CompassDirection {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_char(char::from(*self))
    }
}

impl From<CompassDirection> for char {
    fn from(d: CompassDirection) -> char {
        use CompassDirection::*;
        match d {
            North => 'N',
            East => 'E',
            South => 'S',
            West => 'W',
        }
    }
}

pub const ALL_MOVE_OPTIONS: [CompassDirection; 4] = [
    CompassDirection::North,
    CompassDirection::East,
    CompassDirection::South,
    CompassDirection::West,
];

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position {
    pub x: i64,
    pub y: i64,
}

impl Display for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

impl Debug for Position {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Position{{x:{},y:{}}}", self.x, self.y)
    }
}

impl Position {
    pub fn move_direction(&self, d: &CompassDirection) -> Position {
        match d {
            CompassDirection::North => Position {
                y: self.y - 1,
                ..*self
            },
            CompassDirection::South => Position {
                y: self.y + 1,
                ..*self
            },
            CompassDirection::East => Position {
                x: self.x + 1,
                ..*self
            },
            CompassDirection::West => Position {
                x: self.x - 1,
                ..*self
            },
        }
    }

    pub fn neighbour_xbearing(&self, to: &Position) -> Result<Option<CompassDirection>, String> {
        match self.x - to.x {
            -1 => Ok(Some(CompassDirection::West)),
            0 => Ok(None),
            1 => Ok(Some(CompassDirection::East)),
            _ => Err(format!(
                "x-coordinates {} and {} are too far apart",
                self.x, to.x
            )),
        }
    }

    pub fn neighbour_ybearing(&self, to: &Position) -> Result<Option<CompassDirection>, String> {
        match self.y - to.y {
            -1 => Ok(Some(CompassDirection::North)),
            0 => Ok(None),
            1 => Ok(Some(CompassDirection::South)),
            _ => Err(format!(
                "y-coordinates {} and {} are too far apart",
                self.y, to.y
            )),
        }
    }
}

pub fn maybe_update_min(min: &mut Option<i64>, val: i64) {
    match min {
        None => {
            *min = Some(val);
        }
        Some(v) if *v > val => *min = Some(val),
        Some(_) => (),
    }
}

pub fn maybe_update_max(max: &mut Option<i64>, val: i64) {
    match max {
        None => {
            *max = Some(val);
        }
        Some(v) if *v < val => *max = Some(val),
        Some(_) => (),
    }
}

pub fn update_min(min: &mut i64, val: i64) {
    if val < *min {
        *min = val;
    }
}

pub fn update_max(max: &mut i64, val: i64) {
    if val > *max {
        *max = val;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct BoundingBox {
    pub top_left: Position,
    pub bottom_right: Position,
}

impl BoundingBox {
    pub fn new(pos: &Position) -> BoundingBox {
        BoundingBox {
            top_left: *pos,
            bottom_right: *pos,
        }
    }

    pub fn columns(&self) -> impl Iterator<Item = i64> + Clone {
        self.top_left.x..=self.bottom_right.x
    }

    pub fn rows(&self) -> impl Iterator<Item = i64> + Clone {
        self.top_left.y..=self.bottom_right.y
    }

    pub fn width(&self) -> i64 {
        1 + self.bottom_right.x - self.top_left.x
    }

    pub fn height(&self) -> i64 {
        1 + self.bottom_right.y - self.top_left.y
    }

    pub fn area(&self) -> i64 {
        self.width() * self.height()
    }

    pub fn perimeter(&self) -> impl Iterator<Item = Position> + '_ {
        let top = (self.top_left.x..self.bottom_right.x).map(|x| Position {
            x,
            y: self.top_left.y,
        });
        let bottom = (self.top_left.x..self.bottom_right.x).map(|x| Position {
            x,
            y: self.bottom_right.y,
        });
        let left = (self.top_left.y..self.bottom_right.y).map(|y| Position {
            x: self.top_left.x,
            y,
        });
        let right = (self.top_left.y..self.bottom_right.y).map(|y| Position {
            x: self.bottom_right.x,
            y,
        });
        left.chain(right).chain(top).chain(bottom)
    }

    pub fn surface(&self) -> impl Iterator<Item = Position> + '_ {
        self.rows()
            .cartesian_product(self.columns())
            .map(|(y, x)| Position { x, y })
    }

    pub fn update(&mut self, pos: &Position) {
        self.top_left = Position {
            x: min(self.top_left.x, pos.x),
            y: min(self.top_left.y, pos.y),
        };
        self.bottom_right = Position {
            x: max(self.bottom_right.x, pos.x),
            y: max(self.bottom_right.y, pos.y),
        };
    }

    pub fn contains(&self, pos: &Position) -> bool {
        self.top_left.x <= pos.x
            && self.top_left.y <= pos.y
            && self.bottom_right.x >= pos.x
            && self.bottom_right.y >= pos.y
    }
}

#[test]
fn test_bbox_contains() {
    let b = BoundingBox {
        top_left: Position { x: 1, y: 0 },
        bottom_right: Position { x: 5, y: 2 },
    };
    assert!(b.contains(&Position { x: 1, y: 1 }));
    assert!(!b.contains(&Position { x: 0, y: 1 })); // x too low
    assert!(!b.contains(&Position { x: 6, y: 1 })); // x too high
    assert!(!b.contains(&Position { x: 1, y: -1 })); // y too low
    assert!(!b.contains(&Position { x: 1, y: 3 })); // y too high
}

#[test]
fn test_bbox_update() {
    let mut b = BoundingBox {
        top_left: Position { x: 5, y: 5 },
        bottom_right: Position { x: 5, y: 5 },
    };

    b.update(&Position { x: 6, y: 5 });
    assert_eq!(
        b,
        BoundingBox {
            top_left: Position { x: 5, y: 5 },
            bottom_right: Position { x: 6, y: 5 },
        }
    );

    b.update(&Position { x: 5, y: 6 });
    assert_eq!(
        b,
        BoundingBox {
            top_left: Position { x: 5, y: 5 },
            bottom_right: Position { x: 6, y: 6 },
        }
    );

    b.update(&Position { x: 4, y: 5 });
    assert_eq!(
        b,
        BoundingBox {
            top_left: Position { x: 4, y: 5 },
            bottom_right: Position { x: 6, y: 6 }
        }
    );

    b.update(&Position { x: 5, y: 4 });
    assert_eq!(
        b,
        BoundingBox {
            top_left: Position { x: 4, y: 4 },
            bottom_right: Position { x: 6, y: 6 },
        },
    );
}

pub fn bounds<'a, I>(points: I) -> Option<BoundingBox>
where
    I: IntoIterator<Item = &'a Position>,
{
    let mut min_x: Option<i64> = None;
    let mut max_x: Option<i64> = None;
    let mut min_y: Option<i64> = None;
    let mut max_y: Option<i64> = None;
    for p in points.into_iter() {
        maybe_update_min(&mut min_x, p.x);
        maybe_update_max(&mut max_x, p.x);
        maybe_update_min(&mut min_y, p.y);
        maybe_update_max(&mut max_y, p.y);
    }
    match (min_x, max_x, min_y, max_y) {
        (Some(xlow), Some(xhigh), Some(ylow), Some(yhigh)) => Some(BoundingBox {
            top_left: Position { x: xlow, y: ylow },
            bottom_right: Position { x: xhigh, y: yhigh },
        }),
        _ => None,
    }
}

pub fn manhattan(a: &Position, b: &Position) -> i64 {
    let dx = (a.x - b.x).abs();
    let dy = (a.y - b.y).abs();
    dx + dy
}

#[test]
fn test_manhattan() {
    assert_eq!(
        manhattan(&Position { x: 1, y: -2 }, &Position { x: 12, y: 7 }),
        11 + 9
    );
}
