use lib::grid::Position;
use std::str;

#[derive(Debug, Clone, Copy)]
enum Direction8 {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}
const ALL_DIRECTIONS: &[Direction8] = &[
    Direction8::N,
    Direction8::NE,
    Direction8::E,
    Direction8::SE,
    Direction8::S,
    Direction8::SW,
    Direction8::W,
    Direction8::NW,
];

impl Direction8 {
    fn delta(&self) -> (i64, i64) {
        use Direction8::*;
        let y = match self {
            N | NE | NW => -1,
            E | W => 0,
            SE | S | SW => 1,
        };
        let x = match self {
            NE | E | SE => 1,
            N | S => 0,
            NW | W | SW => -1,
        };
        (x, y)
    }
}

fn repeated_travel(pos: &Position, dir: &Direction8, count: i64) -> Position {
    let delta = dir.delta();
    Position {
        x: pos.x + (delta.0 * count),
        y: pos.y + (delta.1 * count),
    }
}

struct Grid {
    letters: Vec<Vec<char>>,
    h: i64,
    w: i64,
}

impl Grid {
    fn get(&self, pos: &Position) -> char {
        self.letters[pos.y as usize][pos.x as usize]
    }

    fn inrange(&self, pos: Position) -> bool {
        if pos.y < 0 || pos.y >= self.h {
            false
        } else {
            pos.x >= 0 && pos.x < self.w
        }
    }
}

fn parse_input(s: &str) -> Grid {
    let cells: Vec<Vec<char>> = s.lines().map(|line| line.chars().collect()).collect();
    let h = cells.len() as i64;
    if h == 0 {
        panic!("empty grid is not supported");
    }
    let w = cells[0].len() as i64;
    Grid {
        letters: cells,
        h,
        w,
    }
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "MMMSXXMASM\n",
        "MSAMXMSMSA\n",
        "AMXSXMAAMM\n",
        "MSAMASMSMX\n",
        "XMASAMXAMM\n",
        "XXAMMXXAMA\n",
        "SMSMSASXSS\n",
        "SAXAMASAAA\n",
        "MAMMMXMMMM\n",
        "MXMXAXMASX\n",
    )
}

#[test]
fn test_parse_input() {
    assert_eq!(
        parse_input(sample_input()).letters,
        vec![
            vec!['M', 'M', 'M', 'S', 'X', 'X', 'M', 'A', 'S', 'M'],
            vec!['M', 'S', 'A', 'M', 'X', 'M', 'S', 'M', 'S', 'A'],
            vec!['A', 'M', 'X', 'S', 'X', 'M', 'A', 'A', 'M', 'M'],
            vec!['M', 'S', 'A', 'M', 'A', 'S', 'M', 'S', 'M', 'X'],
            vec!['X', 'M', 'A', 'S', 'A', 'M', 'X', 'A', 'M', 'M'],
            vec!['X', 'X', 'A', 'M', 'M', 'X', 'X', 'A', 'M', 'A'],
            vec!['S', 'M', 'S', 'M', 'S', 'A', 'S', 'X', 'S', 'S'],
            vec!['S', 'A', 'X', 'A', 'M', 'A', 'S', 'A', 'A', 'A'],
            vec!['M', 'A', 'M', 'M', 'M', 'X', 'M', 'M', 'M', 'M'],
            vec!['M', 'X', 'M', 'X', 'A', 'X', 'M', 'A', 'S', 'X'],
        ]
    );
}

mod part1 {
    use super::*;

    fn is_hit(haystack: &Grid, needle: &str, pos: Position, dir: &Direction8) -> bool {
        let n = needle.len() - 1;
        let final_letter_pos = repeated_travel(&pos, dir, n as i64);
        if !(haystack.inrange(pos) && haystack.inrange(final_letter_pos)) {
            false
        } else {
            for (i, expected) in needle.chars().enumerate() {
                let here = repeated_travel(&pos, dir, i as i64);
                let got = haystack.get(&here);
                if got != expected {
                    return false;
                }
            }
            true
        }
    }

    fn part1_search(haystack: &Grid, needle: &str) -> usize {
        let mut result: usize = 0;
        for y in 0..haystack.h {
            for x in 0..haystack.w {
                let pos = Position { x, y };
                for dir in ALL_DIRECTIONS.iter() {
                    if is_hit(haystack, needle, pos, dir) {
                        result += 1;
                    }
                }
            }
        }
        result
    }

    pub fn solve(s: &str) -> usize {
        let grid = parse_input(s);
        let needle = "XMAS";
        part1_search(&grid, needle)
    }

    #[test]
    fn test_solve() {
        assert_eq!(solve(sample_input()), 18);
    }
}

mod part2 {
    use super::*;

    fn is_x_mas(haystack: &Grid, pos: &Position) -> bool {
        fn is_ms_or_sm(first: char, last: char) -> bool {
            matches!((first, last), ('M', 'S') | ('S', 'M'))
        }

        haystack.get(pos) == 'A' &&
            is_ms_or_sm(haystack.get(&Position{x: pos.x-1, y: pos.y-1}), // NW
			haystack.get(&Position{x: pos.x+1, y: pos.y+1})) // SE
	    &&
	    is_ms_or_sm(haystack.get(&Position{x: pos.x+1, y: pos.y-1}), // NE
			haystack.get(&Position{x: pos.x-1, y: pos.y+1})) // SW
    }

    fn part2_search(grid: &Grid) -> Vec<Position> {
        let mut result = Vec::new();
        for y in 1..(grid.h - 1) {
            for x in 1..(grid.w - 1) {
                let pos = Position { x, y };
                if is_x_mas(grid, &pos) {
                    result.push(pos);
                }
            }
        }
        result
    }

    pub fn solve(s: &str) -> usize {
        let grid = parse_input(s);
        part2_search(&grid).len()
    }

    #[test]
    fn test_solve() {
        assert_eq!(solve(sample_input()), 9);
    }
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 04 part 1: {}", part1::solve(input));
    println!("day 04 part 2: {}", part2::solve(input));
}
