use lib::grid::Position;
use std::collections::HashSet;
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

fn parse_input(s: &str) -> Vec<Vec<char>> {
    s.lines().map(|line| line.chars().collect()).collect()
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
        parse_input(sample_input()),
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

struct Found {
    pos: Position,
    dir: Direction8,
}

fn repeated_travel(pos: &Position, dir: &Direction8, count: i64) -> Position {
    let delta = dir.delta();
    Position {
        x: pos.x + (delta.0 * count),
        y: pos.y + (delta.1 * count),
    }
}

fn inrange(haystack: &[Vec<char>], pos: Position) -> bool {
    if pos.y < 0 || pos.y >= haystack.len() as i64 {
        false
    } else {
        let w = haystack[0].len() as i64;
        pos.x >= 0 && pos.x < w
    }
}

fn is_hit(haystack: &[Vec<char>], needle: &str, pos: Position, dir: &Direction8) -> bool {
    let n = needle.len() - 1;
    let final_letter_pos = repeated_travel(&pos, dir, n as i64);
    if !(inrange(haystack, pos) && inrange(haystack, final_letter_pos)) {
        false
    } else {
        for (i, expected) in needle.chars().enumerate() {
            let here = repeated_travel(&pos, dir, i as i64);
            let got = haystack[here.y as usize][here.x as usize];
            if got != expected {
                return false;
            }
        }
        true
    }
}

fn search(haystack: &[Vec<char>], needle: &str) -> Vec<Found> {
    let mut result = Vec::new();
    let h = haystack.len();
    let w = haystack[0].len();
    fn p(x: usize, y: usize) -> Position {
        Position {
            x: x as i64,
            y: y as i64,
        }
    }
    for y in 0..h {
        for x in 0..w {
            let pos = p(x, y);
            for dir in ALL_DIRECTIONS.iter() {
                if is_hit(haystack, needle, pos, dir) {
                    result.push(Found { pos, dir: *dir });
                }
            }
        }
    }
    result
}

fn print_grid_with_hits(grid: &[Vec<char>], hits: &[Found], hit_len: usize) -> String {
    let h = grid.len();
    let w = grid[0].len();
    let mut hit_positions: HashSet<Position> = HashSet::new();
    for hit in hits {
        for i in 0..hit_len {
            let pos = repeated_travel(&hit.pos, &hit.dir, i as i64);
            hit_positions.insert(pos);
        }
    }
    let mut result = String::new();
    for y in 0..h {
        for x in 0..w {
            let pos = Position {
                x: x as i64,
                y: y as i64,
            };
            let ch = grid[y][x];
            if hit_positions.contains(&pos) {
                result.push(ch);
            } else {
                result.push('.');
            }
        }
        result.push('\n');
    }
    result
}

fn part1(s: &str) -> usize {
    let grid = parse_input(s);
    let needle = "XMAS";
    let hits = search(&grid, needle);
    println!("{}", print_grid_with_hits(&grid, &hits, needle.len()));
    hits.len()
}

#[test]
fn test_part1() {
    assert_eq!(part1(sample_input()), 18);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("day 04 part 1: {}", part1(input));
    //println!("day 04 part 2: {}", part2(input));
}
