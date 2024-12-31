use std::str;

const TUMBLERS: usize = 5;
const MAX_SPACE: i8 = 5;

#[derive(Debug, PartialEq, Eq)]
struct Profile {
    height: [i8; TUMBLERS],
}

impl Profile {
    fn accepts(&self, other: &Profile) -> bool {
        self.height
            .iter()
            .zip(other.height.iter())
            .all(|(lh, kh)| lh + kh <= MAX_SPACE)
    }
}

/// We follow the convention in the examples, where the lock has pins
/// with a shared base at the top (i.e. the pins point down).  This
/// doesn't resemble a normal (Yale-type) door lock, for which the
/// pins point up.  But it's simpler to be consistent with the
/// examples in the AoC puzzle.
#[derive(Debug, PartialEq, Eq)]
enum ParseResult {
    Key(Profile),
    Lock(Profile),
}

#[derive(Debug, PartialEq, Eq, Default)]
struct Schematics {
    locks: Vec<Profile>,
    keys: Vec<Profile>,
}

impl Schematics {
    fn keys(&self) -> impl Iterator<Item = &Profile> {
        self.keys.iter()
    }

    fn locks(&self) -> impl Iterator<Item = &Profile> {
        self.locks.iter()
    }

    fn add_key(&mut self, profile: Profile) {
        self.keys.push(profile);
    }

    fn add_lock(&mut self, profile: Profile) {
        self.locks.push(profile);
    }
}

fn parse_profile(lines: &[&str]) -> Profile {
    fn convert_height(h: usize) -> i8 {
        h.try_into().expect("profiles should be small")
    }

    let mut height: [i8; TUMBLERS] = [0; TUMBLERS];
    for (y, line) in lines
        .iter()
        .enumerate()
        .skip(1)
        .map(|(y, line)| (convert_height(y), line))
    {
        for (entry, ch) in height.iter_mut().zip(line.chars()) {
            match ch {
                '.' => (),
                '#' => {
                    assert!(y <= MAX_SPACE, "key or lock has too great a height {y}");
                    if 1 + *entry == y {
                        *entry = y;
                    } else {
                        panic!("discontinuity in tumbler at y={y}");
                    }
                }
                other => {
                    panic!("unrecognised character '{other}' in key/lock profile");
                }
            }
        }
    }
    Profile { height }
}

fn parse_schematic_item(s: &str) -> ParseResult {
    let (is_key, lines): (bool, Vec<&str>) = match s.lines().next() {
        Some(line) => {
            if line == "#####" {
                (false, s.lines().collect())
            } else {
                (true, s.lines().rev().collect())
            }
        }
        None => {
            panic!("empty item: '{s}'");
        }
    };
    let profile = parse_profile(&lines);
    if is_key {
        ParseResult::Key(profile)
    } else {
        ParseResult::Lock(profile)
    }
}

#[test]
fn test_parse_schematic_item_lock() {
    assert_eq!(
        // This is the "first lock" from the example.
        parse_schematic_item(concat!(
            "#####\n", // do not join lines
            ".####\n", // do not join lines
            ".####\n", // do not join lines
            ".####\n", // do not join lines
            ".#.#.\n", // do not join lines
            ".#...\n", // do not join lines
            ".....\n",
        )),
        ParseResult::Lock(Profile {
            height: [0, 5, 3, 4, 3]
        })
    )
}

#[test]
fn test_parse_schematic_item_key() {
    assert_eq!(
        // This is the "first key" from the example.
        parse_schematic_item(concat!(
            ".....\n", // do not join lines
            "#....\n", // do not join lines
            "#....\n", // do not join lines
            "#...#\n", // do not join lines
            "#.#.#\n", // do not join lines
            "#.###\n", // do not join lines
            "#####\n",
        )),
        ParseResult::Key(Profile {
            height: [5, 0, 2, 1, 3]
        })
    )
}

fn parse_schematics(input: &str) -> Schematics {
    let mut result: Schematics = Default::default();
    for item in input.split("\n\n") {
        match parse_schematic_item(item) {
            ParseResult::Key(profile) => {
                result.add_key(profile);
            }
            ParseResult::Lock(profile) => {
                result.add_lock(profile);
            }
        }
    }
    result
}

fn count_fit_combinations(sch: &Schematics) -> usize {
    sch.keys()
        .flat_map(|key| sch.locks().map(move |lock| (lock, key)))
        .filter(|(lock, key)| lock.accepts(key))
        .count()
}

#[cfg(test)]
fn sample_input() -> &'static str {
    concat!(
        "#####\n", // do not join lines
        ".####\n", // do not join lines
        ".####\n", // do not join lines
        ".####\n", // do not join lines
        ".#.#.\n", // do not join lines
        ".#...\n", // do not join lines
        ".....\n", // do not join lines
        "\n",      // do not join lines
        "#####\n", // do not join lines
        "##.##\n", // do not join lines
        ".#.##\n", // do not join lines
        "...##\n", // do not join lines
        "...#.\n", // do not join lines
        "...#.\n", // do not join lines
        ".....\n", // do not join lines
        "\n",      // do not join lines
        ".....\n", // do not join lines
        "#....\n", // do not join lines
        "#....\n", // do not join lines
        "#...#\n", // do not join lines
        "#.#.#\n", // do not join lines
        "#.###\n", // do not join lines
        "#####\n", // do not join lines
        "\n",      // do not join lines
        ".....\n", // do not join lines
        ".....\n", // do not join lines
        "#.#..\n", // do not join lines
        "###..\n", // do not join lines
        "###.#\n", // do not join lines
        "###.#\n", // do not join lines
        "#####\n", // do not join lines
        "\n",      // do not join lines
        ".....\n", // do not join lines
        ".....\n", // do not join lines
        ".....\n", // do not join lines
        "#....\n", // do not join lines
        "#.#..\n", // do not join lines
        "#.#.#\n", // do not join lines
        "#####\n", // do not join lines
    )
}

fn part1(sch: &Schematics) -> usize {
    count_fit_combinations(sch)
}

#[test]
fn test_lock_accepts_key() {
    let sch: Schematics = parse_schematics(sample_input());
    let lock_05343 = &sch.locks[0];
    let lock_12053 = &sch.locks[1];
    let key_50213 = &sch.keys[0];
    let key_43402 = &sch.keys[1];
    let key_30201 = &sch.keys[2];
    assert!(!lock_05343.accepts(key_50213));
    assert!(!lock_05343.accepts(key_43402));
    assert!(lock_05343.accepts(key_30201));
    assert!(!lock_12053.accepts(key_50213));
    assert!(lock_12053.accepts(key_43402));
    assert!(lock_12053.accepts(key_30201));
}

#[test]
fn test_part1() {
    let sch: Schematics = parse_schematics(sample_input());
    assert_eq!(part1(&sch), 3);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let schematics = parse_schematics(input_str);
    println!("Day 25 part 1: {}", part1(&schematics));
}
