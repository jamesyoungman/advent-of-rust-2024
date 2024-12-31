use std::{collections::BTreeMap, ops::Range, str};

const TUMBLERS: usize = 5;
const MAX_SPACE: i8 = 5;

#[derive(Debug, PartialEq, Eq)]
struct Profile {
    height: [i8; TUMBLERS],
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

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct LockId(usize);

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
struct KeyId(usize);

#[derive(Debug, PartialEq, Eq, Default)]
struct Schematics {
    locks: Vec<Profile>,
    keys: Vec<Profile>,
    lock_tumbler_index: [BTreeMap<i8, Vec<LockId>>; TUMBLERS],
}

#[derive(Debug, PartialEq, Eq)]
struct LockIter<'a> {
    lockref: &'a [Profile],
    inner: Range<usize>,
}

impl Iterator for LockIter<'_> {
    type Item = LockId;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(LockId)
    }
}

#[derive(Debug, PartialEq, Eq)]
struct KeyIter<'a> {
    keyref: &'a [Profile],
    inner: Range<usize>,
}

impl Iterator for KeyIter<'_> {
    type Item = KeyId;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(KeyId)
    }
}

impl Schematics {
    fn keys(&self) -> KeyIter<'_> {
        KeyIter {
            keyref: &self.keys,
            inner: 0..self.keys.len(),
        }
    }

    fn locks(&self) -> LockIter<'_> {
        LockIter {
            lockref: &self.locks,
            inner: 0..self.locks.len(),
        }
    }

    fn add_key(&mut self, profile: Profile) -> KeyId {
        let id = KeyId(self.keys.len());
        self.keys.push(profile);
        id
    }

    fn add_lock(&mut self, profile: Profile) -> LockId {
        let id = LockId(self.locks.len());
        for (tumbler, height) in profile.height.iter().enumerate() {
            self.lock_tumbler_index[tumbler]
                .entry(*height)
                .or_default()
                .push(id);
        }
        self.locks.push(profile);
        id
    }

    fn lock_accepts_key(&self, l: LockId, k: KeyId) -> bool {
        let lock = &self.locks[l.0];
        let key = &self.keys[k.0];
        lock.height
            .iter()
            .zip(key.height.iter())
            .all(|(lh, kh)| lh + kh <= MAX_SPACE)
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
    // This implementation is slower than necessary since it doesn't
    // use the index.
    sch.keys()
        .flat_map(|key_id| sch.locks().map(move |lock_id| (lock_id, key_id)))
        .filter(|(lock_id, key_id)| sch.lock_accepts_key(*lock_id, *key_id))
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
    let lock_05343 = LockId(0);
    let lock_12053 = LockId(1);
    let key_50213 = KeyId(0);
    let key_43402 = KeyId(1);
    let key_30201 = KeyId(2);
    assert!(!sch.lock_accepts_key(lock_05343, key_50213));
    assert!(!sch.lock_accepts_key(lock_05343, key_43402));
    assert!(sch.lock_accepts_key(lock_05343, key_30201));
    assert!(!sch.lock_accepts_key(lock_12053, key_50213));
    assert!(sch.lock_accepts_key(lock_12053, key_43402));
    assert!(sch.lock_accepts_key(lock_12053, key_30201));
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
