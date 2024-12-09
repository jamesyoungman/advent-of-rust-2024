use std::cmp::min;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{Display, Write};
use std::str;

/// Extent represents part of a file, followed by zero or more blocks
/// of empty space.
#[derive(Debug, PartialEq, Eq, Clone)]
struct Extent {
    /// The id of the file which owns this extent.
    file_id: u32,
    len: u32,
    following_space: u32,
}

impl Extent {
    fn label(&self) -> char {
        char::from_digit(self.file_id, 10).unwrap_or('x')
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct DiskMap {
    extents: BTreeMap<u32, Extent>,
}

fn parse_input(s: &str) -> DiskMap {
    fn parse_char(ch: char) -> u32 {
        match ch.to_digit(10) {
            Some(n) => n,
            _ => {
                panic!("all input characters should be decimal digits, but '{ch}' is not")
            }
        }
    }

    let s = s.trim();
    let mut extents = BTreeMap::new();
    let mut it = s.chars().enumerate();
    let mut block_location: u32 = 0;
    while let Some((input_pos, len)) = it.next() {
        let id = (input_pos as u32) / 2;
        let len = parse_char(len);
        let extent = Extent {
            file_id: id,
            len,
            following_space: match it.next() {
                Some((_, space_len)) => parse_char(space_len),
                None => u32::MAX,
            },
        };
        let next_block_location = match extent.following_space {
            u32::MAX => u32::MAX,
            n => block_location + extent.len + n,
        };
        if extents.insert(block_location, extent).is_some() {
            panic!("duplicate insertion at disk location {block_location}");
        }
        block_location = next_block_location;
    }
    DiskMap { extents }
}

#[test]
fn test_parse_input_12345() {
    assert_eq!(parse_input("12345"), rep_sample_12345());
}

#[test]
fn test_parse_input_90909() {
    assert_eq!(parse_input("90909"), rep_sample_90909(),);
}

impl FromIterator<(u32, Extent)> for DiskMap {
    fn from_iter<T: IntoIterator<Item = (u32, Extent)>>(iter: T) -> Self {
        let extents: BTreeMap<u32, Extent> = iter.into_iter().collect();
        DiskMap { extents }
    }
}

#[test]
fn test_diskmap_from_iterator() {
    DiskMap::from_iter([]);
    DiskMap::from_iter(vec![]);
    DiskMap::from_iter([(
        1,
        Extent {
            file_id: 1,
            len: 2,
            following_space: u32::MAX,
        },
    )]);
}

#[cfg(test)]
fn rep_sample_12345() -> DiskMap {
    DiskMap::from_iter([
        (
            0u32,
            Extent {
                file_id: 0,
                len: 1,
                following_space: 2,
            },
        ),
        (
            3u32,
            Extent {
                file_id: 1,
                len: 3,
                following_space: 4,
            },
        ),
        (
            10u32,
            Extent {
                file_id: 2,
                len: 5,
                following_space: u32::MAX,
            },
        ),
    ])
}

#[cfg(test)]
fn rep_sample_90909() -> DiskMap {
    DiskMap::from_iter([
        (
            0,
            Extent {
                file_id: 0,
                len: 9,
                following_space: 0,
            },
        ),
        (
            9,
            Extent {
                file_id: 1,
                len: 9,
                following_space: 0,
            },
        ),
        (
            18,
            Extent {
                file_id: 2,
                len: 9,
                following_space: u32::MAX,
            },
        ),
    ])
}

impl DiskMap {
    fn compact_part1(mut self) -> DiskMap {
        let mut result = BTreeMap::new();
        while let Some((position, extent)) = self.extents.pop_first() {
            let mut remaining_space: u32 = extent.following_space;
            let mut space_pos: u32 = position + extent.len;
            result.insert(
                position,
                Extent {
                    file_id: extent.file_id,
                    len: extent.len,
                    following_space: 0,
                },
            );

            while remaining_space > 0 && !self.extents.is_empty() {
                if let Some((mut pos, mut extent)) = self.extents.pop_last() {
                    let moved_blocks = if self.extents.is_empty() {
                        remaining_space = u32::MAX;
                        extent.len
                    } else {
                        min(remaining_space, extent.len)
                    };
                    let moved_extent = Extent {
                        file_id: extent.file_id,
                        len: moved_blocks,
                        following_space: 0,
                    };
                    result.insert(space_pos, moved_extent);
                    extent.len -= moved_blocks;
                    pos += moved_blocks;
                    if extent.len > 0 {
                        self.extents.insert(pos, extent);
                    }

                    space_pos += moved_blocks;
                    remaining_space -= moved_blocks;
                }
            }
        }
        if let Some(mut entry) = result.last_entry() {
            entry.get_mut().following_space = u32::MAX;
        }
        DiskMap { extents: result }
    }

    fn compact_part2(mut self) -> DiskMap {
        let mut result = BTreeMap::new();
        let mut frozen_files: BTreeSet<u32> = BTreeSet::new();
        while let Some((highest_pos, mut highest)) = self.extents.pop_last() {
            fn fit((_, extent): &(&u32, &mut Extent), size_requirement: u32) -> bool {
                extent.following_space >= size_requirement
            }
            if frozen_files.contains(&highest.file_id) {
                result.insert(highest_pos, highest);
                continue;
            }
            if let Some((insert_after_pos, insert_after)) =
                self.extents.iter_mut().find(|item| fit(item, highest.len))
            {
                let new_pos = insert_after_pos + insert_after.len;
                highest.following_space = insert_after.following_space - highest.len;
                frozen_files.insert(highest.file_id); // because it's already been moved
                insert_after.following_space = 0;
                self.extents.insert(new_pos, highest);
            } else {
                // There is no place to put `highest`.  Keep the current location;
                result.insert(highest_pos, highest);
            }
        }
        DiskMap { extents: result }
    }

    fn checksum(&self) -> u64 {
        fn term((pos, extent_id): (u32, u32)) -> u64 {
            (pos as u64) * (extent_id as u64)
        }
        fn extent_blocks<'a>(
            (pos, extent): (&'a u32, &'a Extent),
        ) -> impl Iterator<Item = (u32, u32)> + use<'a> {
            (*pos..(*pos + extent.len)).map(|pos| (pos, extent.file_id))
        }
        self.extents.iter().flat_map(extent_blocks).map(term).sum()
    }
}

impl Display for DiskMap {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self
            .extents
            .iter()
            .map(|(pos, extent)| pos + extent.len)
            .max()
        {
            None => Ok(()),
            Some(len) => {
                let len = len as usize;
                let mut v: Vec<char> = Vec::with_capacity(len);
                v.resize(len, '.');
                for (&pos, extent) in self.extents.iter() {
                    let label = extent.label();
                    for ch in v.iter_mut().skip(pos as usize).take(extent.len as usize) {
                        *ch = label;
                    }
                }
                for ch in v {
                    f.write_char(ch)?;
                }
                Ok(())
            }
        }
    }
}

#[test]
fn test_diskmap_display_90909() {
    let input = DiskMap {
        extents: BTreeMap::from([
            (
                0,
                Extent {
                    file_id: 0,
                    len: 9,
                    following_space: 0,
                },
            ),
            (
                9,
                Extent {
                    file_id: 1,
                    len: 9,
                    following_space: 0,
                },
            ),
            (
                18,
                Extent {
                    file_id: 2,
                    len: 9,
                    following_space: u32::MAX,
                },
            ),
        ]),
    };
    assert_eq!(input.to_string(), "000000000111111111222222222");
}

#[test]
fn test_diskmap_display_12345() {
    assert_eq!(rep_sample_12345().to_string(), "0..111....22222");
}

#[test]
fn test_compact_90909_part1() {
    let disk = parse_input("90909");
    assert_eq!(
        disk.compact_part1(),
        DiskMap {
            extents: BTreeMap::from([
                (
                    0,
                    Extent {
                        file_id: 0,
                        len: 9,
                        following_space: 0,
                    }
                ),
                (
                    9,
                    Extent {
                        file_id: 1,
                        len: 9,
                        following_space: 0,
                    }
                ),
                (
                    18,
                    Extent {
                        file_id: 2,
                        len: 9,
                        following_space: u32::MAX,
                    }
                ),
            ])
        }
    );
}

#[test]
fn test_compact_12345_part1() {
    let disk = rep_sample_12345(); // extent ids 0 (1 block),1 (3 blocks),2 (5 blocks)
    let expected = DiskMap {
        extents: BTreeMap::from([
            (
                0,
                Extent {
                    // extent 0 (1 block) unmoved
                    file_id: 0,
                    len: 1,
                    following_space: 0,
                },
            ),
            (
                1,
                Extent {
                    // filling the first (2-block) space
                    file_id: 2,
                    len: 2,
                    following_space: 0,
                },
            ),
            (
                3,
                Extent {
                    // extent 1 (3 blocks) unmoved
                    file_id: 1,
                    len: 3,
                    following_space: 0,
                },
            ),
            (
                6,
                Extent {
                    // the remainder of extent 2
                    file_id: 2,
                    len: 3,
                    following_space: u32::MAX,
                },
            ),
        ]),
    };
    assert_eq!(disk.compact_part1(), expected);
}

#[test]
fn test_compact_2333133121414131402_part1() {
    let disk = parse_input(sample_input());
    assert_eq!(
        disk.to_string(),
        "00...111...2...333.44.5555.6666.777.888899"
    );
    let compacted = disk.compact_part1();
    assert_eq!(compacted.to_string(), "0099811188827773336446555566");
}

#[test]
fn test_compact_2333133121414131402_part2() {
    let disk = parse_input(sample_input());
    assert_eq!(
        disk.to_string(),
        "00...111...2...333.44.5555.6666.777.888899"
    );
    let compacted = disk.compact_part2();
    assert_eq!(
        compacted.to_string(),
        "00992111777.44.333....5555.6666.....8888"
    );
}

#[test]
fn test_disk_checksum() {
    let disk = DiskMap::from_iter([
        (
            0,
            Extent {
                file_id: 0,
                len: 2,
                following_space: 0,
            },
        ),
        (
            2,
            Extent {
                file_id: 9,
                len: 2,
                following_space: 0,
            },
        ),
        (
            4,
            Extent {
                file_id: 8,
                len: 1,
                following_space: u32::MAX,
            },
        ),
    ]);
    assert_eq!(disk.checksum(), vec![0, 0, 18, 27, 32].into_iter().sum());
}

#[cfg(test)]
fn sample_input() -> &'static str {
    "2333133121414131402\n"
}

fn part1(disk: DiskMap) -> u64 {
    disk.compact_part1().checksum()
}

fn part2(disk: DiskMap) -> u64 {
    disk.compact_part2().checksum()
}

#[test]
fn test_part1() {
    let disk = parse_input(sample_input());
    assert_eq!(part1(disk), 1928);
}

#[test]
fn test_part2() {
    let disk = parse_input(sample_input());
    assert_eq!(part2(disk), 2858);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let disk = parse_input(input);
    println!("day 09 part 1: {}", part1(disk.clone()));
    println!("day 09 part 2: {}", part2(disk));
}
