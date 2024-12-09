use std::cmp::min;
use std::collections::VecDeque;
use std::str;

#[derive(Debug, PartialEq, Eq)]
struct File {
    id: u32,
    len: u32,
    following_space: u32,
}

#[derive(Debug, PartialEq, Eq)]
struct DiskMap {
    files: VecDeque<File>,
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
    let mut files = VecDeque::new();
    let mut it = s.chars().enumerate();
    loop {
        match it.next() {
            Some((pos, len)) => {
                let id = (pos as u32) / 2;
                files.push_back(File {
                    id,
                    len: parse_char(len),
                    following_space: match it.next() {
                        Some((_, space_len)) => parse_char(space_len),
                        None => u32::MAX,
                    },
                });
            }
            None => {
                break;
            }
        }
    }
    DiskMap { files }
}

#[test]
fn test_parse_input_12345() {
    assert_eq!(
        parse_input("12345"),
        DiskMap {
            files: VecDeque::from(vec![
                File {
                    id: 0,
                    len: 1,
                    following_space: 2
                },
                File {
                    id: 1,
                    len: 3,
                    following_space: 4
                },
                File {
                    id: 2,
                    len: 5,
                    following_space: u32::MAX
                }
            ]),
        }
    );
}

#[test]
fn test_parse_input_90909() {
    assert_eq!(
        parse_input("90909"),
        DiskMap {
            files: VecDeque::from(vec![
                File {
                    id: 0,
                    len: 9,
                    following_space: 0
                },
                File {
                    id: 1,
                    len: 9,
                    following_space: 0
                },
                File {
                    id: 2,
                    len: 9,
                    following_space: u32::MAX
                }
            ]),
        }
    );
}

impl DiskMap {
    fn compact_part1(mut self) -> Vec<u32> {
        let mut result = Vec::new();
        while let Some(file) = self.files.pop_front() {
            let mut remaining_space: u32 = file.following_space;
            for _ in 0..file.len {
                result.push(file.id);
            }
            while remaining_space > 0 && !self.files.is_empty() {
                if let Some(mut file) = self.files.pop_back() {
                    let moved_blocks = min(remaining_space, file.len);
                    for _ in 0..moved_blocks {
                        result.push(file.id);
                    }
                    file.len -= moved_blocks;
                    if file.len > 0 {
                        self.files.push_back(file);
                    }
                    remaining_space -= moved_blocks;
                }
            }
        }
        result
    }
}

#[test]
fn test_compact_90909_part1() {
    let disk = parse_input("90909");
    assert_eq!(
        disk.compact_part1(),
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,]
    );
}

#[test]
fn test_compact_12345_part1() {
    let disk = parse_input("12345"); // file ids 0 (1 block),1 (3 blocks),2 (5 blocks)
    assert_eq!(
        disk.compact_part1(),
        vec![
            0, // file 0 (1 block) unmoved
            2, 2, // filling the first (2-block) space
            1, 1, 1, // file 1 (3 blocks) unmoved
            2, 2, 2 // the remainder of file 2
        ]
    )
}

#[test]
fn test_compact_2333133121414131402_part1() {
    let disk = parse_input(sample_input());
    assert_eq!(
        disk.compact_part1(),
        vec![0, 0, 9, 9, 8, 1, 1, 1, 8, 8, 8, 2, 7, 7, 7, 3, 3, 3, 6, 4, 4, 6, 5, 5, 5, 5, 6, 6]
    );
}

fn disk_checksum(ids: &[u32]) -> u64 {
    fn term((pos, id): (usize, &u32)) -> u64 {
        (pos as u64) * (*id as u64)
    }
    ids.iter().enumerate().map(term).sum()
}

#[test]
fn test_disk_checksum() {
    assert_eq!(
        disk_checksum(&[0, 0, 9, 9, 8]),
        vec![0, 0, 18, 27, 32].into_iter().sum()
    );
}

#[cfg(test)]
fn sample_input() -> &'static str {
    "2333133121414131402\n"
}

fn part1(disk: DiskMap) -> u64 {
    disk_checksum(&disk.compact_part1())
}

#[test]
fn test_part1() {
    let disk = parse_input(sample_input());
    assert_eq!(part1(disk), 1928);
}

fn main() {
    let input = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let disk = parse_input(input);
    println!("day 09 part 1: {}", part1(disk));
}
