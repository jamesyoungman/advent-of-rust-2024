use std::cmp::Ordering;
use std::ops::Range;

use std::env;

use std::iter;
use std::str;

mod machine;

use machine::{Computer, Fault, Number, OutputCheckMode, Parser, Program};

#[cfg(test)]
fn sample_part1_input() -> &'static str {
    concat!(
        "Register A: 729\n",
        "Register B: 0\n",
        "Register C: 0\n",
        "\n",
        "Program: 0,1,5,4,3,0\n",
    )
}

#[test]
fn test_run_main_example() {
    let parser = Parser::new();
    let (mut cpu, program) = parser.parse_input(sample_part1_input());
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, 0, true),
        Ok(vec![4, 6, 3, 5, 6, 3, 5, 2, 1, 0])
    );
}

#[test]
fn test_run_26() {
    let mut cpu = Computer { a: 0, b: 0, c: 9 };
    let program = vec![2, 6].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, 0, false),
        Ok(vec![])
    );
    dbg!(&cpu);
    assert_eq!(cpu.b, 1);
}

#[test]
fn test_run_505154() {
    let mut cpu = Computer { a: 10, b: 0, c: 0 };
    let program = vec![5, 0, 5, 1, 5, 4].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, 0, false),
        Ok(vec![0, 1, 2])
    );
    dbg!(&cpu);
}

#[test]
fn test_run_015430() {
    let mut cpu = Computer {
        a: 2024,
        b: 0,
        c: 0,
    };
    let program = vec![0, 1, 5, 4, 3, 0].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, 0, false),
        Ok(vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0])
    );
    assert_eq!(cpu.a, 0);
    dbg!(&cpu);
}

#[test]
fn test_run_17() {
    let mut cpu = Computer { a: 0, b: 29, c: 0 };
    let program = vec![1, 7].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, 0, false),
        Ok(vec![])
    );
    assert_eq!(cpu.b, 26);
    dbg!(&cpu);
}

#[test]
fn test_run_40() {
    let mut cpu = Computer {
        a: 0,
        b: 2024,
        c: 43690,
    };
    let program = vec![4, 0].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, 0, false),
        Ok(vec![])
    );
    assert_eq!(cpu.b, 44354);
    dbg!(&cpu);
}

fn run_program_with_a_value(
    cpu: &Computer,
    program: &Program,
    a: Number,
) -> Result<Vec<u8>, Fault> {
    let mut cpu = Computer { a, ..*cpu };
    cpu.run(program, &OutputCheckMode::Off, 0, false)
}

fn part1(cpu: &Computer, program: &Program) -> String {
    match run_program_with_a_value(cpu, program, cpu.a) {
        Ok(values) => values
            .iter()
            .map(|n| n.to_string())
            .collect::<Vec<_>>()
            .join(","),
        Err(e) => {
            panic!("part 1: unexpected error {e}");
        }
    }
}

fn check_quine(
    orig_cpu: &Computer,
    program: &Program,
    a: Number,
    check_mode: &OutputCheckMode,
) -> Option<Ordering> {
    let mut cpu = Computer {
        a,
        b: orig_cpu.b,
        c: orig_cpu.c,
    };
    match cpu.run(program, check_mode, 0, false) {
        Ok(_values) => Some(Ordering::Equal),
        Err(Fault::OutputTooShort) => {
            // A was too low.
            Some(Ordering::Less)
        }
        Err(Fault::OutputTooLong) => {
            // A was too high.
            Some(Ordering::Greater)
        }
        Err(Fault::IncorrectOutput { pos: _, value: _ }) => {
            // A holds a wrong value, but we don't know if it is too
            // low or too high.
            None
        }
        Err(e) => {
            panic!("cpu execution fault: {e:?}");
        }
    }
}

fn binary_search<F>(probe: F, mut range: Range<Number>) -> Option<Number>
where
    F: Fn(Number) -> Ordering,
{
    match probe(range.start) {
        Ordering::Greater => None,
        Ordering::Equal => Some(range.start),
        Ordering::Less => {
            match probe(range.end) {
                Ordering::Less => None,
                Ordering::Equal => Some(range.end),
                Ordering::Greater => {
                    // the range is good.
                    while !range.is_empty() {
                        let len = range.end - range.start;
                        assert!(len > 0);
                        if len == 1 {
                            return Some(range.start);
                        } else {
                            let mid = range.start + len / 2;
                            match probe(mid) {
                                Ordering::Less => {
                                    // probe point is too low.
                                    range = mid..range.end;
                                }
                                Ordering::Greater => {
                                    // probe point is too high
                                    range = range.start..mid;
                                }
                                Ordering::Equal => {
                                    return Some(mid);
                                }
                            }
                        }
                    }
                    // This is unexpected.
                    panic!("solution slipped through the cracks of binary_search");
                }
            }
        }
    }
}

fn suffix_match(v1: &[u8], v2: &[u8], len: usize) -> bool {
    if v1.len() < len || v2.len() < len {
        false
    } else {
        v1.iter()
            .rev()
            .zip(v2.iter().rev())
            .take(len)
            .all(|(a, b)| a == b)
    }
}

#[test]
fn test_suffix_match() {
    let a = [1, 2, 3, 4];
    let b = [6, 7, 3, 4];
    assert!(suffix_match(&a, &b, 0));
    assert!(suffix_match(&a, &b, 1));
    assert!(suffix_match(&a, &b, 2));
    assert!(suffix_match(&a, &a, 4));
    assert!(suffix_match(&b, &b, 4));
    assert!(!suffix_match(&a, &b, 3));
}

fn search_quine_2(orig_cpu: &Computer, program: &Program) -> Option<Number> {
    let mut a: Number = 0;
    let mut suffix = 1;
    while suffix <= program.values.len() {
        let mut cpu = Computer { a, ..*orig_cpu };
        match cpu.run(program, &OutputCheckMode::Off, 0, false) {
            Err(_) => (),
            Ok(values) => {
                if values == program.values {
                    return Some(a);
                } else {
                    if suffix_match(values.as_slice(), program.values.as_slice(), suffix) {
                        a *= 8;
                        suffix += 1;
                    } else {
                        a += 1;
                    }
                }
            }
        }
    }
    None
}

fn search_quine(orig_cpu: &Computer, program: &Program) -> Option<Number> {
    // Find an upper limit in `upper`.
    let mut upper = 1;
    loop {
        println!("Binary search: trying upper={upper}");
        match check_quine(orig_cpu, program, upper, &OutputCheckMode::LengthOnly) {
            Some(Ordering::Less) | None | Some(Ordering::Equal) => {
                // `upper` is too low or (the None case) we're not
                // certain it is too high.
                match upper.checked_mul(2) {
                    Some(n) => {
                        upper = n;
                        continue;
                    }
                    None => {
                        panic!("A value is out of range");
                    }
                }
            }
            Some(Ordering::Greater) => {
                break;
            }
        }
    }
    let mut lower = 0;
    loop {
        println!("Binary search setup: trying lower={lower}");
        lower = match check_quine(orig_cpu, program, lower, &OutputCheckMode::LengthOnly) {
            Some(Ordering::Less) => match lower.checked_mul(2) {
                Some(0) => 1,
                Some(n) => n,
                None => {
                    panic!("A value is out of range");
                }
            },
            Some(Ordering::Greater) | None | Some(Ordering::Equal) => {
                lower /= 2;
                break;
            }
        };
    }
    println!("Binary search coarse set-up is complete:\nlower={lower:#b}\nupper={upper:#b}");
    println!("Binary search coarse set-up is complete:\nlower={lower:10}\nupper={upper:10}");

    let r_lower = binary_search(
        |a| match check_quine(orig_cpu, program, a, &OutputCheckMode::LengthOnly) {
            None | Some(Ordering::Equal) => Ordering::Greater,
            Some(ordering) => ordering,
        },
        lower..upper,
    );
    let r_upper = binary_search(
        |a| match check_quine(orig_cpu, program, a, &OutputCheckMode::LengthOnly) {
            None | Some(Ordering::Equal) => Ordering::Less,
            Some(ordering) => ordering,
        },
        lower..upper,
    );
    match (r_lower, r_upper) {
        (Some(l), Some(u)) => {
            println!("lower: {l:x}");
            println!("upper: {u:x}");
            println!(
                "there are still {0} ({1:.3e}) values to test",
                u - l,
                (u - l) as f64,
            );
        }
        _ => {
            println!("one side of the range is unbounded");
        }
    }
    // Unfortunately, the above technique yields a value for
    // (upper-lower) which is around 1e14.  That's too large a range
    // to search linearly like this.
    (lower..=upper).find(|a| {
        Some(Ordering::Equal) == check_quine(orig_cpu, program, *a, &OutputCheckMode::Content)
    })
}

fn part2(cpu: &Computer, program: &Program) -> Option<Number> {
    //search_quine(cpu, program)

    search_quine_2(cpu, program)
}

#[test]
fn test_part1() {
    let parser = Parser::new();
    let (cpu, program) = parser.parse_input(sample_part1_input());
    assert_eq!(part1(&cpu, &program), "4,6,3,5,6,3,5,2,1,0".to_string());
}

#[cfg(test)]
fn sample_part2_input() -> &'static str {
    concat!(
        "Register A: 2024\n",
        "Register B: 0\n",
        "Register C: 0\n",
        "\n",
        "Program: 0,3,5,4,3,0\n",
    )
}

#[test]
fn test_part2() {
    let parser = Parser::new();
    let (cpu, program) = parser.parse_input(sample_part2_input());
    assert_eq!(part2(&cpu, &program), Some(117440));
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let parser = Parser::new();
    let (cpu, program) = parser.parse_input(input_str);

    match env::var_os("TRANSPILE_ONLY") {
        Some(_) => {
            print!("{}", program.to_native_code());
        }
        None => {
            println!("Day 17 part 1: {}", part1(&cpu, &program));
            match part2(&cpu, &program) {
                Some(a) => {
                    println!("Day 17 part 2: {a}");
                }
                None => {
                    println!("Day 17 part 2: found no solution");
                }
            }
        }
    }
}

#[test]
fn test_native_impl() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let got = str::from_utf8(include_bytes!("machine/native_body.include")).unwrap();
    let parser = Parser::new();
    let (_cpu, program) = parser.parse_input(input_str);
    let transpiled = program.to_native_code();
    if got.trim() != transpiled.trim() {
        panic!(concat!(
            "The transpiled code is out-of-date; please replace the body of ",
            "the file machine/native_body.include with the result of ",
            "running:\n",
            "env TRANSPILE_ONLY=1 cargo run --bin day17"
        ))
    }
}
