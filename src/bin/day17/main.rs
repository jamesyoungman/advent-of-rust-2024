use std::str;

mod machine;

use machine::{Computer, Fault, Number, Parser, Program};

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
        cpu.run(&program, 0, true),
        Ok(vec![4, 6, 3, 5, 6, 3, 5, 2, 1, 0])
    );
}

#[test]
fn test_run_26() {
    let mut cpu = Computer { a: 0, b: 0, c: 9 };
    let program = vec![2, 6].into();
    assert_eq!(cpu.run(&program, 0, false), Ok(vec![]));
    dbg!(&cpu);
    assert_eq!(cpu.b, 1);
}

#[test]
fn test_run_505154() {
    let mut cpu = Computer { a: 10, b: 0, c: 0 };
    let program = vec![5, 0, 5, 1, 5, 4].into();
    assert_eq!(cpu.run(&program, 0, false), Ok(vec![0, 1, 2]));
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
        cpu.run(&program, 0, false),
        Ok(vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0])
    );
    assert_eq!(cpu.a, 0);
    dbg!(&cpu);
}

#[test]
fn test_run_17() {
    let mut cpu = Computer { a: 0, b: 29, c: 0 };
    let program = vec![1, 7].into();
    assert_eq!(cpu.run(&program, 0, false), Ok(vec![]));
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
    assert_eq!(cpu.run(&program, 0, false), Ok(vec![]));
    assert_eq!(cpu.b, 44354);
    dbg!(&cpu);
}

fn run_program_with_a_value(
    cpu: &Computer,
    program: &Program,
    a: Number,
) -> Result<Vec<u8>, Fault> {
    let mut cpu = Computer { a, ..*cpu };
    cpu.run(program, 0, false)
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

fn part2(orig_cpu: &Computer, program: &Program) -> Option<Number> {
    let mut a: Number = 0;
    let mut suffix = 1;
    while suffix <= program.values.len() {
        let mut cpu = Computer { a, ..*orig_cpu };
        match cpu.run(program, 0, false) {
            Err(_) => (),
            Ok(values) => {
                if values == program.values {
                    return Some(a);
                } else if suffix_match(values.as_slice(), program.values.as_slice(), suffix) {
                    a *= 8;
                    suffix += 1;
                } else {
                    a += 1;
                }
            }
        }
    }
    None
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
