use std::cmp::Ordering;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::ops::Range;
use std::str;

use regex::Regex;

use lib::error::Fail;
use lib::parse::parse_number;

type Number = i64;

#[derive(Debug, PartialEq, Eq, Clone)]
enum OutputCheckMode {
    Off,
    Content,
    LengthOnly,
}

#[derive(Debug, PartialEq, Eq, Clone, Default)]
struct Computer {
    a: Number,
    b: Number,
    c: Number,
    pc: usize,
}

#[derive(Debug, PartialEq, Eq)]
struct Program {
    values: Vec<u8>,
}

impl From<Vec<u8>> for Program {
    fn from(values: Vec<u8>) -> Program {
        Program { values }
    }
}

#[repr(u8)]
#[derive(Debug)]
enum Opcode {
    Adv = 0,
    Bxl = 1,
    Bst = 2,
    Jnz = 3,
    Bxc = 4,
    Out = 5,
    Bdv = 6,
    Cdv = 7,
}

#[derive(Debug, PartialEq, Eq)]
enum Fault {
    Halt,
    OutputTooShort,
    OutputTooLong,
    IncorrectOutput { pos: usize, value: u8 },
    Failed(Fail),
}

impl Display for Fault {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Fault::Halt => f.write_str("program terminated normally"),
            Fault::Failed(e) => write!(f, "program failed: {e}"),
            Fault::OutputTooShort => f.write_str("output is too short"),
            Fault::OutputTooLong => f.write_str("output is too long"),
            Fault::IncorrectOutput { pos, value } => {
                write!(
                    f,
                    "incorrect program output value {value} at position {pos}"
                )
            }
        }
    }
}

impl From<Fail> for Fault {
    fn from(e: Fail) -> Self {
        Fault::Failed(e)
    }
}

impl Error for Fault {}

impl Program {
    fn fetch(&self, pc: usize) -> Result<(Opcode, u8), Fault> {
        use Opcode::*;
        if let Some(val) = self.values.get(pc) {
            let opcode = match val {
                0 => Ok(Adv),
                1 => Ok(Bxl),
                2 => Ok(Bst),
                3 => Ok(Jnz),
                4 => Ok(Bxc),
                5 => Ok(Out),
                6 => Ok(Bdv),
                7 => Ok(Cdv),
                other => Err(Fault::Failed(Fail(format!("opcode {other} is invalid")))),
            }?;
            if let Some(raw_operand) = self.values.get(pc + 1) {
                Ok((opcode, *raw_operand))
            } else {
                Err(Fault::Failed(Fail(format!(
                    "attempted to fetch operand from out-of-range program location {pc}"
                ))))
            }
        } else {
            Err(Fault::Halt)
        }
    }
}

impl Computer {
    fn fetch_combo_operand(&self, raw: u8) -> Result<Number, Fail> {
        match raw {
            0..=3 => Ok(raw.into()),
            4 => Ok(self.a),
            5 => Ok(self.b),
            6 => Ok(self.c),
            7 => Err(Fail("combo operand 7 is invalid".to_string())),
            other => {
                panic!("program contains out of range combo operand {other}");
            }
        }
    }

    fn fetch_literal_operand(&self, raw: u8) -> Number {
        raw.into()
    }

    fn fetch_actual_operand(&self, opcode: &Opcode, raw_operand: u8) -> Result<Number, Fail> {
        use Opcode::*;
        match opcode {
            Adv | Bst | Out | Bdv | Cdv => self.fetch_combo_operand(raw_operand),
            Bxl | Jnz => Ok(self.fetch_literal_operand(raw_operand)),
            Bxc => Ok(Number::MIN), // operand of BXC is supposed to be ignored.
        }
    }

    fn run(
        &mut self,
        program: &Program,
        check_mode: &OutputCheckMode,
        verbose: bool,
    ) -> Result<Vec<u8>, Fault> {
        let mut output_values = Vec::new();
        loop {
            match self.execute_single_instruction(program, verbose) {
                Ok(None) => (),
                Ok(Some(output_value)) => {
                    if verbose {
                        dbg!(&output_value);
                    }
                    match check_mode {
                        OutputCheckMode::Content => match program.values.get(output_values.len()) {
                            None => {
                                return Err(Fault::OutputTooLong);
                            }
                            Some(&expected) if expected != output_value => {
                                return Err(Fault::IncorrectOutput {
                                    pos: output_values.len(),
                                    value: output_value,
                                });
                            }
                            Some(_) => (),
                        },
                        OutputCheckMode::LengthOnly => {
                            if output_values.len() >= program.values.len() {
                                return Err(Fault::OutputTooLong);
                            }
                        }
                        OutputCheckMode::Off => (),
                    }
                    output_values.push(output_value);
                }
                Err(Fault::Halt) => {
                    match check_mode {
                        OutputCheckMode::Off => (),
                        OutputCheckMode::Content | OutputCheckMode::LengthOnly => {
                            if output_values.len() < program.values.len() {
                                return Err(Fault::OutputTooShort);
                            }
                        }
                    }
                    return Ok(output_values);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }

    fn execute_single_instruction(
        &mut self,
        program: &Program,
        verbose: bool,
    ) -> Result<Option<u8>, Fault> {
        let (opcode, operand) = program.fetch(self.pc)?;
        let operand = self.fetch_actual_operand(&opcode, operand)?;
        let mut output: Option<u8> = None;
        let jumped: bool = match opcode {
            Opcode::Adv => {
                self.a >>= operand;
                false
            }
            Opcode::Bxl => {
                self.b ^= operand;
                false
            }
            Opcode::Bst => {
                self.b = operand & 7;
                false
            }
            Opcode::Jnz => {
                //eprintln!("JNZ: a={:x}", &self.a);
                if self.a == 0 {
                    false
                } else {
                    match operand.try_into() {
                        Ok(new_pc) => {
                            self.pc = new_pc;
                            true
                        }
                        Err(e) => {
                            return Err(Fault::Failed(Fail(format!(
                                "operand {operand} is not valid as as a new PC value: {e}"
                            ))));
                        }
                    }
                }
            }
            Opcode::Bxc => {
                self.b ^= self.c;
                false
            }
            Opcode::Out => {
                if verbose {
                    dbg!(&operand);
                }
                let outval: u8 = (operand % 8) as u8;
                //eprintln!("OUT {outval:x}");
                output = Some(outval);
                false
            }
            Opcode::Bdv => {
                self.b = self.a >> operand;
                false
            }
            Opcode::Cdv => {
                // This opcode does not appear in examples.
                self.c = self.a >> operand;
                false
            }
        };
        if !jumped {
            self.pc += 2;
        }
        Ok(output)
    }
}

struct Parser {
    reg_re: Regex,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            reg_re: Regex::new(r"^Register ([ABC]): (\d+)$").unwrap(),
        }
    }

    fn parse_program(&self, s: &str) -> Program {
        const PREFIX: &str = "Program: ";
        match s.strip_prefix(PREFIX) {
            None => {
                panic!("program '{s}' does not begin with prefix '{PREFIX}'");
            }
            Some(code) => Program {
                values: code
                    .split(',')
                    .map(|s| {
                        let value = parse_number(s);
                        if value > 7 {
                            panic!("invalid program value {value}");
                        }
                        value
                    })
                    .collect(),
            },
        }
    }

    fn parse_cpu(&self, s: &str) -> Computer {
        let mut a: Option<Number> = None;
        let mut b: Option<Number> = None;
        let mut c: Option<Number> = None;
        for line in s.lines() {
            if let Some(m) = self.reg_re.captures(line) {
                let (_, [reg_name, reg_value]) = m.extract();
                let value = parse_number(reg_value);
                match reg_name {
                    "A" => {
                        a = Some(value);
                    }
                    "B" => {
                        b = Some(value);
                    }
                    "C" => {
                        c = Some(value);
                    }
                    _ => unreachable!(),
                }
            } else {
                panic!("invalid program line {line}");
            }
        }
        match (a, b, c) {
            (Some(a), Some(b), Some(c)) => Computer { a, b, c, pc: 0 },
            _ => {
                panic!("program input doesn't set all the registers");
            }
        }
    }

    fn parse_input(&self, input: &str) -> (Computer, Program) {
        match input.split_once("\n\n") {
            Some((regs, prog)) => (self.parse_cpu(regs), self.parse_program(prog)),
            None => {
                panic!("not a valid input");
            }
        }
    }
}

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
fn test_parse() {
    let parser = Parser::new();
    let (cpu, prog) = parser.parse_input(sample_part1_input());
    assert_eq!(cpu.a, 729);
    assert_eq!(cpu.b, 0);
    assert_eq!(cpu.c, 0);
    assert_eq!(cpu.pc, 0);
    assert_eq!(&prog.values, &[0, 1, 5, 4, 3, 0]);
}

#[test]
fn test_run_main_example() {
    let parser = Parser::new();
    let (mut cpu, program) = parser.parse_input(sample_part1_input());
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, true),
        Ok(vec![4, 6, 3, 5, 6, 3, 5, 2, 1, 0])
    );
}

#[test]
fn test_run_26() {
    let mut cpu = Computer {
        a: 0,
        b: 0,
        c: 9,
        pc: 0,
    };
    let program = vec![2, 6].into();
    assert_eq!(cpu.run(&program, &OutputCheckMode::Off, false), Ok(vec![]));
    dbg!(&cpu);
    assert_eq!(cpu.b, 1);
}

#[test]
fn test_run_505154() {
    let mut cpu = Computer {
        a: 10,
        b: 0,
        c: 0,
        pc: 0,
    };
    let program = vec![5, 0, 5, 1, 5, 4].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, false),
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
        pc: 0,
    };
    let program = vec![0, 1, 5, 4, 3, 0].into();
    assert_eq!(
        cpu.run(&program, &OutputCheckMode::Off, false),
        Ok(vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0])
    );
    assert_eq!(cpu.a, 0);
    dbg!(&cpu);
}

#[test]
fn test_run_17() {
    let mut cpu = Computer {
        a: 0,
        b: 29,
        c: 0,
        pc: 0,
    };
    let program = vec![1, 7].into();
    assert_eq!(cpu.run(&program, &OutputCheckMode::Off, false), Ok(vec![]));
    assert_eq!(cpu.b, 26);
    dbg!(&cpu);
}

#[test]
fn test_run_40() {
    let mut cpu = Computer {
        a: 0,
        b: 2024,
        c: 43690,
        pc: 0,
    };
    let program = vec![4, 0].into();
    assert_eq!(cpu.run(&program, &OutputCheckMode::Off, false), Ok(vec![]));
    assert_eq!(cpu.b, 44354);
    dbg!(&cpu);
}

fn run_program_with_a_value(
    cpu: &Computer,
    program: &Program,
    a: Number,
) -> Result<Vec<u8>, Fault> {
    let mut cpu = Computer { a, ..*cpu };
    cpu.run(program, &OutputCheckMode::Off, false)
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
        pc: 0,
    };
    match cpu.run(program, check_mode, false) {
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
    search_quine(cpu, program)
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
