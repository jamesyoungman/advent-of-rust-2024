use std::error::Error;
use std::fmt::{Display, Formatter, LowerHex};
use std::str;

use regex::Regex;

use lib::error::Fail;
use lib::parse::parse_number;

type Number = i64;

#[derive(Debug, PartialEq, Eq, Clone)]
enum OutputCheckMode {
    Off,
    On,
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct Computer {
    a: Number,
    b: Number,
    c: Number,
    pc: usize,
}

impl Default for Computer {
    fn default() -> Self {
        Computer {
            a: 0,
            b: 0,
            c: 0,
            pc: 0,
        }
    }
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
    IncorrectOutput(String),
    Failed(Fail),
}

impl Display for Fault {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Fault::Halt => f.write_str("program terminated normally"),
            Fault::Failed(e) => write!(f, "program failed: {e}"),
            Fault::IncorrectOutput(explanation) => {
                write!(f, "incorrect program output: {explanation}")
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
                        OutputCheckMode::On => match program.values.get(output_values.len()) {
                            None => {
                                return Err(Fault::IncorrectOutput("too much output".to_string()));
                            }
                            Some(&expected) if expected != output_value => {
                                return Err(Fault::IncorrectOutput(
					format!("at output position {0}, expected {expected} but got {output_value}",
						output_values.len())));
                            }
                            Some(_) => (),
                        },
                        OutputCheckMode::Off => (),
                    }
                    output_values.push(output_value);
                }
                Err(Fault::Halt) => {
                    match check_mode {
                        OutputCheckMode::Off => (),
                        OutputCheckMode::On => {
                            if output_values.len() < program.values.len() {
                                return Err(Fault::IncorrectOutput(format!(
                                    "program halted after producing only {0} output values",
                                    output_values.len()
                                )));
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
                self.a /= 1 << operand;
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
                eprintln!("JNZ: a={:x}", &self.a);
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
                eprintln!("OUT {outval:x}");
                output = Some(outval);
                false
            }
            Opcode::Bdv => {
                self.b = self.a / (1 << operand);
                false
            }
            Opcode::Cdv => {
                // This opcode does not appear in examples.
                self.c = self.a / (1 << operand);
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

fn binary_search<F>(mut lower: i64, mut upper: i64, target: i64, eval: F) -> i64
where
    F: Fn(i64) -> i64,
{
    let mut iteration = 0;
    while upper > lower + 1 {
        iteration += 1;
        let mid = (upper - lower) / 2 + lower;
        assert!(lower < mid);
        assert!(mid < upper);
        let y = eval(mid);
        println!(
            "iteration {iteration:3}: lower={lower:8} upper={upper:8} target={target:8} y={y:8}"
        );
        if y > target {
            upper = mid;
        } else {
            lower = mid;
        }
    }
    lower
}

fn values_as_hex<T: LowerHex>(values: &[T]) -> String {
    values
        .iter()
        .map(|v| format!("{v:#02x}"))
        .collect::<Vec<_>>()
        .join(",")
}

fn count_correct_final_digits(got: &[u8], expected: &[u8]) -> usize {
    expected
        .iter()
        .rev()
        .zip(got.iter().rev())
        .take_while(|(e, g)| e == g)
        .count()
}

#[test]
fn test_count_correct_final_digits() {
    assert_eq!(count_correct_final_digits(&[], &[]), 0);
    assert_eq!(count_correct_final_digits(&[0], &[0]), 1);
    assert_eq!(count_correct_final_digits(&[1], &[2]), 0);
}

fn search_lowest_bits(
    cpu: &Computer,
    program: &Program,
    a_prefix: Number,
    want_output_digits: usize,
) -> Vec<u8> {
    let accept = |lower_bits: &u8| -> bool {
        let a: Number = (a_prefix << 3) | Number::from(*lower_bits);
        match run_program_with_a_value(&cpu, program, a) {
            Err(e) => {
                panic!("cpu failed: {e}");
            }
            Ok(values) => {
                let correct = count_correct_final_digits(&values, &program.values);
                correct >= want_output_digits
            }
        }
    };

    (0u8..8u8).filter(accept).collect()
}

fn part2(cpu: &Computer, program: &Program, a_reg_limit: Number) -> Option<Number> {
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

//#[test]
//fn test_part2() {
//    let parser = Parser::new();
//    let (cpu, program) = parser.parse_input(sample_part2_input());
//    assert_eq!(part2(cpu, &program, 117441), Some(117440));
//}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let parser = Parser::new();
    let (cpu, program) = parser.parse_input(input_str);

    println!("Day 17 part 1: {}", part1(&cpu, &program));
    match part2(&cpu, &program, Number::MAX) {
        Some(a) => {
            println!("Day 17 part 2: {a}");
        }
        None => {
            println!("Day 17 part 2: found no solution");
        }
    }
}
