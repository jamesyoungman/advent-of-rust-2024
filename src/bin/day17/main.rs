use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str;

use regex::Regex;

use lib::error::Fail;
use lib::parse::parse_number;

type Number = u64;

#[derive(Debug, PartialEq, Eq)]
enum Fault {
    Halt,
    Failed(Fail),
}

impl Display for Fault {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Fault::Halt => f.write_str("program terminated normally"),
            Fault::Failed(e) => write!(f, "program failed: {e}"),
        }
    }
}

impl From<Fail> for Fault {
    fn from(e: Fail) -> Self {
        Fault::Failed(e)
    }
}

impl Error for Fault {}

#[derive(Debug, PartialEq, Eq)]
struct Computer {
    pub a: Number,
    pub b: Number,
    pub c: Number,
}

impl Computer {
    fn run_until_output(
        &mut self,
        program: &Program,
        mut pc: usize,
        verbose: bool,
    ) -> Result<(u8, usize), Fault> {
        loop {
            match self.execute_single_instruction(program, pc, verbose)? {
                (None, new_pc) => {
                    pc = new_pc;
                }
                (Some(output_value), new_pc) => {
                    return Ok((output_value, new_pc));
                }
            }
        }
    }

    pub fn run(
        &mut self,
        program: &Program,
        mut pc: usize,
        verbose: bool,
    ) -> Result<Vec<u8>, Fault> {
        let mut output_values = Vec::new();
        loop {
            match self.run_until_output(program, pc, verbose) {
                Ok((output_value, new_pc)) => {
                    pc = new_pc;
                    if verbose {
                        dbg!(&output_value);
                    }
                    output_values.push(output_value);
                }
                Err(Fault::Halt) => {
                    return Ok(output_values);
                }
                Err(e) => {
                    return Err(e);
                }
            }
        }
    }

    fn fetch_actual_operand(&self, opcode: &Opcode, raw_operand: u8) -> Result<Number, Fail> {
        use Opcode::*;
        match opcode {
            Adv | Bst | Out | Bdv | Cdv => match raw_operand {
                0..=3 => Ok(raw_operand.into()),
                4 => Ok(self.a),
                5 => Ok(self.b),
                6 => Ok(self.c),
                7 => {
                    panic!("combo operand 7 is invalid");
                }
                other => {
                    panic!("program contains out of range combo operand {other}");
                }
            },
            Bxl | Jnz => Ok(raw_operand.into()),
            Bxc => Ok(u64::MIN), // operand of BXC is supposed to be ignored.
        }
    }

    fn execute_single_instruction(
        &mut self,
        program: &Program,
        mut pc: usize,
        verbose: bool,
    ) -> Result<(Option<u8>, usize), Fault> {
        let (opcode, operand) = program.fetch(pc)?;
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
                            pc = new_pc;
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
            pc += 2;
        }
        Ok((output, pc))
    }
}

struct Parser {
    reg_re: Regex,
}

impl Default for Parser {
    fn default() -> Parser {
        Parser {
            reg_re: Regex::new(r"^Register ([ABC]): (\d+)$").unwrap(),
        }
    }
}

impl Parser {
    fn parse_program(&self, s: &str) -> Program {
        const PREFIX: &str = "Program: ";
        match s.strip_prefix(PREFIX) {
            None => {
                panic!("program '{s}' does not begin with prefix '{PREFIX}'");
            }
            Some(code) => Program::new(
                code.split(',')
                    .map(|s| {
                        let value = parse_number(s);
                        if value > 7 {
                            panic!("invalid program value {value}");
                        }
                        value
                    })
                    .collect(),
            ),
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
            (Some(a), Some(b), Some(c)) => Computer { a, b, c },
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

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
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

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Opcode::Adv => "ADV",
            Opcode::Bxl => "BXL",
            Opcode::Bst => "BST",
            Opcode::Jnz => "JNZ",
            Opcode::Bxc => "BXC",
            Opcode::Out => "OUT",
            Opcode::Bdv => "BDV",
            Opcode::Cdv => "CDV",
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Program {
    pub values: Vec<u8>,
}

impl From<Vec<u8>> for Program {
    fn from(values: Vec<u8>) -> Program {
        Program { values }
    }
}

fn decode_opcode(val: u8) -> Option<Opcode> {
    use Opcode::*;
    match val {
        0 => Some(Adv),
        1 => Some(Bxl),
        2 => Some(Bst),
        3 => Some(Jnz),
        4 => Some(Bxc),
        5 => Some(Out),
        6 => Some(Bdv),
        7 => Some(Cdv),
        _ => None,
    }
}

impl Program {
    pub fn new(values: Vec<u8>) -> Self {
        Self { values }
    }

    pub fn fetch(&self, pc: usize) -> Result<(Opcode, u8), Fault> {
        if let Some(val) = self.values.get(pc) {
            let opcode = match decode_opcode(*val) {
                Some(op) => op,
                None => {
                    return Err(Fault::Failed(Fail(format!("opcode {val} is invalid"))));
                }
            };
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
    let parser = Parser::default();
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
    let parser = Parser::default();
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
    let parser = Parser::default();
    let (cpu, program) = parser.parse_input(sample_part2_input());
    assert_eq!(part2(&cpu, &program), Some(117440));
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let parser = Parser::default();
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
