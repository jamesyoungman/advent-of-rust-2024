use std::error::Error;
use std::fmt::{Display, Formatter};
use std::str;

use regex::Regex;

use lib::error::Fail;
use lib::parse::parse_number;

type Number = i64;

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug)]
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

    fn run(&mut self, program: &Program) -> Result<Vec<Number>, Fail> {
        let mut output = Vec::new();
        loop {
            match self.execute_single_instruction(program, &mut output) {
                Ok(()) => (),
                Err(Fault::Halt) => {
                    return Ok(output);
                }
                Err(Fault::Failed(e)) => {
                    return Err(e);
                }
            }
        }
    }

    fn execute_single_instruction(
        &mut self,
        program: &Program,
        output: &mut Vec<Number>,
    ) -> Result<(), Fault> {
        let (opcode, operand) = program.fetch(self.pc)?;
        let operand = self.fetch_actual_operand(&opcode, operand)?;
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
                output.push(operand % 8);
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
        Ok(())
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
fn sample_input() -> &'static str {
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
    let (cpu, prog) = parser.parse_input(sample_input());
    assert_eq!(cpu.a, 729);
    assert_eq!(cpu.b, 0);
    assert_eq!(cpu.c, 0);
    assert_eq!(cpu.pc, 0);
    assert_eq!(&prog.values, &[0, 1, 5, 4, 3, 0]);
}

#[test]
fn test_run_main_example() {
    let parser = Parser::new();
    let (mut cpu, program) = parser.parse_input(sample_input());
    assert_eq!(cpu.run(&program), Ok(vec![4, 6, 3, 5, 6, 3, 5, 2, 1, 0]));
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
    assert_eq!(cpu.run(&program), Ok(vec![]));
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
    assert_eq!(cpu.run(&program), Ok(vec![0, 1, 2]));
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
    assert_eq!(cpu.run(&program), Ok(vec![4, 2, 5, 6, 7, 7, 7, 7, 3, 1, 0]));
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
    assert_eq!(cpu.run(&program), Ok(vec![]));
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
    assert_eq!(cpu.run(&program), Ok(vec![]));
    assert_eq!(cpu.b, 44354);
    dbg!(&cpu);
}

fn part1(mut cpu: Computer, program: &Program) -> String {
    match cpu.run(program) {
        Ok(output) => {
            let output: Vec<String> = output.iter().map(|n| n.to_string()).collect();
            output.join(",")
        }
        Err(e) => {
            panic!("part 1: unexpected error {e}");
        }
    }
}

#[test]
fn test_part1() {
    let parser = Parser::new();
    let (cpu, program) = parser.parse_input(sample_input());
    assert_eq!(part1(cpu, &program), "4,6,3,5,6,3,5,2,1,0".to_string());
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let parser = Parser::new();
    let (cpu, program) = parser.parse_input(input_str);

    println!("Day 17 part 1: {}", part1(cpu, &program));
}
