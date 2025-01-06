use std::str::FromStr;

use regex::Regex;

use super::program::Program;
use super::{computer::Computer, computer::Number};

pub fn parse_number<T>(s: &str) -> T
where
    T: FromStr,
{
    match s.trim().parse() {
        Ok(n) => n,
        Err(_) => {
            panic!("failed to parse '{s}' as a number");
        }
    }
}

pub struct Parser {
    reg_re: Regex,
}

impl Parser {
    pub fn new() -> Parser {
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

    pub fn parse_input(&self, input: &str) -> (Computer, Program) {
        match input.split_once("\n\n") {
            Some((regs, prog)) => (self.parse_cpu(regs), self.parse_program(prog)),
            None => {
                panic!("not a valid input");
            }
        }
    }
}
