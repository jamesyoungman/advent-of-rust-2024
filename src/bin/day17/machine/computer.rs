use std::error::Error;
use std::fmt::{Display, Formatter};

use lib::error::Fail;

use super::program::{Opcode, Program};

pub type Number = u64;

#[derive(Debug, PartialEq, Eq)]
pub enum Fault {
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum OutputCheckMode {
    Off,
    Content,
    LengthOnly,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Computer {
    pub a: Number,
    pub b: Number,
    pub c: Number,
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

    fn run_native_until_output(
        &mut self,
        _program: &Program,
        mut pc: usize,
        _verbose: bool,
    ) -> Result<(u8, usize), Fault> {
        let mut got = || include!("native_body.include");
        match got() {
            None => Err(Fault::Halt),
            Some((output, pc)) => Ok((output, pc)),
        }
    }

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
        check_mode: &OutputCheckMode,
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
