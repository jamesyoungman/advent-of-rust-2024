use std::error::Error;
use std::fmt::{Display, Formatter};

use lib::error::Fail;

use super::program::{Opcode, Program};

pub type Number = u64;

#[derive(Debug, PartialEq, Eq)]
pub enum Fault {
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
pub struct Computer {
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
