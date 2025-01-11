use std::fmt::Display;

use lib::error::Fail;

use super::computer::Fault;

#[repr(u8)]
#[derive(Debug, PartialEq, Eq)]
pub enum Opcode {
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
pub struct Program {
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
