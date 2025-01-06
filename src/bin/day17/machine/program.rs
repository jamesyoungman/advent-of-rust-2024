use std::collections::BTreeSet;
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

struct SyntaxError(pub String);

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0.as_str())
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

    fn fetch_symbolic_operand(
        &self,
        opcode: &Opcode,
        raw_operand: u8,
    ) -> Result<String, SyntaxError> {
        use Opcode::*;
        match opcode {
            Adv | Bst | Out | Bdv | Cdv => match raw_operand {
                0..=3 => Ok(raw_operand.to_string()),
                4 => Ok("self.a".to_string()),
                5 => Ok("self.b".to_string()),
                6 => Ok("self.c".to_string()),
                7 => Err(SyntaxError("combo operand 7 is invalid".to_string())),
                other => {
                    panic!("program contains out of range combo operand {other}");
                }
            },
            Bxl | Jnz => Ok(raw_operand.to_string()),
            Bxc => Ok(u64::MIN.to_string()), // operand of BXC is supposed to be ignored.
        }
    }

    fn transpile_insn(
        &self,
        indent: usize,
        pc: usize,
        raw_opcode: &u8,
        raw_operand: &u8,
    ) -> String {
        let opcode = match decode_opcode(*raw_opcode) {
            None => {
                panic!("unable to decode opcode {raw_opcode} at pc={pc}");
            }
            Some(s) => s,
        };
        let operand_string = match self.fetch_symbolic_operand(&opcode, *raw_operand) {
            Ok(s) => s,
            Err(e) => {
                panic!("unable to decode operand {raw_operand}: {e}");
            }
        };
        let comment: String =
            format!("\n/* {pc:02}: {raw_opcode:02}{raw_operand:02} {opcode:3} {raw_operand:2} */");

        let code: String = match opcode {
            Opcode::Adv => format!("self.a >>= {operand_string};"),
            Opcode::Bxl => format!("self.b ^= {operand_string};"),
            Opcode::Bst => format!("self.b = {operand_string} & 7;"),
            Opcode::Jnz => {
                format!(
                    "if self.a != 0 {{ pc = {operand_string}; }} else {{ pc = {}; }}",
                    pc + 2
                )
            }
            Opcode::Bxc => "self.b ^= self.c;".to_string(),
            Opcode::Out => {
                format!(
                    "let outval = (self.a % 8) as u8;\nreturn Some((outval, {}));",
                    pc + 2
                )
            }
            Opcode::Bdv => format!("self.b = self.a >> {operand_string};"),
            Opcode::Cdv => format!("self.c = self.a >> {operand_string};"),
        };
        let indent = |code: &str| -> String {
            const BLANK: &str = "";
            code.lines()
                .map(|line| format!("{BLANK:indent$}{line}"))
                .collect::<Vec<String>>()
                .join("\n")
        };
        indent(format!("{comment}\n{code}").as_str())
    }

    pub fn to_native_code(&self) -> String {
        let mut chunks: Vec<String> =
            vec![concat!("        loop {\n", "            match pc {",).to_string()];

        let mut jump_targets: BTreeSet<u8> = self
            .values
            .iter()
            .enumerate()
            .step_by(2)
            .filter_map(|(pc, opcode)| {
                if decode_opcode(*opcode) == Some(Opcode::Jnz) {
                    // The operand is always literal.
                    let target = self
                        .values
                        .get(pc + 1)
                        .expect("Jnz instructions should be complete");
                    Some(*target)
                } else {
                    None
                }
            })
            .collect();
        // Any instructions following an OUT instruction should also be
        // handled as a jump target, as the run_until_output()
        // function may be called with that as the initial PC value.
        for (pc, opcode) in self.values.iter().enumerate().step_by(2) {
            match decode_opcode(*opcode) {
                Some(Opcode::Out) => {
                    let next_insn = (pc + 2) as u8;
                    jump_targets.insert(next_insn);
                }
                _ => (),
            }
        }
        //dbg!(&jump_targets);
        let mut progit = self.values.iter().enumerate();

        while let Some((pc, opcode)) = progit.next() {
            let is_jump_target = match u8::try_from(pc) {
                Ok(n) => jump_targets.contains(&n),
                Err(_) => {
                    // Out of range of a Jnz operand, so cannot be a
                    // jump target.
                    false
                }
            };

            if is_jump_target || pc == 0 {
                if pc > 0 {
                    chunks.push(format!("                }}"));
                }
                chunks.push(format!("                {pc} => {{"));
            }
            if let Some((_, raw_operand)) = progit.next() {
                chunks.push(self.transpile_insn(20, pc, opcode, raw_operand));
            } else {
                panic!("missing operand at program location {}", pc + 1);
            }
        }
        chunks.push(format!("                }}"));
        chunks.push(
            concat!(
                "                _ => {\n",
                "                    return None;\n",
                "                }\n",
                "            } // end of match statement\n",
                "        } // end of loop\n",
            )
            .to_string(),
        );
        chunks.join("\n")
    }
}
