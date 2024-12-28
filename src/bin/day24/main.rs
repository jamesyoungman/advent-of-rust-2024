use std::collections::{HashMap, HashSet};
use std::str;

use regex::Regex;

use lib::parse::parse_number;

#[derive(Debug, Eq, PartialEq)]
enum Op {
    Xor,
    Or,
    And,
}

impl Op {
    fn eval(&self, a: bool, b: bool) -> bool {
        match self {
            Op::Xor => a ^ b,
            Op::Or => a || b,
            Op::And => a && b,
        }
    }
}

impl From<&str> for Op {
    fn from(s: &str) -> Op {
        match s {
            "XOR" => Op::Xor,
            "AND" => Op::And,
            "OR" => Op::Or,
            other => {
                panic!("unrecognised boolean operation {other}");
            }
        }
    }
}

#[test]
fn test_op_eval_xor() {
    assert!(!(Op::Xor).eval(false, false));
    assert!((Op::Xor).eval(false, true));
    assert!((Op::Xor).eval(true, false));
    assert!(!(Op::Xor).eval(true, true));
}

#[test]
fn test_op_eval_or() {
    assert!(!(Op::Or).eval(false, false));
    assert!((Op::Or).eval(false, true));
    assert!((Op::Or).eval(true, false));
    assert!((Op::Or).eval(true, true));
}

#[test]
fn test_op_eval_and() {
    assert!(!(Op::And).eval(false, false));
    assert!(!(Op::And).eval(false, true));
    assert!(!(Op::And).eval(true, false));
    assert!((Op::And).eval(true, true));
}

#[derive(Debug, Eq, PartialEq)]
struct Gate {
    left_signal: String,
    right_signal: String,
    operation: Op,
    output: String,
}

fn evaluate_known_states(
    gates_by_output: &HashMap<String, Gate>,
    outputs_todo: &mut HashSet<String>,
    states: &mut HashMap<String, bool>,
) -> bool {
    let mut changes = false;
    let mut still_remaining: HashSet<String> = outputs_todo.clone();
    for output_name in outputs_todo.drain() {
        if let Some(gate) = gates_by_output.get(output_name.as_str()) {
            if let Some(left) = states.get(&gate.left_signal) {
                if let Some(right) = states.get(&gate.right_signal) {
                    let output_value = gate.operation.eval(*left, *right);
                    match states.insert(gate.output.to_string(), output_value) {
                        Some(_) => {
                            panic!("state of output {} was previously known, but output {output_name} was stil in todo",
				   &gate.output
			    );
                        }
                        None => {
                            still_remaining.remove(gate.output.as_str());
                            changes = true;
                            // Resume at the top of the loop so that we don't add
                            // this output name to `still_remaining`.
                            continue;
                        }
                    }
                } else {
                    // Skip this, we don't know the value of the right input yet
                    if still_remaining.insert(gate.right_signal.to_string()) {
                        changes = true;
                    }
                }
            } else {
                // Skip this, we don't know the value of the left input yet
                if still_remaining.insert(gate.left_signal.to_string()) {
                    changes = true;
                }
            }
        } else {
            panic!("output {output_name} is in todo, but we don't know how it is calculated");
        }
        still_remaining.insert(output_name);
    }
    *outputs_todo = still_remaining;
    changes
}

#[derive(Debug, Eq, PartialEq)]
struct FruitMonitor {
    inputs: HashMap<String, bool>,
    gates: HashMap<String, Gate>,
}

struct Parser {
    gate_re: Regex,
}

impl Parser {
    fn new() -> Parser {
        Parser {
            gate_re: Regex::new("^(.+) (AND|OR|XOR) (.+) -> (.+)$").unwrap(),
        }
    }

    fn parse_input_line(line: &str) -> (String, bool) {
        match line.split_once(':') {
            Some((name, value)) => {
                let value: bool = match parse_number(value) {
                    0 => false,
                    1 => true,
                    other => {
                        panic!("line '{line}' contains unexpected value '{other}'");
                    }
                };
                (name.to_string(), value)
            }
            None => {
                panic!("expected to see a colon on this line: {line}");
            }
        }
    }

    fn parse_inputs(top: &str) -> HashMap<String, bool> {
        top.lines().map(Self::parse_input_line).collect()
    }

    fn parse_gate_line(&self, line: &str) -> (String, Gate) {
        if let Some(m) = self.gate_re.captures(line) {
            let (_, [left, operation, right, output]) = m.extract();
            let operation = Op::from(operation);
            let gate = Gate {
                left_signal: left.to_string(),
                right_signal: right.to_string(),
                operation,
                output: output.to_string(),
            };
            (output.to_string(), gate)
        } else {
            panic!("input line {line} does not match expected format");
        }
    }

    fn parse_gates(&self, bot: &str) -> HashMap<String, Gate> {
        bot.lines().map(|line| self.parse_gate_line(line)).collect()
    }

    pub fn parse_input(&self, input: &str) -> FruitMonitor {
        match input.split_once("\n\n") {
            Some((top, bot)) => {
                let inputs = Self::parse_inputs(top);
                let gates = self.parse_gates(bot);
                FruitMonitor { inputs, gates }
            }
            None => {
                panic!("expected to see a blank line in the input");
            }
        }
    }
}

#[cfg(test)]
fn small_sample_input() -> &'static str {
    concat!(
        "x00: 1\n",
        "x01: 1\n",
        "x02: 1\n",
        "y00: 0\n",
        "y01: 1\n",
        "y02: 0\n",
        "\n",
        "x00 AND y00 -> z00\n",
        "x01 XOR y01 -> z01\n",
        "x02 OR y02 -> z02\n",
    )
}

#[cfg(test)]
fn large_sample_input() -> &'static str {
    concat!(
        "x00: 1\n",
        "x01: 0\n",
        "x02: 1\n",
        "x03: 1\n",
        "x04: 0\n",
        "y00: 1\n",
        "y01: 1\n",
        "y02: 1\n",
        "y03: 1\n",
        "y04: 1\n",
        "\n",
        "ntg XOR fgs -> mjb\n",
        "y02 OR x01 -> tnw\n",
        "kwq OR kpj -> z05\n",
        "x00 OR x03 -> fst\n",
        "tgd XOR rvg -> z01\n",
        "vdt OR tnw -> bfw\n",
        "bfw AND frj -> z10\n",
        "ffh OR nrd -> bqk\n",
        "y00 AND y03 -> djm\n",
        "y03 OR y00 -> psh\n",
        "bqk OR frj -> z08\n",
        "tnw OR fst -> frj\n",
        "gnj AND tgd -> z11\n",
        "bfw XOR mjb -> z00\n",
        "x03 OR x00 -> vdt\n",
        "gnj AND wpb -> z02\n",
        "x04 AND y00 -> kjc\n",
        "djm OR pbm -> qhw\n",
        "nrd AND vdt -> hwm\n",
        "kjc AND fst -> rvg\n",
        "y04 OR y02 -> fgs\n",
        "y01 AND x02 -> pbm\n",
        "ntg OR kjc -> kwq\n",
        "psh XOR fgs -> tgd\n",
        "qhw XOR tgd -> z09\n",
        "pbm OR djm -> kpj\n",
        "x03 XOR y03 -> ffh\n",
        "x00 XOR y04 -> ntg\n",
        "bfw OR bqk -> z06\n",
        "nrd XOR fgs -> wpb\n",
        "frj XOR qhw -> z04\n",
        "bqk OR frj -> z07\n",
        "y03 OR x01 -> nrd\n",
        "hwm AND bqk -> z03\n",
        "tgd XOR rvg -> z12\n",
        "tnw OR pbm -> gnj\n",
    )
}

#[test]
fn test_parse_small_sample_input() {
    let parser = Parser::new();
    let mut mon = parser.parse_input(small_sample_input());
    dbg!(&mon);
    assert_eq!(mon.determine_outputs(), 0b100);
}

#[test]
fn test_parse_large_sample_input() {
    let parser = Parser::new();
    let mut mon = parser.parse_input(large_sample_input());
    dbg!(&mon);
    assert_eq!(mon.determine_outputs(), 0b0011111101000);
}

impl FruitMonitor {
    fn zip_output_bits(&self) -> u64 {
        fn folder(acc: u64, (digit_pos, digit_val): (u64, bool)) -> u64 {
            let digit_val = match digit_val {
                true => 1,
                false => 0,
            };
            acc | (digit_val << digit_pos)
        }

        let select_z_bit = |(name, value): (&String, &bool)| -> Option<(u64, bool)> {
            match name.strip_prefix('z') {
                Some(suffix) => {
                    let bit_pos = parse_number(suffix);
                    Some((bit_pos, *value))
                }
                None => None, // not zXX
            }
        };

        self.inputs.iter().filter_map(select_z_bit).fold(0, folder)
    }

    fn determine_outputs(&mut self) -> u64 {
        fn extract_z_output(gate: &Gate) -> Option<String> {
            if gate.output.starts_with('z') {
                Some(gate.output.to_string())
            } else {
                None
            }
        }
        let mut todo: HashSet<String> = self.gates.values().filter_map(extract_z_output).collect();
        for iteration in 0.. {
            if todo.is_empty() {
                break;
            }
            let changes = evaluate_known_states(&self.gates, &mut todo, &mut self.inputs);
            if !changes {
                panic!("iteration {iteration}: made no progress on to-do items {todo:?}");
            }
        }
        self.zip_output_bits()
    }
}

fn part1(input_str: &str) -> u64 {
    let parser = Parser::new();
    let mut mon = parser.parse_input(input_str);
    mon.determine_outputs()
}

#[test]
fn test_part1() {
    assert_eq!(part1(large_sample_input()), 2024);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("Day 24 part 1: {}", part1(input_str));
}
