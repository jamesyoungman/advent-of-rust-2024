use std::{
    collections::HashMap,
    fmt::{Display, Write},
    hash::Hash,
    ops::{Add, AddAssign},
    str,
};

use lib::{grid::Position, parse::parse_number};

trait Key {
    fn position(&self) -> Position;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy, Debug)]
enum NumpadKey {
    Key0,
    KeyA,
    Key1,
    Key2,
    Key3,
    Key4,
    Key5,
    Key6,
    Key7,
    Key8,
    Key9,
}

impl From<char> for NumpadKey {
    fn from(ch: char) -> Self {
        match ch {
            '0' => NumpadKey::Key0,
            'A' => NumpadKey::KeyA,
            '1' => NumpadKey::Key1,
            '2' => NumpadKey::Key2,
            '3' => NumpadKey::Key3,
            '4' => NumpadKey::Key4,
            '5' => NumpadKey::Key5,
            '6' => NumpadKey::Key6,
            '7' => NumpadKey::Key7,
            '8' => NumpadKey::Key8,
            '9' => NumpadKey::Key9,
            other => {
                panic!("unexpected number pad key '{other}'");
            }
        }
    }
}

impl From<NumpadKey> for char {
    fn from(value: NumpadKey) -> Self {
        match value {
            NumpadKey::Key0 => '0',
            NumpadKey::KeyA => 'A',
            NumpadKey::Key1 => '1',
            NumpadKey::Key2 => '2',
            NumpadKey::Key3 => '3',
            NumpadKey::Key4 => '4',
            NumpadKey::Key5 => '5',
            NumpadKey::Key6 => '6',
            NumpadKey::Key7 => '7',
            NumpadKey::Key8 => '8',
            NumpadKey::Key9 => '9',
        }
    }
}

impl Display for NumpadKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ch: char = (*self).into();
        f.write_char(ch)
    }
}

impl Key for NumpadKey {
    fn position(&self) -> Position {
        let (x, y) = match self {
            NumpadKey::Key0 => (-1, 0),
            NumpadKey::KeyA => (0, 0),
            NumpadKey::Key1 => (-2, -1),
            NumpadKey::Key2 => (-1, -1),
            NumpadKey::Key3 => (0, -1),
            NumpadKey::Key4 => (-2, -2),
            NumpadKey::Key5 => (-1, -2),
            NumpadKey::Key6 => (0, -2),
            NumpadKey::Key7 => (-2, -3),
            NumpadKey::Key8 => (-1, -3),
            NumpadKey::Key9 => (0, -3),
        };
        Position { x, y }
    }
}

const NUMPAD_DANGER_POS: Position = Position { x: -2, y: 0 };

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash, Default)]
enum DirectionalKey {
    #[default]
    KeyA,
    KeyUp,
    KeyLeft,
    KeyDown,
    KeyRight,
}

impl From<char> for DirectionalKey {
    fn from(ch: char) -> Self {
        match ch {
            '^' => DirectionalKey::KeyUp,
            'A' => DirectionalKey::KeyA,
            '<' => DirectionalKey::KeyLeft,
            'v' => DirectionalKey::KeyDown,
            '>' => DirectionalKey::KeyRight,
            other => {
                panic!("unexpected directional pad key '{other}'");
            }
        }
    }
}

impl Display for DirectionalKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            DirectionalKey::KeyUp => '^',
            DirectionalKey::KeyA => 'A',
            DirectionalKey::KeyLeft => '<',
            DirectionalKey::KeyDown => 'v',
            DirectionalKey::KeyRight => '>',
        })
    }
}

impl Key for DirectionalKey {
    fn position(&self) -> Position {
        let (x, y) = match self {
            DirectionalKey::KeyUp => (-1, 0),
            DirectionalKey::KeyA => (0, 0),
            DirectionalKey::KeyLeft => (-2, 1),
            DirectionalKey::KeyDown => (-1, 1),
            DirectionalKey::KeyRight => (0, 1),
        };
        Position { x, y }
    }
}

const DIRECTIONAL_PAD_DANGER_POS: Position = Position { x: -2, y: 0 };

#[derive(Debug, Clone, PartialEq, Eq)]
struct KeySeqResult {
    /// How many buttons have been pressed.
    pub len: u64,
    /// An optional recording of the buttons pressed (at the top level).
    pub recording: Option<Vec<DirectionalKey>>,
    /// Position of the robots, by depth.  Offset zero in
    /// robot_positions corresponds to the robot closest to the
    /// numeric keypad.
    pub robot_positions: Vec<DirectionalKey>,
}

impl KeySeqResult {
    fn concatenate_recordings(
        mut left: Option<Vec<DirectionalKey>>,
        mut right: Option<Vec<DirectionalKey>>,
    ) -> Option<Vec<DirectionalKey>> {
        if let Some(mut recording) = left.take() {
            if let Some(mut rr) = right.take() {
                recording.append(&mut rr);
                return Some(recording);
            }
        }
        None
    }
}

impl Add<KeySeqResult> for KeySeqResult {
    type Output = KeySeqResult;

    fn add(mut self, mut rhs: KeySeqResult) -> Self::Output {
        KeySeqResult {
            len: self.len + rhs.len,
            recording: Self::concatenate_recordings(self.recording.take(), rhs.recording.take()),
            robot_positions: rhs.robot_positions,
        }
    }
}

impl AddAssign<KeySeqResult> for KeySeqResult {
    fn add_assign(&mut self, mut rhs: KeySeqResult) {
        self.len += rhs.len;
        self.recording = Self::concatenate_recordings(self.recording.take(), rhs.recording.take());
        self.robot_positions = rhs.robot_positions;
    }
}

fn repeat(output: &mut Vec<DirectionalKey>, count: i64, key: DirectionalKey) {
    if count > 0 {
        let count = count as usize;
        let mut v = Vec::with_capacity(count);
        v.resize(count, key);
        output.append(&mut v);
    }
}
fn make_simple_path(from: &Position, to: &Position, danger: &Position) -> Vec<DirectionalKey> {
    let size: usize = ((to.x - from.x).unsigned_abs())
        .checked_add((to.y - from.y).unsigned_abs())
        .and_then(|n| n.checked_add(1)) // for A
        .unwrap_or(1)
        .try_into()
        .unwrap_or(1);
    let mut result: Vec<DirectionalKey> = Vec::with_capacity(size);

    repeat(&mut result, from.x - to.x, DirectionalKey::KeyLeft);
    repeat(&mut result, to.y - from.y, DirectionalKey::KeyDown);
    repeat(&mut result, from.y - to.y, DirectionalKey::KeyUp);
    repeat(&mut result, to.x - from.x, DirectionalKey::KeyRight);

    // In general, `path` is a journey with a right-angle
    // bend.  Because the danger spot is in the corner of
    // the keypad, we only need to check for overlap at
    // the bend location.
    if [
        Position { x: from.x, y: to.y },
        Position { x: to.x, y: from.y },
    ]
    .contains(danger)
    {
        result.reverse();
    }
    result.push(DirectionalKey::KeyA);
    result
}

fn create_graph<T: Key + Eq + Hash + Clone>(
    keylist: &[T],
    danger: &Position,
) -> HashMap<(T, T), Vec<DirectionalKey>> {
    let mut result: HashMap<(T, T), Vec<DirectionalKey>> = Default::default();
    for a in keylist {
        for b in keylist {
            let key = (a.clone(), b.clone());
            let apos = a.position();
            let bpos = b.position();
            let path: Vec<DirectionalKey> = make_simple_path(&apos, &bpos, danger);
            result.insert(key, path);
        }
    }
    result
}

fn create_numpad_graph() -> HashMap<(NumpadKey, NumpadKey), Vec<DirectionalKey>> {
    use NumpadKey::*;
    create_graph(
        &[
            Key0, KeyA, Key1, Key2, Key3, Key4, Key5, Key6, Key7, Key8, Key9,
        ],
        &NUMPAD_DANGER_POS,
    )
}

fn create_directionalpad_graph() -> HashMap<(DirectionalKey, DirectionalKey), Vec<DirectionalKey>> {
    use DirectionalKey::*;
    create_graph(
        &[KeyA, KeyUp, KeyLeft, KeyDown, KeyRight],
        &DIRECTIONAL_PAD_DANGER_POS,
    )
}

fn get_numpad_length(
    sequence: &[NumpadKey],
    ngraph: &HashMap<(NumpadKey, NumpadKey), Vec<DirectionalKey>>,
    dgraph: &HashMap<(DirectionalKey, DirectionalKey), Vec<DirectionalKey>>,
    iterations: usize,
    memo: &mut HashMap<(usize, Vec<DirectionalKey>), u64>,
) -> u64 {
    if iterations == 0 {
        sequence.len() as u64
    } else {
        sequence
            .iter()
            .fold(
                (0, NumpadKey::KeyA),
                |(tot, prev): (u64, NumpadKey), key: &NumpadKey| -> (u64, NumpadKey) {
                    if let Some(path) = ngraph.get(&(prev, *key)) {
                        let len = get_dirpad_length(path, dgraph, iterations - 1, memo);
                        (tot + len, *key)
                    } else {
                        unreachable!()
                    }
                },
            )
            .0
    }
}

fn get_dirpad_length(
    sequence: &[DirectionalKey],
    dgraph: &HashMap<(DirectionalKey, DirectionalKey), Vec<DirectionalKey>>,
    iterations: usize,
    memo: &mut HashMap<(usize, Vec<DirectionalKey>), u64>,
) -> u64 {
    if iterations == 0 {
        return sequence.len() as u64;
    }
    let memo_key: (usize, Vec<DirectionalKey>) = (iterations, sequence.to_vec());
    if let Some(cached) = memo.get(&memo_key) {
        return *cached;
    }
    let total_length = sequence
        .iter()
        .fold((0, DirectionalKey::KeyA), |(tot, prev), key| {
            if let Some(path) = dgraph.get(&(prev, *key)) {
                let len = get_dirpad_length(path, dgraph, iterations - 1, memo);
                (tot + len, *key)
            } else {
                unreachable!()
            }
        })
        .0;
    memo.insert(memo_key, total_length);
    total_length
}

fn count_presses(codes: &[&str], iterations: usize) -> Vec<u64> {
    let numpad_graph = create_numpad_graph();
    let dirpad_graph = create_directionalpad_graph();

    let mut memo = Default::default();
    codes
        .iter()
        .map(|line| {
            line.chars()
                .map(NumpadKey::from)
                .collect::<Vec<NumpadKey>>()
        })
        .map(|code| get_numpad_length(&code, &numpad_graph, &dirpad_graph, iterations, &mut memo))
        .collect()
}

#[test]
fn test_count_presses_029a() {
    assert_eq!(count_presses(&["029A"], 0), vec![4]);
}

#[test]
fn test_count_presses_02() {
    assert_eq!(count_presses(&["02"], 0), vec![2]);
    assert_eq!(count_presses(&["02"], 1), vec![4]);
}

fn solve(codes: &[&str], iterations: usize) -> u64 {
    fn complexity(presses: u64, code: &str) -> u64 {
        let num: u64 = match code.strip_suffix('A') {
            Some(prefix) => parse_number(prefix),
            None => {
                panic!("expected 'A' suffix on '{code}'");
            }
        };
        num * presses
    }

    let presses: Vec<u64> = count_presses(codes, iterations);
    presses
        .iter()
        .zip(codes)
        .map(|(presses, code)| complexity(*presses, code))
        .sum()
}

#[test]
fn test_solve() {
    // We use the test cases from part 1.
    #[derive(Debug)]
    struct TestInput {
        code: &'static str,
        iterations: usize,
        expected_presses: &'static str,
    }
    const ITERS: usize = 3;
    let test_inputs: [TestInput; 8] = [
        TestInput {
            code: "029A",
            iterations: 0,
            expected_presses: "029A",
        },
        TestInput {
            code: "029A",
            iterations: 1,
            expected_presses: "<A^A>^^AvvvA",
        },
        TestInput {
            code: "029A",
            iterations: 2,
            expected_presses: "v<<A>>^A<A>AvA<^AA>A<vAAA>^A",
        },
        TestInput {
            code: "029A",
            iterations: ITERS,
            expected_presses:
                "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A",
        },
        TestInput {
            code: "980A",
            iterations: ITERS,
            expected_presses: "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A",
        },
        TestInput {
            code: "179A",
            iterations: ITERS,
            expected_presses:
                "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A",
        },
        TestInput {
            code: "456A",
            iterations: ITERS,
            expected_presses: "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A",
        },
        TestInput {
            code: "379A",
            iterations: ITERS,
            expected_presses: "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A",
        },
    ];

    for (i, t) in test_inputs.iter().enumerate() {
        dbg!(&t);
        let codes = vec![t.code];
        let presses0 = count_presses(&codes, 0);
        dbg!(&presses0);
        let presses_n = count_presses(&codes, t.iterations);
        dbg!(&presses_n);
        assert_eq!(t.code.len() as u64, presses0[0]);
        assert_eq!(
            t.expected_presses.len() as u64,
            presses_n[0],
            "test case {0} ({1}, {2} iterations) failed",
            i,
            t.code,
            t.iterations
        );
    }
}

mod part2 {}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let codes: Vec<&str> = input_str.lines().collect();
    println!("Day 21 part 1: {}", solve(&codes, 3));
    println!("Day 21 part 2: {}", solve(&codes, 26));
}
