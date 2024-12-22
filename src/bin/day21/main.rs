use std::fmt::{Display, Write};
use std::str;

use lib::grid::Position;
use lib::parse::parse_number;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
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

impl Display for NumpadKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
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
        })
    }
}

const NUMPAD_DANGER_POS: Position = Position { x: -2, y: 0 };

fn numeric_keypad_key_pos(k: NumpadKey) -> Position {
    let (x, y) = match k {
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

fn generate_moves(key: DirectionalKey, count: i64, result: &mut Vec<DirectionalKey>) {
    assert!(count > 0);
    result.resize(result.len() + count as usize, key);
}

fn move_right(pos: &mut Position, destpos: &Position, result: &mut Vec<DirectionalKey>) {
    let xdist = destpos.x - pos.x;
    if xdist > 0 {
        generate_moves(DirectionalKey::KeyRight, xdist, result);
        pos.x += xdist;
    }
}

fn move_left(pos: &mut Position, destpos: &Position, result: &mut Vec<DirectionalKey>) {
    let xdist = pos.x - destpos.x;
    if xdist > 0 {
        generate_moves(DirectionalKey::KeyLeft, xdist, result);
        pos.x -= xdist
    }
}

fn move_up(pos: &mut Position, destpos: &Position, result: &mut Vec<DirectionalKey>) {
    let ydist = pos.y - destpos.y;
    if ydist > 0 {
        generate_moves(DirectionalKey::KeyUp, ydist, result);
        pos.y -= ydist
    }
}

fn move_down(pos: &mut Position, destpos: &Position, result: &mut Vec<DirectionalKey>) {
    let ydist = destpos.y - pos.y;
    if ydist > 0 {
        generate_moves(DirectionalKey::KeyDown, ydist, result);
        pos.y += ydist
    }
}

fn generate_key_sequence(
    bot_pos: &Position,
    bot_destpos: &Position,
    bot_danger: &Position,
    result: &mut Vec<DirectionalKey>,
) {
    // Check the invariant still holds.
    assert_ne!(bot_destpos, bot_danger);
    // Check the interface contract
    assert_ne!(bot_pos, bot_danger);

    let column_changes: Vec<DirectionalKey> = {
        let mut v = Vec::new();
        let mut p = *bot_pos;
        move_right(&mut p, bot_destpos, &mut v);
        move_left(&mut p, bot_destpos, &mut v);
        v
    };
    let row_changes: Vec<DirectionalKey> = {
        let mut v = Vec::new();
        let mut p = *bot_pos;
        move_down(&mut p, bot_destpos, &mut v);
        move_up(&mut p, bot_destpos, &mut v);
        v
    };
    let mut changes: [Vec<DirectionalKey>; 2] =
        if bot_destpos.y == bot_danger.y && bot_pos.x == bot_danger.x {
            [column_changes, row_changes]
        } else if bot_destpos.x == bot_danger.x && bot_pos.y == bot_danger.y {
            [row_changes, column_changes]
        } else if column_changes.contains(&DirectionalKey::KeyLeft) {
            [column_changes, row_changes]
        } else {
            [row_changes, column_changes]
        };
    result.append(&mut changes[0]);
    result.append(&mut changes[1]);
    result.push(DirectionalKey::KeyA);
}

fn shortest_sequence_for_numeric_keypad<I>(keys: I) -> Vec<DirectionalKey>
where
    I: Iterator<Item = NumpadKey>,
{
    let mut bot_pos = numeric_keypad_key_pos(NumpadKey::KeyA);
    let mut result: Vec<DirectionalKey> = Vec::new();
    for numpad_key in keys {
        let bot_destpos = numeric_keypad_key_pos(numpad_key);
        let mut v = Vec::new();
        generate_key_sequence(&bot_pos, &bot_destpos, &NUMPAD_DANGER_POS, &mut v);
        bot_pos = bot_destpos;
        result.append(&mut v);
    }
    result
}

fn shortest_sequence_for_directional_keypad<I>(keys: I) -> Vec<DirectionalKey>
where
    I: Iterator<Item = DirectionalKey>,
{
    let a_key_pos = directional_keypad_key_pos(&DirectionalKey::KeyA);
    let mut pos = a_key_pos;
    let mut result: Vec<DirectionalKey> = Vec::new();
    for directional_key in keys {
        let destpos = directional_keypad_key_pos(&directional_key);
        generate_key_sequence(&pos, &destpos, &DIRECTIONAL_PAD_DANGER_POS, &mut result);
        pos = destpos;
    }
    assert_eq!(&pos, &a_key_pos);
    result
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum DirectionalKey {
    KeyUp,
    KeyA,
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

fn directional_keypad_key_pos(k: &DirectionalKey) -> Position {
    let (x, y) = match k {
        DirectionalKey::KeyUp => (-1, 0),
        DirectionalKey::KeyA => (0, 0),
        DirectionalKey::KeyLeft => (-2, 1),
        DirectionalKey::KeyDown => (-1, 1),
        DirectionalKey::KeyRight => (0, 1),
    };
    Position { x, y }
}

const DIRECTIONAL_PAD_DANGER_POS: Position = Position { x: -2, y: 0 };

#[cfg(test)]
fn render_directional_key_sequence<'a, I>(keys: I) -> String
where
    I: Iterator<Item = &'a DirectionalKey>,
{
    let mut result = String::new();
    for k in keys {
        write!(&mut result, "{k}").expect("writes to strings should succeed");
    }
    result
}

#[test]
fn test_shortest_sequence_for_numeric_keypad_029a_depth0() {
    let numkeys: Vec<NumpadKey> = "029A".chars().map(NumpadKey::from).collect();
    let got = shortest_sequence_for_numeric_keypad(numkeys.iter().copied());
    let s = render_directional_key_sequence(got.iter());
    assert_eq!(
        s.len(),
        "<A^A>^^AvvvA".len(),
        "incorrect depth=0 sequence for 029A: {s}"
    );
}

#[test]
fn test_shortest_sequence_for_numeric_keypad_029a_depth1() {
    let dirkeys: Vec<DirectionalKey> = "<A^A>^^AvvvA".chars().map(DirectionalKey::from).collect();
    let got = shortest_sequence_for_directional_keypad(dirkeys.iter().copied());
    let s = render_directional_key_sequence(got.iter());
    assert_eq!(
        s.len(),
        "v<<A>>^A<A>AvA<^AA>A<vAAA>^A".len(),
        "incorrect depth=1 sequence for 029A: {s}"
    );
}

#[test]
fn test_shortest_sequence_for_numeric_keypad_029a_depth2() {
    let dirkeys: Vec<DirectionalKey> = "v<<A>>^A<A>AvA<^AA>A<vAAA>^A"
        .chars()
        .map(DirectionalKey::from)
        .collect();
    let got = shortest_sequence_for_directional_keypad(dirkeys.iter().copied());
    let s = render_directional_key_sequence(got.iter());
    assert_eq!(
        s.len(),
        "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".len(),
        "incorrect depth=2 sequence for 029A: {s}"
    );
}

fn generate_full_sequence<I>(numkeys: I) -> Vec<DirectionalKey>
where
    I: Iterator<Item = NumpadKey>,
{
    // The keypad I am using
    shortest_sequence_for_directional_keypad(
        // The first directional keypad a robot is using.
        shortest_sequence_for_directional_keypad(
            // The second directional keypad a robot is using.
            shortest_sequence_for_numeric_keypad(
                // The numeric keypad (on a door) that a robot is using
                numkeys,
            )
            .into_iter(),
        )
        .into_iter(),
    )
}

#[test]
fn test_generate_full_sequence_029a() {
    let numkeys: Vec<NumpadKey> = "029A".chars().map(NumpadKey::from).collect();
    let seq = generate_full_sequence(numkeys.into_iter());
    let s = render_directional_key_sequence(seq.iter());
    assert_eq!(
        s.len(),
        "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A".len(),
        "incorrect full sequence for 029A: {s}"
    );
}

#[test]
fn test_generate_full_sequence_980a() {
    let numkeys: Vec<NumpadKey> = "980A".chars().map(NumpadKey::from).collect();
    let seq = generate_full_sequence(numkeys.into_iter());
    let s = render_directional_key_sequence(seq.iter());
    assert_eq!(
        s.len(),
        "<v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A".len(),
        "incorrect full sequence for 980A: {s}"
    );
}

#[test]
fn test_generate_full_sequence_179a() {
    let numkeys: Vec<NumpadKey> = "179A".chars().map(NumpadKey::from).collect();
    let seq = generate_full_sequence(numkeys.into_iter());
    let s = render_directional_key_sequence(seq.iter());
    assert_eq!(
        s.len(),
        "<v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A".len(),
        "incorrect full sequence for 179A: {s}"
    );
}

#[test]
fn test_generate_full_sequence_456a() {
    let numkeys: Vec<NumpadKey> = "456A".chars().map(NumpadKey::from).collect();
    let seq = generate_full_sequence(numkeys.into_iter());
    let s = render_directional_key_sequence(seq.iter());
    assert_eq!(
        s.len(),
        "<v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A".len(),
        "incorrect full sequence for 456A: {s}"
    );
}

#[test]
fn test_generate_full_sequence_379a() {
    let numkeys: Vec<NumpadKey> = "379A".chars().map(NumpadKey::from).collect();
    let seq = generate_full_sequence(numkeys.into_iter());
    let s = render_directional_key_sequence(seq.iter());
    assert_eq!(
        s.len(),
        "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A".len(),
        "incorrect full sequence for 379A: {s}"
    );
}

fn complexity_of_code(code: &str) -> u64 {
    let num: u64 = match code.strip_suffix('A') {
        Some(prefix) => parse_number(prefix),
        None => {
            panic!("expected 'A' suffix on '{code}'");
        }
    };

    let numkeys: Vec<NumpadKey> = code.chars().map(NumpadKey::from).collect();
    let seq = generate_full_sequence(numkeys.into_iter());
    let len = seq.len() as u64;
    len * num
}

#[test]
fn test_complexity_of_code_029a() {
    assert_eq!(complexity_of_code("029A"), 68 * 29,);
}

#[test]
fn test_complexity_of_code_980a() {
    assert_eq!(complexity_of_code("980A"), 60 * 980);
}

#[test]
fn test_complexity_of_code_179a() {
    assert_eq!(complexity_of_code("179A"), 68 * 179);
}

#[test]
fn test_complexity_of_code_456a() {
    assert_eq!(complexity_of_code("456A"), 64 * 456);
}

#[test]
fn test_complexity_of_code_379a() {
    assert_eq!(complexity_of_code("379A"), 64 * 379);
}

fn part1(input: &str) -> u64 {
    input.lines().map(complexity_of_code).sum()
}

#[test]
fn test_part1() {
    assert_eq!(
        part1(concat!(
            "029A\n", // please retain line break
            "980A\n", // please retain line break
            "179A\n", // please retain line break
            "456A\n", // please retain line break
            "379A\n",
        )),
        126384
    );
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    println!("Day 21 part 1: {}", part1(input_str));
}
