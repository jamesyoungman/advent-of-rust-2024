use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fmt::{Display, Write};
use std::str;

use lib::grid::{BoundingBox, CompassDirection, Position};

const VERBOSE: bool = false;

#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Clone, Copy)]
enum Item {
    Wall,
    SmallCrate,
    BigCrateLeft,
    BigCrateRight,
}

impl Item {
    fn can_move(&self) -> bool {
        !matches!(self, Item::Wall)
    }
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            Item::Wall => '#',
            Item::SmallCrate => 'O',
            Item::BigCrateLeft => '[',
            Item::BigCrateRight => ']',
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
struct World {
    bbox: BoundingBox,
    map: HashMap<Position, Item>,
    robot_pos: Position,
}

impl From<&str> for World {
    fn from(value: &str) -> Self {
        let mut bbox: BoundingBox = BoundingBox::new(&Position { x: 0, y: 0 });
        let mut robot_pos: Option<Position> = None;
        let mut map = HashMap::new();
        for (y, line) in value.lines().enumerate() {
            for (x, ch) in line.chars().enumerate() {
                let here = Position {
                    y: y as i64,
                    x: x as i64,
                };
                bbox.update(&here);
                if let Some(item) = match ch {
                    '.' => None,
                    '#' => Some(Item::Wall),
                    '@' => {
                        if robot_pos.is_none() {
                            robot_pos = Some(here);
                        } else {
                            panic!("only one robot is supported, but they exist at both {robot_pos:?} and {here}");
                        }
                        None
                    }
                    'O' => Some(Item::SmallCrate),
                    // We support big crates on input just for unit
                    // test cases.
                    '[' => Some(Item::BigCrateLeft),
                    ']' => Some(Item::BigCrateRight),
                    other => {
                        panic!("unrecognised map item '{other}'");
                    }
                } {
                    map.insert(here, item);
                }
            }
        }

        World {
            robot_pos: robot_pos.expect("the map should contain a robot"),
            bbox,
            map,
        }
    }
}

impl Display for World {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in self.bbox.rows() {
            for x in self.bbox.columns() {
                let here = Position { x, y };
                if here == self.robot_pos {
                    f.write_char('@')?;
                } else {
                    match self.map.get(&here) {
                        Some(item) => {
                            write!(f, "{item}")?;
                        }
                        None => {
                            f.write_char('.')?;
                        }
                    }
                }
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

#[cfg(test)]
fn sample_input_large() -> &'static str {
    concat!(
        "##########\n",
        "#..O..O.O#\n",
        "#......O.#\n",
        "#.OO..O.O#\n",
        "#..O@..O.#\n",
        "#O#..O...#\n",
        "#O..O..O.#\n",
        "#.OO.O.OO#\n",
        "#....O...#\n",
        "##########\n",
        "\n",
        "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n",
        "vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n",
        "><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n",
        "<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n",
        "^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n",
        "^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n",
        ">^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n",
        "<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n",
        "^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n",
        "v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n",
    )
}

#[cfg(test)]
fn sample_input_small_part1() -> &'static str {
    concat!(
        "########\n",
        "#..O.O.#\n",
        "##@.O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "<^^>>>vv<v>>v<<\n",
    )
}

#[cfg(test)]
fn sample_input_part2() -> &'static str {
    concat!(
        "#######\n",
        "#...#.#\n",
        "#.....#\n",
        "#..OO@#\n",
        "#..O..#\n",
        "#.....#\n",
        "#######\n",
        "\n",
        "<vv<<^^<<^^\n",
        "\n",
    )
}

fn parse_instructions(instructions: &str) -> Vec<CompassDirection> {
    use CompassDirection::*;
    instructions
        .chars()
        .filter_map(|ch| match ch {
            '\n' => None,
            '>' => Some(East),
            '<' => Some(West),
            '^' => Some(North),
            'v' => Some(South),
            other => {
                panic!("unrecognised movement instruction '{other}'");
            }
        })
        .collect()
}

fn parse_input(input: &str) -> (World, Vec<CompassDirection>) {
    match input.split_once("\n\n") {
        Some((top, bottom)) => {
            let w = World::from(top);
            let instructions = parse_instructions(bottom);
            (w, instructions)
        }
        None => {
            panic!("expected two consecutive newlines");
        }
    }
}

#[test]
fn test_parse_input() {
    let (world, mut instructions) = parse_input(sample_input_large());

    assert_eq!(world.robot_pos, Position { y: 4, x: 4 });
    assert_eq!(world.map.get(&Position { x: 0, y: 0 }), Some(&Item::Wall));
    assert_eq!(world.map.get(&Position { x: 1, y: 1 }), None);
    assert_eq!(
        world.map.get(&Position { x: 3, y: 1 }),
        Some(&Item::SmallCrate)
    );

    assert_eq!(instructions.pop(), Some(CompassDirection::North));
    assert_eq!(instructions.pop(), Some(CompassDirection::West));
    assert_eq!(instructions.pop(), Some(CompassDirection::West));
    assert_eq!(instructions.pop(), Some(CompassDirection::North));
    assert_eq!(instructions.pop(), Some(CompassDirection::South));
}

#[test]
fn test_display_world() {
    let mut world_input_rep = sample_input_large()
        .split_once("\n\n")
        .expect("newlines")
        .0
        .to_string();
    world_input_rep.push('\n');

    let (world, _) = parse_input(sample_input_large());
    assert_eq!(world_input_rep, world.to_string());
}

#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd, Clone)]
struct Move {
    from: Position,
    direction: CompassDirection,
}

impl Move {
    fn to(&self) -> Position {
        self.from.move_direction(&self.direction)
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum CannotMove {
    Blocked(Position),
}

impl Display for CannotMove {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CannotMove::Blocked(at) => {
                write!(f, "cannot move to {at} because it is occupied")
            }
        }
    }
}

impl Error for CannotMove {}

fn merge_parallel_moves(mut v1: Vec<Move>, mut v2: Vec<Move>) -> Vec<Move> {
    let moves_in_v1: HashSet<Move> = v1.iter().cloned().collect();
    v2.retain(|m| !moves_in_v1.contains(m));
    v1.extend(v2);
    v1
}

impl World {
    fn scale_up(self) -> World {
        fn scale_pos(pos: &Position, left: bool) -> Position {
            Position {
                x: pos.x * 2 + if left { 0 } else { 1 },
                y: pos.y,
            }
        }
        fn scale_bbox(bbox: &BoundingBox) -> BoundingBox {
            BoundingBox {
                top_left: scale_pos(&bbox.top_left, true),
                bottom_right: scale_pos(&bbox.bottom_right, false),
            }
        }

        fn scale_up_map(mut input: HashMap<Position, Item>) -> HashMap<Position, Item> {
            let mut newmap = HashMap::new();
            for (pos, what) in input.drain() {
                match what {
                    Item::Wall => {
                        newmap.insert(
                            Position {
                                x: pos.x * 2,
                                ..pos
                            },
                            Item::Wall,
                        );
                        newmap.insert(
                            Position {
                                x: pos.x * 2 + 1,
                                ..pos
                            },
                            Item::Wall,
                        );
                    }
                    Item::SmallCrate => {
                        newmap.insert(
                            Position {
                                x: pos.x * 2,
                                ..pos
                            },
                            Item::BigCrateLeft,
                        );
                        newmap.insert(
                            Position {
                                x: pos.x * 2 + 1,
                                ..pos
                            },
                            Item::BigCrateRight,
                        );
                    }
                    Item::BigCrateLeft | Item::BigCrateRight => {
                        panic!("not allowed to scale up an already-scaled map");
                    }
                }
            }
            newmap
        }
        let new_bbox = scale_bbox(&self.bbox);
        let new_map = scale_up_map(self.map);
        for pos in new_map.keys() {
            assert!(
                new_bbox.contains(pos),
                "new bbox {new_bbox:?} should contain {pos:?}"
            );
        }
        World {
            robot_pos: scale_pos(&self.robot_pos, true),
            bbox: new_bbox,
            map: new_map,
        }
    }

    fn score(&self) -> i64 {
        self.bbox
            .rows()
            .flat_map(|y| {
                self.bbox.columns().filter_map(move |x| {
                    let here = Position { x, y };
                    match self.map.get(&here) {
                        Some(&Item::SmallCrate | &Item::BigCrateLeft) => Some(100 * y + x),
                        _ => None,
                    }
                })
            })
            .sum()
    }

    fn single_move(&mut self, this_move: &Move) {
        if this_move.from == self.robot_pos {
            panic!("should not use single_move to move the robot");
        }
        let moved_piece: Item = match self.map.get(&this_move.from) {
            None => {
                panic!("moving from empty position {0}", this_move.from);
            }
            Some(thing) => {
                assert!(thing.can_move());
                *thing
            }
        };
        if VERBOSE {
            println!("single_move: moving {moved_piece}: {this_move:?}");
        }
        if let Some(blocker) = self.map.get(&this_move.to()) {
            panic!(
                "move of {moved_piece} {this_move:?} is impossible as there is already a {blocker} at {0}",
                this_move.to()
            );
        }
        match moved_piece {
            Item::Wall => {
                unreachable!("walls cannot move");
            }
            Item::SmallCrate | Item::BigCrateLeft | Item::BigCrateRight => {
                self.map.remove(&this_move.from);
                self.map.insert(this_move.to(), moved_piece);
            }
        }
    }

    fn do_moves<I: Iterator<Item = Move>>(&mut self, moves: I) {
        for this_move in moves {
            self.single_move(&this_move);
        }
    }

    fn clear_square(
        &self,
        to_clear: Position,
        direction: CompassDirection,
    ) -> Result<Vec<Move>, CannotMove> {
        let final_move = Move {
            from: to_clear,
            direction,
        };
        let mut moves: Vec<Move> = match self.map.get(&to_clear) {
            None => {
                // There is nothing in the way.
                if VERBOSE {
                    println!("there is nothing in the way at {to_clear}");
                }
                return Ok(Vec::new());
            }
            Some(obstruction) => {
                if VERBOSE {
                    println!("to clear {to_clear} we need to move a {obstruction} {direction} from {to_clear}",);
                }
                match obstruction {
                    Item::Wall => {
                        if VERBOSE {
                            println!("not possible: there is a wall in the way at {to_clear}");
                        }
                        return Err(CannotMove::Blocked(to_clear));
                    }
                    Item::SmallCrate => {
                        self.clear_square(to_clear.move_direction(&direction), direction)
                    }
                    Item::BigCrateRight | Item::BigCrateLeft => {
                        if matches!(direction, CompassDirection::East | CompassDirection::West) {
                            self.clear_square(to_clear.move_direction(&direction), direction)
                        } else {
                            // North or South move; we have to keep
                            // the two halves of the crate together.
                            let other_pos = match obstruction {
                                Item::BigCrateLeft => {
                                    to_clear.move_direction(&CompassDirection::East)
                                }
                                Item::BigCrateRight => {
                                    to_clear.move_direction(&CompassDirection::West)
                                }
                                _ => unreachable!(),
                            };
                            if VERBOSE {
                                println!("because this is part of a crate we also need to move the other half from {other_pos}");
                            }
                            match self.clear_square(to_clear.move_direction(&direction), direction)
                            {
                                Ok(moves1) => {
                                    if VERBOSE {
                                        println!("to clear the dest for this half: {moves1:?}");
                                    }
                                    match self.clear_square(
                                        other_pos.move_direction(&direction),
                                        direction,
                                    ) {
                                        Ok(mut moves2) => {
                                            if VERBOSE {
                                                println!(
                                                    "to clear the dest for the other half: {moves2:?}"
						);
                                            }
                                            // We need to perform both
                                            // `moves1` and `moves2`
                                            // in order to clear the
                                            // way for the current
                                            // move.  But those
                                            // vectors have a move in
                                            // commin in cases like
                                            // this:
                                            //
                                            //  []
                                            // [][]
                                            //   @
                                            //
                                            // Robot moving North.
                                            let other_half_move = Move {
                                                from: other_pos,
                                                direction,
                                            };
                                            if VERBOSE {
                                                println!("plus the final move of the other half of the crate: {other_half_move:?}");
                                            }
                                            moves2.push(other_half_move);
                                            Ok(merge_parallel_moves(moves1, moves2))
                                        }
                                        Err(e) => Err(e),
                                    }
                                }
                                Err(e) => Err(e),
                            }
                        }
                    }
                }
            }
        }?;
        moves.push(final_move);
        if VERBOSE {
            println!("we can clear {to_clear} with moves {moves:?}");
        }
        Ok(moves)
    }

    pub fn move_robot(&mut self, instruction: CompassDirection) -> bool {
        if VERBOSE {
            println!("moving the robot {instruction} from {0}", self.robot_pos);
        }
        let robot_pos: Position = self.robot_pos;
        let dest = robot_pos.move_direction(&instruction);
        match self.clear_square(dest, instruction) {
            Ok(moves) => {
                if VERBOSE {
                    println!(
                        "move_robot: performing moves {moves:?} so we can move the robot to {dest}"
                    );
                }
                self.do_moves(moves.iter().cloned());
                if self.map.contains_key(&dest) {
                    panic!("move sequence {moves:?} failed to clear robot's destination {dest}");
                }
                self.robot_pos = dest;
                true
            }
            Err(CannotMove::Blocked(_)) => false,
        }
    }
}

#[test]
fn test_clear_the_way_for_move() {
    let mut world = World::from(concat!("..@..\n",));
    assert!(world.move_robot(CompassDirection::East));

    let mut world = World::from(concat!(".O@..\n",));
    assert!(world.move_robot(CompassDirection::West));
}

#[cfg(test)]
fn direction_char(dir: &CompassDirection) -> char {
    use CompassDirection::*;
    match dir {
        East => '>',
        West => '<',
        North => '^',
        South => 'v',
    }
}

fn perform_moves<F>(world: &mut World, moves: &[CompassDirection], mut inspector: F)
where
    F: FnMut(usize, &CompassDirection, &World),
{
    for (i, m) in moves.iter().enumerate() {
        world.move_robot(*m);
        inspector(i, m, world);
    }
}

#[cfg(test)]
fn write_state<W: Write>(mut writer: W, _seq: usize, m: &CompassDirection, world: &World) {
    writeln!(writer, "Move {}:\n{world}", direction_char(m))
        .expect("String writes should not fail");
}

#[cfg(test)]
fn run_moves_returning_commentary(world: &mut World, moves: &[CompassDirection]) -> String {
    let mut commentary = String::new();
    writeln!(&mut commentary, "Initial state:\n{world}").expect("String writes should not fail");
    perform_moves(world, moves, |seq, dir, world| {
        write_state(&mut commentary, seq, dir, world);
    });
    commentary
}

fn run_moves_without_commentary(world: &mut World, moves: &[CompassDirection]) {
    fn do_nothing(_seq: usize, _dir: &CompassDirection, _w: &World) {}
    perform_moves(world, moves, do_nothing);
}

#[cfg(test)]
fn assert_long_strings_are_identical(label: &str, expected: &str, got: &str) {
    for (line_num, expected, got) in expected
        .lines()
        .enumerate()
        .zip(got.lines())
        .map(|((n, e), g)| (n, e, g))
    {
        if expected != got {
            let msg = format!(
                "{label}: mismatch at line {}:\nexpected: {expected}\ngot     : {got}",
                line_num + 1
            );
            eprintln!("{msg}");
            panic!("{msg}");
        }
    }
}

#[test]
fn test_run_small_example_part1() {
    let (mut world, instructions) = parse_input(sample_input_small_part1());
    let commentary_got = run_moves_returning_commentary(&mut world, &instructions);
    let commentary_expected = concat!(
        "Initial state:\n",
        "########\n",
        "#..O.O.#\n",
        "##@.O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move <:\n",
        "########\n",
        "#..O.O.#\n",
        "##@.O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move ^:\n",
        "########\n",
        "#.@O.O.#\n",
        "##..O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move ^:\n",
        "########\n",
        "#.@O.O.#\n",
        "##..O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move >:\n",
        "########\n",
        "#..@OO.#\n",
        "##..O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move >:\n",
        "########\n",
        "#...@OO#\n",
        "##..O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move >:\n",
        "########\n",
        "#...@OO#\n",
        "##..O..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#......#\n",
        "########\n",
        "\n",
        "Move v:\n",
        "########\n",
        "#....OO#\n",
        "##..@..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move v:\n",
        "########\n",
        "#....OO#\n",
        "##..@..#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move <:\n",
        "########\n",
        "#....OO#\n",
        "##.@...#\n",
        "#...O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move v:\n",
        "########\n",
        "#....OO#\n",
        "##.....#\n",
        "#..@O..#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move >:\n",
        "########\n",
        "#....OO#\n",
        "##.....#\n",
        "#...@O.#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move >:\n",
        "########\n",
        "#....OO#\n",
        "##.....#\n",
        "#....@O#\n",
        "#.#.O..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move v:\n",
        "########\n",
        "#....OO#\n",
        "##.....#\n",
        "#.....O#\n",
        "#.#.O@.#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move <:\n",
        "########\n",
        "#....OO#\n",
        "##.....#\n",
        "#.....O#\n",
        "#.#O@..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
        "Move <:\n",
        "########\n",
        "#....OO#\n",
        "##.....#\n",
        "#.....O#\n",
        "#.#O@..#\n",
        "#...O..#\n",
        "#...O..#\n",
        "########\n",
        "\n",
    );

    println!("{commentary_got}");
    assert_long_strings_are_identical(
        "moves in test_run_small_example_part1",
        &commentary_expected,
        &commentary_got,
    );
    assert_eq!(commentary_got, commentary_expected);
}

#[test]
fn test_run_large_example() {
    let (mut world, instructions) = parse_input(sample_input_large());
    run_moves_without_commentary(&mut world, &instructions);
    assert_eq!(
        &world.to_string(),
        concat!(
            "##########\n",
            "#.O.O.OOO#\n",
            "#........#\n",
            "#OO......#\n",
            "#OO@.....#\n",
            "#O#.....O#\n",
            "#O.....OO#\n",
            "#O.....OO#\n",
            "#OO....OO#\n",
            "##########\n",
        )
    );
}

#[test]
fn test_score_tiny() {
    let w = World::from(concat!(
        "#######\n", // do not join lines
        "#...O..\n", // do not join lines
        "#.....@\n",
    )); // added robot to make map valid
    println!("{w}");
    assert_eq!(w.score(), 104);
}

fn part1(world: &World, instructions: &[CompassDirection]) -> i64 {
    let mut world = world.clone();
    run_moves_without_commentary(&mut world, instructions);
    world.score()
}

#[test]
fn test_part1_small_example() {
    let (mut world, instructions) = parse_input(sample_input_small_part1());
    assert_eq!(part1(&mut world, &instructions), 2028);
}

#[test]
fn test_part1_large_example() {
    let (mut world, instructions) = parse_input(sample_input_large());
    assert_eq!(part1(&mut world, &instructions), 10092);
}

#[test]
fn test_scale_up() {
    let (world, _) = parse_input(sample_input_large());
    let big_world = world.scale_up();
    let expected = concat!(
        "####################\n",
        "##....[]....[]..[]##\n",
        "##............[]..##\n",
        "##..[][]....[]..[]##\n",
        "##....[]@.....[]..##\n",
        "##[]##....[]......##\n",
        "##[]....[]....[]..##\n",
        "##..[][]..[]..[][]##\n",
        "##........[]......##\n",
        "####################\n",
    );
    let got = big_world.to_string();
    println!("expected scaled-up world:\n{expected}");
    println!("     got scaled-up world:\n{got}");
    assert_long_strings_are_identical("scaled up large example", &expected, &got);
    assert_eq!(expected, got);
}

#[test]
fn test_run_small_example_part2() {
    let (world, instructions) = parse_input(sample_input_part2());
    let mut world = world.scale_up();
    let commentary_got = run_moves_returning_commentary(&mut world, &instructions);
    let commentary_expected = concat!(
        "Initial state:\n",
        "##############\n",
        "##......##..##\n",
        "##..........##\n",
        "##....[][]@.##\n",
        "##....[]....##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move <:\n",
        "##############\n",
        "##......##..##\n",
        "##..........##\n",
        "##...[][]@..##\n",
        "##....[]....##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move v:\n",
        "##############\n",
        "##......##..##\n",
        "##..........##\n",
        "##...[][]...##\n",
        "##....[].@..##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move v:\n",
        "##############\n",
        "##......##..##\n",
        "##..........##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##.......@..##\n",
        "##############\n",
        "\n",
        "Move <:\n",
        "##############\n",
        "##......##..##\n",
        "##..........##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##......@...##\n",
        "##############\n",
        "\n",
        "Move <:\n",
        "##############\n",
        "##......##..##\n",
        "##..........##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##.....@....##\n",
        "##############\n",
        "\n",
        "Move ^:\n",
        "##############\n",
        "##......##..##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##.....@....##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move ^:\n",
        "##############\n",
        "##......##..##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##.....@....##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move <:\n",
        "##############\n",
        "##......##..##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##....@.....##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move <:\n",
        "##############\n",
        "##......##..##\n",
        "##...[][]...##\n",
        "##....[]....##\n",
        "##...@......##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move ^:\n",
        "##############\n",
        "##......##..##\n",
        "##...[][]...##\n",
        "##...@[]....##\n",
        "##..........##\n",
        "##..........##\n",
        "##############\n",
        "\n",
        "Move ^:\n",
        "##############\n",
        "##...[].##..##\n",
        "##...@.[]...##\n",
        "##....[]....##\n",
        "##..........##\n",
        "##..........##\n",
        "##############\n",
        "\n",
    );

    println!("{commentary_got}");
    assert_long_strings_are_identical(
        "moves in test_run_small_example_part2",
        &commentary_expected,
        &commentary_got,
    );
    assert_eq!(commentary_got, commentary_expected);
}

#[test]
fn test_score_part2() {
    let input = concat!(
        "####################\n",
        "##[].......[].[][]##\n",
        "##[]...........[].##\n",
        "##[]........[][][]##\n",
        "##[]......[]....[]##\n",
        "##..##......[]....##\n",
        "##..[]............##\n",
        "##..@......[].[][]##\n",
        "##......[][]..[]..##\n",
        "####################\n",
    );
    let world = World::from(input);
    assert_eq!(world.score(), 9021);
}

fn part2(world: &World, instructions: &[CompassDirection]) -> i64 {
    let mut scaled: World = world.clone().scale_up();
    run_moves_without_commentary(&mut scaled, instructions);
    scaled.score()
}

#[test]
fn test_part2() {
    let (world, instructions) = parse_input(sample_input_large());
    assert_eq!(part2(&world, &instructions), 9021);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let (world, instructions) = parse_input(input_str);
    println!("day 15 part 1: {}", part1(&world, &instructions));
    println!("day 15 part 2: {}", part2(&world, &instructions));
}
