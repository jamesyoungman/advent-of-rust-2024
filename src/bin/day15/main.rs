use std::collections::HashMap;
use std::fmt::{Display, Write};
use std::str;

use lib::grid::{BoundingBox, CompassDirection, Position};

#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum Item {
    Wall,
    Crate,
}

#[derive(Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
enum MovableItem {
    Item(Item),
    Robot,
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(match self {
            Item::Wall => '#',
            Item::Crate => 'O',
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
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
                    'O' => Some(Item::Crate),
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
fn sample_input_small() -> &'static str {
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
    assert_eq!(world.map.get(&Position { x: 3, y: 1 }), Some(&Item::Crate));

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

impl World {
    fn score(&self) -> i64 {
        self.bbox
            .rows()
            .flat_map(|y| {
                self.bbox.columns().filter_map(move |x| {
                    let here = Position { x, y };
                    if let Some(&Item::Crate) = self.map.get(&here) {
                        Some(100 * y + x)
                    } else {
                        None
                    }
                })
            })
            .sum()
    }

    fn do_move(&mut self, from: &Position, to: Position, what: MovableItem) {
        match what {
            MovableItem::Robot => {
                assert_eq!(&self.robot_pos, from);
                assert!(!self.map.contains_key(&to));
                self.robot_pos = to;
            }
            MovableItem::Item(Item::Wall) => {
                unreachable!("walls cannot move");
            }
            MovableItem::Item(Item::Crate) => {
                if let Some(moving) = self.map.remove(from) {
                    assert_eq!(moving, Item::Crate);
                }
                self.map.insert(to, Item::Crate);
            }
        }
    }

    fn try_to_move_item(
        &mut self,
        from: &Position,
        direction: CompassDirection,
        what: MovableItem,
    ) -> bool {
        let to: Position = from.move_direction(&direction);
        match self.map.get(&to) {
            Some(Item::Wall) => false, // walls cannot be moved
            None => {
                // There is nothing in the way.
                self.do_move(from, to, what);
                true
            }
            Some(Item::Crate) => {
                // If we can move the crate out of the way...
                if self.try_to_move_item(&to, direction, MovableItem::Item(Item::Crate)) {
                    // .. then we can move this item into the now-empty spot.
                    self.do_move(from, to, what);
                    true
                } else {
                    // The crate cannot move because something is blocking it.
                    false
                }
            }
        }
    }

    pub fn move_robot(&mut self, instruction: CompassDirection) -> bool {
        let robot_pos: Position = self.robot_pos;
        self.try_to_move_item(&robot_pos, instruction, MovableItem::Robot)
    }
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

#[test]
fn test_run_small_example() {
    let (mut world, instructions) = parse_input(sample_input_small());
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
    for (line_num, expected, got) in commentary_expected
        .lines()
        .enumerate()
        .zip(commentary_got.lines())
        .map(|((n, e), g)| (n, e, g))
    {
        if expected != got {
            panic!(
                "mismatch at line {}:expected: {expected}\ngot     : {got}",
                line_num + 1
            );
        }
    }
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
    dbg!(&w.bbox);
    assert_eq!(w.score(), 104);
}

fn part1(world: &mut World, instructions: &[CompassDirection]) -> i64 {
    run_moves_without_commentary(world, instructions);
    world.score()
}

#[test]
fn test_part1_small_example() {
    let (mut world, instructions) = parse_input(sample_input_small());
    assert_eq!(part1(&mut world, &instructions), 2028);
}

#[test]
fn test_part1_large_example() {
    let (mut world, instructions) = parse_input(sample_input_large());
    assert_eq!(part1(&mut world, &instructions), 10092);
}

fn main() {
    let input_str = str::from_utf8(include_bytes!("input.txt")).unwrap();
    let (mut world, instructions) = parse_input(input_str);
    println!("day 15 part 1: {}", part1(&mut world, &instructions));
}
