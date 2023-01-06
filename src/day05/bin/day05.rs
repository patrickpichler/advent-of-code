use std::cell::RefCell;

use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{all_consuming, map},
    multi::separated_list1,
    sequence::{delimited, tuple},
    Finish, IResult,
};

#[repr(transparent)]
#[derive(Debug, Clone, Copy)]
struct Crate(char);

fn parse_crate(input: &str) -> IResult<&str, Crate> {
    let first_char = |s: &str| Crate(s.chars().next().unwrap());
    let f = delimited(tag("["), take(1_usize), tag("]"));
    map(f, first_char)(input)
}

fn parse_hole(input: &str) -> IResult<&str, ()> {
    map(tag("   "), drop)(input)
}

fn parse_crate_or_hole(input: &str) -> IResult<&str, Option<Crate>> {
    alt((map(parse_crate, Some), map(parse_hole, |_| None)))(input)
}

fn parse_crate_line(input: &str) -> IResult<&str, Vec<Option<Crate>>> {
    separated_list1(tag(" "), parse_crate_or_hole)(input)
}

#[derive(Debug, Clone, Copy)]
struct Move {
    amount: usize,
    from: usize,
    to: usize,
}

fn parse_digit(input: &str) -> IResult<&str, usize> {
    map(nom::character::complete::u32, |n| n as _)(input)
}

fn parse_move_line(input: &str) -> IResult<&str, Move> {
    let (input, (_, amount, _, from, _, to)) = tuple((
        tag("move "),
        parse_digit,
        tag(" from "),
        parse_digit,
        tag(" to "),
        parse_digit,
    ))(input)?;

    // adjust index by -1 since arrays start at 0
    Ok((
        input,
        Move {
            amount,
            from: from - 1,
            to: to - 1,
        },
    ))
}

fn transpose<T>(v: Vec<Vec<Option<T>>>) -> Vec<Vec<T>> {
    assert!(!v.is_empty());
    let len = v[0].len();
    let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();

    (0..len)
        .map(|_| {
            iters
                .iter_mut()
                .rev()
                .filter_map(|n| n.next().unwrap())
                .collect::<Vec<T>>()
        })
        .collect()
}

type Pile = Vec<RefCell<Vec<Crate>>>;

#[repr(transparent)]
struct CrateMover9000(Pile);

impl CrateMover9000 {
    fn apply_moves(&mut self, moves: &Vec<Move>) {
        for m in moves {
            for _ in 0..m.amount {
                let c = self.0[m.from].borrow_mut().pop().unwrap();
                self.0[m.to].borrow_mut().push(c);
            }
        }
    }
}

struct CrateMover9001(Pile);

impl CrateMover9001 {
    fn apply_moves(&mut self, moves: Vec<Move>) {
        for m in moves {
            let from_len = self.0[m.from].borrow().len();

            self.0[m.to]
                .borrow_mut()
                .extend(self.0[m.from].borrow_mut().drain((from_len - m.amount)..));
        }
    }
}

fn read_result(p: Pile) -> String {
    p.into_iter()
        .map(|p| p.borrow().last().unwrap().clone())
        .map(|c| c.0)
        .collect()
}

fn main() {
    let mut lines = include_str!("../input.txt").lines();
    let crate_lines = lines
        .by_ref()
        .map_while(|line| {
            all_consuming(parse_crate_line)(line)
                .finish()
                .ok()
                .map(|(_, line)| line)
        })
        .collect();

    let moves = lines
        .by_ref()
        .filter_map(|line| {
            all_consuming(parse_move_line)(line)
                .finish()
                .ok()
                .map(|(_, m)| m)
        })
        .collect();

    let stacks: Pile = transpose(crate_lines)
        .into_iter()
        .map(RefCell::new)
        .collect();

    let mut crate_mover_9000 = CrateMover9000(stacks.clone());
    crate_mover_9000.apply_moves(&moves);

    println!("{}", read_result(crate_mover_9000.0));

    let mut crate_mover_9001 = CrateMover9001(stacks.clone());
    crate_mover_9001.apply_moves(moves);

    println!("{}", read_result(crate_mover_9001.0));
}
