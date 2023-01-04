#[derive(PartialEq, Clone, Copy)]
enum Shape {
    Rock,
    Paper,
    Scissor,
}

impl Shape {
    fn value(&self) -> u8 {
        match *self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissor => 3,
        }
    }
}

impl From<u8> for Shape {
    fn from(num: u8) -> Self {
        return match num {
            1 => Shape::Rock,
            2 => Shape::Paper,
            3 => Shape::Scissor,
            _ => panic!("oops"),
        };
    }
}

fn main() {
    let lines = include_str!("../input.txt").lines();

    let part1: u32 = lines
        .clone()
        .map(|l| {
            let mut chars = l.chars();
            let first = chars.next().unwrap();
            chars.next();
            let last = chars.next().unwrap();

            let other = match first {
                'A' => Shape::Rock,
                'B' => Shape::Paper,
                'C' => Shape::Scissor,
                _ => panic!("oops"),
            };

            let me = match last {
                'X' => Shape::Rock,
                'Y' => Shape::Paper,
                'Z' => Shape::Scissor,
                _ => panic!("oops"),
            };

            return (other, me);
        })
        .map(|(other, me)| -> u8 {
            if other == me {
                return 3 + me.value();
            }

            return match (other, me) {
                (Shape::Scissor, Shape::Rock)
                | (Shape::Paper, Shape::Scissor)
                | (Shape::Rock, Shape::Paper) => 6 + me.value(),
                _ => me.value(),
            };
        })
        .map(|x| x as u32)
        .sum();

    println!("{}", part1);

    let part2: u32 = lines
        .clone()
        .map(|l| {
            let mut chars = l.chars();
            let first = chars.next().unwrap();
            chars.next();
            let last = chars.next().unwrap();

            let other = match first {
                'A' => Shape::Rock,
                'B' => Shape::Paper,
                'C' => Shape::Scissor,
                _ => panic!("oops"),
            };

            return match last {
                'X' => match other {
                    Shape::Rock => Shape::Scissor.value(),
                    Shape::Paper => Shape::Rock.value(),
                    Shape::Scissor => Shape::Paper.value(),
                },
                'Y' => other.value() + 3,
                _ => match other {
                    Shape::Rock => Shape::Paper.value() + 6,
                    Shape::Paper => Shape::Scissor.value() + 6,
                    Shape::Scissor => Shape::Rock.value() + 6,
                },
            };
        })
        .map(|x| x as u32)
        .sum();

    println!("{}", part2)
}
