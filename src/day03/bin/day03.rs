use itertools::Itertools;

use std::collections::HashSet;

fn main() {
    let lines = include_str!("../input.txt").lines();

    let part1: u32 = lines
        .clone()
        .map(get_shared_items)
        .flatten()
        .map(calculate_prio)
        .map(|s| s as u32)
        .sum();

    println!("{}", part1);

    let part2: u32 = lines
        .clone()
        .into_iter()
        .map(|line| line.bytes().collect::<HashSet<_>>())
        .tuples()
        .map(|(a, b, c)| {
            a.iter()
                .copied()
                .find(|i| b.contains(i) && c.contains(i))
                .unwrap()
        })
        .map(calculate_prio)
        .map(|s| s as u32)
        .sum();

    println!("{}", part2);
}

fn calculate_prio(item: u8) -> u8 {
    return if item < 91 {
        (item - 64) + 26
    } else {
        item - 96
    };
}

fn get_shared_items(line: &str) -> HashSet<u8> {
    let data = line.as_bytes();
    let (first, second) = data.split_at(data.len() / 2);

    let mut duplicate = HashSet::new();

    for item in second.iter() {
        for f in first.iter() {
            if *f == *item {
                duplicate.insert(*f);
            }
        }
    }

    return duplicate;
}
