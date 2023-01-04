use itertools::Itertools;

fn main() {
    let lines = include_str!("../input.txt").lines();

    let part1: i32 = lines
        .clone()
        .into_iter()
        .group_by(|str| str.is_empty())
        .into_iter()
        .map(|(_, group)| group.filter_map(|s| s.parse::<i32>().ok()).sum())
        .sorted()
        .last()
        .unwrap();

    println!("{}", part1);

    let part2: i32 = lines
        .clone()
        .into_iter()
        .group_by(|str| str.is_empty())
        .into_iter()
        .map(|(_, group)| group.filter_map(|s| s.parse::<i32>().ok()).sum::<i32>())
        .sorted_by(|a, b| Ord::cmp(a, b).reverse())
        .take(3)
        .sum();

    println!("{}", part2);
}
