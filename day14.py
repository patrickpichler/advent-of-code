#!/usr/bin/env python3


from collections import defaultdict


def parse_input(path: str) -> tuple[str, dict[str, str]]:
    with open(path) as f:
        lines = f.read().splitlines()

        lookup: dict[str, str] = {}

        for line in lines[2:]:
            k, v = line.split(" -> ")

            lookup[k] = v

        return (lines[0], lookup)


def count_chars(template: str) -> defaultdict[str, int]:
    count: dict[str, int] = defaultdict(int)

    for t in template:
        count[t] += 1

    return count


def count_pairs(template: str) -> defaultdict[str, int]:
    count: dict[str, int] = defaultdict(int)

    for i in range(0, len(template) - 1):
        count[template[i] + template[i + 1]] += 1

    return count


def find_solution(path: str, steps: int):
    template, lookup = parse_input(path)
    char_count = count_chars(template)
    pairs_count = count_pairs(template)

    for i in range(0, steps):
        for pair, count in dict(pairs_count).items():
            pairs_count[pair] -= count

            new_char = lookup[pair]
            pairs_count[pair[0] + new_char] += count
            pairs_count[new_char + pair[1]] += count

            char_count[new_char] += count

    chars = sorted(char_count.items(), key=lambda x: x[1])
    print(chars[-1][1] - chars[0][1])


def solve_part_1():
    find_solution("inputs/day14.txt", 10)


def solve_part_2():
    find_solution("inputs/day14.txt", 40)


solve_part_1()
solve_part_2()
