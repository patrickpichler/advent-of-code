#!/usr/bin/env python3

from functools import cache


def parse_input() -> list[int]:
    with open("inputs/day6.txt") as f:
        return [int(n) for n in f.readline().strip().split(",")]


@cache
def calc_offsprings(days_left: int, days_total: int) -> int:
    if days_total <= days_left:
        return 0

    return (
        1
        + calc_offsprings(6, days_total - days_left - 1)
        + calc_offsprings(8, days_total - days_left - 1)
    )


def calc_fish(fish: list[int], days: int) -> int:
    sum = 0

    for f in fish:
        sum += calc_offsprings(f, days)

    return sum


def solve_part_1():
    fish = parse_input()
    print(calc_fish(fish, 80))


def solve_part_2():
    fish = parse_input()
    print(calc_fish(fish, 256))


solve_part_1()
solve_part_2()
