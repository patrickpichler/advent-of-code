#!/usr/bin/env python3

from collections import defaultdict
from dataclasses import dataclass


@dataclass
class Fish:
    num_fish: int
    days_left: int


def parse_input() -> list[Fish]:
    count: dict[int, int] = defaultdict(int)

    with open("inputs/day6.txt") as f:
        for n in f.readline().strip().split(","):
            count[int(n)] += 1

    return [Fish(num, days) for days, num in count.items()]


def merge_fish(fish: list[Fish]) -> list[Fish]:
    map: dict[int, Fish] = {}

    for f in fish:
        if f.days_left in map:
            map[f.days_left].num_fish += f.num_fish
        else:
            map[f.days_left] = f

    return [f for _, f in map.items()]


def calculate_fish(fish: list[Fish], days: int):
    for d in range(0, days):
        fish_to_add = 0

        for f in fish:
            if f.days_left == 0:
                fish_to_add += f.num_fish
                f.days_left = 6
            else:
                f.days_left -= 1

        if fish_to_add > 0:
            fish.append(Fish(fish_to_add, 8))

        fish = merge_fish(fish)

    sum = 0
    for f in fish:
        sum += f.num_fish

    print(sum)


def solve_part_1():
    fish = parse_input()

    calculate_fish(fish, 80)


def solve_part_2():
    fish = parse_input()

    calculate_fish(fish, 256)


solve_part_1()
solve_part_2()
