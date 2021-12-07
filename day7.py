#!/usr/bin/env python3


import functools


def parse_input(path: str) -> list[int]:
    with open(path) as f:
        return [int(n) for n in f.readline().strip().split(",")]


def calc_fuel_needed(crabs: list[int], pos: int) -> int:
    sum = 0

    for c in crabs:
        sum += abs(pos - c)

    return sum


def solve_part_1():
    input = parse_input("inputs/day7.txt")

    max_num = functools.reduce(max, input)
    min_num = functools.reduce(min, input)

    min_fuel = -1
    min_fuel_pos = -1

    for i in range(min_num, max_num + 1):
        fuel = calc_fuel_needed(input, i)

        if min_fuel < 0:
            min_fuel = fuel
            min_fuel_pos = i
        elif fuel < min_fuel:
            min_fuel = fuel
            min_fuel_pos = i

    print(min_fuel_pos, min_fuel)


def calc_sum(n: int) -> int:
    return int((n * (n + 1)) / 2)


def calc_fuel_needed_extended(crabs: list[int], pos: int) -> int:
    sum = 0

    for c in crabs:
        sum += calc_sum(abs(pos - c))

    return sum


def solve_part_2():
    input = parse_input("inputs/day7.txt")

    max_num = functools.reduce(max, input)
    min_num = functools.reduce(min, input)

    min_fuel = -1
    min_fuel_pos = -1

    for i in range(min_num, max_num + 1):
        fuel = calc_fuel_needed_extended(input, i)

        if min_fuel < 0:
            min_fuel = fuel
            min_fuel_pos = i
        elif fuel < min_fuel:
            min_fuel = fuel
            min_fuel_pos = i

    print(min_fuel_pos, min_fuel)


solve_part_1()
solve_part_2()
