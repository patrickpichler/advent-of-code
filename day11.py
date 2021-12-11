#!/usr/bin/env python3

from collections import namedtuple
from typing import Generator
import itertools


Map = list[list[int]]
Point = namedtuple("Point", ["x", "y"])


def parse_input(path: str) -> Map:
    result: list[list[int]] = []

    with open(path) as f:
        for line in f.read().splitlines():
            result.append([int(o) for o in line])

    return result


def print_map(map: Map):
    for y in range(0, len(map)):
        for x in range(0, len(map[y])):
            print(f"{map[y][x]}", " ", end="")

        print()


def find_neighbours(map: Map, x: int, y: int) -> Generator[Point, None, None]:
    if y > 0:
        if x > 0:
            yield Point(x - 1, y - 1)

        yield Point(x, y - 1)

        if x < len(map[y]) - 1:
            yield Point(x + 1, y - 1)

    if x > 0:
        yield Point(x - 1, y)

    if x < len(map[y]) - 1:
        yield Point(x + 1, y)

    if y < len(map) - 1:
        if x > 0:
            yield Point(x - 1, y + 1)

        yield Point(x, y + 1)

        if x < len(map[y]) - 1:
            yield Point(x + 1, y + 1)


def solve_part_1():
    input = parse_input("inputs/day11.txt")
    days = 100
    flashes = 0

    for _ in range(0, days):
        already_flashed: set[Point] = set()

        for y in range(0, len(input)):
            for x in range(0, len(input[y])):
                if Point(x, y) in already_flashed:
                    continue

                input[y][x] += 1

                if input[y][x] > 9:
                    flashes += 1
                    already_flashed.add(Point(x, y))
                    input[y][x] = 0

                    to_visit = list(find_neighbours(input, x, y))

                    while len(to_visit) > 0:
                        p = to_visit.pop()

                        if p in already_flashed:
                            continue

                        input[p.y][p.x] += 1

                        if input[p.y][p.x] > 9:
                            flashes += 1
                            input[p.y][p.x] = 0
                            already_flashed.add(p)
                            to_visit.extend(find_neighbours(input, p.x, p.y))

    print(flashes)


def solve_part_2():
    input = parse_input("inputs/day11.txt")
    num_octopus = len(input) * len(input[0])
    flashes = 0

    for d in itertools.count():
        already_flashed: set[Point] = set()

        for y in range(0, len(input)):
            for x in range(0, len(input[y])):
                if Point(x, y) in already_flashed:
                    continue

                input[y][x] += 1

                if input[y][x] > 9:
                    flashes += 1
                    already_flashed.add(Point(x, y))
                    input[y][x] = 0

                    to_visit = list(find_neighbours(input, x, y))

                    while len(to_visit) > 0:
                        p = to_visit.pop()

                        if p in already_flashed:
                            continue

                        input[p.y][p.x] += 1

                        if input[p.y][p.x] > 9:
                            flashes += 1
                            input[p.y][p.x] = 0
                            already_flashed.add(p)
                            to_visit.extend(find_neighbours(input, p.x, p.y))

        if len(already_flashed) == num_octopus:
            print(d + 1)
            break



solve_part_1()
solve_part_2()
