#!/usr/bin/env python3

from collections import defaultdict, namedtuple
import re
from typing import DefaultDict, Tuple

Point = namedtuple("Point", ["x", "y"])
Map = DefaultDict[Point, int]


def parse_line(line: str) -> Tuple[Point, Point]:
    m = re.match(r"(\d+),(\d+) -> (\d+),(\d+)", line.strip())

    if m is None:
        print("Line doesn't match regex!! Bug!!")
        print(line)
        raise SystemExit()

    x1 = m.group(1)
    y1 = m.group(2)
    x2 = m.group(3)
    y2 = m.group(4)

    return Point(int(x1), int(y1)), Point(int(x2), int(y2))


def fill_line_points(map: Map, p1: Point, p2: Point):
    if p1.x != p2.x and p1.y != p2.y:
        return

    if p1.x == p2.x:
        for y in range(min(p1.y, p2.y), max(p1.y, p2.y) + 1):
            map[Point(p1.x, y)] += 1

    if p1.y == p2.y:
        for x in range(min(p1.x, p2.x), max(p1.x, p2.x) + 1):
            map[Point(x, p1.y)] += 1


def print_map(map: Map):
    max_x = 0
    max_y = 0

    for p, _ in map.items():
        if max_x < p.x:
            max_x = p.x

        if max_y < p.y:
            max_y = p.y

    for x in range(0, max_x + 1):
        for y in range(0, max_y + 1):
            print(map[Point(x, y)], end="")

        print()


def solve_part_1():
    with open("inputs/day5.txt") as f:
        lines = f.readlines()

        map: Map = defaultdict(int)

        for line in lines:
            p1, p2 = parse_line(line)
            fill_line_points(map, p1, p2)

        count = 0

        for _, c in map.items():
            if c > 1:
                count += 1

        print(count)


def sign(a):
    return bool(a > 0) - bool(a < 0)


def fill_line_points_2(map: Map, p1: Point, p2: Point):
    mod_x = sign(p2.x - p1.x)
    mod_y = sign(p2.y - p1.y)
    pos = p1

    while pos != p2:
        map[pos] += 1
        pos = Point(pos.x + mod_x, pos.y + mod_y)

    map[pos] += 1


def solve_part_2():
    with open("inputs/day5.txt") as f:
        lines = f.readlines()

        map: Map = defaultdict(int)

        for line in lines:
            p1, p2 = parse_line(line)
            fill_line_points_2(map, p1, p2)

        count = 0

        for _, c in map.items():
            if c > 1:
                count += 1

        print(count)


solve_part_1()
solve_part_2()
