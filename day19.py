#!/usr/bin/env python3

from dataclasses import dataclass
from typing import Optional
import re
import math


@dataclass(frozen=True)
class Point:
    x: int
    y: int
    z: int


def parse_input(path: str) -> list[list[Point]]:
    result: list[list[Point]] = []

    with open(path) as f:
        current: Optional[list[Point]] = None

        for line in f.read().splitlines():
            if not line:
                continue

            match = re.match(r"--- scanner \d* ---", line)

            if match is not None:
                if current is not None:
                    result.append(current)

                current = []
                continue

            x, y, z = line.split(",", 3)

            if current is None:
                print("Bug!! Current is none!")
                raise SystemError()

            current.append(Point(int(x), int(y), int(z)))

        if current is not None:
            result.append(current)

    return result


def calculate_length(p: Point) -> float:
    return math.sqrt(pow(p.x, 2) + pow(p.y, 2) + pow(p.z, 2))


def add(p1: Point, p2: Point) -> Point:
    return Point(p1.x + p2.x, p1.y + p2.y, p1.z + p2.z)


def solve_part_1():
    sensors = parse_input("inputs/day19_sample_small.txt")

    first = sensors[0]
    second = sensors[1]

    for p in second:
        length_to_second = calculate_length(p)

        location_second = add(first[0], p)

        break


def solve_part_2():
    with open("inputs/day8.txt") as f:
        pass


solve_part_1()
solve_part_2()
