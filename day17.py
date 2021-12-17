#!/usr/bin/env python3

import re
from dataclasses import dataclass
from typing import Optional


@dataclass(frozen=True)
class TargetArea:
    x: tuple[int, int]
    y: tuple[int, int]


@dataclass(frozen=True)
class Point:
    x: int
    y: int


def parse_input(path: str) -> TargetArea:
    with open(path) as f:
        line = f.read().strip()
        match = re.match(r"target area: x=(-?\d+)..(-?\d+), y=(-?\d+)..(-?\d+)", line)

        if match is None:
            print(f"Bug!! Cannot parse line: {line}")
            raise SystemExit()

        x1 = int(match.group(1))
        x2 = int(match.group(2))
        y1 = int(match.group(3))
        y2 = int(match.group(4))

        return TargetArea((x1, x2), (y1, y2))


def point_in_target_area(point: Point, target_area: TargetArea) -> bool:
    return (
        target_area.x[0] <= point.x
        and target_area.x[1] >= point.x
        and target_area.y[0] <= point.y
        and target_area.y[1] >= point.y
    )


def simulate_trajectory(
    power: tuple[int, int], target_area: TargetArea
) -> Optional[int]:
    x = 0
    y = 0
    highest_y = 0

    current_power_x, current_power_y = power

    while True:
        x += current_power_x
        y += current_power_y

        if y > highest_y:
            highest_y = y

        if point_in_target_area(Point(x, y), target_area):
            return highest_y

        if x > target_area.x[1] or y < target_area.y[0]:
            return None

        current_power_y -= 1

        if current_power_x < 0:
            current_power_x += 1
        elif current_power_x > 0:
            current_power_x -= 1


def find_min_x_into_target_area(target_area: TargetArea) -> int:
    num = 1

    while True:
        sum = (num * (num + 1)) / 2

        if sum >= target_area.x[0]:
            if sum <= target_area.x[1]:
                return num
            else:
                print(f"Bug!! No number found! TargetArea: {target_area}")
                raise SystemExit()

        num += 1


def solve_part_1():
    target_area = parse_input("inputs/day17.txt")

    min_x = find_min_x_into_target_area(target_area)

    max_y = 0

    for i in range(target_area.y[0], -target_area.y[0]):
        y = simulate_trajectory((min_x, i), target_area)

        if y is not None:
            if y > max_y:
                max_y = y

    print(max_y)


def solve_part_2():
    target_area = parse_input("inputs/day17.txt")

    min_x = find_min_x_into_target_area(target_area)

    hits = 0

    for pow_x in range(min_x, target_area.x[1] + 1):
        for pow_y in range(target_area.y[0], -target_area.y[0]):
            y = simulate_trajectory((pow_x, pow_y), target_area)

            if y is not None:
                hits += 1

    print(hits)


solve_part_1()
solve_part_2()
