#!/usr/bin/env python3

from dataclasses import dataclass
from enum import Enum
import re
from typing import Iterable


@dataclass(frozen=True)
class Point:
    x: int
    y: int


class Axis(Enum):
    X = 1
    Y = 2


@dataclass
class Fold:
    axis: Axis
    point: int


def parse_input(path: str) -> tuple[list[Point], list[Fold]]:
    points: list[Point] = []
    folds: list[Fold] = []

    with open(path) as f:
        lines = f.read().splitlines()

        empty_line = lines.index("")
        for line in lines[0:empty_line]:
            x, y = line.split(",")
            points.append(Point(int(x), int(y)))

        for line in lines[empty_line + 1 :]:
            match = re.match(r"fold along ([xy])=(\d*)", line)

            if match is None:
                print(f"Cannot match line '{line}'")
                raise SystemExit()

            axis = match.group(1)
            point = match.group(2)

            folds.append(Fold(Axis.X if axis == "x" else Axis.Y, int(point)))

        return (points, folds)


def find_size(points: Iterable[Point]) -> tuple[int, int]:
    max_x = 0
    max_y = 0

    for p in points:
        if max_x < p.x:
            max_x = p.x

        if max_y < p.y:
            max_y = p.y

    return (max_x, max_y)


def do_fold_x(fold_point: int, map: set[Point]) -> set[Point]:
    result: set[Point] = set()

    for p in map:
        point = p
        if p.x > fold_point:
            point = Point((fold_point * 2) - p.x, p.y)

        result.add(point)

    return result


def do_fold_y(fold_point: int, map: set[Point]):
    result: set[Point] = set()

    for p in map:
        if p.y > fold_point:
            result.add(Point(p.x, (fold_point * 2) - p.y))
        else:
            result.add(p)

    return result


def print_map(points: Iterable[Point]):
    max_x, max_y = find_size(points)

    for y in range(0, max_y + 1):
        for x in range(0, max_x + 1):
            if Point(x, y) in points:
                print("#", end="")
            else:
                print(".", end="")

        print()


def solve_part_1():
    initial_points, folds = parse_input("inputs/day13.txt")

    points = set(initial_points)

    for f in folds:
        if f.axis == Axis.X:
            points = do_fold_x(f.point, points)
        else:
            points = do_fold_y(f.point, points)
        break

    print(len(points))


def solve_part_2():
    initial_points, folds = parse_input("inputs/day13.txt")

    points = set(initial_points)

    for f in folds:
        if f.axis == Axis.X:
            points = do_fold_x(f.point, points)
        else:
            points = do_fold_y(f.point, points)

    print(len(points))
    print_map(points)


solve_part_1()
solve_part_2()
