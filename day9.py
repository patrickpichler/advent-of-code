#!/usr/bin/env python3

from collections import namedtuple
from typing import Generator


HeightMap = list[list[int]]
Point = namedtuple("Point", ["x", "y"])


def parse_input(path: str) -> HeightMap:
    result: HeightMap = []

    with open(path) as f:
        for line in f.readlines():
            result.append([int(c) for c in line.strip()])

    return result


def render_heightmap(map: HeightMap):
    for row in map:
        for c in row:
            print(c, end="")

        print()


def is_lowest_neighbour(map: HeightMap, x: int, y: int) -> bool:
    num = map[y][x]
    result = True

    if x > 0:
        if map[y][x - 1] <= num:
            return False

    if y > 0:
        if map[y - 1][x] <= num:
            return False

    if y < len(map) - 1:
        if map[y + 1][x] <= num:
            return False

    if x < len(map[y]) - 1:
        if map[y][x + 1] <= num:
            return False

    return result


def print_surrounding(map: HeightMap, x: int, y: int):
    if y > 0:
        if x > 0:
            print(map[y - 1][x - 1], end="")

        print(map[y - 1][x], end="")

        if x < len(map[y - 1]) - 1:
            print(map[y - 1][x + 1], end="")

        print()

    if x > 0:
        print(map[y][x - 1], end="")
    print(map[y][x], end="")

    if x < len(map[y]) - 1:
        print(map[y][x + 1], end="")

    print()

    if y < len(map) - 1:
        if x > 0:
            print(map[y + 1][x - 1], end="")

        print(map[y + 1][x], end="")

        if x < len(map[y + 1]) - 1:
            print(map[y + 1][x + 1], end="")

        print()


def solve_part_1():
    input = parse_input("inputs/day9.txt")

    low_points: list[Point] = []

    for y in range(0, len(input)):
        for x in range(0, len(input[y])):
            if is_lowest_neighbour(input, x, y):
                low_points.append(Point(x, y))

    sum = 0

    for p in low_points:
        sum += input[p.y][p.x] + 1

    print(sum)


def get_neighbouring_points(
    map: HeightMap, p: Point
) -> Generator[Point, None, None]:
    if p.y > 0:
        yield p._replace(y=p.y - 1)

    if p.x > 0:
        yield p._replace(x=p.x - 1)

    if p.x < len(map[p.y]) - 1:
        yield p._replace(x=p.x + 1)

    if p.y < len(map) - 1:
        yield p._replace(y=p.y + 1)


def render_heightmap_with_basins(map: HeightMap, basins: dict[Point, int]):
    for y in range(0, len(map)):
        for x in range(0, len(map[y])):
            p = Point(x, y)

            if p in basins:
                print(basins[p], end="")
            else:
                print(map[y][x], end="")

        print()


def solve_part_2():
    input = parse_input("inputs/day9.txt")

    visited_points: set[Point] = set()

    basins: list[int] = []

    basin_map: dict[Point, int] = {}

    basin_id = 0

    for y in range(0, len(input)):
        for x in range(0, len(input[y])):
            if Point(x, y) in visited_points or input[y][x] == 9:
                continue

            basin_size = 0
            basin_id += 1

            points_to_traverse: list[Point] = [Point(x, y)]

            while len(points_to_traverse) > 0:
                p = points_to_traverse.pop()

                if p in visited_points or input[p.y][p.x] == 9:
                    continue

                visited_points.add(p)
                basin_map[p] = basin_id

                basin_size += 1

                points_to_traverse.extend(get_neighbouring_points(input, p))

            basins.append(basin_size)

    basins.sort()

    print(basins[-1] * basins[-2] * basins[-3])


solve_part_1()
solve_part_2()
