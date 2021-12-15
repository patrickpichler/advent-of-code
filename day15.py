#!/usr/bin/env python3

from collections import defaultdict, namedtuple
import queue
from typing import Generator
from queue import PriorityQueue
from dataclasses import dataclass, field


Point = namedtuple("Point", ["x", "y"])
Map = list[list[int]]


@dataclass(frozen=True, order=True)
class Node:
    pos: Point = field(compare=False)
    weight: int


def parse_input(path: str) -> Map:
    result: list[list[int]] = []

    with open(path) as f:
        for line in f.read().splitlines():
            result.append([int(n) for n in line])

    return result


def get_neighbours(map: Map, pos: Point) -> Generator[Point, None, None]:
    if pos.x > 0:
        yield pos._replace(x=pos.x - 1)

    if pos.x < len(map[pos.y]) - 1:
        yield pos._replace(x=pos.x + 1)

    if pos.y > 0:
        yield pos._replace(y=pos.y - 1)

    if pos.y < len(map) - 1:
        yield pos._replace(y=pos.y + 1)


def print_map(map: Map, path: list[Point]):
    for y in range(0, len(map)):
        for x in range(0, len(map[y])):
            if Point(x, y) in path:
                print("x", end="")
            else:
                print(map[y][x], end="")

        print()


def solve_part_1():
    map = parse_input("inputs/day15.txt")

    start = Point(0, 0)
    end = Point(len(map[0]) - 1, len(map) - 1)

    dist: dict[Point, int] = defaultdict(lambda: 100000000)
    prev: dict[Point, Point] = {}

    dist[start] = 0

    to_visit: queue.PriorityQueue[Node] = PriorityQueue()
    visited: set[Point] = set()

    to_visit.put(Node(start, 0))

    while not to_visit.empty():
        node = to_visit.get()

        if node.pos in visited:
            continue

        visited.add(node.pos)

        for n in get_neighbours(map, node.pos):
            danger = dist[node.pos] + map[n.y][n.x]

            if danger < dist[n]:
                dist[n] = danger
                prev[n] = node.pos

            if n not in visited:
                to_visit.put(Node(n, danger))

    path: list[Point] = list()
    curr = end
    while curr != start:
        path.append(curr)
        curr = prev[curr]

    path.append(start)

    path.reverse()

    sum = 0

    for p in path[1:]:
        sum += map[p.y][p.x]

    print(sum)


def get_neighbours_expanded(
    map: Map, pos: Point
) -> Generator[Point, None, None]:
    if pos.x > 0:
        yield pos._replace(x=pos.x - 1)

    if pos.x < (len(map[0]) * 5) - 1:
        yield pos._replace(x=pos.x + 1)

    if pos.y > 0:
        yield pos._replace(y=pos.y - 1)

    if pos.y < (len(map) * 5) - 1:
        yield pos._replace(y=pos.y + 1)


def get_danger(map: Map, pos: Point) -> int:
    danger_mod_x = int(pos.x / len(map[0]))
    danger_mod_y = int(pos.y / len(map))

    adjusted_x = pos.x % len(map[0])
    adjusted_y = pos.y % len(map)

    danger = map[adjusted_y][adjusted_x] + (danger_mod_x + danger_mod_y)

    if danger > 9:
        return danger % 9

    return danger


def print_map_expanded(map: Map, path: list[Point]):
    for y in range(0, len(map) * 5):
        for x in range(0, len(map[0]) * 5):
            if Point(x, y) in path:
                print("x", end="")
            else:
                print(get_danger(map, Point(x, y)), end="")

        print()


def solve_part_2():
    map = parse_input("inputs/day15.txt")

    start = Point(0, 0)
    end = Point((len(map[0]) * 5) - 1, (len(map) * 5) - 1)

    dist: dict[Point, int] = defaultdict(lambda: 100000000)
    prev: dict[Point, Point] = {}

    dist[start] = 0

    to_visit: queue.PriorityQueue[Node] = PriorityQueue()
    visited: set[Point] = set()

    to_visit.put(Node(start, 0))

    while not to_visit.empty():
        node = to_visit.get()

        if node.pos in visited:
            continue

        visited.add(node.pos)

        for n in get_neighbours_expanded(map, node.pos):
            danger = dist[node.pos] + get_danger(map, node.pos)

            if danger < dist[n]:
                dist[n] = danger
                prev[n] = node.pos

            if n not in visited:
                to_visit.put(Node(n, danger))

    path: list[Point] = list()
    curr = end
    while curr != start:
        path.append(curr)
        curr = prev[curr]

    path.append(start)

    path.reverse()

    sum = 0

    for p in path[1:]:
        sum += get_danger(map, p)

    print(sum)


solve_part_1()
solve_part_2()
