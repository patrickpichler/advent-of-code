#!/usr/bin/env python3

from collections import defaultdict
import re

Node = str
Graph = dict[str, list[Node]]


def parse_input(path: str) -> Graph:
    result: Graph = defaultdict(list)

    with open(path) as f:
        for line in f.read().splitlines():
            m = re.match(r"(\w*)-(\w*)", line)

            if m is None:
                print(f"Cannot match '{line}'")
                raise SystemExit()

            result[m.group(1)].append(m.group(2))
            result[m.group(2)].append(m.group(1))

    return result


def solve_part_1():
    input = parse_input("inputs/day12.txt")

    to_visit: list[tuple[set[str], Node]] = [(set(), "start")]
    num_paths = 0

    while len(to_visit) > 0:
        path, node = to_visit.pop()

        if node == "end":
            num_paths += 1
            continue

        if node.islower() and node in path:
            continue

        new_path = set(path)
        new_path.add(node)

        for n in input[node]:
            to_visit.append((new_path, n))

    print(num_paths)


def solve_part_2():
    input = parse_input("inputs/day12.txt")

    to_visit: list[tuple[set[str], int, Node]] = [(set(), 0, "start")]
    num_paths = 0

    while len(to_visit) > 0:
        path, visits_same_small, node = to_visit.pop()

        if len(path) > 0 and node == "start":
            continue

        if node == "end":
            num_paths += 1
            continue

        if node.islower() and node in path and visits_same_small > 0:
            continue

        new_path = set(path)
        new_path.add(node)

        if node.islower() and node in path:
            visits_same_small += 1

        for n in input[node]:
            to_visit.append((new_path, visits_same_small, n))

    print(num_paths)


solve_part_1()
solve_part_2()
