#!/usr/bin/env python3
import re


def solve_part_1():
    with open("inputs/day2.txt") as f:
        lines = f.readlines()
        horizontal = 0
        depth = 0

        for line in lines:
            m = re.match(r"(\w+) (\d+).*", line)

            if m is None:
                print(f"Line didn't match {line}", end="")
                raise SystemExit()

            action = m.group(1)
            distance = int(m.group(2))

            if action == "forward":
                horizontal += distance
                continue
            elif action == "down":
                depth += distance
                continue
            elif action == "up":
                depth -= distance
                continue

        result = horizontal * depth
        print(result)


def solve_part_2():
    with open("inputs/day2.txt") as f:
        lines = f.readlines()
        horizontal = 0
        depth = 0
        aim = 0

        for line in lines:
            m = re.match(r"(\w+) (\d+).*", line)

            if m is None:
                print(f"Line didn't match {line}", end="")
                raise SystemExit()

            action = m.group(1)
            distance = int(m.group(2))

            if action == "forward":
                horizontal += distance
                depth += aim * distance
            elif action == "down":
                aim += distance
            elif action == "up":
                aim -= distance

        result = horizontal * depth
        print(result)


solve_part_1()
solve_part_2()
