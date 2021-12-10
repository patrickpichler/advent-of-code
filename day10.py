#!/usr/bin/env python3

from typing import Optional


BRACES = {"(": ")", "[": "]", "{": "}", "<": ">"}
OPENED = {"(", "[", "{", "<"}
CLOSING = {")", "]", "}", ">"}


def solve_part_1():
    POINTS: dict[str, int] = {")": 3, "]": 57, "}": 1197, ">": 25137}

    sum = 0

    with open("inputs/day10.txt") as f:
        for line in f.read().splitlines():
            opened: list[str] = []
            error_char: Optional[str] = None

            for c in line:
                if c in OPENED:
                    opened.append(c)
                elif BRACES[opened[-1]] == c:
                    opened.pop()
                else:
                    error_char = c
                    break

            if error_char is not None:
                sum += POINTS[error_char]

    print(sum)


def solve_part_2():
    POINTS: dict[str, int] = {")": 1, "]": 2, "}": 3, ">": 4}

    scores = []

    with open("inputs/day10.txt") as f:
        for line in f.read().splitlines():
            opened: list[str] = []
            error_char: Optional[str] = None

            for c in line:
                if c in OPENED:
                    opened.append(c)
                elif BRACES[opened[-1]] == c:
                    opened.pop()
                else:
                    error_char = c
                    break

            if error_char is not None:
                continue

            if len(opened) > 0:
                score = 0

                for _ in range(0, len(opened)):
                    score *= 5
                    opening = opened.pop()
                    closing = BRACES[opening]

                    score += POINTS[closing]

                scores.append(score)

    scores.sort()
    print(scores[int(len(scores) / 2)])


solve_part_1()
solve_part_2()
