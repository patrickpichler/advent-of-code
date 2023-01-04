#!/usr/bin/env python3

import itertools


def solve_part_1():
    positions = [3, 9]
    scores = [0, 0]
    current_player = 0
    rolls = 0

    for i in itertools.count(0, 3):
        n = (i % 100) + 1

        move = n + n + 1 + n + 2

        old_pos = positions[current_player]
        new_pos = (old_pos + move) % 10
        positions[current_player] = new_pos
        scores[current_player] += new_pos + 1

        if scores[current_player] >= 1000:
            rolls = i + 3
            break

        current_player = (current_player + 1) % len(positions)

    print(
        scores[(current_player + 1) % len(scores)],
        rolls,
        scores[(current_player + 1) % len(scores)] * rolls,
    )


def solve_part_2():
    start_positions = [4, 8]


solve_part_1()
solve_part_2()
