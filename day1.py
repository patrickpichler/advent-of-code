#!/usr/bin/env python3


def solve_part_1():
    with open("inputs/day1.txt") as f:
        lines = f.readlines()

        no_increases = 0
        last_depth = -1

        for l in lines:
            depth = int(l.strip())

            if last_depth < 0:
                last_depth = depth
                continue

            if depth > last_depth:
                no_increases += 1

            last_depth = depth

        print(no_increases)


def solve_part_2():
    with open("inputs/day1.txt") as f:
        lines = f.readlines()

        no_increases = 0
        last_depth = -1
        last_depth2 = -1
        last_sum = -1

        counter = 0

        for l in lines:
            counter += 1
            depth = int(l.strip())

            if counter == 1:
                last_depth2 = depth
                continue
            elif counter == 2:
                last_depth = depth
                continue

            sum = depth + last_depth + last_depth2

            last_depth2 = last_depth
            last_depth = depth

            if last_sum < 0:
                last_sum = sum
                continue

            if sum > last_sum:
                no_increases += 1

            last_sum = sum

        print(no_increases)


solve_part_1()
solve_part_2()
