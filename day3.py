#!/usr/bin/env python3


from typing import Dict


def solve_part_1():
    with open("inputs/day3.txt") as f:
        lines = f.readlines()

        lookup: Dict[int, Dict[str, int]] = {}

        for line in lines:
            for idx, num in enumerate(line.strip()):
                if idx not in lookup:
                    lookup[idx] = {num: 1}
                    continue

                if num not in lookup[idx]:
                    lookup[idx][num] = 1

                lookup[idx][num] += 1

        gamma = 0
        epsilon = 0

        for idx, dict in lookup.items():
            num_0 = dict["0"]
            num_1 = dict["1"]

            if num_0 > num_1:
                gamma = gamma << 1
                epsilon = epsilon << 1
                epsilon = epsilon | 1
            else:
                gamma = gamma << 1
                gamma = gamma | 1
                epsilon = epsilon << 1

        print(gamma * epsilon)


def solve_part_2():
    trie: Dict[str, int] = {}
    with open("inputs/day3.txt") as f:
        lines = f.readlines()

        for line in lines:
            path = ""
            for c in line:
                path += c

                if path in trie:
                    v = trie[path]
                    if isinstance(v, int):
                        trie[path] = v + 1
                    else:
                        print(f"trie value is not a number: {v}")
                        raise SystemExit()

                else:
                    trie[path] = 1

        oxygen = ""
        co2 = ""

        for _ in range(0, len(lines[0]) - 1):
            num_0: int = -1
            num_1: int = -1

            if oxygen + "0" in trie:
                num_0 = trie[oxygen + "0"]

            if oxygen + "1" in trie:
                num_1 = trie[oxygen + "1"]

            if num_0 > num_1:
                oxygen += "0"
            else:
                oxygen += "1"

        for _ in range(0, len(lines[0]) - 1):
            num_0: int = -1
            num_1: int = -1

            if co2 + "0" in trie:
                num_0 = trie[co2 + "0"]
            else:
                co2 += "1"
                continue

            if co2 + "1" in trie:
                num_1 = trie[co2 + "1"]
            else:
                co2 += "0"
                continue

            if num_1 < num_0:
                co2 += "1"
            else:
                co2 += "0"

        print(int(oxygen, 2) * int(co2, 2))


solve_part_1()
solve_part_2()
