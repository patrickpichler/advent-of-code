#!/usr/bin/env python3


def parse_digits(digits: str) -> list[str]:
    return ["".join(sorted(s)) for s in filter(None, digits.split(" "))]


def parse_input(path: str) -> list[tuple[list[str], list[str]]]:
    with open(path) as f:
        result: list[tuple[list[str], list[str]]] = []

        for line in f.readlines():
            signals_raw, display_raw = line.strip().split("|", 2)

            signals = parse_digits(signals_raw)
            display = parse_digits(display_raw)

            result.append((signals, display))

        return result


def solve_part_1():
    input = parse_input("inputs/day8.txt")

    num_segments_searched = {2, 4, 3, 7}

    num_count = 0

    for _, display in input:
        for digit in display:
            if len(digit) in num_segments_searched:
                num_count += 1

    print(num_count)


def get_num(input: list[str], num: int) -> str:
    unique_nums = [1, 4, 7, 8]
    unique_nums_sizes = [2, 4, 3, 7]

    if num not in unique_nums:
        print(f"{num} is not in {unique_nums}")
        raise SystemExit()

    searched_size = unique_nums_sizes[unique_nums.index(num)]

    for s in input:
        if len(s) == searched_size:
            return s

    print(f"{num} not found!! Bug!!")
    raise SystemExit()


def get_nums_with_len(input: list[str], searched_len: int) -> list[str]:
    return list(filter(lambda str: len(str) == searched_len, input))


def get_diffs(nums: list[str], searched: str) -> list[tuple[str, str]]:
    result: list[tuple[str, str]] = []

    for num in nums:
        diff = "".join(filter(lambda s: s not in searched, num))
        result.append((num, diff))

    return result


def find_num6(nums: list[str], one: str) -> str:
    for num in nums:
        all_match = True
        for s in one:
            if s not in num:
                all_match = False
                break

        if not all_match:
            return num

    print("6 not found... Bug!!")
    raise SystemExit()


def find_num3(nums: list[str], one: str) -> str:
    for num in nums:
        all_match = True
        for s in one:
            if s not in num:
                all_match = False
                break

        if all_match:
            return num

    print("3 not found... Bug!!")
    raise SystemExit()


def find_num9(nums: list[str], three: str) -> str:
    for num in nums:
        mismatch_count = 0
        for s in num:
            if s not in three:
                mismatch_count += 1

        if mismatch_count == 1:
            return num

    print("9 not found... Bug!!")
    raise SystemExit()


def find_num5(nums: list[str], nine: str) -> str:
    for num in nums:
        all_match = True
        for s in num:
            if s not in nine:
                all_match = False
                break

        if all_match:
            return num

    print("5 not found... Bug!!")
    raise SystemExit()


def build_lookup(digits: list[str]) -> dict[str, int]:
    one = get_num(digits, 1)
    four = get_num(digits, 4)
    seven = get_num(digits, 7)
    eight = get_num(digits, 8)
    nums_6 = get_nums_with_len(digits, 6)
    nums_5 = get_nums_with_len(digits, 5)

    six = find_num6(nums_6, one)
    nums_6.remove(six)

    three = find_num3(nums_5, one)
    nums_5.remove(three)

    nine = find_num9(nums_6, three)
    nums_6.remove(nine)

    zero = nums_6[0]

    five = find_num5(nums_5, nine)
    nums_5.remove(five)

    two = nums_5[0]

    return {
        zero: 0,
        one: 1,
        two: 2,
        three: 3,
        four: 4,
        five: 5,
        six: 6,
        seven: 7,
        eight: 8,
        nine: 9,
    }


def solve_part_2():
    input = parse_input("inputs/day8.txt")

    sum = 0

    for nums, display in input:
        lookup = build_lookup(nums)

        first_pos = lookup[display[0]]
        second_pos = lookup[display[1]]
        third_pos = lookup[display[2]]
        fourth_pos = lookup[display[3]]

        num = first_pos * 1000 + second_pos * 100 + third_pos * 10 + fourth_pos

        sum += num

    print(sum)


solve_part_1()
solve_part_2()
