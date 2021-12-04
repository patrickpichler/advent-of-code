#!/usr/bin/env python3

Board = list[str]
BoardState = int


def generate_win_conditions() -> list[BoardState]:
    row = 0b11111
    column = 1 | 1 << 5 | 1 << 10 | 1 << 15 | 1 << 20

    conditions: list[int] = []

    for i in range(0, 5):
        conditions.append(row << (5 * i))
        conditions.append(column << i)

    return conditions


def parse_input() -> tuple[list[str], list[Board]]:
    with open("inputs/day4.txt") as f:
        lines = f.readlines()
        numbers = lines[0].strip().split(",")

        boards: list[Board] = []

        current_board: Board = []
        current_row = 0

        for i, line in enumerate(lines):
            line = line.strip()
            if i < 2:
                continue

            if current_row == 5:
                current_row = 0
                boards.append(current_board)
                current_board = []
                continue

            current_row += 1
            current_board.extend(
                [n.strip() for n in line.split(" ") if len(n.strip()) > 0]
            )

        boards.append(current_board)

        return numbers, boards


def find_pos_in_board(board: Board, num: str) -> int:
    for idx, n in enumerate(board):
        if n == num:
            return idx

    return -1


def check_if_win(b: BoardState, win_conditions: list[BoardState]) -> bool:
    for w in win_conditions:
        if b & w == w:
            return True
    return False


def print_board_state(state: int):
    num = bin(state)[2:].zfill(25)[::-1]

    for i in range(5):
        for c in range(5):
            print(f"{num[(i * 5) + c]} ", end="")
        print()


def play_bingo() -> tuple[Board, BoardState, int]:
    numbers, boards = parse_input()
    win_conditions = generate_win_conditions()

    boards_state = []

    for b in boards:
        boards_state.append(0)

    for n in numbers:
        for i, b in enumerate(boards):
            pos = find_pos_in_board(b, n)

            if pos < 0:
                continue

            boards_state[i] |= 1 << pos

        for i, state in enumerate(boards_state):
            if check_if_win(state, win_conditions):
                return boards[i], state, int(n)

    print("Found no winner! Bug!!")
    raise SystemExit()


def solve_part_1():
    board, state, final_num = play_bingo()

    sum = 0

    for idx, num in enumerate(board):
        if state & (1 << idx) == 0:
            sum += int(num)

    print(sum * final_num)


def play_bingo_find_looser() -> tuple[Board, BoardState, int]:
    numbers, boards = parse_input()
    win_conditions = generate_win_conditions()

    done_boards: list[int] = []
    boards_state = []

    for b in boards:
        boards_state.append(0)

    for n in numbers:
        for i, b in enumerate(boards):
            if i in done_boards:
                continue

            pos = find_pos_in_board(b, n)

            if pos < 0:
                continue

            boards_state[i] |= 1 << pos

        for i, state in enumerate(boards_state):
            if i in done_boards:
                continue

            if check_if_win(state, win_conditions):
                done_boards.append(i)

        if len(boards) == len(done_boards):
            last = done_boards[-1]

            return boards[last], boards_state[last], int(n)

    print("Found no winner! Bug!!")
    raise SystemExit()


def solve_part_2():
    board, state, final_num = play_bingo_find_looser()

    sum = 0

    for idx, num in enumerate(board):
        if state & (1 << idx) == 0:
            sum += int(num)

    print(sum * final_num)


solve_part_1()
solve_part_2()
