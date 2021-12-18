#!/usr/bin/env python3

from dataclasses import dataclass, field
from typing import Optional, Union
import math
import uuid
import copy

Node = Union["UnionNode", "ValueNode"]


@dataclass
class ValueNode:
    value: int = field(compare=False)
    id: uuid.UUID = field(default_factory=uuid.uuid4)
    parent: Union["UnionNode", None] = field(default=None, compare=False)


@dataclass
class UnionNode:
    id: uuid.UUID = field(default_factory=uuid.uuid4)
    parent: Union["UnionNode", None] = field(default=None, compare=False)
    left: Node | None = field(default=None, compare=False)
    right: Node | None = field(default=None, compare=False)


def parse_line(line: str) -> Node:
    current: UnionNode | None = None
    root: Node | None = None

    for c in line:
        if c == ",":
            continue

        if c == "[":
            parent = current
            current = UnionNode(parent=parent)

            if root is None:
                root = current

            continue

        if current is None:
            print("Bug!!! Current is none!")
            raise SystemError()

        if c.isdigit():
            if current.left is None:
                current.left = ValueNode(int(c), parent=current)
            else:
                current.right = ValueNode(int(c), parent=current)
            continue

        if c == "]":
            n = current
            current = current.parent

            if current is not None:
                if current.left is None:
                    current.left = n
                else:
                    current.right = n
            continue

    if root is None:
        print("Bug!!! No root node found!")
        raise SystemError()

    return root


def parse_input(path: str) -> list[Node]:
    with open(path) as f:
        return [parse_line(line) for line in f.read().splitlines()]


def render(node: Node | None, newline=True):
    if node is None:
        print("", end="")
        if newline:
            print()

        return

    if isinstance(node, ValueNode):
        print(node.value, end="")

        if newline:
            print()

        return

    print("[", end="")
    render(node.left, newline=False)
    print(",", end="")
    render(node.right, newline=False)
    print("]", end="")

    if newline:
        print()


def split(value: ValueNode):
    if value.parent is None:
        print("Bug!! Cannot split root node!")
        raise SystemError()

    half = value.value / 2

    node = UnionNode(parent=value.parent)
    left = ValueNode(math.floor(half), parent=node)
    right = ValueNode(math.ceil(half), parent=node)

    node.left = left
    node.right = right

    if value.parent.left == value:
        value.parent.left = node
    elif value.parent.right == value:
        value.parent.right = node
    else:
        print("Bug!! Value is neither left nor right of parent")
        raise SystemError()


def find_rightmost_node(node: Node) -> ValueNode | None:
    if isinstance(node, ValueNode):
        return node

    current = node

    while isinstance(current.right, UnionNode):
        current = current.right

    return current.right


def find_leftmost_node(node: Node) -> ValueNode | None:
    if isinstance(node, ValueNode):
        return node

    current = node

    while isinstance(current.left, UnionNode):
        current = current.left

    return current.left


def find_left_node(node: Node) -> ValueNode | None:
    curr = node

    while curr.parent is not None and curr.parent.left == curr:
        curr = curr.parent

    if curr.parent is None:
        return None

    if curr.parent.left is None:
        print("Bug!! Parent left is none!")
        raise SystemError()

    return find_rightmost_node(curr.parent.left)


def find_right_node(node: Node) -> ValueNode | None:
    curr = node

    while curr.parent is not None and curr.parent.right == curr:
        curr = curr.parent

    if curr.parent is None:
        return None

    if curr.parent.right is None:
        print("Bug!! Parent right is none!")
        raise SystemError()

    return find_leftmost_node(curr.parent.right)


def list_all_child_union_nodes(node: UnionNode) -> list[UnionNode]:
    to_search: list[Node] = [node]
    found: list[UnionNode] = []

    while len(to_search) > 0:
        curr = to_search.pop()

        if isinstance(curr, UnionNode):
            found.append(curr)

            if curr.left is None or curr.right is None:
                print("Bug!! Either left or right is none!")
                raise SystemError()

            to_search.append(curr.left)
            to_search.append(curr.right)

    return found


def do_explode(node: UnionNode):
    if node.parent is None:
        print("Bug!! Cannot explode root node")
        raise SystemError()

    if not isinstance(node.left, ValueNode) or not isinstance(node.right, ValueNode):
        print("Bug!! Can only explode UnionNode with two ValueNodes")
        raise SystemError()

    left = find_left_node(node)
    right = find_right_node(node)

    if node.parent.left == node:
        node.parent.left = ValueNode(0, parent=node.parent)

    if node.parent.right == node:
        node.parent.right = ValueNode(0, parent=node.parent)

    if left is not None:
        left.value += node.left.value

    if right is not None:
        right.value += node.right.value


def find_root(node: Node) -> Node:
    curr = node

    while curr.parent is not None:
        curr = curr.parent

    return curr


def explode(node: UnionNode):
    children = list_all_child_union_nodes(node)
    children.reverse()

    for c in children:
        do_explode(c)


def add(left: Node, right: Node) -> Node:
    node = UnionNode()
    node.left = left
    node.right = right

    left.parent = node
    right.parent = node

    return node


def get_depth(node: Node) -> int:
    depth = 0
    curr = node

    while curr.parent is not None:
        depth += 1
        curr = curr.parent

    return depth


def search_node_to_explode(node: Node) -> Optional[UnionNode]:
    to_check: list[Node] = [node]

    while len(to_check) > 0:
        curr = to_check.pop()

        if isinstance(curr, UnionNode):
            if curr.left is None or curr.right is None:
                print("Bug!! Either left or right is none!")
                raise SystemError()

            if get_depth(curr) >= 4:
                if isinstance(curr.left, ValueNode) and isinstance(
                    curr.right, ValueNode
                ):
                    return curr

            to_check.append(curr.right)
            to_check.append(curr.left)

    return None


def search_node_to_split(node: Node) -> Optional[ValueNode]:
    to_check: list[Node] = [node]

    while len(to_check) > 0:
        curr = to_check.pop()

        if isinstance(curr, ValueNode):
            if curr.value > 9:
                return curr
            continue

        if curr.left is None or curr.right is None:
            print("Bug!! Either left or right is none!")
            raise SystemError()

        to_check.append(curr.right)
        to_check.append(curr.left)

    return None


def simplify(node: Node):
    while True:
        explode_node = search_node_to_explode(node)

        if explode_node is not None:
            do_explode(explode_node)
            continue

        split_node = search_node_to_split(node)

        if split_node is not None:
            split(split_node)
            continue

        break


def calculate_magnitude(node: Node) -> int:
    if isinstance(node, ValueNode):
        return node.value

    if node.left is None or node.right is None:
        print("Bug!! Either left or right is none!")
        raise SystemError()

    return calculate_magnitude(node.left) * 3 + calculate_magnitude(node.right) * 2


def solve_part_1():
    lines = parse_input("inputs/day18.txt")
    curr = lines[0]

    for line in lines[1:]:
        curr = add(curr, line)
        simplify(curr)

    print(calculate_magnitude(curr))


def solve_part_2():
    lines = parse_input("inputs/day18.txt")

    max = 0

    for ix, x in enumerate(lines):
        for iy, y in enumerate(lines):
            if ix == iy:
                continue
            n = add(copy.deepcopy(x), copy.deepcopy(y))

            simplify(n)

            m = calculate_magnitude(n)

            if m > max:
                max = m

    print(max)


solve_part_1()
solve_part_2()
