import itertools
import pathlib
from collections import namedtuple
from typing import Iterable

basedir = pathlib.Path(__file__).parent


Tree = namedtuple("Tree", "k,l,height")
Coordinate = namedtuple("Coordinate", "k,l")


def munch_data(payload: str) -> list[list[Tree]]:
    all_lines = []
    for k, line in enumerate(payload.splitlines(keepends=False)):
        current_line = []
        for l, tree in enumerate(line):
            current_line.append(Tree(k, l, tree))
        all_lines.append(current_line)
    return all_lines


def visible_count(row: Iterable[Tree]) -> set[Coordinate]:
    current_max = None
    output_set = set()
    for tree in row:
        if current_max is None:
            output_set.add(Coordinate(tree.k, tree.l))
            current_max = tree.height
        elif tree.height > current_max:
            output_set.add(Coordinate(tree.k, tree.l))
            current_max = tree.height
    return output_set


def view_distance(k: int, l: int, tree_map: list[list[Tree]]) -> int:
    total_view = 1
    right = (tree_map[k][d] for d in range(l + 1, len(tree_map[0])))
    left = (tree_map[k][d] for d in range(l - 1, -1, -1))
    up = (tree_map[d][l] for d in range(k - 1, -1, -1))
    down = (tree_map[d][l] for d in range(k + 1, len(tree_map)))
    current_tree = tree_map[k][l]

    for iterator in (right, left, up, down):
        current_view_distance = 0
        for tree in iterator:
            current_view_distance += 1
            if tree.height >= current_tree.height:
                break
        total_view *= current_view_distance
    return total_view


def all_tree_rows(forest: list[list[Tree]]) -> Iterable[list[Tree]]:
    for row in forest:
        yield list(row)

    for row in forest:
        yield list(row[::-1])

    for row in zip(*forest):
        yield list(row)

    for row in zip(*forest):
        yield list(row[::-1])


def day_1_first_puzzle(payload):
    full_set = set()
    for tree_row in all_tree_rows(munch_data(payload)):
        output_set = visible_count(tree_row)
        full_set = full_set.union(output_set)
    return len(full_set)


def day_1_second_puzzle(payload):
    longest_distance = 0
    data = munch_data(payload)
    for k, l in itertools.product(range(len(data)), range(len(data[0]))):
        if (d := view_distance(k, l, data)) > longest_distance:
            longest_distance = d
    return longest_distance


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
