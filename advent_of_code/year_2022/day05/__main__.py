import dataclasses
import pathlib
import re
from typing import Mapping, TypeAlias

basedir = pathlib.Path(__file__).parent


@dataclasses.dataclass
class Move:
    quantity: int
    source: int
    destination: int


Stacks: TypeAlias = Mapping[int, list]


def munch_data(payload: str) -> tuple[Stacks, list[Move]]:
    all_lines = payload.splitlines(keepends=False)
    for pos, l in enumerate(all_lines):
        if not l:
            break
    stacks = parse_stacks(all_lines[: pos - 1])
    moves = list(map(parse_move, [l for l in all_lines[pos + 1 :] if l]))
    return {k: s for k, s in enumerate(stacks, 1)}, moves


def parse_stacks(stacks):
    def split_row(row):
        output = []
        for k in range(100):
            elem = row[k * 4 : k * 4 + 4]
            if not elem:
                break
            elem = elem.strip(" []")
            output.append(elem)
        return output

    all_stacks = []
    for stack in zip(*(split_row(r) for r in stacks)):
        cur_stack = []
        for elem in stack[::-1]:
            if not elem:
                break
            cur_stack.append(elem)
        all_stacks.append(cur_stack)
    return all_stacks


def parse_move(m: str) -> Move:
    d = re.match(r"move (\d+) from (\d+) to (\d+)", m).groups()
    return Move(quantity=int(d[0]), source=int(d[1]), destination=int(d[2]))


def apply_move(move: Move, stacks: Stacks):
    for k in range(move.quantity):
        elem = stacks[move.source].pop()
        stacks[move.destination].append(elem)


def apply_move2(move: Move, stacks: Stacks):
    moved_stack = stacks[move.source][-move.quantity :]
    new_stack = stacks[move.source][: -move.quantity]
    stacks[move.source] = new_stack
    stacks[move.destination].extend(moved_stack)


def day_1_first_puzzle(payload):
    stacks, moves = munch_data(payload)
    for move in moves:
        apply_move(move, stacks)
    print(*(stacks[k][-1] for k in range(1, 10)), sep="")


def day_1_second_puzzle(payload):
    stacks, moves = munch_data(payload)
    for move in moves:
        apply_move2(move, stacks)
    print(*(stacks[k][-1] for k in range(1, 10)), sep="")


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
