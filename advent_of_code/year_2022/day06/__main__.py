import dataclasses
import pathlib
from typing import Mapping, TypeAlias

basedir = pathlib.Path(__file__).parent


@dataclasses.dataclass
class Move:
    quantity: int
    source: int
    destination: int


Stacks: TypeAlias = Mapping[int, list]


def munch_data(payload: str) -> str:
    return payload.strip()


def solve(message: str, size=4):
    total_length = len(message)
    for pos, c in enumerate(message):
        if total_length - pos < size:
            raise ValueError("not found")
        if len(set(message[pos : pos + size])) == size:
            return pos + size


def day_1_first_puzzle(payload):
    print(solve(munch_data(payload)))


def day_1_second_puzzle(payload):
    print(solve(munch_data(payload), size=14))


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
