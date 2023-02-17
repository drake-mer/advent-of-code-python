import dataclasses
import pathlib
from enum import Enum
from typing import Iterable

basedir = pathlib.Path(__file__).parent


class Direction(str, Enum):
    UP = 'U'
    DOWN = 'D'
    LEFT = 'L'
    RIGHT = 'R'


@dataclasses.dataclass(frozen=True)
class Coordinate:
    x: int
    y: int

    def __add__(self, other):
        return Coordinate(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Coordinate(self.x - other.x, self.y - other.y)


@dataclasses.dataclass(frozen=True)
class Move:
    direction: Direction
    steps: int

    @property
    def step(self) -> Coordinate:
        match self.direction:
            case Direction.UP:
                return Coordinate(0, 1)
            case Direction.DOWN:
                return Coordinate(0, -1)
            case Direction.LEFT:
                return Coordinate(-1, 0)
            case Direction.RIGHT:
                return Coordinate(1, 0)
            case _:
                raise ValueError()

    def explode(self) -> Iterable[Coordinate]:
        return [self.step for _ in range(self.steps)]


class Rope(list[Coordinate]):
    def __init__(self):
        super().__init__([Coordinate(0, 0) for _ in range(10)])

    @property
    def head(self):
        return self[0]

    @head.setter
    def head(self, val: Coordinate):
        self[0] = val

    @property
    def tail(self) -> Coordinate:
        return self[-1]

    def move_head(self, step: Coordinate):
        self.head = self.head + step
        for k in range(1, 10):
            self[k] = tail_next_pos(self[k-1], self[k])


def box(move: Coordinate):
    """Will constrain a move in a 3x3 square."""
    def box_(value: int):
        return -1 if value < -1 else 1 if value > 1 else value
    return Coordinate(box_(move.x), box_(move.y))


def is_touching(head: Coordinate, tail: Coordinate):
    return box(head - tail) == (head - tail)


def tail_next_pos(head: Coordinate, tail: Coordinate):
    return tail if is_touching(head, tail) else (tail + box(head - tail))


def munch_data(payload: str) -> list[Move]:
    all_lines = []
    for _, line in enumerate(payload.splitlines(keepends=False)):
        d, s = line.split()
        all_lines.append(Move(Direction(d), int(s)))
    return all_lines


def day_1_first_puzzle(payload):
    head = Coordinate(0, 0)
    tail = Coordinate(0, 0)
    visited = set()
    visited.add(tail)
    for move in munch_data(payload):
        for step in move.explode():
            head = head + step
            tail = tail_next_pos(head, tail)
            visited.add(tail)
    return len(visited)


def day_1_second_puzzle(payload):
    rope = Rope()
    visited = set()
    visited.add(rope.tail)
    for move in munch_data(payload):
        for step in move.explode():
            rope.move_head(step)
            visited.add(rope.tail)
    return len(visited)


print(day_1_first_puzzle(open(basedir / 'input.txt').read()))
print(day_1_second_puzzle(open(basedir / 'input.txt').read()))
