import dataclasses
import functools
import itertools
from typing import TypeAlias, Callable

from advent_of_code.solution import Solution


def prime_factors(n):
    """
    Shamelessly copy-pasted from
    https://stackoverflow.com/questions/15347174/python-finding-prime-factors
    """
    i = 2
    factors = []
    while i * i <= n:
        if n % i:
            i += 1
        else:
            n //= i
            factors.append(i)
    if n > 1:
        factors.append(n)
    return factors


class Left:
    pass


class Right:
    pass

@dataclasses.dataclass(frozen=True)
class Node:
    label: str


Direction: TypeAlias = Left | Right


def direction_factory(direction: str) -> Left | Right:
    match direction:
        case "L":
            return Left()
        case "R":
            return Right()
        case _:
            raise ValueError(f"{direction} does not correspond to a valid direction")


@dataclasses.dataclass
class Game:
    directions: list[Direction]
    tree: dict[Node, tuple[Node, Node]]

    @classmethod
    def parse_row(cls, row) -> tuple[Node, Node, Node]:
        st, rm = row.split(" = ")
        rm1, rm2 = rm.strip("()").split(", ")
        return Node(st), Node(rm1), Node(rm2)

    def next_node(self, position: Node, direction: Direction) -> Node:
        left, right = self.tree[position]
        match direction:
            case Left():
                position = left
            case Right():
                position = right
        return position

    def find_nb_steps(self, start: Node, condition: Callable[[Node], bool]) -> int:
        position = start
        for (step, direction) in enumerate(
            itertools.cycle(self.directions),
            1
        ):
            position = self.next_node(position, direction)
            if condition(position):
                return step


class Day08(Solution):
    def parse(self):
        return Game(
            directions=[direction_factory(c) for c in self.lines[0]],
            tree={
                n: (l, r)
                for (n, l, r) in (
                    Game.parse_row(line) for line in self.lines[2:]
                )
            }
        )

    def solution1(self):
        return self.parsed.find_nb_steps(start=Node("AAA"), condition=lambda n: n.label == "ZZZ")

    def solution2(self):
        game: Game = self.parsed
        positions = [n for n in game.tree if n.label.endswith("A")]
        required_steps = [
            game.find_nb_steps(start, condition=lambda n: n.label.endswith("Z"))
            for start in positions
        ]
        factors = set()
        for st in required_steps:
            factors.update(prime_factors(st))
        return functools.reduce(lambda x, y: x*y, factors)


class Day08Test(Day08):
    def solution1(self):
        return

    @property
    def data(self):
        return """LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"""
