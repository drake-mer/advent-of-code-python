import dataclasses
import math

from advent_of_code.datastructures.dimension2 import (
    BaseMatrix,
    Coordinate,
    Direction,
    Map2D,
)
from advent_of_code.solution import Solution


@dataclasses.dataclass(frozen=True)
class DiggerInstruction:
    direction: Direction
    length: int
    color: int

    @classmethod
    def parse(cls, data: str):
        c, l, color = data.split()
        return cls(direction(c), int(l), int(color.strip("(#)"), 16))


def direction(c: str) -> Direction:
    match c:
        case "R":
            return Direction.RIGHT
        case "D":
            return Direction.DOWN
        case "U":
            return Direction.UP
        case "L":
            return Direction.LEFT
        case _:
            raise ValueError()


@dataclasses.dataclass
class TrenchMap(BaseMatrix):
    trench: set[Coordinate]


def generate_map(instructions: list[DiggerInstruction]):
    position = Coordinate(0, 0)
    new_map = Map2D()
    new_map[position] = Coordinate(0, 0)
    for instruction in instructions:
        for k in range(instruction.length):
            position = position + instruction.direction
            new_map[position] = position
    assert position == Coordinate(0, 0)
    x_min = min([x for (x, y) in new_map])
    x_max = max([x for (x, y) in new_map])
    y_min = min([y for (x, y) in new_map])
    y_max = max([y for (x, y) in new_map])
    height = y_max - y_min + 1
    width = x_max - x_min + 1
    upper_left = Coordinate(x_min, y_min)
    new_map = {c - upper_left for c in new_map}
    return TrenchMap(
        content=[["#" if Coordinate(x, y) in new_map else "." for x in range(width)] for y in range(height)],
        trench=new_map,
    )


class Day18(Solution):
    def parse(self):
        return [DiggerInstruction.parse(line) for line in self.lines]

    def solution1(self):
        inner_coordinates = Coordinate(70, 50)  # YMMV
        trench_map = generate_map(self.parsed)
        print(trench_map.pretty_print(lambda c: c))
        flooded = trench_map.flood_fill(inner_coordinates, is_boundary=lambda c: trench_map[c] == "#")
        return len(trench_map.trench) + len(flooded)

    def solution2(self):
        instructions = list(self.parsed)


class Day18Test(Day18):
    @property
    def data(self):
        return """R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)"""
