import dataclasses
import itertools
from functools import cached_property

from advent_of_code.datastructures import BaseMatrix, Coordinate
from advent_of_code.solution import Solution


def counter():
    for k in range(1, 10**6):
        yield k


GLOBAL_COUNTER = counter()


@dataclasses.dataclass
class Galaxy:
    id: int
    coordinates: Coordinate

    def __eq__(self, other: "Galaxy"):
        return self.id == other.id

    def __str__(self):
        return str(self.id)


@dataclasses.dataclass
class Void:
    coordinates: Coordinate

    def __str__(self):
        return "."


def wrapper(character, coordinates: Coordinate):
    match character:
        case ".":
            return Void(coordinates)
        case "#":
            return Galaxy(id=next(GLOBAL_COUNTER), coordinates=coordinates)


@dataclasses.dataclass
class Universe:
    space: BaseMatrix

    empty_rows: list[int]
    empty_cols: list[int]

    @cached_property
    def galaxies(self) -> list[Galaxy]:
        all_galaxies = list()
        for element in self.space.all_values():
            match element:
                case Galaxy():
                    all_galaxies.append(element)
        return all_galaxies


class Day11(Solution):
    game: Universe

    @cached_property
    def game(self) -> Universe:
        return self.parse()

    def parse(self):
        empty_rows = []
        for p, line in enumerate(self.lines):
            if line.count(".") == len(line):
                empty_rows.append(p)

        empty_cols = []
        for k, col in enumerate(zip(*self.lines)):
            if "".join(col).count(".") == len(col):
                empty_cols.append(k)

        return Universe(
            space=BaseMatrix.parse_matrix(data=self.lines, wrapper=wrapper),
            empty_cols=empty_cols,
            empty_rows=empty_rows,
        )

    def solution1(self, offset=1):
        total = 0
        for g1, g2 in itertools.product(self.game.galaxies, self.game.galaxies):
            if g1.id >= g2.id:
                continue
            x0, x1 = sorted((g1.coordinates.x, g2.coordinates.x))
            y0, y1 = sorted((g1.coordinates.y, g2.coordinates.y))
            total += (
                g1.coordinates.manhattan(g2.coordinates)
                + offset * sum(True for row in self.game.empty_rows if y0 < row < y1)
                + offset * sum(True for col in self.game.empty_cols if x0 < col < x1)
            )
        return total

    def solution2(self):
        return self.solution1(offset=10**6 - 1)


class Day11Test(Day11):
    @property
    def data(self):
        return """...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."""
