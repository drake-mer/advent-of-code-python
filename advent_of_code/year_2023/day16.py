import abc
import dataclasses
import enum
from typing import Iterable

from advent_of_code.datastructures.dimension2 import BaseMatrix, Coordinate
from advent_of_code.solution import Solution


@dataclasses.dataclass
class Square:
    coordinates: Coordinate

    c = None

    @abc.abstractmethod
    def direction(self, direction: "Direction") -> Iterable["Direction"]:
        raise NotImplementedError()

    def __str__(self):
        return self.c


class Empty(Square):
    c = "."

    def direction(self, direction: "Direction"):
        yield direction


class RightMirror(Square):
    c = "/"

    def direction(self, direction: "Direction"):
        match direction:
            case Direction.RIGHT:
                yield Direction.UP
            case Direction.LEFT:
                yield Direction.DOWN
            case Direction.UP:
                yield Direction.RIGHT
            case Direction.DOWN:
                yield Direction.LEFT


class LeftMirror(Square):
    c = "\\"

    def direction(self, direction: "Direction"):
        match direction:
            case Direction.RIGHT:
                yield Direction.DOWN
            case Direction.LEFT:
                yield Direction.UP
            case Direction.UP:
                yield Direction.LEFT
            case Direction.DOWN:
                yield Direction.RIGHT


class HorizontalSplitter(Square):
    c = "-"

    def direction(self, direction: "Direction"):
        match direction:
            case Direction.RIGHT | Direction.LEFT:
                yield direction
            case Direction.UP | Direction.DOWN:
                yield Direction.RIGHT
                yield Direction.LEFT


class VerticalSplitter(Square):
    c = "|"

    def direction(self, direction: "Direction"):
        match direction:
            case Direction.RIGHT | Direction.LEFT:
                yield Direction.UP
                yield Direction.DOWN
            case Direction.UP | Direction.DOWN:
                yield direction


def square_factory(p, coord):
    match p:
        case ".":
            return Empty(coord)
        case "/":
            return RightMirror(coord)
        case '\\':
            return LeftMirror(coord)
        case "-":
            return HorizontalSplitter(coord)
        case "|":
            return VerticalSplitter(coord)
        case _:
            raise ValueError(f"Could not parse {p}")


class Direction(Coordinate, enum.Enum):
    UP = Coordinate(0, -1)
    DOWN = Coordinate(0, 1)
    LEFT = Coordinate(-1, 0)
    RIGHT = Coordinate(1, 0)


@dataclasses.dataclass(frozen=True)
class Beam:
    position: Coordinate
    direction: Direction


class OpticalBench(BaseMatrix):
    light_path: set[Beam] = None

    @classmethod
    def parse(cls, data):
        return OpticalBench(
            content=BaseMatrix.parse_matrix(
                data=data, wrapper=lambda p, coord: square_factory(p, coord)
            ).content
        )

    def total_energy(self, start_beam=None) -> int:
        visited = set()
        to_visit = [start_beam or Beam(Coordinate(0, 0), Direction.RIGHT)]
        while to_visit:
            beam = to_visit.pop()
            visited.add(beam)
            for direction in self[beam.position].direction(beam.direction):
                new_position = beam.position + direction.value
                if new_position not in self:
                    continue
                if (b := Beam(new_position, direction)) in visited:
                    continue
                to_visit.append(b)
        return len({b.position for b in visited})


class Day16(Solution):
    def parse(self):
        return OpticalBench.parse(self.lines)

    def solution1(self):
        return self.parsed.total_energy()

    def solution2(self):
        left = [
            Beam(Coordinate(0, y), Direction.RIGHT) for y in range(self.parsed.height)
        ]
        right = [
            Beam(Coordinate(self.parsed.width - 1, y), Direction.LEFT)
            for y in range(self.parsed.height)
        ]
        up = [Beam(Coordinate(x, 0), Direction.DOWN) for x in range(self.parsed.width)]
        down = [
            Beam(Coordinate(x, self.parsed.height - 1), Direction.UP)
            for x in range(self.parsed.width)
        ]
        return max(
            self.parsed.total_energy(start_beam=beam)
            for beam in (*left, *right, *up, *down)
        )


class Day16Test(Day16):
    @property
    def data(self):
        return """.|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...."""
