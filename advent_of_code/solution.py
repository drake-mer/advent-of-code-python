import abc
import dataclasses
import pathlib
from functools import cached_property
from typing import NamedTuple, Callable, TypeVar, Iterable, Generic


class Coordinate(NamedTuple):
    x: int
    y: int


T = TypeVar("T")

ParseResult = TypeVar("ParseResult")


class BaseMatrix(list[list[T]]):

    def get(self, c: Coordinate) -> T:
        if not self.in_map(c):
            raise ValueError(f"Coordinate {c} seems out of bound for matrix")
        return self[c.y][c.x]

    def all_values(self) -> Iterable[T]:
        for row in self:
            for pixel in row:
                yield pixel

    @cached_property
    def width(self):
        return len(self[0]) if self else 0

    @cached_property
    def height(self):
        return len(self) if self else 0

    def in_map(self, c: Coordinate):
        return 0 <= c.x < self.width and 0 <= c.y < self.height

    def neighbours(self, c: Coordinate, diagonal=True):
        return [
            self.get(neighbour_coordinate)
            for neighbour_coordinate in self.neighbour_coordinates(c, diagonal=diagonal)
        ]

    def neighbour_coordinates(self, c: Coordinate, diagonal=True) -> list[Coordinate]:
        """Assumption is that the matrix is of rectangular shape"""
        y, x = c.y, c.x

        def diagonal_neighbours(y, x):
            return [
                Coordinate(y=y-1, x=x-1),
                Coordinate(y=y-1, x=x+1),
                Coordinate(y=y+1, x=x-1),
                Coordinate(y=y+1, x=x+1)
            ]

        return [
            c for c in [
                Coordinate(y=y-1, x=x),
                Coordinate(y=y+1, x=x),
                Coordinate(y=y, x=x-1),
                Coordinate(y=y, x=x+1),
                *(diagonal_neighbours(y, x) if diagonal else [])
            ] if self.in_map(c)
        ]


def default_wrapper(*, value: str, _: Coordinate):
    return value


@dataclasses.dataclass(frozen=True)
class Solution:
    """A base class to inherit from when implementing solutions."""
    day: int
    year: int

    @cached_property
    def data(self):
        with open(
            pathlib.Path(__file__).parent
            / f"year_{self.year}"
            / "data"
            / f"day{self.day:02d}.txt"
        ) as f:
            data = f.read()
        return data

    @cached_property
    def lines(self):
        return [line.rstrip() for line in self.data.splitlines()]

    @cached_property
    def line(self):
        """Useful if the input contains a single line."""
        (line,) = self.lines
        return line

    @abc.abstractmethod
    def solution1(self):
        pass

    @abc.abstractmethod
    def solution2(self):
        pass

    def parse(self) -> ParseResult:
        raise NotImplementedError()

    def parse_matrix(
        self,
        *,
        start=0,
        end=None,
        wrapper: Callable[[str, Coordinate], T] = default_wrapper
    ) -> BaseMatrix[T]:
        slice_ = self.lines[start:end]
        output = [
            [wrapper(char, Coordinate(x=x, y=y)) for x, char in enumerate(row)]
            for y, row in enumerate(slice_)
        ]
        return BaseMatrix[T](output)

    @cached_property
    def parsed(self) -> ParseResult:
        """It is not mandatory to implement this function, but it helps for standardization"""
        return self.parse()
