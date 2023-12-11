from functools import cached_property
from typing import Callable, Iterable, NamedTuple, TypeVar

T = TypeVar("T")


class Coordinate(NamedTuple):
    x: int
    y: int

    def __sub__(self, other):
        return Coordinate(self.x - other.x, self.y - other.y)

    def __add__(self, other):
        return Coordinate(self.x + other.x, self.y + other.y)

    def manhattan(self, other: "Coordinate"):
        x, y = self
        ox, oy = other
        return abs(ox - x) + abs(oy - y)


def default_wrapper(*, value: str, _: Coordinate):
    return value


class BaseMatrix(list[list[T]]):
    def get(self, c: Coordinate) -> T:
        if not self.in_map(c):
            raise ValueError(f"Coordinate {c} seems out of bound for matrix")
        return self[c.y][c.x]

    def display(self):
        return "\n".join("".join([str(c) for c in row]) for row in self.rows)

    @property
    def rows(self):
        for row in self:
            yield row

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
            self.get(neighbour_coordinate) for neighbour_coordinate in self.neighbour_coordinates(c, diagonal=diagonal)
        ]

    def neighbour_coordinates(self, c: Coordinate, diagonal=True) -> list[Coordinate]:
        """Assumption is that the matrix is of rectangular shape"""
        y, x = c.y, c.x

        def diagonal_neighbours(y, x):
            return [
                Coordinate(y=y - 1, x=x - 1),
                Coordinate(y=y - 1, x=x + 1),
                Coordinate(y=y + 1, x=x - 1),
                Coordinate(y=y + 1, x=x + 1),
            ]

        return [
            c
            for c in [
                Coordinate(y=y - 1, x=x),
                Coordinate(y=y + 1, x=x),
                Coordinate(y=y, x=x - 1),
                Coordinate(y=y, x=x + 1),
                *(diagonal_neighbours(y, x) if diagonal else []),
            ]
            if self.in_map(c)
        ]

    @staticmethod
    def parse_matrix(
        *,
        data: list[str] = None,
        start=0,
        end=None,
        wrapper: Callable[[str, Coordinate], T] = default_wrapper,
    ) -> "BaseMatrix":
        slice_ = data[start:end]
        output = [[wrapper(char, Coordinate(x=x, y=y)) for x, char in enumerate(row)] for y, row in enumerate(slice_)]
        return BaseMatrix[T](output)
