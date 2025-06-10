import dataclasses
import enum
from functools import cached_property
from typing import Callable, Generic, Iterable, NamedTuple, TypeVar

from .dimensionN import GenericMap

C = TypeVar("C")
T = TypeVar("T")


class Coordinate(NamedTuple):
    x: int
    y: int

    def __add__(self, other):
        return Coordinate(self.x + other.x, self.y + other.y)

    def __sub__(self, other):
        return Coordinate(self.x - other.x, self.y - other.y)

    def __mul__(self, factor: int):
        return Coordinate(self.x * factor, self.y * factor)

    def rotate_pi_over_2(self):
        return Coordinate(self.y, -self.x)

    def manhattan(self, other: "Coordinate"):
        x, y = self
        ox, oy = other
        return abs(ox - x) + abs(oy - y)

    def scalar(self, other: "Coordinate"):
        return self.x * other.x + self.y * other.y


def default_wrapper(*, value: str, _: Coordinate):
    return value


def discrete_orientations(diagonal=False):
    for c in [Coordinate(0, 1), Coordinate(1, 0), Coordinate(0, -1), Coordinate(-1, 0)]:
        yield c
    if diagonal:
        for c in [
            Coordinate(-1, -1),
            Coordinate(-1, 1),
            Coordinate(1, 1),
            Coordinate(1, -1),
        ]:
            yield c


def adjacent_coordinates(c: Coordinate, diagonal=False) -> Iterable[Coordinate]:
    for direction in discrete_orientations(diagonal=diagonal):
        yield c + direction


@dataclasses.dataclass
class BaseMatrix(Generic[T]):
    content: list[list[T]]

    def __getitem__(self, c: Coordinate) -> T:
        if not self.in_map(c):
            raise ValueError(f"Coordinate {c} seems out of bound for matrix")
        return self.content[c.y][c.x]

    def __setitem__(self, c: Coordinate, item: T):
        if not self.in_map(c):
            raise ValueError(f"Coordinate {c} is out of bound for this matrix")
        self.content[c.y][c.x] = item

    def __contains__(self, item: Coordinate):
        return self.in_map(item)

    def pretty_print(self, translate_function: Callable[[T], str]) -> str:
        output = []
        for y, row in enumerate(self.rows):
            new_row = "".join(translate_function(c) for c in row)
            output.append(new_row)
        return "\n".join(output)

    def all_values(self) -> Iterable[T]:
        for row in self.rows:
            for pixel in row:
                yield pixel

    @property
    def columns(self):
        return [list(c) for c in zip(*self.rows)]

    def find(self, value: T):
        for x in range(self.width):
            for y in range(self.height):
                if self[c := Coordinate(x, y)] == value:
                    return c

    @cached_property
    def width(self):
        return len(self.content[0]) if self else 0

    @cached_property
    def height(self):
        return len(self.content) if self else 0

    @property
    def rows(self) -> list[list[T]]:
        return list(self.content)

    def in_map(self, c: Coordinate):
        return 0 <= c.x < self.width and 0 <= c.y < self.height

    def neighbours(self, c: Coordinate, diagonal=True) -> Iterable[Coordinate]:
        return [
            neighbour_coordinate
            for neighbour_coordinate in self.neighbour_coordinates(c, diagonal=diagonal)
        ]

    def neighbour_coordinates(self, c: Coordinate, diagonal=True) -> list[Coordinate]:
        """Assumption is that the matrix is of rectangular shape"""
        y, x = c.y, c.x

        return [
            c
            for c in [
                Coordinate(y=y - 1, x=x),
                Coordinate(y=y + 1, x=x),
                Coordinate(y=y, x=x - 1),
                Coordinate(y=y, x=x + 1),
                *(
                    [
                        Coordinate(y=y - 1, x=x - 1),
                        Coordinate(y=y - 1, x=x + 1),
                        Coordinate(y=y + 1, x=x - 1),
                        Coordinate(y=y + 1, x=x + 1),
                    ]
                    if diagonal
                    else []
                ),
            ]
            if self.in_map(c)
        ]

    def flood_fill(
        self,
        coordinate: Coordinate,
        accumulator: set[Coordinate] = None,
        is_boundary: Callable[[Coordinate], bool] = None,
        diagonal: bool = False,  # should the flood fill use diagonal (most of the time it's not necessary)
    ) -> set[Coordinate]:
        if is_boundary is None:
            is_boundary = self.in_map

        if accumulator is None:
            accumulator = set()

        if is_boundary(coordinate):
            return accumulator

        accumulator.add(coordinate)
        pixels_to_check = set(
            c
            for c in self.neighbours(coordinate, diagonal=diagonal)
            if not is_boundary(c) and c not in accumulator
        )
        while pixels_to_check:
            next_pixel = pixels_to_check.pop()
            accumulator.add(next_pixel)
            pixels_to_check.update(
                c
                for c in self.neighbours(next_pixel, diagonal=diagonal)
                if not is_boundary(c) and c not in accumulator
            )
        return accumulator

    @classmethod
    def parse_matrix(
        cls,
        *,
        data: list[str] = None,
        start=0,
        end=None,
        wrapper: Callable[[str, Coordinate], T] = default_wrapper,
    ) -> "BaseMatrix":
        slice_ = data[start:end]
        output = [
            [wrapper(char, Coordinate(x=x, y=y)) for x, char in enumerate(row)]
            for y, row in enumerate(slice_)
        ]
        return cls(content=output)


class Map2D(GenericMap[Coordinate, T]):
    pass


class Direction(Coordinate, enum.Enum):
    UP = (0, -1)
    DOWN = (0, 1)
    LEFT = (-1, 0)
    RIGHT = (1, 0)


def turn_left(direction: Direction):
    return Coordinate(direction.y, -direction.x)


def turn_right(direction: Direction):
    return Coordinate(-direction.y, direction.x)
