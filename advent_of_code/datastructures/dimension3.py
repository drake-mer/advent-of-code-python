import dataclasses
import itertools
from functools import cached_property
from typing import Any, Callable, Generic, Iterable, NamedTuple, TypeAlias, TypeVar

from .dimensionN import GenericMap

T: TypeAlias = TypeVar("T")


class Coordinate(NamedTuple):
    x: int
    y: int
    z: int

    def __add__(self, other):
        return Coordinate(x=self.x + other.x, y=self.y + other.y, z=self.z + other.z)

    def neighbours(self) -> Iterable["Coordinate"]:
        for delta_c in itertools.product((-1, 0, 1), (-1, 0, 1), (-1, 0, 1)):
            if delta_c == (0, 0, 0):
                continue
            yield self + Coordinate(*delta_c)


@dataclasses.dataclass
class Map3D(GenericMap[Coordinate, T]):
    """A generic map (dict-based)"""

    pass


@dataclasses.dataclass
class BaseMatrix(Generic[T]):
    content: list[list[list[T]]]

    def __getitem__(self, c: Coordinate) -> T:
        if not self.in_map(c):
            raise ValueError(f"Coordinate {c} seems out of bound for matrix")
        return self.content[c.z][c.y][c.x]

    def __setitem__(self, c: Coordinate, item: T):
        if not self.in_map(c):
            raise ValueError(f"Coordinate {c} is out of bound for this matrix")
        self.content[c.z][c.y][c.x] = item

    def __contains__(self, item: Coordinate):
        return self.in_map(item)

    def all_values(self) -> Iterable[T]:
        for plane in self.planes:
            for row in plane:
                for pixel in row:
                    yield pixel

    def find(self, value: T):
        for x in range(self.x_width):
            for y in range(self.y_width):
                for z in range(self.height):
                    if self[c := Coordinate(x, y, z)] == value:
                        return c

    @cached_property
    def y_width(self):
        return len(self.content[0]) if self else 0

    @cached_property
    def x_width(self):
        return len(self.content[0][0] if self else 0)

    @cached_property
    def height(self):
        return len(self.content) if self else 0

    @property
    def planes(self) -> list[list[T]]:
        return list(self.content)

    def in_map(self, c: Coordinate):
        return 0 <= c.x < self.x_width and 0 <= c.y < self.y_width and 0 <= c.z < self.height

    def neighbours(self, c: Coordinate, diagonal: bool = True) -> Iterable[Coordinate]:
        return [self[neighbour_coordinate] for neighbour_coordinate in self.neighbour_coordinates(c, diagonal=diagonal)]

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
            c for c in self.neighbours(coordinate, diagonal=diagonal) if not is_boundary(c) and c not in accumulator
        )
        while pixels_to_check:
            next_pixel = pixels_to_check.pop()
            accumulator.add(next_pixel)
            pixels_to_check.update(
                c for c in self.neighbours(next_pixel, diagonal=diagonal) if not is_boundary(c) and c not in accumulator
            )
        return accumulator

    @classmethod
    def parse_matrix(
        cls,
        *,
        data: list[list[str]] = None,
        start=0,
        end=None,
        wrapper: Callable[[str, Coordinate], T] = lambda u, v: u,
    ) -> "BaseMatrix":
        slice_ = data[start:end]
        output = [
            [[wrapper(char, Coordinate(x=x, y=y, z=z)) for x, char in enumerate(row)] for y, row in enumerate(plane)]
            for z, plane in enumerate(slice_)
        ]
        return cls(content=output)
