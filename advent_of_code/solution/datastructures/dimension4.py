import dataclasses
import itertools
from typing import Any, Iterable, NamedTuple, TypeAlias, TypeVar

from .dimensionN import GenericMap

T: TypeAlias = TypeVar("T")


class Coordinate(NamedTuple):
    x: int
    y: int
    z: int
    w: int

    def __add__(self, other):
        return Coordinate(
            x=self.x + other.x,
            y=self.y + other.y,
            z=self.z + other.z,
            w=self.w + other.w,
        )

    def neighbours(self) -> Iterable["Coordinate"]:
        for delta_c in itertools.product((-1, 0, 1), (-1, 0, 1), (-1, 0, 1), (-1, 0, 1)):
            if delta_c == (0, 0, 0, 0):
                continue
            yield self + Coordinate(*delta_c)


@dataclasses.dataclass
class Map4D(GenericMap[Coordinate, T]):
    pass
