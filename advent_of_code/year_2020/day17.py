import dataclasses
from typing import Generic, Self, TypeAlias, Union

from advent_of_code.datastructures import Coordinate as Coord3D
from advent_of_code.datastructures import Coordinate as Coord4D
from advent_of_code.datastructures.dimensionN import C, GenericMap, T
from advent_of_code.solution import Solution


@dataclasses.dataclass(frozen=True)
class Empty:
    pass


@dataclasses.dataclass(frozen=True)
class Lit:
    pass


Pixel: TypeAlias = Union[Lit, Empty]


def pixel_factory(c: str) -> Pixel:
    if c == ".":
        return Empty()
    if c == "#":
        return Lit()
    raise ValueError(f"{c} is not an acceptable value")


@dataclasses.dataclass
class Game(Generic[C, T]):
    space: GenericMap[C, T]

    def cycle(self) -> Self:
        new_space = GenericMap[C, T](default=Empty(), content=({} | self.space.content))
        for pix_coordinate in self.space:
            for neighbour in pix_coordinate.neighbours():
                if neighbour not in new_space:
                    new_space[neighbour] = Empty()
        print("new map created with size", len(new_space))
        must_lit = []
        must_switch_off = []
        for pix_coordinate in new_space:
            if new_space[pix_coordinate] == Empty():
                if len(list(Game(space=new_space).lit_neighbours(pix_coordinate))) == 3:
                    must_lit.append(pix_coordinate)
            else:
                assert new_space[pix_coordinate] == Lit()
                if len(
                    list(Game(space=new_space).lit_neighbours(pix_coordinate))
                ) not in (2, 3):
                    must_switch_off.append(pix_coordinate)
        for pixel_to_switch in must_lit:
            new_space[pixel_to_switch] = Lit()
        for pixel_to_switch in must_switch_off:
            new_space[pixel_to_switch] = Empty()
        return Game(space=new_space)

    def lit_neighbours(self, c: C):
        return list(filter(lambda c_: self.space[c_] == Lit(), c.neighbours()))

    @property
    def lit_pixels(self):
        return sum(True for c in self.space if self.space[c] == Lit())


class Day17(Solution):
    tags = ("map", "3D", "4D", "geometry", "inflate", "pixel")

    def parse_4D(self) -> Game[Coord4D, Pixel]:
        game = GenericMap[Coord4D, Pixel](default=Empty())
        for y, row in enumerate(self.lines):
            for x, pix in enumerate(row):
                game[Coord4D(x, y, 0, 0)] = pixel_factory(pix)
        return Game[Coord4D, Pixel](space=game)

    def parse(self) -> Game[Coord3D, Pixel]:
        game = GenericMap[Coord3D, Pixel](default=Empty())
        for y, row in enumerate(self.lines):
            for x, pix in enumerate(row):
                game[Coord3D(x, y, 0)] = pixel_factory(pix)
        return Game(space=game)

    def solution1(self):
        game = self.parsed
        for k in range(6):
            game = game.cycle()
        return game.lit_pixels

    def solution2(self):
        game = self.parse_4D()
        for k in range(6):
            game = game.cycle()
        return game.lit_pixels


class Day17Test(Day17):
    @property
    def data(self):
        return """.#.
..#
###"""
