import collections
import dataclasses
import enum
from functools import cached_property

from advent_of_code.datastructures import BaseMatrix, Coordinate
from advent_of_code.solution import Solution

North = Coordinate(0, -1)
South = Coordinate(0, 1)
East = Coordinate(1, 0)
West = Coordinate(-1, 0)


class Tile(str, enum.Enum):
    SE = "F"
    SW = "7"
    NE = "L"
    NW = "J"
    EW = "-"
    NS = "|"
    G = "."
    S = "S"


def format_char(c: str):
    return c


AsciiMapping = {
    k: format_char(v)
    for k, v in {
        Tile.SE: "\u250C",
        Tile.SW: "\u2510",
        Tile.NS: "\u2502",
        Tile.EW: "\u2500",
        Tile.NE: "\u2514",
        Tile.NW: "\u2518",
        Tile.S: "\u2749",
        Tile.G: "\ue25A2",
    }.items()
}


@dataclasses.dataclass(frozen=True)
class Pixel:
    tile: Tile
    coordinate: Coordinate = None
    distance: int = None

    @property
    def is_pipe(self):
        return self.tile not in (Tile.G, Tile.S)

    def update_distance(self, distance: int):
        return Pixel(
            tile=self.tile,
            coordinate=self.coordinate,
            distance=distance,
        )

    def __hash__(self):
        return (self.tile, self.coordinate).__hash__()

    def on_edge(self, g: "HuntingGround") -> bool:
        h, w = g.height, g.width
        x, y = self.coordinate
        return x == (w - 1) or x == 0 or y == (h - 1) or y == 0

    def on_path(self, g: "HuntingGround") -> bool:
        return self in g.cached_path

    @cached_property
    def connecting(self) -> tuple[Coordinate, Coordinate]:
        match self.tile:
            case Tile.NS:
                delta = North, South
            case Tile.EW:
                delta = East, West
            case Tile.NE:
                delta = North, East
            case Tile.SE:
                delta = South, East
            case Tile.NW:
                delta = North, West
            case Tile.SW:
                delta = South, West
            case _:
                raise ValueError(f"problem at {self.coordinate}")
        d1, d2 = delta
        return self.coordinate + d1, self.coordinate + d2

    def __eq__(self, other):
        return self.tile == other.tile


class HuntingGround(BaseMatrix[Pixel]):
    @cached_property
    def cached_path(self) -> dict[Pixel, int]:
        return self.mark_distance()

    @cached_property
    def start_point(self) -> Pixel:
        return [p for p in self.cached_path if p.tile == Tile.S][0]

    @property
    def loop(self):
        return list(self.cached_path)

    def mark_distance(self) -> dict[Pixel, int]:
        distance = 0
        starting_point = self[self.find(Pixel(Tile.S))]
        possible_starts = [
            self[c]
            for c in self.neighbours(starting_point.coordinate, diagonal=False)
            if self[c].is_pipe and any([n == starting_point.coordinate for n in self[c].connecting])
        ]
        visited_: dict[Pixel, int] = dict()
        visited_[starting_point.update_distance(0)] = distance
        to_visit: collections.deque[Pixel] = collections.deque()
        to_visit.extend(
            [square.update_distance(distance + 1) for square in possible_starts if square not in visited_][:1]
        )
        while to_visit:
            next_node = to_visit.popleft()
            if next_node in visited_:
                assert visited_[next_node] <= next_node.distance
                continue
            visited_[next_node] = next_node.distance
            to_visit.extend(
                [
                    self[c].update_distance(next_node.distance + 1)
                    for c in next_node.connecting
                    if self[c] not in visited_
                ][:1]
            )
        return visited_


class Day10(Solution):
    def parse(self) -> HuntingGround:
        game = HuntingGround.parse_matrix(data=self.lines, wrapper=lambda c, coord: Pixel(Tile(c), coord))
        return game

    def path(self):
        game = self.parsed
        return game.cached_path

    def solution1(self):
        full_path_length = max(self.path().values())
        return full_path_length // 2 + full_path_length % 2

    def solution2(self):
        game: HuntingGround = self.parse()
        path = game.mark_distance()
        nodes = [p.coordinate for p in path]
        # cheat code
        # so my original idea was to find the direction of the path using a derivative
        # rotate by pi/2 this direction to find the inner pixels; flood fill the neighbours
        # found in the inner pixels.
        # The idea was sensible because it gave me 440 instead of 445 that is the real answer. I probably forgot some
        # edge case in my computation of the derivatives. Here matplotlib is doing the heavy lifting
        # I'll just leave it at that.
        import matplotlib

        polygon = matplotlib.path.Path(nodes)
        nodes = set(nodes)
        return sum(
            1
            for y in range(1, game.height)
            for x in range(1, game.width)
            if (c := (y, x)) not in nodes and polygon.contains_point(c)
        )


class Day10Test(Day10):
    @cached_property
    def data(self):
        return """FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"""
