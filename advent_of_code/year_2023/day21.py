import enum
from functools import cached_property

from advent_of_code.datastructures.dimension2 import BaseMatrix, Coordinate
from advent_of_code.solution import Solution


class MapElement(str, enum.Enum):
    Plot = "."
    Start = "S"
    Rock = "#"


def wrapper(c: str, coord: Coordinate):
    return MapElement(c), coord


def bfs(map_: BaseMatrix, start: Coordinate = None, starting_points=None, depth=0, max_depth=64):
    """
    c: starting point
    depth: number of iterations
    """
    if depth == max_depth:
        return starting_points
    if starting_points is None:
        starting_points = {start: depth}
    next_nodes = {}
    while starting_points:
        node, depth = starting_points.popitem()
        for neighbour in map_.neighbour_coordinates(node, diagonal=False):
            (neighbour_val, coordinate) = map_[neighbour]
            if neighbour_val == MapElement.Rock:
                continue
            if coordinate in next_nodes:
                continue
            next_nodes[coordinate] = depth + 1

    return bfs(map_, starting_points=next_nodes, depth=depth + 1)


class Garden(BaseMatrix):
    @cached_property
    def starting_point(self):
        for val, coord in self.all_values():
            if val == MapElement.Start:
                return coord
        else:
            raise ValueError("could not find the starting point")


class Day21(Solution):
    def parse(self) -> Garden:
        return Garden.parse_matrix(data=self.lines, wrapper=wrapper)

    def solution1(self):
        return len(set(bfs(self.parsed, self.parsed.starting_point)))

    def solution2(self):
        return "not implemented"


class Day21Test(Day21):
    @property
    def data(self):
        return """...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........."""
