import enum

from advent_of_code.datastructures.dimension2 import Coordinate, Direction
from advent_of_code.solution import Solution


class Arrow(str, enum.Enum):
    up = "^"
    down = "v"
    left = "<"
    right = ">"


def arrow_to_vector(a: Arrow) -> Direction:
    match a:
        case a.up:
            return Direction.UP
        case a.down:
            return Direction.DOWN
        case a.left:
            return Direction.LEFT
        case a.right:
            return Direction.RIGHT
        case _:
            raise ValueError(f"Invalid arrow: {a}")


class Day03(Solution):
    def parse(self):
        (d,) = self.lines
        return map(Arrow, d)

    def solution1(self):
        visited = set()
        santa = Coordinate(0, 0)
        visited.add(santa)
        for pos, a in enumerate(self.parsed):
            santa += arrow_to_vector(Arrow(a))
            visited.add(santa)

        return len(visited)

    def solution2(self):
        visited = set()
        santa = Coordinate(0, 0)
        robot = Coordinate(0, 0)
        visited.add(santa)

        for pos, a in enumerate(self.parsed):
            if pos % 2 == 0:
                santa += arrow_to_vector(a)
                visited.add(santa)
            else:
                robot += arrow_to_vector(a)
                visited.add(robot)
        return len(visited)


class Day03Test(Day03):
    @property
    def data(self):
        return """"""
