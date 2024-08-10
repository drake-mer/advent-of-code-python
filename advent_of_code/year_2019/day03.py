import dataclasses

from advent_of_code.datastructures.dimension2 import Coordinate, Direction, Map2D
from advent_of_code.solution import Solution


def from_string(c):
    match c:
        case "U":
            return Direction.UP
        case "D":
            return Direction.DOWN
        case "L":
            return Direction.LEFT
        case "R":
            return Direction.RIGHT


@dataclasses.dataclass
class Vector:
    direction: Direction
    steps: int

    @classmethod
    def from_string(cls, d: str):
        steps = int(d[1:])
        direction = from_string(d[0])
        return cls(direction=direction, steps=steps)


class Day03(Solution):
    def solution1(self):
        wire_one, wire_two = self.parsed
        intersections = set(wire_one) & set(wire_two)
        return min((c.x + c.y) for c in intersections)

    def solution2(self):
        wire_one, wire_two = self.parsed
        intersections = set(wire_one) & set(wire_two)
        return min(wire_one[c] + wire_two[c] for c in intersections)

    @staticmethod
    def mark_map(wire: list[Vector], default_value=1):
        new_map = Map2D()
        coordinate = Coordinate(0, 0)
        start = 0
        for vector in wire:
            for _ in range(vector.steps):
                start += 1
                coordinate += vector.direction
                if coordinate in new_map:
                    continue
                new_map[coordinate] = start
        return new_map

    def parse(self):
        wire_one, wire_two = self.data.splitlines()
        wire_one = [Vector.from_string(vec) for vec in wire_one.split(",")]
        wire_two = [Vector.from_string(vec) for vec in wire_two.split(",")]
        map_one = self.mark_map(wire_one)
        map_two = self.mark_map(wire_two)
        print(f"len map 1, {len(map_one)}")
        print(f"len map 2, {len(map_two)}")
        return map_one, map_two
