import enum

from advent_of_code.datastructures.dimension2 import (
    Coordinate,
    Direction,
    turn_left,
    turn_right,
)
from advent_of_code.datastructures.dimensionN import GenericMap
from advent_of_code.solution import Solution
from advent_of_code.year_2019.day05 import ComputerMemory


class Pixel(int, enum.Enum):
    white = 1
    black = 0


class Rotate(int, enum.Enum):
    left = 0
    right = 1


class Action(int, enum.Enum):
    paint = 0
    move = 1


class SpaceHull[Coordinate, Pixel](GenericMap):
    pass


class PaintingRobot(ComputerMemory):
    def __init__(self, instructions: list[str], hull: SpaceHull = None):
        if hull is None:
            raise ValueError("we need a hull")
        self.robot_position = Coordinate(0, 0)
        super().__init__(instructions + ['0'] * 1200)
        self.hull: SpaceHull = hull
        self.action = Action.paint
        self.direction = Direction.UP

    def store_input(self, arg_1: str | int = None):
        assert not self.input_buffer
        self.input_buffer = [self.hull[self.robot_position]]
        super().store_input(arg_1)

    def output_value(self, value):
        super().output_value(value)
        match self.action:
            case Action.paint:
                self.hull[self.robot_position] = Pixel(int(value))
                self.action = Action.move
            case Action.move:
                match Rotate(value):
                    case Rotate.left:
                        self.direction = turn_left(self.direction)
                    case Rotate.right:
                        self.direction = turn_right(self.direction)
                self.robot_position = self.robot_position + self.direction
                self.action = Action.paint


class Day11(Solution):
    def parse(self):
        return self.line.split(",")

    def solution1(self):
        spacecraft_hull = SpaceHull(default=Pixel.black)
        program = PaintingRobot(self.parsed, hull=spacecraft_hull)
        program.run_program()
        return len(program.hull)

    def solution2(self):
        spacecraft_hull = SpaceHull(default=Pixel.black)
        spacecraft_hull[Coordinate(0, 0)] = Pixel.white
        program = PaintingRobot(self.parsed, hull=spacecraft_hull)
        program.run_program()
        max_x = max(c.x for c in program.hull)
        min_x = min(c.x for c in program.hull)
        min_y = min(c.y for c in program.hull)
        max_y = max(c.y for c in program.hull)
        output = []
        for y in range(min_y, max_y + 1):
            output.append(
                "".join(
                    "*" if program.hull[Coordinate(x, y)] == Pixel.white else " "
                    for x in range(min_x, max_x + 1)
                )
            )

        print("\n".join(output))


class Day11Test(Day11):
    @property
    def data(self):
        return """"""
