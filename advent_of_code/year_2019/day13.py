import dataclasses
import enum
import random

from advent_of_code.datastructures.dimension2 import Coordinate
from advent_of_code.datastructures.dimension2 import Map2D
from advent_of_code.solution import Solution
from advent_of_code.year_2019.day05 import ComputerMemory, OpCode


class TileType(int, enum.Enum):
    empty = 0
    wall = 1
    block = 2
    paddle = 3
    ball = 4


@dataclasses.dataclass
class Tile:
    kind: TileType
    coordinate: Coordinate


def tile_to_ascii(t: TileType):
    match t:
        case TileType.empty:
            return " "
        case TileType.wall:
            return "X"
        case TileType.ball:
            return "o"
        case TileType.block:
            return "+"
        case TileType.paddle:
            return "_"


class ArcadeCabinet(ComputerMemory):
    def __init__(self, input_instructions: list[str], input_buffer=None):
        super().__init__(input_instructions + [0] * 10_000, input_buffer=input_buffer)
        self.map = Map2D(default=TileType.empty)
        self.ball = None
        self.paddle = None
        self.score = 0

    def __repr__(self):
        max_x = max(c.x for c in self.map)
        max_y = max(c.y for c in self.map)
        output = []
        for y in range(0, max_y + 1):
            output.append("".join(tile_to_ascii(self.map[Coordinate(x, y)]) for x in range(0, max_x + 1)))
        return "\n".join(output)

    def update_tile(self, tile_type: TileType | int, coordinate: Coordinate):
        if coordinate == Coordinate(-1, 0):
            self.score = tile_type
            if any(t == TileType.block for t in self.map.values()):
                return
            else:
                self[self.position] = OpCode.HALT
        tile = Tile(TileType(tile_type), coordinate)
        self.map[tile.coordinate] = tile.kind
        match tile.kind:
            case TileType.ball:
                self.ball = tile.coordinate
            case TileType.paddle:
                self.paddle = tile.coordinate
        # print(repr(self))

    def tiles(self) -> list[Tile]:
        return list(self.map.values())

    def output_value(self, value):
        super().output_value(value)
        if len(self.output_buffer) == 3:
            x, y, t = self.output_buffer
            self.output_buffer = []
            self.update_tile(t, Coordinate(x, y))

    def get_input(self):
        if self.ball.x < self.paddle.x:
            return -1
        elif self.ball.x == self.paddle.x:
            return 0
        else:
            return 1


class Day13(Solution):
    def parse(self):
        return ArcadeCabinet(self.line.split(","))

    def solution1(self):
        computer: ArcadeCabinet = self.parsed
        computer.run_program()
        print(repr(computer))
        return computer.tiles().count(TileType.block)

    def solution2(self):
        input_program = self.line.split(",")
        input_program[0] = 2
        cabinet = ArcadeCabinet(input_program)
        cabinet.run_program()
        return cabinet.score



class Day13Test(Day13):
    @property
    def data(self):
        return """"""
