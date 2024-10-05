import enum
import itertools

from advent_of_code.solution import Solution


class OpCode(int, enum.Enum):
    ADD = 1
    MULTIPLY = 2
    HALT = 99


class Position(int):
    pass


class ReturnCode(int, enum.Enum):
    OK = True
    STOP = False


def apply(
    op: OpCode, arg1: Position, arg2: Position, arg3: Position, buffer: list[int]
):
    match op:
        case OpCode.ADD:
            buffer[arg3] = buffer[arg2] + buffer[arg1]
            return ReturnCode.OK
        case OpCode.MULTIPLY:
            buffer[arg3] = buffer[arg2] * buffer[arg1]
            return ReturnCode.OK
        case OpCode.HALT:
            return ReturnCode.STOP
        case _:
            raise ValueError(f"Operation {op} is not supported")


class Day02(Solution):
    def parse(self):
        return list(map(int, self.line.split(",")))

    def solution1(self):
        return self.compute_solution(
            self.parse(),
        )

    def compute_solution(self, parsed: list[int], init_1=12, init_2=2):
        parsed[1] = init_1
        parsed[2] = init_2
        for k in range(len(parsed) // 4):
            (op, arg1, arg2, arg3) = parsed[k * 4 : k * 4 + 4]
            match (
                res := apply(
                    OpCode(op), Position(arg1), Position(arg2), Position(arg3), parsed
                )
            ):
                case ReturnCode.OK:
                    continue
                case ReturnCode.STOP:
                    break
        return parsed[0]

    def solution2(self):
        for k, l in itertools.product(range(100), range(100)):
            if self.compute_solution(self.parse(), init_1=k, init_2=l) == 19690720:
                return k * 100 + l
