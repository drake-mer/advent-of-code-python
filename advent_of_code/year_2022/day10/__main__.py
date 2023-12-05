import dataclasses
import pathlib
from enum import Enum
from typing import Iterable

basedir = pathlib.Path(__file__).parent


class OpCode(str, Enum):
    noop = "noop"
    addx = "addx"


@dataclasses.dataclass(frozen=True)
class Instruction:
    opcode: OpCode
    operand: int = None


def cycles(op: OpCode):
    match op:
        case op.noop:
            return 1
        case op.addx:
            return 2
        case _:
            raise ValueError()


@dataclasses.dataclass
class State:
    register: int = 1
    cycle: int = 1

    def update(self, instruction: Instruction) -> Iterable["State"]:
        yield State(self.register, self.cycle + 1)
        if instruction.opcode == OpCode.addx:
            yield State(self.register + instruction.operand, self.cycle + 2)


class Screen(list[str]):
    def __new__(cls, val: Iterable):
        output = list()
        for k in range(6):
            output.append("".join(val[k * 40 : 40 + k * 40]))
        return output


def all_cycles(state: State, instructions: list[Instruction]):
    yield state
    for inst in instructions:
        for state in state.update(inst):
            yield state


def munch_data(payload: str) -> list[Instruction]:
    all_lines = []
    for _, line in enumerate(payload.splitlines(keepends=False)):
        op, *_ = line.split()
        op = OpCode(op)
        match op:
            case OpCode.noop:
                all_lines.append(Instruction(op))
            case OpCode.addx:
                all_lines.append(Instruction(op, int(line.split()[-1])))
            case _:
                raise ValueError
    return all_lines


def day_1_first_puzzle(payload):
    return sum(
        {
            state.cycle: (state.cycle * state.register)
            for state in all_cycles(State(), munch_data(payload))
            if state.cycle in {20, 60, 100, 140, 180, 220}
        }.values(),
    )


def day_1_second_puzzle(payload):
    return "\n".join(
        Screen(
            [
                "#" if (pixel % 40) in (state.register - 1, state.register, state.register + 1) else "."
                for pixel, state in enumerate(all_cycles(State(), munch_data(payload)))
                if pixel < 240
            ],
        ),
    )


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
