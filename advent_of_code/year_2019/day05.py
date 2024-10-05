import enum

from advent_of_code.solution import Solution


class OpCode(int, enum.Enum):
    ADD = 1
    MULTIPLY = 2
    INPUT = 3
    OUTPUT = 4
    JUMP_IF_TRUE = 5
    JUMP_IF_FALSE = 6
    LESS_THAN = 7
    EQUALS = 8
    HALT = 99


class Mode(str, enum.Enum):
    PARAM = "0"
    VALUE = "1"


class ReturnCode(int, enum.Enum):
    OK = True
    STOP = False


class ComputerMemory(list):
    def __init__(self, instructions: list[str], input_buffer=None):
        super().__init__(instructions)
        self.output_buffer = list()
        self.input_buffer = input_buffer
        self.position = 0

    def get_value(self, arg: str | int, mode) -> int:
        match mode:
            case Mode.PARAM:
                return int(self[int(arg)])
            case Mode.VALUE:
                return int(arg)
        raise ValueError("incorrect state in the calling function")

    def store_input(self, arg_1: str | int = None):
        arg_1 = int(arg_1, 10) if isinstance(arg_1, str) else arg_1
        if not self.input_buffer:
            val = int(input("please enter a number"))
        else:
            val, *remaining = self.input_buffer
            self.input_buffer = remaining
        self[arg_1] = val
        self.position += 2

    def output_value(self, value):
        self.output_buffer.append(value)
        self.position += 2

    def run_program(self):
        element = self[self.position]
        remains = self[self.position + 1 :]
        element = str(element)  # convert back to string
        operation = OpCode(int(element[-2:], 10))
        mode_a = mode_b = mode_c = Mode.PARAM
        if len(element[:-2]) == 1:
            mode_c = element[0]  # this is default mode
        elif len(element[:-2]) == 2:
            mode_b, mode_c = element[:-2]
        elif len(element[:-2]) == 3:
            mode_a, mode_b, mode_c = element[:-2]
        match operation:
            case OpCode.HALT:
                return self.output_buffer
            case OpCode.MULTIPLY | OpCode.ADD | OpCode.LESS_THAN | OpCode.EQUALS:
                arg_1, arg_2, arg_3, *remains = remains
                assert mode_a == Mode.PARAM
                arg_1 = self.get_value(arg_1, mode=Mode(mode_c))
                arg_2 = self.get_value(arg_2, mode=Mode(mode_b))
                arg_3 = int(arg_3)  # should always be mode PARAM
                match operation:
                    case OpCode.MULTIPLY:
                        self[arg_3] = arg_1 * arg_2
                    case OpCode.ADD:
                        self[arg_3] = arg_1 + arg_2
                    case OpCode.LESS_THAN:
                        if arg_1 < arg_2:
                            self[arg_3] = 1
                        else:
                            self[arg_3] = 0
                    case OpCode.EQUALS:
                        if arg_1 == arg_2:
                            self[arg_3] = 1
                        else:
                            self[arg_3] = 0
                self.position += 4
            case OpCode.INPUT | OpCode.OUTPUT:
                arg_1, *remains = remains
                match operation:
                    case OpCode.INPUT:
                        assert mode_c == Mode.PARAM
                        self.store_input(arg_1)
                    case OpCode.OUTPUT:
                        value = self.get_value(arg_1, mode=Mode(mode_c))
                        self.output_value(value)
            case OpCode.JUMP_IF_TRUE | OpCode.JUMP_IF_FALSE:
                arg_1, arg_2, *remains = remains
                arg_1 = self.get_value(arg_1, mode=Mode(mode_c))
                arg_2 = self.get_value(arg_2, mode=Mode(mode_b))
                if arg_1 != 0 and operation == OpCode.JUMP_IF_TRUE:
                    self.position = arg_2
                elif arg_1 == 0 and operation == OpCode.JUMP_IF_FALSE:
                    self.position = arg_2
                else:
                    self.position += 3

            case _:
                raise ValueError("unsupported case")
        return self.run_program()


class Day05(Solution):
    def parse(self) -> list[str]:
        return self.line.split(",")

    def solution1(self):
        memory = ComputerMemory(self.parsed, input_buffer=[1])
        return memory.run_program()[-1]

    def solution2(self):
        memory = ComputerMemory(self.parsed, input_buffer=[5])
        return memory.run_program()[-1]
