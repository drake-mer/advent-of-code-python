import itertools

from advent_of_code.solution import Solution
from advent_of_code.year_2019.day05 import ComputerMemory


def thruster_output(program: list[str], init_phase: list[int]):
    signal = 0
    for k in range(5):
        amplifier = ComputerMemory(list(program), input_buffer=[init_phase[k], signal])
        signal = amplifier.run_program()[-1]
    return signal


class ThrustComputer(ComputerMemory):
    def output_value(self, value):
        super().output_value(value)
        raise AmplifierOutputGenerated(value=value)


def max_thrust_feedback_loop(program: list[int | str], phase_vector: list[int]):
    amplifiers = [
        ThrustComputer(program, input_buffer=[phase_vector[k]]) for k in range(5)
    ]
    sig = 0
    for k, comp in itertools.cycle(zip(range(5), amplifiers)):
        comp.input_buffer.append(sig)
        try:
            comp.run_program()
        except AmplifierOutputGenerated as e:
            sig = e.value
        else:
            return amplifiers[-1].output_buffer[-1]


class Day07(Solution):
    def parse(self):
        return self.line.split(",")

    def solution1(self):
        return max(
            thruster_output(self.parsed, init_phase=list(phase))
            for phase in itertools.permutations(range(5))
        )

    def solution2(self):
        return max(
            max_thrust_feedback_loop(self.parsed, list(phase_vector))
            for phase_vector in itertools.permutations(range(5, 10))
        )


class Day07Test(Day07):
    @property
    def parsed(self):
        return [
            3,
            26,
            1001,
            26,
            -4,
            26,
            3,
            27,
            1002,
            27,
            2,
            27,
            1,
            27,
            26,
            27,
            4,
            27,
            1001,
            28,
            -1,
            28,
            1005,
            28,
            6,
            99,
            0,
            0,
            5,
        ]


class AmplifierOutputGenerated(Exception):
    def __init__(self, value):
        self.value = value
