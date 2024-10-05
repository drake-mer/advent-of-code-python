import itertools

from advent_of_code.solution import Solution
from advent_of_code.year_2019.day05 import ComputerMemory, compute_solution


def thruster_output(program: list[str], init_phase: list[int]):
    signal = 0
    for k in range(5):
        amplifier = ComputerMemory(list(program), input_buffer=[init_phase[k], signal])
        signal = compute_solution(amplifier)[-1]
    return signal


class Day08(Solution):
    def parse(self):
        return self.line.split(",")

    def solution1(self):
        return max(
            thruster_output(self.parsed, init_phase=list(phase)) for phase in itertools.combinations(range(5), 5)
        )

    def solution2(self):
        return "not implemented"
