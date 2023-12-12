from dataclasses import dataclass
from functools import cached_property
from typing import Iterable

from advent_of_code.solution import Solution
from advent_of_code.utils import tokenize


@dataclass
class Sequence:
    sequence: list[int]

    @cached_property
    def is_final(self):
        return all(s == 0 for s in self.sequence)

    @property
    def next_sequence(self) -> "Sequence":
        return Sequence(sequence=[(v - k) for k, v in zip(self.sequence, self.sequence[1:])])


def all_sequences(seq: Sequence) -> Iterable[Sequence]:
    yield seq
    while not seq.is_final:
        seq = seq.next_sequence
        yield seq


def extrapolate(seq: Sequence) -> int:
    sequence_list = list(all_sequences(seq))
    for pos, sequence in enumerate(sequence_list[::-1]):
        if pos == 0:
            assert all(s == 0 for s in sequence.sequence)
            sequence.sequence.append(0)
        else:
            sequence.sequence.append(sequence_list[-pos].sequence[-1] + sequence.sequence[-1])
    else:
        return sequence_list[0].sequence[-1]


def extrapolate_v2(seq: Sequence) -> int:
    sequence_list = list(all_sequences(seq))
    for pos, sequence in enumerate(sequence_list[::-1]):
        if pos == 0:
            assert all(s == 0 for s in sequence.sequence)
            sequence.sequence = [0, *sequence.sequence]
        else:
            n = sequence.sequence[0] - sequence_list[-pos].sequence[0]
            sequence.sequence = [n, *sequence.sequence]
    else:
        return sequence_list[0].sequence[0]


class Day09(Solution):
    def parse(self):
        return [Sequence(tokenize(raw_line, wrapper=int)) for raw_line in self.lines]

    def solution1(self):
        return sum(extrapolate(seq) for seq in self.parse())

    def solution2(self):
        return sum(extrapolate_v2(seq) for seq in self.parse())


class Day09Test(Day09):
    @property
    def data(self):
        return """0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"""
