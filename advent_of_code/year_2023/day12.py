import dataclasses
import itertools

from advent_of_code.solution import Solution
from advent_of_code.utils import tokenize

def mutations(row: str):
    for g, l in itertools.groupby(row):
        print(g, list(l))

@dataclasses.dataclass
class SpringRow:
    row: str
    groups: list[int]

    @classmethod
    def parse(cls, r: str):
        row, g_ = r.split(" ")
        groups = tokenize(g_, int, ",")
        return cls(row=row, groups=groups)


class Day12(Solution):
    def parse(self):
        return [
            SpringRow.parse(r) for r in self.lines
        ]

    def solution1(self):
        for sr in self.parsed:
            mutations(sr.row)
        return "not implemented"

    def solution2(self):
        return "not implemented"
