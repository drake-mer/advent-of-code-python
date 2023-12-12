import dataclasses
import functools

from advent_of_code.solution import Solution


@dataclasses.dataclass
class Game:
    groups: list[list[tuple]]


class Day06(Solution):
    def parse(self):
        all_groups = []
        current_group = []
        for line in self.lines:
            if not line:
                all_groups.append(current_group)
                current_group = []
                continue
            current_group.append(tuple(line))
        else:
            all_groups.append(current_group)
        return Game(all_groups)

    def answer_generic(self, fun=None):
        return sum(len(functools.reduce(fun, (set(person) for person in group))) for group in self.parsed.groups)

    def solution1(self):
        return self.answer_generic(lambda p1, p2: p1 | p2)

    def solution2(self):
        return self.answer_generic(lambda p1, p2: p1 & p2)


class Day06Test(Day06):
    @property
    def data(self):
        return """abc

a
b
c

ab
ac

a
a
a
a

b"""
