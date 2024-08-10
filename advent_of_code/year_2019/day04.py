from itertools import groupby

from advent_of_code.solution import Solution


class Day04(Solution):
    def parse(self):
        return 147981, 691423

    @staticmethod
    def is_candidate(k: int) -> bool:
        return sorted(str(k)) == list(str(k)) and any(len(list(v)) >= 2 for _, v in groupby(str(k)))

    @staticmethod
    def has_group_of_two(k: int):
        return any(len(list(v)) == 2 for k, v in groupby(str(k)))

    def candidates(self):
        a, b = self.parse()
        return [k for k in range(a, b + 1) if self.is_candidate(k)]

    def solution1(self):
        return len(self.candidates())

    def solution2(self):
        return len([c for c in self.candidates() if self.has_group_of_two(c)])


class Day04Test(Day04):
    @property
    def data(self):
        return """"""
