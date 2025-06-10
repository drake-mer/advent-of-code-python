from collections import Counter

from advent_of_code.solution import Solution


class Day01(Solution):
    def parse(self):
        l1 = []
        l2 = []
        for row in self.lines:
            a, b = row.split("  ")
            a = int(a)
            l1.append(a)
            b = int(b)
            l2.append(b)
        return l1, l2

    def solution1(self):
        l1, l2 = self.parse()
        l1.sort()
        l2.sort()
        return sum(abs(b - a) for a, b in zip(l1, l2))

    def solution2(self):
        l1, l2 = self.parse()
        counted = Counter(l2)
        return sum(x * counted[x] for x in l1)


class Day01Test(Day01):
    @property
    def data(self):
        return """"""
