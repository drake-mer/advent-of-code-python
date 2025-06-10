import dataclasses
from collections import defaultdict

from advent_of_code.solution import Solution


@dataclasses.dataclass(frozen=True)
class Pair:
    lower: int
    higher: int


class OrderMapping(dict):
    pass


class Day05(Solution):
    def parse(self):
        series = []
        pairs = []
        page_mode = False

        for row in self.lines:
            if not row.strip():
                page_mode = True
                continue

            if page_mode:
                series.append([int(a) for a in row.strip().split(",")])
            else:
                a, b = row.split("|")
                pairs.append(Pair(int(a), int(b)))
        return pairs, series

    def solution1(self):
        pairs, series = self.parsed
        upper_bounds = defaultdict(set)
        for pair in pairs:
            upper_bounds[pair.lower].add(pair.higher)

        def is_lower(a, b, bounds: dict[int, set[int]]):
            if b in bounds[a]:
                return True 
            return any(
                is_lower(ap, b, bounds) for ap in bounds[a]
            )
        for s in series:
            for x, y in zip(s, s[1:]):
                if not is_lower(x, y, upper_bounds):
                    print("series is not in order", s)
                    break
            print("series is in order", s)
        return "not implemented"

    def solution2(self):
        return "not implemented"


class Day05Test(Day05):
    @property
    def data(self):
        return """"""
