from collections import deque
import dataclasses

from advent_of_code.solution import Solution
from advent_of_code.utils import tokenize


def cache(f):
    ccc = {}

    def wrapper(a: deque[str], b: int, c: tuple[int, ...]):
        if (t := tuple((*a, b, *c))) not in ccc:
            ccc[t] = f(a, b, c)
        return ccc[t]

    return wrapper


@cache
def recurse(remains: str, current_group: int = 0, groups: tuple[int, ...] = ()) -> int:
    if not remains:
        return (current_group,) == groups or (current_group == 0 and not groups)

    if not groups:
        return current_group == 0 and remains.count("#") == 0

    c, *remains = remains
    wished_group, *other_groups = groups
    if c == "#":
        current_group += 1
        if current_group > wished_group:
            return 0
    elif c == "." and current_group != 0:
        if current_group == wished_group:
            groups = tuple(other_groups)
            current_group = 0
        else:
            return 0

    match c:
        case "?":
            return recurse(["#", *remains], current_group, groups) + recurse([".", *remains], current_group, groups)
        case _:
            return recurse(remains, current_group, groups)


@dataclasses.dataclass
class SpringRow:
    row: str
    groups: list[int]

    @classmethod
    def parse(cls, r: str):
        row, g_ = r.split(" ")
        groups = tuple(tokenize(g_, int, ","))
        return cls(row=row, groups=groups)

    def inflate(self) -> "SpringRow":
        return SpringRow(
            row="?".join([self.row] * 5), groups=[*self.groups, *self.groups, *self.groups, *self.groups, *self.groups]
        )


class Day12(Solution):
    def parse(self):
        return [SpringRow.parse(r) for r in self.lines]

    def solution1(self):
        total = 0
        for sr in self.parsed:
            total += recurse(deque(sr.row), 0, sr.groups)
        return total

    def solution2(self):
        total = 0
        for sr in self.parsed:
            sr = sr.inflate()
            total += recurse(deque(sr.row), 0, sr.groups)
        return total


class Day12Test(Day12):
    @property
    def data(self):
        return """???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1"""
