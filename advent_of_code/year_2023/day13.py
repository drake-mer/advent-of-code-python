import dataclasses
import math
from functools import cached_property

from advent_of_code.solution import Solution


def to_int(row: str):
    return int(row.replace("#", "0").replace(".", "1"), 2)


def possible_rows(pattern: "Pattern"):
    for pos, number in enumerate(pattern.rows):
        nb_col = pattern.width
        for k in range(0, nb_col):
            rows = list(pattern.rows)
            cols = list(pattern.columns)
            rows[pos] ^= 2**k
            cols[k] ^= 2**pos
            yield Pattern(rows=rows, columns=cols)


def find_symmetry_axis(row: list[int]):
    for k in range(0, len(row)):
        s1, s2 = row[:k], row[k:]
        if len(s1) % 2 == 0 and s1 and s1[::-1] == s1:
            return len(s1) // 2
        if len(s2) % 2 == 0 and s2 and s2[::-1] == s2:
            return k + len(s2) // 2


@dataclasses.dataclass
class Pattern:
    rows: list[int]
    columns: list[int]

    @cached_property
    def width(self):
        return len(self.columns)

    @cached_property
    def height(self):
        return len(self.rows)

    @classmethod
    def parse(cls, lines: list[str]):
        return cls(rows=[to_int(line) for line in lines], columns=[to_int("".join(col)) for col in zip(*lines)])

    @cached_property
    def x_axis(self):
        return find_symmetry_axis(self.columns)

    @cached_property
    def y_axis(self):
        return find_symmetry_axis(self.rows)

    @property
    def score(self):
        if self.x_axis:
            return self.x_axis
        elif self.y_axis:
            return 100 * self.y_axis
        else:
            return 0

    def expand(self):
        yield from possible_rows(self)


class Day13(Solution):
    def parse(self):
        output: list[Pattern] = []
        current_matrix: list[str] = []
        for line in self.lines:
            if not line:
                output.append(Pattern.parse(current_matrix))
                current_matrix = []
                continue
            current_matrix.append(line)
        else:
            output.append(Pattern.parse(current_matrix))
        return output

    def solution1(self):
        return sum(pat.score for pat in self.parsed)

    def solution2(self):
        return sum(pattern.score for pat in self.parsed for pattern in pat.expand())


class Day13Test(Day13):
    @property
    def data(self):
        return """#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"""
