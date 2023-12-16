import dataclasses
import itertools

from advent_of_code.solution import Solution
from advent_of_code.solution.datastructures.dimension2 import BaseMatrix


def to_int(row: list[bool]):
    return sum(item * (2**pos) for pos, item in enumerate(row))


def find_symmetry_axis(vector: list[int]) -> list[int]:
    all_axis = []
    for k in range(0, len(vector)):
        s1, s2 = vector[:k], vector[k:]
        if len(s1) % 2 == 0 and s1 and s1[::-1] == s1:
            all_axis.append(len(s1) // 2)
        if len(s2) % 2 == 0 and s2 and s2[::-1] == s2:
            all_axis.append(k + len(s2) // 2)
    return all_axis


class Pattern(BaseMatrix[bool]):
    def flip(self, row: int, col: int):
        self.content[row][col] = not self.content[row][col]

    @property
    def col_values(self):
        for c in self.columns:
            yield to_int(c)

    @property
    def row_values(self):
        for r in self.rows:
            yield to_int(r)

    @property
    def x_axis(self):
        return find_symmetry_axis(list(self.col_values))

    @property
    def y_axis(self):
        return find_symmetry_axis(list(self.row_values))

    @property
    def score(self):
        (score,) = self.scores
        return score

    @property
    def scores(self):
        for x_axis in self.x_axis:
            yield x_axis
        for y_axis in self.y_axis:
            yield y_axis * 100

    @property
    def new_score(self):
        initial_score = self.score
        for row_number, col_number in itertools.product(range(self.height), range(self.width)):
            self.flip(row_number, col_number)
            for score in self.scores:
                if score == initial_score:
                    continue
                return score
            self.flip(row_number, col_number)


class Day13(Solution):
    def parse(self):
        output: list[Pattern] = []
        current_matrix: list[str] = []
        parsed = lambda: Pattern.parse_matrix(data=current_matrix, wrapper=lambda c, _: c == "#")
        for line in self.lines:
            if not line:
                output.append(parsed())
                current_matrix = []
                continue
            current_matrix.append(line)
        else:
            output.append(parsed())
        return output

    def solution1(self):
        return sum(pat.score for pat in self.parsed)

    def solution2(self):
        return sum(pat.new_score for pat in self.parsed)


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
