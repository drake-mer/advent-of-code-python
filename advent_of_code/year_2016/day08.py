import functools
import unittest
import itertools
from advent_of_code.solution import Solution


def shift_indexable(line, shift):
    """shift a line"""
    new_line = line[-shift:] + line[:-shift]
    return new_line


def shift_row(row_pos, shift, screen):
    screen[row_pos] = shift_indexable(screen[row_pos], shift)


def shift_column(col_pos, shift, screen):
    new_col = [x[col_pos] for x in screen[-shift:] + screen[:-shift]]
    for row_pos, new_pix in enumerate(new_col):
        screen[row_pos][col_pos] = new_pix


def light(x, y, screen):
    for k in range(x):
        for l in range(y):
            screen[l][k] = "#"


class Day08(Solution):
    @functools.cached_property
    def screen(self):
        screen = [list(itertools.repeat(".", 50)) for _ in range(6)]
        for line_num, instruction in enumerate(self.lines):
            if instruction.startswith("rect"):
                _, coord = instruction.split()
                x, y = map(int, coord.split("x"))
                light(x, y, screen)
            elif instruction.startswith("rotate"):
                _, row_or_col, pos, __, shift = instruction.split()
                pos = int(pos.split("=")[-1])
                if row_or_col == "row":
                    shift_row(pos, int(shift), screen)
                elif row_or_col == "column":
                    shift_column(pos, int(shift), screen)
                else:
                    # non anticipated case
                    raise ValueError("UnknownOperation")
            else:
                # non anticipated case
                raise ValueError("UnknownOperation")
        return screen

    def solution1(self):
        return "\n" + "\n".join(map(lambda x: "".join([" " if y == "." else y for y in x]), self.screen))

    def solution2(self):
        return sum((x.count("#") for x in self.screen))


class TestScript(unittest.TestCase):
    def test_shift_indexable(self):
        line = [1, 2, 3, 4]
        self.assertEqual(shift_indexable(line, 2), [3, 4, 1, 2])
        self.assertEqual(shift_indexable("aabbccdd", 2), "ddaabbcc")

    def test_shift_col(self):
        screen = [
            [".", ".", ".", "."],
            [".", ".", ".", "."],
            [".", ".", "x", "."],
            [".", ".", ".", "."],
            [".", ".", ".", "."],
        ]
        shift_column(2, 1, screen)
        assert screen == [
            [".", ".", ".", "."],
            [".", ".", ".", "."],
            [".", ".", ".", "."],
            [".", ".", "x", "."],
            [".", ".", ".", "."],
        ]

    def test_shift_row(self):
        screen = [
            [".", ".", ".", "."],
            [".", ".", ".", "."],
            [".", ".", "x", "."],
            [".", ".", ".", "."],
            [".", ".", ".", "."],
        ]
        shift_row(2, 1, screen)
        assert screen == [
            [".", ".", ".", "."],
            [".", ".", ".", "."],
            [".", ".", ".", "x"],
            [".", ".", ".", "."],
            [".", ".", ".", "."],
        ]

    def test_light_screen(self):
        screen = [[".", ".", "."], [".", ".", "."]]
        light(2, 2, screen)
        assert screen == [
            ["#", "#", "."],
            ["#", "#", "."],
        ]
