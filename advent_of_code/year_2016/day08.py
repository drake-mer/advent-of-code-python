import unittest
import itertools


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


def main():
    """
    Solve day8
    """
    data = [instruction for instruction in open("input_day_8.txt")]
    screen = [list(itertools.repeat(".", 50)) for _ in range(6)]
    for line_num, instruction in enumerate(data):
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
                raise BaseException("UnknownOperation")
        else:
            # non anticipated case
            raise BaseException("UnknownOperation")

    else:
        print(
            *map(lambda x: "".join([" " if y == "." else y for y in x]), screen),
            sep="\n"
        )
        print(sum((x.count("#") for x in screen)))


class TestScript(unittest.TestCase):
    """
    Test case
    """

    def setUp(self):
        pass

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


if __name__ == "__main__":
    """To run tests, uncomment the following statement:
    unittest.main()
    """
    main()  # solve problem effectively
