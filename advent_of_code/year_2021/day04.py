import dataclasses
from functools import cached_property

from advent_of_code.solution import Solution


def read_board(rows):
    output = []
    for row in rows:
        output.append(list(map(int, row.strip().split())))
    return output


@dataclasses.dataclass
class Coordinates:
    x: int
    y: int
    val: int
    marked: bool = False

    def mark(self):
        self.marked = True


class Board(dict):
    def __init__(self, data: list[list[int]]):
        for y, row in enumerate(data):
            for x, col in enumerate(data):
                self[(x, y)] = Coordinates(x=x, y=y, val=data[y][x])

    def mark_values(self, val):
        for key, value in self.items():
            if val == value.val:
                value.marked = True

    @property
    def wins(self):
        return (
            any(all(self[x, y].marked for x in range(5)) for y in range(5))
            or any(all(self[x, y].marked for y in range(5)) for x in range(5))
        )


class Day04(Solution):
    def parse(self):
        bingo_numbers = list(map(int, self.lines[0].split(",")))
        all_boards = []
        k = 2
        while self.lines[k:]:
            all_boards.append(Board(read_board(self.lines[k:k+5])))
            k += 6
        return bingo_numbers, all_boards

    @property
    def bingo_numbers(self):
        return self.parsed[0]

    @property
    def all_boards(self):
        return self.parsed[1]

    @cached_property
    def first_winning_board(self) -> tuple[int, Board]:
        bingo_numbers, all_boards = self.parse()
        for number in bingo_numbers:
            for board in all_boards:
                board.mark_values(number)
                if board.wins:
                    break
            if board.wins:
                break
        return number, board

    @cached_property
    def last_winning_board(self) -> tuple[int, Board]:
        bingo_numbers, all_boards = self.parse()
        boards = {k: v for k, v in enumerate(all_boards)}
        for number in list(bingo_numbers):
            for board_id in list(boards):
                boards[board_id].mark_values(number)
                if boards[board_id].wins:
                    last_board = boards.pop(board_id)
                if not boards:
                    last_number = number
                    break
            else:
                if not boards:
                    break

        return last_number, last_board

    def solution1(self):
        bingo, board = self.first_winning_board
        return bingo * sum(square.val for square in board.values() if not square.marked)

    def solution2(self):
        bingo, board = self.last_winning_board
        return bingo * sum(square.val for square in board.values() if not square.marked)
