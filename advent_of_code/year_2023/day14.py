import dataclasses
import functools
import hashlib
from collections import Counter, defaultdict
from typing import Self, TypeAlias, Union

from advent_of_code.solution import Solution
from advent_of_code.solution.datastructures.dimension2 import BaseMatrix, Coordinate


class SquareContent:
    pass


class CubicRock(SquareContent):
    def __str__(self):
        return "#"


class RoundedRock(SquareContent):
    def __str__(self):
        return "O"


class EmptySpace(SquareContent):
    def __str__(self):
        return "."


class Dish(BaseMatrix[SquareContent]):
    def __str__(self):
        return self.pretty_print(lambda c: str(c)) + "\n"

    def __hash__(self):
        return self.pretty_print(lambda c: str()).__hash__()

    def move_north(self) -> Self:
        content = [[c for c in row] for row in self.rows]
        dish = Dish(content)
        for y, row in enumerate(dish.rows):
            for x, c in enumerate(row):
                if not isinstance(c, RoundedRock):
                    continue
                current_y = y
                while (next_y := current_y - 1) >= 0 and isinstance(dish[Coordinate(x, next_y)], EmptySpace):
                    dish[Coordinate(x, next_y)] = c
                    dish[Coordinate(x, current_y)] = EmptySpace()
                    current_y = next_y
        return dish

    def move_south(self) -> Self:
        content = [[c for c in row] for row in self.rows]
        dish = Dish(content)
        for y, row in enumerate(dish.rows[::-1]):
            y = dish.height - y - 1
            for x, c in enumerate(row):
                if not isinstance(c, RoundedRock):
                    continue
                current_y = y
                while (next_y := current_y + 1) < dish.height and isinstance(dish[Coordinate(x, next_y)], EmptySpace):
                    dish[Coordinate(x, next_y)] = c
                    dish[Coordinate(x, current_y)] = EmptySpace()
                    current_y = next_y

        return dish

    def move_east(self) -> Self:
        content = [[c for c in row] for row in self.rows]
        dish = Dish(content)
        for x, column in enumerate(dish.columns[::-1]):
            x = dish.width - x - 1
            for y, c in enumerate(column):
                if not isinstance(c, RoundedRock):
                    continue
                current_x = x
                while (next_x := current_x + 1) < dish.width and isinstance(dish[Coordinate(next_x, y)], EmptySpace):
                    dish[Coordinate(next_x, y)] = c
                    dish[Coordinate(current_x, y)] = EmptySpace()
                    current_x = next_x
        return dish

    def move_west(self) -> Self:
        content = [[c for c in row] for row in self.rows]
        dish = Dish(content)
        for x, column in enumerate(dish.columns):
            for y, c in enumerate(column):
                if not isinstance(c, RoundedRock):
                    continue
                current_x = x
                while (next_x := current_x - 1) >= 0 and isinstance(dish[Coordinate(next_x, y)], EmptySpace):
                    dish[Coordinate(next_x, y)] = c
                    dish[Coordinate(current_x, y)] = EmptySpace()
                    current_x = next_x
        return dish

    @property
    def rb(self):
        total = 0
        for c in self.all_values():
            if isinstance(c, RoundedRock):
                total += 1
        return total

    def cycle(self):
        # print(" START CYCLE ".center(80, "-"))
        first = self.move_north()
        second = first.move_west()
        third = second.move_south()
        fourth = third.move_east()
        # print(self, first, second, third, fourth, sep="-"*80 + "\n")
        # assert first.rb == second.rb == third.rb == fourth.rb
        return fourth

    @property
    def score(self):
        return sum(
            sum(1 for c in row if isinstance(c, RoundedRock)) * (self.height - position)
            for position, row in enumerate(self.rows)
        )


def wrap_row(c: str, coord: Coordinate) -> SquareContent:
    match c:
        case "O":
            return RoundedRock()
        case "#":
            return CubicRock()
        case ".":
            return EmptySpace()
        case _:
            raise ValueError(f"could not parse {c}")


class Day14(Solution[Dish]):
    def parse(self) -> Dish:
        return Dish.parse_matrix(data=self.lines, wrapper=wrap_row)

    def solution1(self):
        dish = self.parse()
        dish = dish.move_north()
        return dish.score

    def solution2(self):
        def h(dish: Dish):
            return hashlib.sha256(str(dish).encode()).hexdigest()

        d = self.parsed
        cycle_map = defaultdict(list)
        score_map = {}
        k = 0
        while True:
            hashed = h(d)
            if len(cycle_map[hashed]) == 2:
                break  # we have all
            if hashed not in score_map:
                score_map[hashed] = d.score
            cycle_map[hashed].append(k)
            k += 1
            d = d.cycle()

        start = None
        for h_value, meeting_point in cycle_map.items():
            try:
                (val,) = meeting_point
            except ValueError:
                continue
            if start is None or start < val:
                start = val
        start += 1
        assert (
            len(
                cycle_len := set(
                    (k_second - k_first) for (k_first, k_second) in filter(lambda u: len(u) == 2, cycle_map.values())
                )
            )
            == 1
        )
        (cycle_len,) = cycle_len
        n_to_hash = {k[0] - start: ha for (ha, k) in cycle_map.items() if len(k) == 2}
        assert len(n_to_hash) == cycle_len
        result = n_to_hash[(1000000000 - start) % cycle_len]
        return score_map[result]


class Day14Test(Day14):
    @property
    def data(self):
        return """O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."""
