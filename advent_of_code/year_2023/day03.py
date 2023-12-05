import dataclasses
from collections import defaultdict
from functools import cached_property

from advent_of_code.solution import BaseMatrix, Coordinate, Solution


class EngineSchematic(BaseMatrix):
    pass


@dataclasses.dataclass
class CharValue:
    value: str
    coordinate: Coordinate

    def isdigit(self):
        return self.value.isdigit()


class Day03(Solution):
    @cached_property
    def symbols(self):
        symbols = set(c.value for c in self.parsed.all_values() if not c.isdigit() and c.value != ".")
        return symbols

    @cached_property
    def part_numbers(self) -> list[tuple[CharValue, ...]]:
        all_numbers = []
        for row in self.parsed:
            current_number = ()
            for c in row:
                if current_number and not c.isdigit():
                    all_numbers.append(current_number)
                    current_number = ()
                elif c.isdigit():
                    current_number = (*current_number, c)
            if current_number:
                all_numbers.append(current_number)
        return all_numbers

    def parse(self) -> EngineSchematic[list[CharValue]]:
        data = EngineSchematic(self.parse_matrix(wrapper=CharValue))
        return data

    def solution1(self):
        output = 0
        for number in self.part_numbers:
            for digit in number:
                if any(c.value in self.symbols for c in self.parsed.neighbours(digit.coordinate)):
                    output += int("".join(c.value for c in number))
                    break
        return output

    def solution2(self):
        result = defaultdict(set)
        output = 0
        for number in self.part_numbers:
            number_tuple = (
                int("".join(c.value for c in number)),
                (c.coordinate for c in number),
            )
            for digit in number:
                for c in self.parsed.neighbours(digit.coordinate):
                    if c.value == "*":
                        result[c.coordinate].add(number_tuple)
        for star in result:
            if len(result[star]) == 2:
                (n1, coordinates), (n2, coordinates) = result[star]
                output += n1 * n2
        return output
