from advent_of_code.solution import Solution


class Day01(Solution):
    def parse(self):
        return [int(line) for line in self.lines]

    def calculate_fuel(self, craft: int):
        return craft // 3 - 2

    def all_fuel(self, craft: int):
        fuel = self.calculate_fuel(craft)
        next_fuel = self.calculate_fuel(fuel)
        while next_fuel > 0:
            fuel += next_fuel
            next_fuel = self.calculate_fuel(next_fuel)
        return fuel

    def solution1(self):
        return sum(self.calculate_fuel(craft) for craft in self.parsed)

    def solution2(self):
        return sum(self.all_fuel(craft) for craft in self.parsed)
