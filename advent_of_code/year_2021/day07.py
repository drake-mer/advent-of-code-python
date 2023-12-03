import statistics as stats

from advent_of_code.solution import Solution


class Day07(Solution):
    def parse(self):
        return list(map(int, self.line.split(",")))

    def solution1(self):
        median = round(stats.median(self.parsed))
        total_fuel = sum(abs(x - median) for x in self.parsed)
        return total_fuel

    def solution2(self):
        """
        Instead of optimizing the difference with the target,
        We optimize the quadratic difference with the target
        We try all possible solution in the range (0, 1000), this just works.
        A true least-square optimization algorithm would work too but let's be lazy for this one.
        """

        def fuel(target, crab_position):
            d = abs(target - crab_position)
            return ((d + 1) * d) // 2

        def total_fuel(target):
            return sum(fuel(target, crab) for crab in self.parsed)

        return min(total_fuel(target) for target in range(0, 1000))
