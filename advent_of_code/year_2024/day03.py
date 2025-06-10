import re

from advent_of_code.solution import Solution


class Day03(Solution):
    def parse(self):
        return self.data

    def solution1(self):
        pattern = re.compile(r"mul\(([0-9]+),([0-9]+)\)")
        return sum(int(x) * int(y) for x, y in re.findall(pattern, self.parsed))

    def solution2(self):
        pattern = re.compile(r"(mul\(([0-9]+),([0-9]+)\))|(do\(\))|(don't\(\))")
        do = True
        total = 0
        for _, x, y, do_multiply, dont_multiply in re.findall(pattern, self.parsed):
            if do_multiply:
                do = True
                continue
            if dont_multiply:
                do = False
                continue
            if do:
                total += int(x) * int(y)
        return total


class Day03Test(Day03):
    @property
    def data(self):
        return """"""
