import itertools

from advent_of_code.solution import Solution


class Day01(Solution):
    def parse(self):
        return [int(line.strip()) for line in self.lines if line]

    def solution1(self):
        data = self.parsed
        for k in range(len(data)):
            for l in range(len(data)):
                if k == l:
                    continue
                if data[k] + data[l] == 2020:
                    answer = data[k] * data[l]
                    return answer

    def solution2(self):
        s1 = sorted(self.parsed)
        s2 = sorted(self.parsed)
        s3 = sorted(self.parsed)
        for k, l, m in itertools.product(s1, s2, s3):
            if k + l + m == 2020:
                return k * l * m


class Day01Test(Day01):
    @property
    def data(self):
        return """1721
979
366
299
675
1456
"""
