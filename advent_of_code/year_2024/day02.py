from advent_of_code.solution import Solution


def is_safe(level: list[int]):
    delta = [k - l for k, l in zip(level, level[1:])]
    return all(-3 <= d <= -1 for d in delta) or all(1 <= d <= 3 for d in delta)


def is_safe2(level: list[int]):
    delta = [k - l for k, l in zip(level, level[1:])]
    return all(-3 <= d <= -1 for d in delta) or all(1 <= d <= 3 for d in delta)


class Day02(Solution):
    def parse(self):
        output = []
        for row in self.lines:
            row = [int(c) for c in row.split()]
            output.append(row)
        return output

    def solution1(self):
        return sum(is_safe(level) for level in self.parsed)

    def solution2(self):
        total = 0
        for level in self.parsed:
            if is_safe(level):
                total += 1
            else:
                for k in range(0, len(level)):
                    if is_safe(level[:k] + level[k + 1 :]):
                        total += 1
                        break
        return total


class Day02Test(Day02):
    @property
    def data(self):
        return """"""
