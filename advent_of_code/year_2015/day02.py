from advent_of_code.solution import Solution


class Day02(Solution):
    def parse(self):
        output = []
        for line in self.lines:
            output.append(tuple(map(int, line.split("x"))))
        return output

    def solution1(self):
        total = 0
        for (l, w, h) in self.parsed:
            side_areas = (w * l, l * h, w * h)
            total += 2 * sum(side_areas) + min(*side_areas)
        return total

    def solution2(self):
        total = 0
        for (l, w, h) in self.parsed:
            vol = l * w * h
            u, v, w = sorted([l, w, h])
            total += vol + 2 * (u + v)
        return total
