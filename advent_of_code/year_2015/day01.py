
from advent_of_code.solution import Solution


class Day01(Solution):
    def solution1(self):
        line, = self.lines
        total_up = line.count("(")
        total_down = line.count(")")
        return total_up - total_down

    def solution2(self):
        line, = self.lines
        starting_point = 0
        for pos, c in enumerate(line, 1):
            if c == "(":
                starting_point += 1
            if c == ")":
                starting_point -= 1
            if starting_point < 0:
                return pos

