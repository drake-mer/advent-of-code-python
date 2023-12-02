from advent_of_code.solution import Solution


class Day01(Solution):
    def solution1(self):
        total_up = self.line.count("(")
        total_down = self.line.count(")")
        return total_up - total_down

    def solution2(self):
        starting_point = 0
        for pos, c in enumerate(self.line, 1):
            if c == "(":
                starting_point += 1
            if c == ")":
                starting_point -= 1
            if starting_point < 0:
                return pos
