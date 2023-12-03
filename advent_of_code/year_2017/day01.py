from advent_of_code.solution import Solution


class Day01(Solution):
    @staticmethod
    def solution_generic(buf1: str, shift: int):
        output = 0
        buf_len = len(buf1)
        for position, c in enumerate(buf1, 0):
            if c == buf1[(position + shift) % buf_len]:
                output += int(c)
        return output

    def solution1(self):
        return self.solution_generic(self.line, shift=1)

    def solution2(self):
        return self.solution_generic(self.line, shift=len(self.line) // 2)
