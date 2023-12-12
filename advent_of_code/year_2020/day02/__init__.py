from advent_of_code.solution import Solution


class Day02(Solution):
    def solution1(self):
        ans = 0
        for r, l, password in self.parsed:
            if is_valid(r, l, password):
                ans += 1
        return ans

    def solution2(self):
        ans = 0
        for (x, y), l, password in self.parsed:
            if sum(True for pos in (x, y) if password[pos - 1] == l) == 1:
                ans += 1
        return ans

    def parse(self):
        output = []
        for line in self.lines:
            line = line.strip()
            r, l, password = line.split(" ")
            r = tuple(map(int, r.split("-")))
            l = l.strip(":")
            output.append((r, l, password))
        return output


def is_valid(r, l, password):
    x, y = r
    return x <= password.count(l) <= y
