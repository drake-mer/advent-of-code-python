from advent_of_code.solution import Solution


class Day05(Solution):
    def parse(self):
        all_segments = []
        xmax = 0
        ymax = 0
        for line in self.lines:
            a, b = line.split(" -> ")
            x, y = map(int, a.split(","))
            xp, yp = map(int, b.split(","))
            xmax = max(x, xp, xmax)
            ymax = max(y, yp, ymax)
            all_segments.append(((x, y), (xp, yp)))
        return all_segments

    def solution1(self):
        MAP = [([0] * 991) for _ in range(991)]
        for (x, y), (xp, yp) in self.parsed:
            if x != xp and y != yp:
                continue
            elif y == yp:
                for abs_ in range(min(x, xp), max(x, xp) + 1):
                    MAP[y][abs_] += 1
            elif x == xp:
                for ord_ in range(min(y, yp), max(y, yp) + 1):
                    MAP[ord_][x] += 1
            elif x == xp and y == yp:
                MAP[y][x] += 1
            else:
                raise ValueError()
        return sum(sum(1 for x in row if x > 1) for row in MAP)

    def solution2(self):
        MAP = [([0] * 991) for _ in range(991)]
        for (x, y), (xp, yp) in self.parsed:
            if x != xp and y != yp:
                dy = -1 if yp < y else 1
                dx = -1 if xp < x else 1
                cur_x, cur_y = x, y
                MAP[cur_y][cur_x] += 1
                while (cur_x, cur_y) != (xp, yp):
                    cur_x += dx
                    cur_y += dy
                    MAP[cur_y][cur_x] += 1
            elif y == yp:
                for abs_ in range(min(x, xp), max(x, xp) + 1):
                    MAP[y][abs_] += 1
            elif x == xp:
                for ord_ in range(min(y, yp), max(y, yp) + 1):
                    MAP[ord_][x] += 1
            elif x == xp and y == yp:
                MAP[y][x] += 1
            else:
                raise ValueError()
        return sum(sum(1 for x in row if x > 1) for row in MAP)
