from advent_of_code.datastructures.dimension2 import Map2D, discrete_orientations
from advent_of_code.solution import Solution


class XmasPanel(Map2D):
    def words(self, x_position, length=4):
        xmas_count = 0

        for delta in discrete_orientations(diagonal=True):
            guess_position = {
                1: "M",
                2: "A",
                3: "S",
            }
            current_pos = x_position
            for k in range(1, length):
                current_pos += delta

                if current_pos not in self.content:
                    break

                if guess_position[k] != self.content[current_pos]:
                    break
            else:
                xmas_count += 1

        return xmas_count


class Day04(Solution):
    def parse(self):
        return Map2D(content=self.data)

    def solution1(self):
        xmas_panel: Map2D = self.parsed
        x_positions = [pos for pos, val in xmas_panel.items() if val == "X"]
        return "not implemented"

    def solution2(self):
        return "not implemented"


class Day04Test(Day04):
    @property
    def data(self):
        return """"""
