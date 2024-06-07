from collections import Counter

from advent_of_code.solution import Solution


class Day02(Solution):
    def parse(self):
        return self.lines

    def solution1(self):
        all_res = [Counter(line).values() for line in self.lines]
        nb_two = sum(1 for frequency in all_res if 2 in frequency)
        nb_three = sum(1 for frequency in all_res if 3 in frequency)
        return nb_two * nb_three

    def solution2(self):
        size_box_id = len(self.lines[0])
        for k in range(size_box_id):
            new_set = Counter(b[:k] + b[k + 1 :] for b in self.lines)
            box_id_counter_inversed = {v: k for k, v in new_set.items()}
            if 2 in box_id_counter_inversed:
                return box_id_counter_inversed[2]


class Day02Test(Day02):
    @property
    def data(self):
        return """"""
