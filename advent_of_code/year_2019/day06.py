from collections import defaultdict

from advent_of_code.solution import Solution


def compute_sum_of_depths(tree: dict, start_node="COM", start_depth=1):
    total = 0

    for k in tree[start_node]:
        total += start_depth
        total += compute_sum_of_depths(tree, start_node=k, start_depth=start_depth + 1)

    return total


def all_paths(tree: dict, target="SAN", path=("COM",)):
    start_node = path[-1]
    for k in tree[start_node]:
        yield *path, k
        if tree[k]:
            yield from all_paths(tree, target=target, path=(*path, k))


class Day06(Solution):
    def parse(self):
        output_dict = defaultdict(list)
        for row in self.lines:
            center, satellite = row.split(")")
            assert center not in satellite
            output_dict[center].append(satellite)
        return output_dict

    def solution1(self):
        return compute_sum_of_depths(self.parsed)

    def solution2(self):
        for san_path in all_paths(self.parsed):
            if san_path[-1] == "SAN":
                break

        for you_path in all_paths(self.parsed):
            if you_path[-1] == "YOU":
                break

        common_path = []
        for k, l in zip(you_path, san_path):
            if k != l:
                break
            common_path.append(k)

        missing_moves = len(you_path) + len(san_path) - 2 * len(common_path) - 2
        return missing_moves


class Day06Test(Day06):
    @property
    def data(self):
        return """"""
