import pathlib
import string
from functools import reduce
from typing import Mapping

basedir = pathlib.Path(__file__).parent


class Priority(int):
    pass


priority: Mapping[str, Priority] = dict(
    zip(string.ascii_lowercase + string.ascii_uppercase, map(Priority, range(1, 53))),
)


def common_element(*args: str) -> str:
    intersection = reduce(lambda a, b: a & b, map(set, args), set(priority))
    return intersection.pop()


def munch_data(payload: str):
    return [(line[: len(line) // 2], line[len(line) // 2 :]) for line in payload.splitlines(keepends=False)]


def day_1_first_puzzle(payload):
    return sum([priority[common_element(*data)] for data in munch_data(payload)])


def day_1_second_puzzle(payload):
    all_rucksacks = list(map(lambda x: "".join(x), munch_data(payload)))
    group_of_three = [all_rucksacks[3 * k : 3 * k + 3] for k in range(len(all_rucksacks) // 3)]
    return sum([priority[common_element(*gr)] for gr in group_of_three])


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
