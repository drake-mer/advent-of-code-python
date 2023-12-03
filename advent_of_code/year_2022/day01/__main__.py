import pathlib

basedir = pathlib.Path(__file__).parent


def munch_data(payload: str):
    elves = payload.split("\n\n")
    elves = [map(int, f.splitlines(keepends=False)) for f in elves]
    return elves


def day_1_first_puzzle(payload):
    return max([sum(foo) for foo in munch_data(payload)])


def day_1_second_puzzle(payload):
    return sum(sorted([sum(foo) for foo in munch_data(payload)])[-3:])


print(day_1_first_puzzle(open(basedir / "data.txt").read()))
print(day_1_second_puzzle(open(basedir / "data.txt").read()))
