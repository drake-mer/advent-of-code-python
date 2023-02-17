import pathlib
basedir = pathlib.Path(__file__).parent


class Interval(tuple):
    pass


def munch_data(payload: str) -> list[tuple[Interval, Interval]]:
    output = []
    for line in payload.splitlines(keepends=False):
        i1, i2 = line.split(',')
        a = tuple(map(int, i1.split('-')))
        b = tuple(map(int, i2.split('-')))
        output.append((Interval(a), Interval(b)))
    return output


def full_overlap(i1: Interval, i2: Interval) -> bool:
    i1, i2 = sorted([i1, i2])
    a, b = i1
    c, d = i2
    if a == c:
        return c <= b <= d
    if a < c:
        return a <= c <= b and a <= d <= b
    else:
        raise ValueError()


def simple_overlap(i1: Interval, i2: Interval) -> bool:
    i1, i2 = sorted([i1, i2])
    a, b = i1
    c, d = i2
    if a == c:
        return True
    if a < c:
        return a <= c <= b or a <= d <= b
    else:
        raise ValueError()


def day_1_first_puzzle(payload):
    return sum([1 for i1, i2 in munch_data(payload) if full_overlap(i1, i2)])


def day_1_second_puzzle(payload):
    return sum([1 for i1, i2 in munch_data(payload) if simple_overlap(i1, i2)])


print(day_1_first_puzzle(open(basedir / 'input.txt').read()))
print(day_1_second_puzzle(open(basedir / 'input.txt').read()))
