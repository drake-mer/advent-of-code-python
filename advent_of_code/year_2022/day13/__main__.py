import functools
import pathlib

basedir = pathlib.Path(__file__).parent

LT = -1
EQ = 0
GT = 1


def compare(left: list, right: list):
    if left == [] and right == []:
        return EQ
    elif left == [] and isinstance(right, list) and right:
        return LT
    elif isinstance(left, list) and left and right == []:
        return GT

    a, *aa = left
    b, *bb = right
    if isinstance(a, int) and isinstance(b, int):
        if a < b:
            return LT
        elif a == b:
            return compare(aa, bb)
        else:
            return GT
    elif isinstance(a, int):
        a, b = [a], b
    elif isinstance(b, int):
        a, b = a, [b]
    if (res := compare(a, b)) == EQ:
        return compare(aa, bb)
    return res


def munch_data(payload: str) -> list[tuple[list, list]]:
    all_data = []
    for pos, payload in enumerate(payload.split("\n\n")):
        l1, l2 = payload.splitlines(keepends=False)
        ll1, ll2 = eval(l1), eval(l2)
        assert repr(ll1).replace(" ", "") == l1
        assert repr(ll2).replace(" ", "") == l2
        all_data.append((ll1, ll2))
    return all_data


def day_1_first_puzzle(payload):
    return sum(
        k for k, (a, b) in enumerate(munch_data(payload), 1) if compare(a, b) == LT
    )


def day_1_second_puzzle(payload):
    all_data = []
    for k, v in munch_data(payload):
        all_data.extend((k, v))
    all_data.extend(([[2]], [[6]]))
    d = sorted(all_data, key=functools.cmp_to_key(compare))
    return (d.index([[6]]) + 1) * (d.index([[2]]) + 1)


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
