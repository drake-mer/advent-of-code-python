import itertools


def test_simple_example():
    data = [
        int(l.strip())
        for l in """
1721
979
366
299
675
1456
""".splitlines()
        if l
    ]
    assert answer(data) == 514579


def read_data():
    with open("input.txt") as f:
        data = f.read()

    return [int(line.strip()) for line in data.splitlines() if line]


def answer(data):
    for k in range(len(data)):
        for l in range(len(data)):
            if k == l:
                continue
            if data[k] + data[l] == 2020:
                answer = data[k] * data[l]
                return answer


def answer2(data):
    s1 = sorted(data)
    s2 = sorted(data)
    s3 = sorted(data)
    for k, l, m in itertools.product(s1, s2, s3):
        if k + l + m == 2020:
            return k * l * m


print(answer(read_data()))
print(answer2(read_data()))
