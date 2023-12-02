


def input_data():
    with open('input.txt') as f:
        data = f.read()
    return data


def answer():
    ans = 0
    for r, l, password in parse_data(input_data()):
        if is_valid(r, l, password):
            ans += 1
    return ans


def answer2():
    ans = 0
    for (x, y), l, password in parse_data(input_data()):
        if sum(True for pos in (x, y) if password[pos - 1] == l) == 1:
            ans += 1
    return ans
    

def parse_data(data):
    output = []
    for line in data.splitlines():
        line = line.strip()
        r, l, password = line.split(' ')
        r = tuple(map(int, r.split('-')))
        l = l.strip(':')
        output.append((r, l, password))
    return output


def is_valid(r, l, password):
    x, y = r
    return x <= password.count(l) <= y


print(answer()) 
print(answer2())
