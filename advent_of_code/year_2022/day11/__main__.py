import dataclasses
import functools
import pathlib
from collections import deque
from typing import Callable

basedir = pathlib.Path(__file__).parent


class WorryLevel(int):
    pass


class MonkeyPos(int):
    pass


primes = {2, 3, 5, 7, 11, 13, 17, 19}

CONSTANT = functools.reduce(lambda x, y: x * y, primes, 1)


@dataclasses.dataclass
class Monkey:
    items: deque[WorryLevel]
    operation: Callable[[WorryLevel], WorryLevel]
    action: Callable[[WorryLevel], MonkeyPos]
    inspection: int = 0


class Monkeys(list[Monkey]):
    def throw(self, mfrom: MonkeyPos, mto: MonkeyPos):
        self[mto].items.append(self[mfrom].items.popleft())


def munch_data(payload: str) -> Monkeys:
    all_lines = Monkeys()
    for pos, payload in enumerate(payload.split("\n\n")):
        monkey_true = monkey_false = worry_levels = operation = condition = None
        for line in payload.splitlines(keepends=False):
            line = line.strip()
            if line.startswith("Monkey"):
                continue
            elif line.startswith("Starting items: "):
                line = line.removeprefix("Starting items: ")
                worry_levels = deque([WorryLevel(level) for level in line.split(",")])
            elif line.startswith("Operation: "):
                line = line.removeprefix("Operation:")
                l = "lambda" + line.replace("=", ":").replace("new", "old")
                operation = functools.cache(eval(l))
            elif line.startswith("Test:"):

                def condition(
                    x: WorryLevel, divisor: int = int(line.split()[-1])
                ) -> bool:
                    return (x % divisor) == 0

            elif line.startswith("If true"):
                monkey_true = MonkeyPos(line.split()[-1])
            elif line.startswith("If false"):
                monkey_false = MonkeyPos(line.split()[-1])
            else:
                raise

        @functools.cache
        def action(
            w: WorryLevel,
            mt: MonkeyPos = monkey_true,
            mf: MonkeyPos = monkey_false,
            condition_=condition,
        ):
            if condition_(w):
                return mt
            else:
                return mf

        all_lines.append(
            Monkey(
                items=deque(worry_levels),
                operation=operation,
                action=action,
            )
        )
    return all_lines


def day_1_first_puzzle(payload):
    monkeys = munch_data(payload)
    for k in range(10):
        for monkey in monkeys:
            while monkey.items:
                monkey.inspection += 1
                item = monkey.items.popleft()
                item = monkey.operation(item)
                item = item // 3
                next_monkey = monkey.action(item)
                monkeys[next_monkey].items.append(item)
    a, b, *c = sorted(monkeys, key=lambda monkey: monkey.inspection, reverse=True)
    return a.inspection * b.inspection


def day_1_second_puzzle(payload):
    monkeys = munch_data(payload)
    for k in range(10000):
        for monkey in monkeys:
            while monkey.items:
                monkey.inspection += 1
                item = monkey.items.popleft()
                item = monkey.operation(item)
                next_monkey = monkey.action(item)
                monkeys[next_monkey].items.append(item % CONSTANT)
    a, b, *c = sorted(monkeys, key=lambda monkey: monkey.inspection, reverse=True)
    return a.inspection * b.inspection


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
