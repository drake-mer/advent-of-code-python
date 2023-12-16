import dataclasses
from typing import Generic, TypeVar

from advent_of_code.solution import Solution

T = TypeVar("T")
N = TypeVar("N")


@dataclasses.dataclass(frozen=True)
class LinkedList(Generic[T, N]):
    value: T
    label: N
    next: "LinkedList[T, N]" = None

    def __iter__(self):
        yield self.value
        if self.next is None:
            return
        yield from self.next

    def update(self, label: N, value: T):
        if self.label == label:
            return LinkedList(value=value, label=label, next=self.next)
        if self.next is None:
            return LinkedList(value=self.value, label=self.label, next=LinkedList(value=value, label=label))
        return LinkedList(value=self.value, label=self.label, next=self.next.update(label, value))

    def remove(self, label: N):
        if self.label == label:
            return self.next
        if self.next is None:
            return self
        return LinkedList(value=self.value, label=self.label, next=self.next.remove(label))

    def get(self, label: N):
        """Not mandatory for the exercise but let's have fun"""
        if self.label == label:
            return self.value
        if self.next is None:
            raise KeyError(f"key={label} not found")
        return self.next.get(label)


class ToyHashMap(Generic[T, N]):
    def __init__(self):
        self.content: list[LinkedList | None] = [None] * 256

    def __getitem__(self, label: str):
        if self.content[(hash_val := hash_algo(label))]:
            return self.content[hash_val].get(label)
        raise KeyError(f"key={label} not found")

    def __delitem__(self, label: str):
        if self.content[(hash_val := hash_algo(label))]:
            self.content[hash_val] = self.content[hash_val].remove(label)

    def __setitem__(self, label: str, value: int):
        if self.content[(hash_val := hash_algo(label))]:
            self.content[hash_val] = self.content[hash_val].update(label=label, value=value)
        else:
            self.content[hash_val] = LinkedList(label=label, value=value)

    @property
    def total_focal_power(self):
        return sum(
            (k * sum((power * pos for pos, power in enumerate(self.content[k - 1], 1)))) if self.content[k - 1] else 0
            for k in range(1, len(self.content) + 1)
        )


def hash_algo(seq: str):
    starting_value = 0
    for c in seq:
        if c == "\n":
            continue
        val = ord(c)
        starting_value += val
        starting_value *= 17
        starting_value %= 256
    return starting_value


class Day15(Solution):
    def parse(self):
        return self.data.split(",")

    def solution1(self):
        return sum(hash_algo(seq) for seq in self.parsed)

    def solution2(self):
        my_super_hash_map = ToyHashMap()
        for operation in self.parsed:
            if operation.endswith("-"):
                label = operation.strip("-")
                del my_super_hash_map[label]
            else:
                label, val = operation.split("=")
                my_super_hash_map[label] = int(val)

        return my_super_hash_map.total_focal_power
