import dataclasses
from typing import Generic, TypeVar

T = TypeVar("T")


@dataclasses.dataclass
class Range(Generic[T]):
    start: T
    end: T

    def __contains__(self, item: T):
        return self.start <= item <= self.end


@dataclasses.dataclass
class Segment:
    start: int
    end: int

    def __contains__(self, item: int):
        return self.start <= item <= self.end


class Infinity(int):
    def __gt__(self, other):
        return True

    def __eq__(self, other):
        return False

    def __lt__(self, other):
        return False

    def __repr__(self):
        return "+Inf"

    def __add__(self, other):
        if isinstance(self, mInfinity):
            raise ValueError()
        return self


class mInfinity(int):
    def __gt__(self, other):
        return False

    def __eq__(self, other):
        return False

    def __lt__(self, other):
        return True

    def __repr__(self):
        return "-Inf"

    def __add__(self, other):
        return self
