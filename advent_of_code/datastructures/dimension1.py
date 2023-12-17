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
