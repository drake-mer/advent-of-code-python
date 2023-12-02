import abc
import dataclasses
import pathlib
from functools import cached_property


@dataclasses.dataclass
class Solution:
    day: int
    year: int

    @cached_property
    def data(self):
        with open(pathlib.Path(__file__).parent / f"year_{self.year}" / "data" / f"day{self.day:02d}.txt") as f:
            data = f.read()
        return data

    @cached_property
    def lines(self):
        return [line.rstrip() for line in self.data.splitlines()]

    @abc.abstractmethod
    def solution1(self):
        pass

    @abc.abstractmethod
    def solution2(self):
        pass

    def parse(self):
        raise NotImplementedError()

    @cached_property
    def parsed(self):
        """It is not mandatory to implement this function, but it helps for standardization"""
        return self.parse()

