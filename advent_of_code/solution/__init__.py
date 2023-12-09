import abc
import dataclasses
import pathlib
from functools import cached_property
from typing import Callable, TypeVar

ParseResult = TypeVar("ParseResult")


@dataclasses.dataclass(frozen=True)
class Solution:
    """A base class to inherit from when implementing solutions."""

    day: int
    year: int

    @cached_property
    def data(self):
        with open(
            pathlib.Path(__file__).parent.parent
            / f"year_{self.year}"
            / "data"
            / f"day{self.day:02d}.txt",
        ) as f:
            data = f.read()
        return data

    @cached_property
    def lines(self):
        return [line.rstrip() for line in self.data.splitlines()]

    @cached_property
    def line(self):
        """Useful if the input contains a single line."""
        (line,) = self.lines
        return line

    @abc.abstractmethod
    def solution1(self):
        pass

    @abc.abstractmethod
    def solution2(self):
        pass

    def parse(self) -> ParseResult:
        raise NotImplementedError()

    @cached_property
    def parsed(self) -> ParseResult:
        """It is not mandatory to implement this function, but it helps for standardization"""
        return self.parse()
