import abc
import dataclasses
from collections import Counter, defaultdict
from enum import Enum
from functools import cached_property
from typing import Any, Literal, Self, TypeAlias, Union

from advent_of_code.datastructures.dimension1 import Infinity, mInfinity
from advent_of_code.solution import Solution


class Label(str):
    pass


class Segment(abc.ABC):
    @property
    @abc.abstractmethod
    def length(self) -> int:
        raise NotImplementedError()

    def __contains__(self, item) -> bool:
        raise NotImplementedError()

    def __and__(self, other) -> Self:
        raise NotImplementedError()


class Empty(Segment):
    def __contains__(self, item: Any) -> bool:
        return False

    def __and__(self, other) -> Self:
        """Intersection"""
        if not isinstance(other, Segment):
            raise ValueError("not the right type")

        return Empty()

    @property
    def length(self) -> int:
        return 0

    def __lt__(self, other):
        if isinstance(other, Empty):
            return False
        elif isinstance(other, Segment):
            return True
        raise ValueError()

    def __eq__(self, other):
        if isinstance(other, Empty):
            return True
        return False


@dataclasses.dataclass
class Range(Segment):
    low: int = dataclasses.field(default_factory=mInfinity)
    up: int = dataclasses.field(default_factory=Infinity)

    def __contains__(self, item: int):
        if not isinstance(item, int):
            raise ValueError()
        return self.low < item < self.up

    def __and__(self, other: Segment) -> Segment:
        match other:
            case Empty():
                return Empty()
            case Range():
                lower, upper = sorted((self, other))
            case _:
                raise ValueError(f"{other} is not of type Segment")

        if lower.up <= other.low:
            return Empty()
        elif lower.up in other:
            return Range(upper.low, lower.up)
        elif lower.up >= other.up:
            return Range(upper.low, upper.up)
        else:
            raise ValueError()

    @property
    def length(self) -> int:
        return self.up - self.low - 1

    def __lt__(self, other):
        if isinstance(other, Empty):
            return False
        elif isinstance(other, Range):
            if self.low < other.low:
                return True
            elif self.low == other.low:
                return self.up < other.up
            else:
                return False

        raise ValueError()

    def __eq__(self, other):
        if isinstance(other, Empty):
            return False
        elif isinstance(other, Range):
            return self.up == other.up and self.low == other.low
        return False


class Action(str, Enum):
    ACCEPT = "A"
    REJECT = "R"


@dataclasses.dataclass
class EvalTree:
    condition: "CompOp" = None
    falsy: Union["EvalTree", Action] = None
    truthy: Union["EvalTree", Action] = None


@dataclasses.dataclass(frozen=True)
class CompOp:
    variable: Literal["x", "m", "s", "a"]
    operator: Literal[">", "<", "="]
    value: int
    action: str

    @cached_property
    def range(self):
        match self.operator:
            case "<":
                return Range(up=self.value)
            case ">":
                return Range(low=self.value)
            case _:
                raise ValueError()

    def true(self, part: Union["Part", int]):
        if isinstance(part, Part):
            return getattr(part, self.variable) in self.range
        else:
            raise ValueError()

    @classmethod
    def parse(cls, data: str):
        if "<" in data:
            cmp: Literal["<"] = "<"
        elif ">" in data:
            cmp: Literal[">"] = ">"
        else:
            raise ValueError()
        var, other = data.split(cmp)
        val, action = other.split(":")
        return cls(var, cmp, int(val), action)


@dataclasses.dataclass
class Part:
    x: int = None
    m: int = None
    a: int = None
    s: int = None

    @classmethod
    def parse(cls, line: str):
        line = line.strip("{}")
        tokens = [v.split("=") for v in line.split(",")]
        return Part(**{k: int(v) for k, v in tokens})


@dataclasses.dataclass
class Workflow:
    label: str
    conditions: list[CompOp]
    fallback: str

    def process(self, part):
        for cond in self.conditions:
            if cond.true(part):
                return cond.action
        else:
            return self.fallback

    @classmethod
    def parse(cls, line):
        label = line[: line.find("{")]
        instructions = line[line.find("{") :]
        instructions = instructions.strip("{}")
        instructions = instructions.split(",")
        return cls(label, [CompOp.parse(w) for w in instructions[:-1]], instructions[-1])


@dataclasses.dataclass
class Game:
    workflows: dict[Label, Workflow]
    parts: list[Part]

    def process_part(self, part: Part):
        label = Label("in")
        flow = self.workflows[label]
        while (label := flow.process(part)) not in ("A", "R"):
            flow = self.workflows[label]
        if label == "A":
            return part.a + part.s + part.m + part.x
        elif label == "R":
            return 0
        else:
            raise ValueError()

    def sum_part_values(self):
        acc_parts = 0
        for part in self.parts:
            acc_parts += self.process_part(part)
        return acc_parts

    def make_tree(self, label: Label = None, other: list[CompOp] = None, fallback: Label = None):
        if label == "A" or label == "R":
            return Action(label)
        if other is None and fallback is None:
            workflow = self.workflows[label]
            return self.make_tree(label, workflow.conditions, Label(workflow.fallback))
        elif not other:
            # we must fall back
            if fallback == "A" or fallback == "R":
                return Action(fallback)
            next_node = self.workflows[fallback]
            return self.make_tree(
                label=Label(next_node.label), other=next_node.conditions, fallback=Label(next_node.fallback)
            )
        else:
            cond, *other = other
            return EvalTree(
                condition=cond,
                truthy=self.make_tree(label=Label(cond.action)),
                falsy=self.make_tree(other=other, fallback=fallback),
            )


def reduce_branch(branch):
    result = {
        "a": Range(),
        "x": Range(),
        "s": Range(),
        "m": Range(),
    }
    return result["a"].length * result["x"].length * result["s"].length * result["m"].length


class Day19(Solution):
    def parse(self):
        data1, data2 = self.data.split("\n\n")
        workflows = []
        parts = []
        for line in data1.splitlines():
            workflows.append(Workflow.parse(line.strip()))
        for line in data2.splitlines():
            parts.append(Part.parse(line.strip()))
        return Game({w.label: w for w in workflows}, parts)

    def solution1(self):
        return self.parsed.sum_part_values()

    def solution2(self):
        game: Game = self.parsed
        all_ways = game.make_tree(Label("in"))

        def reduce_tree(t: EvalTree, current_chain=None):
            if current_chain is None:
                current_chain = list()
            if isinstance(t.falsy, Action):
                result = list(current_chain)
                result.append((False, t.falsy))
                yield result
            else:
                yield from reduce_tree(t.falsy, list(current_chain) + [(False, t.falsy.condition)])
            if isinstance(t.truthy, Action):
                result = list(current_chain)
                result.append((True, t.truthy))
                yield result
            else:
                yield from reduce_tree(t.truthy, list(current_chain) + [(True, t.truthy.condition)])

        branches = [b for b in reduce_tree(all_ways) if b[-1][-1] == Action.ACCEPT]
        return sum(reduce_branch(branch) for branch in branches)


class Day19Test(Day19):
    @property
    def data(self):
        return """"""
