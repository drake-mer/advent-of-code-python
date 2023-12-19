import dataclasses
from enum import Enum
from typing import Literal, TypeAlias, Union

from advent_of_code.solution import Solution


class Label(str):
    pass


class Action(str, Enum):
    Accepted = "A"
    Rejected = "R"


@dataclasses.dataclass
class EvalTree:
    condition: Union["CompOp", bool] = None
    falsy: Union["EvalTree", Action] = None
    truthy: Union["EvalTree", Action] = None


@dataclasses.dataclass
class CompOp:
    variable: Literal["x", "m", "s", "a"]
    operator: Literal[">", "<", "="]
    value: int
    action: str

    def true(self, part: "Part"):
        match self.operator:
            case ">":
                return getattr(part, self.variable) > self.value
            case "<":
                return getattr(part, self.variable) < self.value
            case _:
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
    x: int
    m: int
    a: int
    s: int

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

    def sort_parts(self):
        acc_parts = 0
        for part in self.parts:
            acc_parts += self.process_part(part)
        return acc_parts

    def eval_tree(self, label: Label = None, other: list[CompOp] = None, fallback: Label = None):
        if label == "A" or label == "R":
            return Action(label)
        if other is None and fallback is None:
            # initialized with just a label
            workflow = self.workflows[label]
            return self.eval_tree(label, workflow.conditions, workflow.fallback)
        elif not other:
            # we must fall back
            if fallback == "A" or fallback == "R":
                return Action(fallback)
            next_node = self.workflows[fallback]
            return self.eval_tree(
                label=Label(next_node.label), other=next_node.conditions, fallback=Label(next_node.fallback)
            )
        else:
            cond, *other = other
            return EvalTree(
                condition=cond,
                truthy=self.eval_tree(label=Label(cond.action)),
                falsy=self.eval_tree(other=other, fallback=fallback),
            )


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
        return self.parsed.sort_parts()

    def solution2(self):
        game: Game = self.parsed
        all_ways = game.eval_tree(Label("in"))
        print(all_ways)


class Day19Test(Day19):
    @property
    def data(self):
        return """"""
