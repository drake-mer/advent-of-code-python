import dataclasses
from typing import Literal

from advent_of_code.solution import Solution


@dataclasses.dataclass
class CompOp:
    variable: Literal["x", "m", "s", "a"]
    operator: Literal[">", "<", "="]
    value: int
    action: str

    def result(self, part: "Part"):
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
    condition: list[CompOp]
    fallback: str

    @classmethod
    def parse(cls, line):
        label = line[: line.find("{")]
        instructions = line[line.find("{") :]
        instructions = instructions.strip("{}")
        instructions = instructions.split(",")
        return cls(label, [CompOp.parse(w) for w in instructions[:-1]], instructions[-1])


@dataclasses.dataclass
class Game:
    workflows: list[Workflow]
    parts: list[Part]


class Day19(Solution):
    def parse(self):
        data1, data2 = self.data.split("\n\n")
        workflows = []
        parts = []
        for line in data1.splitlines():
            workflows.append(Workflow.parse(line.strip()))
        for line in data2.splitlines():
            parts.append(Part.parse(line.strip()))
        return Game(workflows, parts)

    def solution1(self):
        print(*self.parsed.parts, sep="\n")
        print(*self.parsed.workflows, sep="\n")

    def solution2(self):
        return "not implemented"


class Day19Test(Day19):
    @property
    def data(self):
        return """"""
