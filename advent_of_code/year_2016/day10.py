import dataclasses
import re
from collections import defaultdict
from typing import Any, TypeAlias, Union

from advent_of_code.solution import Solution

"""
NB: this should be representable by a tree data structure, with the leaf being the terminal values.

A node to be complete must have either two values for a bot or 1 value for a bin.

The trick is not to interpret the input as a set of instructions to be run sequentially, but instead
as a description of the tree.


Example
--------

Value X goes to bot Y = one of the leaf of bot Y has value X

Bot X gives low to Y and high to Z: one of the leaf of bot Z has high(X) as a value

Finally you have a set of instructions that looks like

Bot(id=1, Leaf(High(Bot(id=2))), Leaf(High(Bot(id=12)))))
Bot(id=4, Leaf(High(Bot(id=4))), Leaf(Low(Bot(id=7)))))
Output(id=3, Leaf(Low(Bot(id=18))))
etc

So the way to solve this now is to have the whole data set as a dictionary where you would recursively descend
to the interesting part.

NB: on psychology of programming. In my brain I had a hard time deciding what was the good approach. I started
to implement it in the more abstract way with this notion High and Low, but not seeing exactly how it would come 
out in practice, I decided to go down the route of brutally interpreting stuff. I even started thinking that a
sequential execution would give me the answer, albeit it was not working.

Also, if you want to solve the tree, you actually want to evaluate all the leaf until you meet the good pair.
This means that you can’t really work with immutable objects, you at least need a big dictionary to store the 
changed items.

NB: it is a good idea to use immutable values whenever possible.

So indeed, we need to resolve the tree as a lazy data structure. Beautiful exercise but a bit hard if you ask me
for a 10th day.
"""


@dataclasses.dataclass
class Bot:
    id: int
    values: tuple[Any, Any] = (None, None)

    def update(self, val):
        if all(v is not None for v in self.values):
            raise ValueError("bot cannot handle more than 2 values")
        elif all(v is None for v in self.values):
            return Bot(id=self.id, values=(val, None))
        else:
            return Bot(id=self.id, values=(val, *(v for v in self.values if v is not None)))

    @property
    def full(self):
        return all(v is not None for v in self.values)


@dataclasses.dataclass
class Bin:
    id: int
    value: int = None

    def update(self, val):
        return Bin(id=self.id, value=val)


def get_recipient(obj_type, obj_id):
    match obj_type:
        case "output":
            return Bin(id=int(obj_id))
        case "bot":
            return Bot(id=int(obj_id))
        case _:
            raise ValueError(f"unknown type {obj_type}")


@dataclasses.dataclass
class SetValue:
    value: int
    recipient: Bot

    @classmethod
    def parse(cls, line):
        val, bot_id = map(int, re.search(r"value (\d+) goes to bot (\d+)", line).groups())
        return cls(value=val, recipient=Bot(id=bot_id))


@dataclasses.dataclass
class MoveValue:
    low: Bot | Bin
    high: Bot | Bin
    sender: Bot

    @classmethod
    def parse(cls, line):
        bot_id, _, low_rec_type, low_rec, _, high_rec_type, high_rec = re.search(
            r"bot (\d+) gives low to ((output|bot) (\d+)) and high to ((output|bot) (\d+))",
            line,
        ).groups()
        return cls(
            low=get_recipient(low_rec_type, low_rec),
            high=get_recipient(high_rec_type, high_rec),
            sender=Bot(id=int(bot_id))
        )


class StateMachine(dict):
    def __init__(self):
        self.solver = None
        super().__setitem__(Bin.__name__, dict())
        super().__setitem__(Bot.__name__, dict())

    def __getitem__(self, obj):
        if not isinstance(obj, (Bin, Bot)):
            raise NotImplementedError()

        class_dict = super().__getitem__(obj.__class__.__name__)
        if obj.id not in class_dict:
            super().__getitem__(obj.__class__.__name__)[obj.id] = obj
        return class_dict[obj.id]

    def __setitem__(self, obj, obj_value):
        if obj.id != obj_value.id:
            raise ValueError("id mismatch")
        super().__getitem__(obj.__class__.__name__).__setitem__(obj.id,  obj_value)

    def run(self, instruction):
        if isinstance(instruction, SetValue):
            self[instruction.recipient] = self[instruction.recipient].update(instruction.value)
        elif isinstance(instruction, MoveValue):
            high_rec = instruction.high
            low_rec = instruction.low
            sender = self[instruction.sender]
            if not sender.full:
                raise ValueError("could not process this instruction")
            low, high = sorted(sender.values)
            if (low, high) == (17, 61):
                self.solver = sender.id
            self.run(SetValue(recipient=low_rec, value=low))
            self.run(SetValue(recipient=high_rec, value=high))


class Day10(Solution):
    def parse(self):
        output = []
        for line in self.lines:
            if line.startswith("value"):
                output.append(SetValue.parse(line))
            elif line.startswith("bot"):
                output.append(MoveValue.parse(line))
            else:
                raise ValueError(f"unable to parse line {line}")
        return output

    def run_sm(self):
        sm = StateMachine()
        set_values = list(instruction for instruction in self.parsed if isinstance(instruction, SetValue))
        distribute = list(instruction for instruction in self.parsed if isinstance(instruction, MoveValue))
        for instruction in set_values:
            sm.run(instruction)

        playable = [instruction for instruction in distribute if sm[instruction.sender].full]
        while distribute:
            distribute = [instruction for instruction in distribute if not sm[instruction.sender].full]
            for instruction in playable:
                sm.run(instruction)
            playable = [instruction for instruction in distribute if sm[instruction.sender].full]
            distribute = [instruction for instruction in distribute if not sm[instruction.sender].full]
        return sm

    def solution1(self):
        return self.run_sm().solver

    def solution2(self):
        machine = self.run_sm()
        r = 1
        for k in (0, 1, 2):
            r *= machine[Bin(id=k)].value
        return r


