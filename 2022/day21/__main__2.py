# Thought process
# What are we discussing ??
"""
1. Monkeys
2. Yelling
3. Operations and Math, that's the core business

Let's find some vocabulary to match these ideas ??

First question -> should we assimilate a value and an operation to the same basic type?
Let say no, let's create a 'value' type.

Let's use a dataclass to describe an operation

--> now we get some problem: parsing; is this is in the domain model?
--> No, it's a generic problem, of the kind extract-transform-load. Let's solve it apart

But problem, the problem space does not deal with values only, but with references. OMG !!!

How are we going to deal with that? Let's use a union type.

Also, how about the Callable? It's not any callable, it's basically +*-/. Maybe an enum is better in this case?

And maybe we need to use some fancy pattern matching to find the right op?

Ok, it's almost done. Notice that so fare we didn't even try to address the real problem, we just defined
names, convenient types for the problem in the most descriptive way possible. Instead of focusing on the HOW?
We focus on the WHAT
In some sense, DDD is close to functional programming in the sense that we have a descriptive approach
We tell the computer how the solution looks like before even thinking of an algorithm to solve the problem.

It's also akin to what Einstein said: a problem well posed is 50% of the solution. Anyway let's continue on our
monkey business.

At this point, we think that the parsing is correct and that the model is correct for the domain we are trying to solve.
It remains some kind of ambiguity. We know that this problem is typical of lazy loading of values and that we can probably
come with an elegant way to solve it. Will we manage to derive an elegant solution from this?

"""
import dataclasses
import functools
import re
from typing import Callable, TypeAlias, Mapping

from enum import Enum


class Value(int):
    pass


Operation: TypeAlias = Callable[[Value, Value], Value]


class Reference(str):
    pass


class _Operation(Enum):
    ADD: Operation = lambda a, b: a + b
    SUB: Operation = lambda a, b: a - b
    MUL: Operation = lambda a, b: a * b
    DIV: Operation = lambda a, b: a // b


@dataclasses.dataclass
class MonkeyOperation:
    operation: Operation
    lhs: Reference
    rhs: Reference


MonkeyReference: TypeAlias = Reference

MonkeyAssembly: TypeAlias = Mapping[MonkeyReference, MonkeyOperation | Value]


def cache_result(f):
    """unnecessary cache for the yell function"""
    cache_dict = {}

    @functools.wraps(f)
    def wrapper(*args):
        a, *remaining = args
        if a in cache_dict:
            print("cache called")
            return cache_dict[a]
        cache_dict[a] = f(*args)
        return cache_dict[a]

    return wrapper


# @cache_result
def yell(ref: MonkeyReference, assembly: MonkeyAssembly) -> Value:
    match (op := assembly[ref]):
        case Value(_):
            return op
        case MonkeyOperation(_):
            return op.operation(yell(op.lhs, assembly), yell(op.rhs, assembly))
        case _:
            raise ValueError("what to do with this?")


def read_operation(source: str) -> _Operation:
    match source:
        case '+':
            return _Operation.ADD
        case '-':
            return _Operation.SUB
        case '*':
            return _Operation.MUL
        case '/':
            return _Operation.DIV


def read_monkey_value(source: str) -> Value | Reference:
    if source.isdecimal():
        return Value(source)
    return Reference(source)


def read_monkey(source: str) -> tuple[MonkeyReference, MonkeyOperation | Value]:
    op = re.compile(r"^(\w+): (\w+) ([+\-*/]) (\w+)$")
    val = re.compile(r"^(\w+): (\d+)$")
    if match := op.match(source):
        monkey_ref, lhs_value, operation, rhs_value = match.groups()
        return (
            Reference(monkey_ref),
            MonkeyOperation(read_operation(operation), read_monkey_value(lhs_value), read_monkey_value(rhs_value))
        )
    elif match := val.match(source):
        monkey_ref, value = match.groups()
        return (
            Reference(monkey_ref), Value(value)
        )
    else:
        raise ValueError("could not tell what this line is about")


def read_monkey_assembly() -> MonkeyAssembly:
    return dict(read_monkey(line) for line in open('input.txt'))


print(yell(MonkeyReference('root'), read_monkey_assembly()))
