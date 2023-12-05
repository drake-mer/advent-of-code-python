# Thought process
# What are we discussing ??
r"""
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
It remains some kind of ambiguity. We know that this problem is typical of lazy loading of values
and that we can probably
come with an elegant way to solve it. Will we manage to derive an elegant solution from this?

HO NO! Domain Model changed! Instead of computing operations, we need to solve an equation! That's so painful! How
can we carry over with this?

Stick to your DDD approach dude! It will help you find clarity in this mess!

OK!

So now, we have a pretty clear overview of the domain. We basically have a tree of operation, but we need
to invert the whole tree to find exactly what number put with the ref 'humn' So we will probably need to invert
a large equation looking like

1234 = (456 + (2 * (6 // (123 + (... ( 123 * humn))))))

So we need a way to invert operation, that's a lot of thinking. HO NO \o/

More domain model knowledge to invest, that's *DIFFICULT* and Math is *HARD*

Let's try anyway

Let's call 'S' the symbolic operation to carry

Let's call 'N' the actual number we know in the OP

Let's call 'I' the number we know we are looking for.



"""
import dataclasses
import re
from enum import Enum
from typing import Callable, Mapping, TypeAlias


class Value(int):
    pass


class Reference(str):
    pass


Operation: TypeAlias = Callable[[Value, Value], Value]


class _Operation(Enum):
    def ADD(a, b):
        return a + b

    def SUB(a, b):
        return a - b

    def MUL(a, b):
        return a * b

    def DIV(a, b):
        return a // b


@dataclasses.dataclass
class MonkeyOperation:
    operation: Operation
    lhs: Reference
    rhs: Reference


MonkeyReference: TypeAlias = Reference
HumanReference: TypeAlias = Reference
MonkeyAssembly: TypeAlias = Mapping[
    MonkeyReference | HumanReference,
    MonkeyOperation | Value,
]


@dataclasses.dataclass
class Equation:
    value: Value
    expression: MonkeyOperation


class MissingHuman(Exception):
    pass


def yell(ref: MonkeyReference, assembly: MonkeyAssembly) -> Value:
    match (op := assembly[ref]):
        case Value(_):
            return op
        case MonkeyOperation(_):
            return op.operation(yell(op.lhs, assembly), yell(op.rhs, assembly))
        case None:
            raise MissingHuman("Are you going to yell?")
        case _:
            raise ValueError("what to do with this?")


def solve_equation(equation: Equation, assembly: MonkeyAssembly) -> Equation | Value:
    op = equation.expression
    if op is None:
        return equation.value
    try:
        op_rhs: Value = yell(op.rhs, assembly)
        op_lhs: MonkeyOperation = assembly[op.lhs]
        match op.operation:
            case _Operation.DIV:
                return Equation(Value(op_rhs * equation.value), op_lhs)
            case _Operation.SUB:
                return Equation(Value(op_rhs + equation.value), op_lhs)
        num, expression = op_rhs, op_lhs
    except MissingHuman:
        op_lhs: Value = yell(op.lhs, assembly)
        op_rhs: MonkeyOperation = assembly[op.rhs]
        match op.operation:
            case _Operation.DIV:
                return Equation(Value(op_lhs // equation.value), op_rhs)
            case _Operation.SUB:
                return Equation(Value(op_lhs - equation.value), op_rhs)
        num, expression = op_lhs, op_rhs
    match op.operation:
        case _Operation.MUL:
            return Equation(Value(equation.value // num), expression)
        case _Operation.ADD:
            return Equation(Value(equation.value - num), expression)


def read_operation(source: str) -> _Operation:
    match source:
        case "+":
            return _Operation.ADD
        case "-":
            return _Operation.SUB
        case "*":
            return _Operation.MUL
        case "/":
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
            MonkeyOperation(
                read_operation(operation),
                read_monkey_value(lhs_value),
                read_monkey_value(rhs_value),
            ),
        )
    elif match := val.match(source):
        monkey_ref, value = match.groups()
        return (Reference(monkey_ref), Value(value))
    else:
        raise ValueError("could not tell what this line is about")


def read_monkey_assembly() -> MonkeyAssembly:
    return dict(read_monkey(line) for line in open("input.txt"))


def solve(reference: HumanReference, assembly: MonkeyAssembly):
    assembly[reference] = None
    trust_source = assembly[Reference("root")]
    try:
        number, rhs = yell(trust_source.lhs, assembly), assembly[trust_source.rhs]
    except MissingHuman:
        number, rhs = yell(trust_source.rhs, assembly), assembly[trust_source.lhs]
    equation = Equation(number, rhs)

    while True:
        match (equation := solve_equation(equation, assembly)):
            case Value(_):
                return equation
            case _:
                continue


print(yell(MonkeyReference("root"), read_monkey_assembly()))
print(solve(HumanReference("humn"), read_monkey_assembly()))
