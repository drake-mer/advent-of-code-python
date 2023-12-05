import pathlib
from enum import Enum
from typing import Literal, Tuple

basedir = pathlib.Path(__file__).parent


class Score(int):
    pass


class Choice(str, Enum):
    rock = "rock"
    paper = "paper"
    scissors = "scissors"


class Issue(str, Enum):
    lose = "X"
    draw = "Y"
    win = "Z"


class P1:
    A = "rock"
    B = "paper"
    C = "scissors"


class P2:
    X = "rock"
    Y = "paper"
    Z = "scissors"


def intrinsic_score(c: Choice) -> Score:
    match c:
        case Choice.rock:
            return Score(1)
        case Choice.paper:
            return Score(2)
        case Choice.scissors:
            return Score(3)


def match_score(c1: Choice, c2: Choice) -> tuple[Score, Score]:
    match c1:
        case Choice.rock:
            match c2:
                case Choice.paper:
                    return Score(0), Score(6)
                case Choice.scissors:
                    return Score(6), Score(0)
                case Choice.rock:
                    return Score(3), Score(3)
        case Choice.paper:
            match c2:
                case Choice.paper:
                    return Score(3), Score(3)
                case Choice.scissors:
                    return Score(0), Score(6)
                case Choice.rock:
                    return Score(6), Score(0)
        case Choice.scissors:
            match c2:
                case Choice.paper:
                    return Score(6), Score(0)
                case Choice.scissors:
                    return Score(3), Score(3)
                case Choice.rock:
                    return Score(0), Score(6)


def play2_from_issue(p1: Choice, p2: Issue) -> Choice:
    match p2:
        case Issue.lose:
            match p1:
                case Choice.rock:
                    return Choice.scissors
                case Choice.scissors:
                    return Choice.paper
                case Choice.paper:
                    return Choice.rock
        case Issue.win:
            match p1:
                case Choice.rock:
                    return Choice.paper
                case Choice.scissors:
                    return Choice.rock
                case Choice.paper:
                    return Choice.scissors
        case Issue.draw:
            return p1


def score1(
    p1: Literal["A", "B", "C"],
    p2: Literal["X", "Y", "Z"],
) -> Tuple[Score, Score]:
    p1 = Choice(getattr(P1, p1))
    p2 = Choice(getattr(P2, p2))
    a, b = intrinsic_score(p1), intrinsic_score(p2)
    c, d = match_score(p1, p2)
    return a + c, b + d


def score2(
    p1: Literal["A", "B", "C"],
    p2: Literal["X", "Y", "Z"],
) -> Tuple[Score, Score]:
    a, b = intrinsic_score(Choice(getattr(P1, p1))), intrinsic_score(
        play2_from_issue(Choice(getattr(P1, p1)), Issue(p2)),
    )
    c, d = match_score(
        Choice(getattr(P1, p1)),
        play2_from_issue(Choice(getattr(P1, p1)), Issue(p2)),
    )
    return a + c, b + d


def munch_data(payload: str):
    return [list(line.split()) for line in payload.splitlines()]


def day_1_first_puzzle(payload):
    return sum([s2 for (s1, s2) in map(lambda foo: score1(*foo), munch_data(payload))])


def day_1_second_puzzle(payload):
    return sum([s2 for (s1, s2) in map(lambda foo: score2(*foo), munch_data(payload))])


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
