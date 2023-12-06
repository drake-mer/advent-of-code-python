import dataclasses
import functools
import math
from typing import TypeAlias

from advent_of_code.solution import Solution
from advent_of_code.utils import tokenize

Distance: TypeAlias = int


@dataclasses.dataclass
class Game:
    times: list[int]
    distances: list[int]


@dataclasses.dataclass
class Race:
    time: int
    distance: int

    def push(self, time: int) -> Distance:
        return time * (self.time - time)

    @property
    def nb_wins(self) -> Distance:
        delta = math.sqrt(self.time ** 2 - 4 * self.distance)
        end = int((self.time + delta)/2)
        end = min(
            filter(
                lambda end_: self.push(end_) - self.distance > 0,
                range(end-10, end + 11)
            ), key=lambda end_: self.push(end_)
        )
        start = int((self.time - delta)/2)
        start = min(
            filter(
                lambda start_: self.push(start_) - self.distance > 0,
                range(start-10, start + 11)
            ), key=lambda start_: self.push(start_)
        )
        nb_wins = end - start + 1
        return nb_wins


class Day06(Solution):
    def parse(self):

        game = Game(
            times=tokenize(self.lines[0].replace("Time: ", ""), int),
            distances=tokenize(self.lines[1].replace("Distance: ", ""), int)
        )
        return [
            Race(t, d) for (t, d) in zip(game.times, game.distances)
        ]

    def parse2(self):
        return Race(
            time=int("".join(tokenize(self.lines[0].replace("Time: ", "")))),
            distance=int("".join(tokenize(self.lines[1].replace("Distance: ", ""))))
        )

    def solution1(self):
        all_winning_per_race = [
            tuple(push_time for push_time in range(0, race.time + 1) if race.push(push_time) > race.distance)
            for race in self.parsed
        ]
        return functools.reduce(
            lambda x, y: x*y, [len(w) for w in all_winning_per_race], 1
        )

    def solution2(self):
        race = self.parse2()
        return race.nb_wins
