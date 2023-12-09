from dataclasses import dataclass

from advent_of_code.solution import Solution


@dataclass
class CubeSample:
    blue: int = 0
    red: int = 0
    green: int = 0

    @classmethod
    def parse(cls, sample: str):
        result = {}
        for s in sample.split(","):
            n, color = s.strip().split(" ")
            result.update({color: int(n)})
        return cls(**result)

    @property
    def power(self):
        return self.blue * self.green * self.red


@dataclass
class Game:
    id: int
    samples: list[CubeSample]

    @classmethod
    def parse(cls, line: str):
        game, samples = line.strip().split(":")
        samples = [CubeSample.parse(sample.strip()) for sample in samples.split(";")]
        game_id = int(game.replace("Game ", ""))
        return cls(id=game_id, samples=samples)

    def is_possible(self, max_red=12, max_green=13, max_blue=14) -> bool:
        return all(
            g.red <= max_red and g.green <= max_green and g.blue <= max_blue
            for g in self.samples
        )

    @property
    def minimum_sample(self):
        min_blue = min_green = min_red = 0
        for sample in self.samples:
            min_blue = max(sample.blue, min_blue)
            min_red = max(sample.red, min_red)
            min_green = max(sample.green, min_green)
        return CubeSample(blue=min_blue, red=min_red, green=min_green)


class Day02(Solution):
    def parse(self):
        return [Game.parse(line) for line in self.lines]

    def solution1(self):
        return sum(
            g.id
            for g in self.parsed
            if g.is_possible(max_red=12, max_green=13, max_blue=14)
        )

    def solution2(self):
        return sum(g.minimum_sample.power for g in self.parsed)
