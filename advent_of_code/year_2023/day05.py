import dataclasses
from functools import cached_property
from typing import Iterable, Optional

from advent_of_code.solution import Solution
from advent_of_code.utils import tokenize

Source = str
Destination = str


@dataclasses.dataclass(frozen=True)
class SeedRange:
    start: int
    length: int

    @cached_property
    def end(self):
        return self.start + self.length

    def __contains__(self, value):
        return self.start <= value <= self.end


@dataclasses.dataclass
class Range:
    source: int
    destination: int
    length: int

    def intersection(self, seed_range: SeedRange) -> Optional[SeedRange]:
        start, end = max(self.source, seed_range.start), min(
            self.source + self.length,
            seed_range.end,
        )
        length = end - start
        if length <= 0:
            return None
        return SeedRange(
            start=start,
            length=length,
        )

    def exclusion(self, seed_range: SeedRange) -> list[SeedRange]:
        if not (intersection := self.intersection(seed_range)):
            return [seed_range]
        elif intersection == seed_range:
            return []
        elif intersection.start == seed_range.start:
            return [
                SeedRange(
                    start=intersection.start + intersection.length,
                    length=seed_range.length - intersection.length,
                ),
            ]
        elif (
            intersection.start + intersection.length
            == seed_range.start + seed_range.length
        ):
            return [
                SeedRange(
                    start=seed_range.start,
                    length=seed_range.length - intersection.length,
                ),
            ]
        elif (
            intersection.start + intersection.length
            < seed_range.start + seed_range.length
        ):
            return [
                SeedRange(
                    start=seed_range.start,
                    length=intersection.start - seed_range.start,
                ),
                SeedRange(
                    start=intersection.end,
                    length=(seed_range.start + seed_range.length) - intersection.end,
                ),
            ]

    def project(self, seed_range: SeedRange) -> Optional[SeedRange]:
        intersect = self.intersection(seed_range)
        return (
            SeedRange(
                start=intersect.start + self.shift,
                length=intersect.length,
            )
            if intersect
            else None
        )

    @property
    def shift(self):
        return self.destination - self.source

    def next_value(self, item: int):
        if item not in self:
            raise ValueError("you can’t move this item, the range is not correct")
        return item + self.shift

    def __contains__(self, item: int):
        return self.source <= item <= self.source + self.length


@dataclasses.dataclass
class Game:
    seeds: list[int]
    item_ranges: dict[Source, tuple[Destination, list[Range]]]

    def location(self, seed: int):
        source = "seed"
        while source != "location":
            destination, ranges = self.item_ranges[source]
            for range_ in ranges:
                if seed not in range_:
                    continue
                seed = range_.next_value(seed)
                break
            source = destination
        return seed

    def range_excluded_from(self, seed_range, ranges):
        excluded_ranges = [seed_range]
        for range_ in ranges:
            new_excluded_ranges = []
            for excluded in excluded_ranges:
                new_excluded_ranges.extend(range_.exclusion(excluded))
            excluded_ranges = list(new_excluded_ranges)
        return excluded_ranges

    def locations(self, seed_range: SeedRange, source="seed") -> Iterable[SeedRange]:
        if source == "location":
            yield seed_range
            return
        destination, ranges = self.item_ranges[source]
        excluded_seed_ranges = list(self.range_excluded_from(seed_range, ranges))
        for range_ in ranges:
            if new_range := range_.project(seed_range):
                yield from self.locations(new_range, source=destination)
        for excluded_range in excluded_seed_ranges:
            yield from self.locations(excluded_range, source=destination)


class Day05(Solution):
    def parse(self):
        seeds = tokenize(self.lines[0].replace("seeds: ", ""), wrapper=int)
        item_ranges = {}
        for line in self.lines[1:]:
            if not line:
                continue
            if line.endswith("map:"):
                line = line.replace(" map:", "")
                source, destination = line.split("-to-")
                item_ranges[source] = (destination, [])
            else:
                destination_range, source_range, length = tokenize(line, wrapper=int)
                item_ranges[source][1].append(
                    Range(
                        source=source_range,
                        destination=destination_range,
                        length=length,
                    ),
                )
        return Game(seeds, item_ranges)

    def solution1(self):
        game = self.parse()
        return min(map(lambda seed: game.location(seed), game.seeds))

    def solution2(self):
        game = self.parse()
        seed_ranges = list(
            map(lambda x: SeedRange(*x), zip(game.seeds[::2], game.seeds[1::2])),
        )
        final_ranges: list[SeedRange] = []
        for range_ in seed_ranges:
            final_ranges.extend(
                list(game.locations(range_)),
            )
        return min(final_ranges, key=lambda obj: obj.start).start


class Day05Test(Day05):
    @property
    def data(self):
        return """seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
"""
