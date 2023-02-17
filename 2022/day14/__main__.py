import pathlib
from collections import namedtuple
from enum import Enum
from typing import Iterable

basedir = pathlib.Path(__file__).parent


class MapValue(str, Enum):
    SAND = 'o'
    ROCK = '#'
    AIR = '.'


Coordinate = namedtuple('Coordinate', 'x y')


class Map(dict[Coordinate, MapValue]):
    def __setitem__(self, key, value):
        if key in self:
            blocking = self[key]
            raise BlockedLocation(f"location {key} contains {blocking}")
        super().__setitem__(key, value)

    def __repr__(self):
        max_y = lowest_rock(self).y + 2
        max_x = max(c.x for c in self) + 2
        min_x = min(c.x for c in self) - 2
        output = [['.' for x in range(min_x, max_x + 1)] for y in range(0, max_y + 1)]
        for pos, item in self.items():
            x, y = pos
            output[y][x - min_x] = item.value
        return "\n".join(''.join(row) for row in output)


class BlockedLocation(Exception):
    pass


class InfiniteFall(Exception):
    pass


def crange(a: Coordinate, b: Coordinate) -> Iterable[Coordinate]:
    def start_end(m, n):
        if m == n:
            raise ValueError()
        elif m > n:
            return list(range(m, n - 1, -1))
        elif m < n:
            return list(range(m, n + 1, 1))

    if a == b:
        return
    if a.x == b.x:
        yield from (Coordinate(a.x, y) for y in start_end(a.y, b.y))
    elif a.y == b.y:
        yield from (Coordinate(x, a.y) for x in start_end(a.x, b.x))
    else:
        raise ValueError()


def tries(a: Coordinate) -> Iterable[Coordinate]:
    for (dx, dy) in [(0, 1), (-1, 1), (1, 1)]:
        yield Coordinate(a.x + dx, a.y + dy)


def munch_data(payload: str) -> Map:
    big_map = Map()
    for pos, payload in enumerate(payload.splitlines(keepends=False)):
        rocks = [Coordinate(*map(int, x.split(','))) for x in payload.split(' -> ')]
        for start, end in zip(rocks, rocks[1:]):
            for position in crange(start, end):
                if position in big_map:
                    continue
                big_map[position] = MapValue.ROCK
    return big_map


def lowest_rock(big_map: Map) -> Coordinate:
    return max([c for c in big_map if big_map[c] == MapValue.ROCK], key=lambda c: c.y)


def sand_fall1(new_map: Map, start=Coordinate(500, 0)) -> Map:
    max_y = lowest_rock(new_map).y + 1
    assert start not in new_map
    sand = start
    new_map[sand] = MapValue.SAND
    while True:
        if sand.y > max_y:
            new_map.pop(sand)
            raise InfiniteFall()
        for possible_position in tries(sand):
            if possible_position not in new_map:
                new_map[possible_position] = new_map.pop(sand)
                sand = possible_position
                break
        else:
            return new_map


def sand_fall2(new_map: Map, sand=Coordinate(500, 0), floor=None):
    if not floor:
        raise ValueError()

    new_map[sand] = MapValue.SAND
    while True:
        for possible_position in tries(sand):
            if possible_position in new_map or possible_position.y >= floor:
                continue
            new_map[possible_position] = new_map.pop(sand)
            sand = possible_position
            break
        else:
            return new_map


def day_1_first_puzzle(payload):
    big_map = munch_data(payload)
    total_sand = 0
    while True:
        try:
            big_map = sand_fall1(big_map)
            total_sand += 1
        except InfiniteFall:
            return total_sand


def day_1_second_puzzle(payload):
    big_map = munch_data(payload)
    floor = lowest_rock(big_map).y + 2
    total_sand = 0
    while True:
        try:
            big_map = sand_fall2(big_map, floor=floor)
            total_sand += 1
        except BlockedLocation:
            print(big_map)
            return total_sand


print(day_1_first_puzzle(open(basedir / 'input.txt').read()))
print(day_1_second_puzzle(open(basedir / 'input.txt').read()))
