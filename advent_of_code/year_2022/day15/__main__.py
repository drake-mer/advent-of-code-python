import pathlib
import re
from collections import namedtuple

basedir = pathlib.Path(__file__).parent

PARSE = r"Sensor at x=(\-?\d+), y=(\-?\d+): closest beacon is at x=(\-?\d+), y=(\-?\d+)"


Coordinate = namedtuple("Coordinate", "x y")


class Beacon(Coordinate):
    pass


class Sensor(Coordinate):
    pass


def manhattan_distance(a: Coordinate, b: Coordinate):
    return abs(a.x - b.x) + abs(b.y - a.y)


def parse_line(line: str) -> tuple[Sensor, Beacon]:
    xs, ys, xb, yb = re.match(PARSE, line).groups()
    return Sensor(int(xs), int(ys)), Beacon(int(xb), int(yb))


class BlockedLocation(Exception):
    pass


class InfiniteFall(Exception):
    pass


def munch_data(payload: str) -> list[tuple[Sensor, Beacon]]:
    return [parse_line(payload) for payload in payload.splitlines(keepends=False)]


def in_range(sensor: Sensor, beacon: Beacon, row: int = None):
    sb = manhattan_distance(sensor, beacon)


def day_1_first_puzzle(payload):
    return len(munch_data((payload)))


def day_1_second_puzzle(payload):
    return len(munch_data(payload))


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
