import dataclasses
import itertools
import math
from collections import defaultdict
from typing import Counter, NamedTuple

from advent_of_code.datastructures.dimension3 import Coordinate
from advent_of_code.solution import Solution


def parse_planet(s: str) -> Coordinate:
    s = s.strip("<>")
    s = s.replace(" ", "")
    s = s.replace("x=", "").replace("y=", "").replace("z=", "")
    return Coordinate(*map(int, s.split(",")))


def potential(p, *others) -> Coordinate:
    p_x = 0
    p_y = 0
    p_z = 0
    for other_planet in others:
        p_x += -1 if (other_planet.x < p.x) else 1 if (other_planet.x > p.x) else 0
        p_y += -1 if (other_planet.y < p.y) else 1 if (other_planet.y > p.y) else 0
        p_z += -1 if (other_planet.z < p.z) else 1 if (other_planet.z > p.z) else 0

    return Coordinate(p_x, p_y, p_z)


def null():
    return Coordinate(0, 0, 0)


def energy(c: Coordinate):
    return abs(c.x) + abs(c.y) + abs(c.z)


@dataclasses.dataclass
class Planet:
    position: Coordinate
    speed: Coordinate = dataclasses.field(default_factory=null)

    @property
    def energy(self):
        return energy(self.position) * energy(self.speed)

    def __repr__(self):
        return (
            f"pos=<x={self.position.x: 3d}, y={self.position.y: 3d}, z={self.position.z: 3d}>"
            + f" vel=<x={self.speed.x: 3d}, y={self.speed.y: 3d}, z={self.speed.z: 3d}>"
        )


@dataclasses.dataclass
class System:
    planets: list[Planet]

    @property
    def total_energy(self):
        return sum(p.energy for p in self.planets)

    def evolve_step(self):
        for k, planet in enumerate(self.planets):
            others = [o for n, o in enumerate(self.planets) if n != k]
            planet.speed += potential(planet.position, *(o.position for o in others))

        for k, planet in enumerate(self.planets):
            planet.position += planet.speed

    def __repr__(self):
        return "\n".join(repr(p) for p in self.planets)


class UniverseEvent(NamedTuple):
    instant: int
    situation: tuple[int, int, int, int, int, int, int, int]


class AllCyclesFound(ValueError):
    pass


class Day12(Solution):
    def parse(self):
        return System(planets=[Planet(parse_planet(p)) for p in self.lines])

    def solution1(self):
        lunar_system: System = self.parsed
        for k in range(1000):
            lunar_system.evolve_step()
        return lunar_system.total_energy

    def solution2(self):
        lunar_system: System = self.parsed
        x_events = defaultdict(list)
        y_events = defaultdict(list)
        z_events = defaultdict(list)
        x_cycle = list()
        y_cycle = list()
        z_cycle = list()

        def record_cycles(iteration: int):
            along_x = tuple(
                itertools.chain(
                    (planet.position.x, planet.speed.x)
                    for planet in lunar_system.planets
                )
            )
            along_y = tuple(
                itertools.chain(
                    (planet.position.y, planet.speed.y)
                    for planet in lunar_system.planets
                )
            )
            along_z = tuple(
                itertools.chain(
                    (planet.position.z, planet.speed.z)
                    for planet in lunar_system.planets
                )
            )
            if not x_cycle:
                x_events[along_x].append(iteration)
            if not y_cycle:
                y_events[along_y].append(iteration)
            if not z_cycle:
                z_events[along_z].append(iteration)
            if len(z_events[along_z]) > 1:
                z_cycle.append(z_events[along_z])
            if len(y_events[along_y]) > 1:
                y_cycle.append(y_events[along_y])
            if len(x_events[along_x]) > 1:
                x_cycle.append(x_events[along_x])

        nb_iterations = 0
        while not all((x_cycle, y_cycle, z_cycle)):
            record_cycles(nb_iterations)
            lunar_system.evolve_step()
            nb_iterations += 1
        x_cycle, *_ = x_cycle
        y_cycle, *_ = y_cycle
        z_cycle, *_ = z_cycle
        a = x_cycle[1] - x_cycle[0]
        b = y_cycle[1] - y_cycle[0]
        c = z_cycle[1] - z_cycle[0]
        return math.lcm(a, b, c)
        print(x_cycle, y_cycle, z_cycle, sep="\n")


class Day12Test(Day12):
    @property
    def data(self):
        return """<x=-1, y=0, z=2>
<x=2, y=-10, z=-7>
<x=4, y=-8, z=8>
<x=3, y=5, z=-1>"""
