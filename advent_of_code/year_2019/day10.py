import dataclasses
import enum
import math
from functools import cache

from advent_of_code.datastructures.dimension2 import BaseMatrix, Coordinate
from advent_of_code.solution import Solution


class SpaceObject(str, enum.Enum):
    asteroid = "#"
    vacuum = "."


@dataclasses.dataclass
class SpaceLocation:
    coordinate: Coordinate
    kind: SpaceObject


def minimal_vector(origin: Coordinate, destination: Coordinate) -> Coordinate:
    dx, dy = destination - origin
    divisor = math.gcd(dx, dy)
    dx = dx // divisor
    dy = dy // divisor
    return Coordinate(dx, dy)


class SpaceMap(BaseMatrix[SpaceLocation]):
    def asteroid_coordinates(self) -> Coordinate:
        for item in self.all_values():
            if item.kind == SpaceObject.asteroid:
                yield item.coordinate

    def find_visible_neighbours(self, c: Coordinate) -> dict[Coordinate, int]:
        visible_neighbour: dict[Coordinate, int] = {}
        for asteroid_coordinate in self.asteroid_coordinates():
            if asteroid_coordinate == c:
                continue
            direction = minimal_vector(c, asteroid_coordinate)
            distance = c.manhattan(asteroid_coordinate)
            if (
                direction in visible_neighbour
                and distance < visible_neighbour[direction]
            ):
                visible_neighbour[direction] = distance
            elif direction not in visible_neighbour:
                visible_neighbour[direction] = distance

        return visible_neighbour


class Day10(Solution):
    def parse(self):
        return SpaceMap.parse_matrix(
            data=self.lines, wrapper=lambda x, c: SpaceLocation(c, SpaceObject(x))
        )

    @cache
    def solution1(self):
        space: SpaceMap = self.parsed
        asteroid_views = {
            ast: len(space.find_visible_neighbours(ast))
            for ast in space.asteroid_coordinates()
        }
        best_station = max(
            (
                (asteroid, nb_visible_asteroids)
                for asteroid, nb_visible_asteroids in asteroid_views.items()
            ),
            key=lambda x: x[1],
        )
        return best_station

    def solution2(self):
        space = self.parsed
        best_station_coordinates, _ = self.solution1()
        visible_neighbours = space.find_visible_neighbours(best_station_coordinates)
        if len(visible_neighbours) <= 200:
            # in case this is needed, just remove all the visible neighbours from the map
            # count the removed neighbours
            # relaunch `find_visible_neighbours`
            # repeat until the whole set of destroyed asteroid can reach 200
            raise NotImplementedError(
                "we did not compute the thing for multiple rotations"
            )

        def angle_with_y_axis(target: Coordinate):
            dx, dy = target
            return math.pi - math.atan2(dx, dy)

        sorted_by_polar = sorted(
            visible_neighbours.keys(), key=lambda x: angle_with_y_axis(x)
        )
        first_asteroid_belt_sorted_by_polar_coordinate = [
            x + best_station_coordinates for x in sorted_by_polar
        ]
        x, y = first_asteroid_belt_sorted_by_polar_coordinate[199]
        return x * 100 + y


class Day10Test(Day10):
    @property
    def data(self):
        return """"""
