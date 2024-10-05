import dataclasses
from collections import defaultdict
from queue import PriorityQueue
from typing import Callable, Iterable

from advent_of_code.datastructures.dimension2 import (
    BaseMatrix,
    Coordinate,
    Direction,
    turn_left,
    turn_right,
)
from advent_of_code.solution import Solution


@dataclasses.dataclass(frozen=True)
class MoveWay:
    position: Coordinate
    direction: Direction
    steps: int = 1

    def manhattan(self, other):
        return self.position.manhattan(other.position)


@dataclasses.dataclass(frozen=True)
class MovePriority:
    priority: int
    move: MoveWay = dataclasses.field(compare=False)

    def __lt__(self, other):
        return self.priority < other.priority

    def __eq__(self, other):
        return self.priority == other.priority


class Infinity(int):
    def __eq__(self, other):
        return False

    def __le__(self, other):
        return False

    def __ge__(self, other):
        return True

    def __gt__(self, other):
        return True

    def __lt__(self, other):
        return False


class City(BaseMatrix[int]):
    def steps(self, current: MoveWay):
        for new_direction, step_value in (
            (turn_left(current.direction), 1),
            (turn_right(current.direction), 1),
            *(((current.direction, current.steps + 1),) if current.steps < 3 else ()),
        ):
            if (new_pos := (current.position + new_direction)) not in self:
                continue
            yield MoveWay(new_pos, new_direction, steps=step_value)

    def steps2(self, current: MoveWay):
        if current.steps == 0:
            yield MoveWay(
                current.position + Direction.RIGHT * 1,
                Direction.RIGHT,
            )
            yield MoveWay(
                current.position + Direction.DOWN * 1,
                Direction.DOWN,
            )
            return

        elif current.steps < 4:
            new = MoveWay(
                current.position + current.direction,
                current.direction,
                steps=current.steps + 1,
            )
            if new.position not in self:
                return
            yield new
        elif current.steps >= 4:
            for new_direction, new_steps in (
                (turn_left(current.direction), 1),
                (turn_right(current.direction), 1),
            ):
                if (new_pos := (current.position + new_direction)) not in self:
                    continue
                yield MoveWay(new_pos, new_direction, steps=new_steps)
            if current.steps < 10:
                res = MoveWay(
                    current.position + current.direction,
                    current.direction,
                    steps=current.steps + 1,
                )
                if res.position not in self:
                    return
                yield res

    def path_lookup(self, step_function: Callable[[MoveWay], Iterable[MoveWay]] = None):
        target = Coordinate(self.width - 1, self.height - 1)

        def heuristic_function(move: MoveWay) -> int:
            return move.position.manhattan(target)

        def get_score(node: MoveWay):
            total = self[node.position]
            while node in previous_node:
                node = previous_node[node]
                total += self[node.position]
            return total - self[Coordinate(0, 0)]

        known_nodes_queue = PriorityQueue()
        known_nodes_queue.put(
            MovePriority(0, start := MoveWay(Coordinate(0, 0), Direction.RIGHT, 0))
        )
        known_nodes = {start}
        # the preceding node for each one of the visited node
        previous_node: dict[MoveWay, MoveWay] = dict()
        real_distance: dict[MoveWay, int] = defaultdict(lambda: Infinity())
        heuristic_guess: dict[MoveWay, int] = defaultdict(lambda: Infinity())
        real_distance[start] = 0
        heuristic_guess[start] = 0
        while known_nodes_queue:
            current_node = known_nodes_queue.get().move
            if current_node.position == target:
                return get_score(current_node)
            known_nodes.remove(current_node)
            for step in step_function(current_node):
                cost = real_distance[current_node] + self[step.position]
                if cost < real_distance[step]:
                    previous_node[step] = current_node
                    real_distance[step] = cost
                    heuristic_guess[step] = cost + heuristic_function(step)
                    if step not in known_nodes:
                        known_nodes_queue.put(
                            MovePriority(priority=heuristic_guess[step], move=step)
                        )
                        known_nodes.add(step)

        raise ValueError()


class Day17(Solution):
    def parse(self):
        return City(
            content=BaseMatrix.parse_matrix(
                data=self.lines, wrapper=lambda x, c: int(x)
            ).content
        )

    def solution1(self):
        return self.parsed.path_lookup(step_function=self.parsed.steps)

    def solution2(self):
        return self.parsed.path_lookup(step_function=self.parsed.steps2)


class Day17Test(Day17):
    @property
    def data(self):
        return """2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533"""
