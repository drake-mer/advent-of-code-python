import dataclasses
import functools
from typing import Iterable, TypeAlias

from advent_of_code.solution import Solution
from advent_of_code.solution.datastructures.dimension1 import Range


class Ticket(list[int]):
    pass


CategoryName: TypeAlias = str


@dataclasses.dataclass(frozen=True)
class Category:
    name: str
    ranges: tuple[Range]

    def __eq__(self, other):
        return self.name == other.name

    def __hash__(self):
        return self.name.__hash__()

    @functools.cache
    def number_allowed(self, number) -> bool:
        return any(number in ran for ran in self.ranges)


@dataclasses.dataclass
class Game:
    ranges: set[Category]
    ticket: Ticket
    nearby_tickets: list[Ticket]

    def __repr__(self):
        return f"""Game(
    ranges={repr(self.ranges)},
    ticket={repr(self.ticket)},
    nearby_tickets={repr(self.nearby_tickets)},
)"""

    def invalid_number(self, number: int):
        return not any(category.number_allowed(number) for category in self.ranges)

    def possible_fields(self, number: int) -> Iterable[str]:
        for cat in self.ranges:
            if any(number in ran for ran in cat.ranges):
                yield cat.name
                continue

    def invalid_ticket(self, ticket: Ticket):
        for n in ticket:
            if not any(range.number_allowed(n) for range in self.ranges):
                return True
        return False

    @functools.cached_property
    def possible_tickets(self):
        return [
            ticket for ticket in self.nearby_tickets if not self.invalid_ticket(ticket)
        ]

    def possible_arrangements(self):
        print("start for ticket")
        data = list(self.iter_categories())
        print("finish for ticket")
        return data

    def iter_categories(
        self,
        position: int = 0,
        categories: list[Category] = None,
        word: tuple[CategoryName, ...] = (),
    ) -> Iterable[list[CategoryName]]:
        if position == len(self.ticket):
            assert not categories
            assert len(word) == len(self.ticket)
            return word
        if categories is None:
            categories = list(self.ranges)
        possible_categories = list(
            cat
            for cat in categories
            if all(cat.number_allowed(t[position]) for t in self.nearby_tickets)
        )
        if not possible_categories and position < 20:
            return
        for cat in possible_categories:
            new_categories = list(set(categories) - {cat})
            assert len(new_categories) == len(self.ticket) - position - 1
            yield from self.iter_categories(
                position=position + 1, categories=new_categories, word=(*word, cat.name)
            )


class Day16(Solution):
    def parse(self):
        acc_ranges = set()
        acc_tickets = []
        iterator = (li for li in self.lines)
        for line in iterator:
            if not line:
                break
            name, ranges = line.split(": ")
            all_ranges = tuple(
                [Range(*map(int, ran.split("-"))) for ran in ranges.split(" or ")]
            )
            acc_ranges.add(Category(name, all_ranges))
        for line in iterator:
            if line == "your ticket:":
                continue
            if not line:
                break
            ticket = Ticket(map(int, line.split(",")))
        for line in iterator:
            if line == "nearby tickets:":
                continue
            acc_tickets.append(Ticket(map(int, line.split(","))))

        return Game(ranges=acc_ranges, ticket=ticket, nearby_tickets=acc_tickets)

    def solution1(self):
        game: Game = self.parsed
        return sum(
            n
            for ticket in game.nearby_tickets
            for n in ticket
            if game.invalid_number(n)
        )

    def solution2(self):
        game: Game = self.parsed

        def reduce_cats(g_: Game):
            number_set = list(zip(*g_.possible_tickets))
            naive_possibilities = {
                k: [cat for cat in game.ranges if all(map(cat.number_allowed, numbers))]
                for k, numbers in enumerate(number_set)
            }
            marked = {
                k: cats[0] for k, cats in naive_possibilities.items() if len(cats) == 1
            }
            while naive_possibilities:
                for k in marked:
                    naive_possibilities.pop(k, None)
                for cat in marked.values():
                    for k, cat_list in naive_possibilities.items():
                        try:
                            cat_list.remove(cat)
                        except ValueError:
                            pass
                marked.update(
                    {
                        k: cats[0]
                        for k, cats in naive_possibilities.items()
                        if len(cats) == 1
                    }
                )
            return marked

        category_mapping = reduce_cats(game)
        result = 1
        for pos, number in enumerate(game.ticket):
            if category_mapping[pos].name.startswith("departure"):
                result *= number
        return result


class Day16Test(Day16):
    @functools.cached_property
    def data(self):
        return """class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"""

    @functools.cached_property
    def data(self):
        return """class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"""
