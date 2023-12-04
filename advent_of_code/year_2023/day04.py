import dataclasses
from collections import Counter
from functools import cached_property

from advent_of_code.solution import Solution


@dataclasses.dataclass
class Card:
    id: int
    winning_numbers: set[int]
    numbers: set[int]

    @cached_property
    def score(self) -> int:
        if number_match := self.numbers & self.winning_numbers:
            return 2 ** (len(number_match) - 1)
        return 0

    @cached_property
    def matching_numbers(self) -> int:
        return len(self.numbers & self.winning_numbers)

    @classmethod
    def parse(cls, line):
        card, numbers = line.split(":")
        card_id = int(card.replace("Card ", ""))
        winning, numbers = numbers.split("|")
        winning = [int(w.strip()) for w in winning.split() if w.strip()]
        you_have = [int(w.strip()) for w in numbers.split() if w.strip()]
        return cls(id=card_id, winning_numbers=set(winning), numbers=set(you_have))


class Day04(Solution):
    def parse(self):
        return [Card.parse(line) for line in self.lines]

    def solution1(self):
        return sum(card.score for card in self.parsed)

    def solution2(self):
        cards = {card.id: card for card in self.parsed}
        result = Counter([card.id for card in self.parsed])
        for card_id in sorted(cards):
            card = cards[card_id]
            for next_card in range(card.id + 1, card.id + 1 + card.matching_numbers):
                result[next_card] += result[card.id]
        return sum(result.values())
