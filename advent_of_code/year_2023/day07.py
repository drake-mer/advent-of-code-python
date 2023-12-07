import dataclasses
from collections import Counter
from functools import cached_property

from advent_of_code.solution import Solution


@dataclasses.dataclass(frozen=True)
class Card:
    figure: str

    def __lt__(self, other):
        assert isinstance(other, Card)
        return self.value < other.value

    @property
    def value(self):
        return CARD_RANKING[self]


CARD_RANKING = {
    Card(card): rank
    for rank, card in enumerate(
        "AKQJT98765432"[::-1],
        1,
    )
}

HAND_RANKING = {
    (1, 1, 1, 1, 1): 1,
    (1, 1, 1, 2): 2,
    (1, 2, 2): 3,
    (1, 1, 3): 4,
    (2, 3): 5,
    (1, 4): 6,
    (5,): 7,
}

HAND_NAME = {
    (1, 1, 1, 1, 1): "none",
    (1, 1, 1, 2): "pair",
    (1, 2, 2): "2-pairs",
    (1, 1, 3): "three-of-a-kind",
    (2, 3): "full-house",
    (1, 4): "square",
    (5,): "five",
}


@dataclasses.dataclass
class Hand:
    cards: list[Card]
    bet: int

    def __eq__(self, other: "Hand"):
        return self.cards == other.cards

    def __lt__(self, other: "Hand"):
        if self.value < other.value:
            return True
        elif self.value > other.value:
            return False
        for sc, oc in zip(self.cards, other.cards):
            if sc == oc:
                continue
            if sc < oc:
                return True
            if sc > oc:
                break
        return False

    @cached_property
    def counted_cards(self):
        return sorted(
            [(c, count) for c, count in Counter(self.cards).items()],
            key=lambda t: (t[1], t[0]),
            reverse=True,
        )

    def __str__(self):
        return str((self.hand, self.counted_cards))

    @cached_property
    def condensed(self):
        return tuple(sorted(Counter(self.cards).values()))

    @cached_property
    def hand(self):
        return HAND_NAME[self.condensed]

    @property
    def value(self):
        return HAND_RANKING[self.condensed]


class Day07(Solution):
    def parse(self):
        all_hands = []
        for row in self.lines:
            cards, bet = row.split()
            cards = list(map(Card, cards))
            bet = int(bet)
            all_hands.append(Hand(cards=cards, bet=bet))
        return all_hands

    def solution1(self):
        print(*[(x.bet, x.value, str(x)) for x in sorted(self.parsed)], sep="\n")
        return sum(
            [position * hand.bet for position, hand in enumerate(sorted(self.parsed), 1)],
        )

    def solution2(self):
        return "not implemented"


class Day07Test(Day07):
    @property
    def data(self):
        return """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""
