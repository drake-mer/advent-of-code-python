import dataclasses
import functools
import itertools
from collections import Counter
from functools import cached_property
from typing import Literal

from advent_of_code.solution import Solution


@functools.cache
def card_ranking_system(system: Literal[1, 2] = 1):
    if system == 1:
        return {
            Card(figure=card): rank
            for rank, card in enumerate(
                "AKQJT98765432"[::-1],
                1,
            )
        }
    elif system == 2:
        return {
            Card2(figure=card): rank
            for rank, card in enumerate(
                "AKQT98765432J"[::-1],
                1,
            )
        }
    else:
        raise ValueError("ranking system unknown")


@dataclasses.dataclass(frozen=True)
class Card:
    figure: str

    @cached_property
    def card_ranking(cls):
        return card_ranking_system(1)

    def __lt__(self, other):
        assert isinstance(other, type(self))
        return self.value < other.value

    @property
    def value(self):
        return self.card_ranking[self]


class Card2(Card):
    @cached_property
    def card_ranking(cls):
        return card_ranking_system(2)


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

    def __hash__(self):
        return (self.bet, *(c.figure for c in self.cards)).__hash__()

    def __eq__(self, other: "Hand"):
        return self.cards == other.cards

    def __lt__(self, other: "Hand"):
        if self.value == other.value:
            return self.cards < other.cards
        return self.value.__lt__(other.value)

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

    @property
    def value_with_joker(self):
        if not (m := list(self.mutations)):
            return self.value
        return max(h.value for h in m)

    @property
    def mutations(self):
        possible_values = [c for c in self.cards if c.figure != "J"]
        change_positions = [pos for pos, c in enumerate(self.cards) if c.figure == "J"]
        for mutated_set in itertools.product(*itertools.repeat(possible_values, len(change_positions))):
            new_cards = list(self.cards)
            for pos, val in zip(change_positions, mutated_set):
                new_cards[pos] = val
            h = Hand(cards=new_cards, bet=self.bet)
            yield h


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
        return sum([position * hand.bet for position, hand in enumerate(sorted(self.parsed), 1)])

    def solution2(self):
        return sum(
            [
                position * hand.bet
                for position, hand in enumerate(
                    sorted(
                        [
                            Hand(
                                cards=[Card2(figure=c.figure) for c in hand.cards],
                                bet=hand.bet,
                            )
                            for hand in self.parsed
                        ],
                        key=lambda h: (h.value_with_joker, h.cards),
                    ),
                    1,
                )
            ],
        )


class Day07Test(Day07):
    @property
    def data(self):
        return """32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483"""
