import dataclasses
from collections import Counter

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
    Card(card): rank for rank, card in enumerate(
        "AKQJT98765432"[::-1], 1
    )
}

HAND_RANKING = {
    (1, 1, 1, 1, 1): 1,
    (1, 1, 1, 2): 2,
    (1, 2, 2): 3,
    (1, 1, 3): 4,
    (2, 3): 5,
    (1, 4): 6,
    (5,): 7
}


@dataclasses.dataclass
class Hand:
    cards: list[Card]
    bet: int

    def __eq__(self, other: "Hand"):
        return self.cards == other.cards

    def __le__(self, other: "Hand"):
        if lesser_hand := self.hand_value < other.hand_value:
            return lesser_hand
        self_cards = list(((card, count) for (card, count) in Counter(self.cards).items()))
        self_cards.sort(
            key=lambda t: t[1],
            reverse=True,
        )
        other_cards = sorted(
            [(card, count) for (card, count) in Counter(other.cards).items()],
            key=lambda t: t[1],
            reverse=True
        )
        assert len(other_cards) == len(self_cards)
        for (sc, _), (oc, _) in zip(self_cards, other_cards):
            if sc == oc:
                continue
            if sc < oc:
                return True
            if sc > oc:
                break
        return False

    @property
    def hand_value(self):
        return HAND_RANKING[tuple(sorted(Counter(self.cards).values()))]



class Day07(Solution):
    def parse(self):
        all_hands = []
        for row in self.lines:
            cards, bet = row.split()
            cards = sorted(Card(c) for c in cards)
            bet = int(bet)
            all_hands.append(Hand(cards=cards, bet=bet))
        return all_hands

    def solution1(self):
        return sum(
            [
                position * hand.bet
                for position, hand in enumerate(sorted(self.parsed), 1)
            ]
        )

    def solution2(self):
        return "not implemented"
