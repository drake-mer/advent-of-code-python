import collections
import dataclasses
from typing import TypeAlias, Counter

from advent_of_code.solution import Solution


Label: TypeAlias = str


@dataclasses.dataclass(frozen=True)
class Compound:
    label: Label
    quantity: int


def parse_compound(comp: str) -> Compound:
    quantity, label = comp.strip().split(" ")
    quantity = int(quantity)
    return Compound(label, quantity)


def ore_equivalent(compound: Compound, formula_list, remains=None):
    # if remains is None:
    #     remains = {}
    if compound.label == "ORE":
        return compound.quantity

    need = compound.quantity
    rhs, lhs = formula_list[compound.label]
    if rhs.quantity < need:
        coef = need // rhs.quantity + (1 if need % rhs.quantity else 0)
    else:
        coef = 1
    return coef * sum(ore_equivalent(comp, formula_list) for comp in lhs)


def parse_line(line: str) -> tuple[Compound, list[Compound]]:
    lhs, rhs = line.split(" => ")
    lhs = [parse_compound(comp) for comp in lhs.split(",")]
    rhs = parse_compound(rhs)
    return rhs, lhs


def convert_compound_store(compound_store: dict[Label, int], left_over: dict[Label, int], recipes):
    print("old store:", compound_store)
    print("left over:", left_over)
    new_compound_store = collections.Counter({k: v for k, v in compound_store.items() if k == "ORE"})
    for element, needed_quantity in compound_store.items():
        if element == "ORE":
            continue
        final_compound, reactives = recipes[element]
        if left_over.get(element):
            if needed_quantity >= left_over[element]:
                left_over[element] = 0
                needed_quantity = needed_quantity - left_over.get(element)
            else:
                left_over[element] = left_over[element] - needed_quantity
                needed_quantity = 0

        needs_extra = bool(needed_quantity % final_compound.quantity)
        multiplier = (needed_quantity // final_compound.quantity) + bool(needed_quantity % final_compound.quantity)
        if needs_extra:
            left_over[element] += needed_quantity % final_compound.quantity
        for reactive in reactives:
            new_compound_store[reactive.label] += (multiplier * reactive.quantity)
    print("new store:", new_compound_store)
    return new_compound_store, left_over


class Day14(Solution):
    def parse(self) -> dict[Label, tuple[Compound, list[Compound]]]:
        final_map = {}
        for line in self.lines:
            rhs, lhs = parse_line(line)
            final_map.update({rhs.label: (rhs, lhs)})
        return final_map

    def solution1(self):
        recipes = self.parsed
        compound_store = collections.Counter({"FUEL": 1})
        left_over = collections.Counter()
        while list(compound_store) != ["ORE"]:
            compound_store, left_over = convert_compound_store(compound_store, left_over, recipes)
        return compound_store["ORE"]

    def solution2(self):
        return "not implemented"


class Day14Test(Day14):
    @property
    def data(self):
        return """157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"""
