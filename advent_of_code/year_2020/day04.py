import re
from enum import Enum
from typing import TypeAlias

from advent_of_code.solution import Solution


class PassportField(str, Enum):
    byr = "byr"
    iyr = "iyr"
    eyr = "eyr"
    hgt = "hgt"
    hcl = "hcl"
    ecl = "ecl"
    pid = "pid"
    cid = "cid"


def check_height(val):
    if val.endswith("cm"):
        return 150 <= int(val.rstrip("cm")) <= 193
    elif val.endswith("in"):
        return 59 <= int(val.rstrip("in")) <= 76


VALIDATORS = {
    PassportField.byr: lambda val: 1920 <= int(val) <= 2002 and len(val) == 4,
    PassportField.iyr: lambda val: 2010 <= int(val) <= 2020 and len(val) == 4,
    PassportField.eyr: lambda val: 2020 <= int(val) <= 2030 and len(val) == 4,
    PassportField.hgt: check_height,
    PassportField.hcl: lambda val: re.match(r"^#([a-f]|[0-9]){6}$", val),
    PassportField.ecl: lambda val: val
    in ("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
    PassportField.pid: lambda val: val.isdigit() and len(val) == 9,
}


Passport: TypeAlias = dict[PassportField, str]


class Day04(Solution):
    def parse(self) -> list[Passport]:
        def parse_passport(raw_passport) -> Passport:
            result = {}
            raw_fields = [f.strip() for f in raw_passport.split(" ") if f.strip()]
            for field in raw_fields:
                k, v = field.split(":")
                result[PassportField(k)] = v
            return result

        return [
            parse_passport(raw_passport.replace("\n", " "))
            for raw_passport in self.data.split("\n\n")
        ]

    def ok_fields(self):
        return [
            p
            for p in self.parsed
            if (set(PassportField) - {PassportField.cid}).issubset(set(p))
        ]

    def solution1(self):
        return len(
            self.ok_fields(),
        )

    def solution2(self):
        valid_passports = 0
        for passport in self.ok_fields():
            valid_passports += all(
                bool(VALIDATORS[key](passport[key])) for key in VALIDATORS
            )
        return valid_passports
