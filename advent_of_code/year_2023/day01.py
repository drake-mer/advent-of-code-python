from advent_of_code.solution import Solution

EXTENDED_DIGITS = {
    "one": "1",
    "two": "2",
    "three": "3",
    "four": "4",
    "five": "5",
    "six": "6",
    "seven": "7",
    "eight": "8",
    "nine": "9",
    "1": "1",
    "2": "2",
    "3": "3",
    "4": "4",
    "5": "5",
    "6": "6",
    "7": "7",
    "8": "8",
    "9": "9",
}


class Day01(Solution):
    def solution1(self):
        def extract_digits(line):
            for c in line:
                if c.isdigit():
                    break
            for d in line[::-1]:
                if d.isdigit():
                    break
            return int(c + d)

        return sum(extract_digits(l) for l in self.lines)

    def solution2(self):
        def extract_digits(line):
            fc = None
            lc = None
            for pos, c in enumerate(line):
                for digit, value in EXTENDED_DIGITS.items():
                    if line[pos:].startswith(digit):
                        fc = value
                        break
                if fc:
                    break
            line = line[::-1]
            for pos, c in enumerate(line):
                for digit, value in EXTENDED_DIGITS.items():
                    if line[pos:].startswith(digit[::-1]):
                        lc = value
                        break
                if lc:
                    break
            return int(fc + lc)

        return sum(extract_digits(line) for line in self.lines)
