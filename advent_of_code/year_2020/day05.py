from advent_of_code.solution import Solution


def seat_id(boarding_pass: str):
    convert_map = {
        "F": 0,
        "B": 1,
        "R": 1,
        "L": 0,
    }
    decimal_range = 0
    decimal_row = 0
    seat_range = boarding_pass[:7]
    seat_row = boarding_pass[7:]
    for pos, c in enumerate(seat_range[::-1]):
        decimal_range += convert_map[c] * (2**pos)
    for pos, c in enumerate(seat_row[::-1]):
        decimal_row += convert_map[c] * (2**pos)
    return decimal_row + decimal_range * 8


class Day05(Solution):
    def solution1(self):
        return max(seat_id(ticket) for ticket in self.lines)

    def solution2(self):
        all_tickets = sorted(seat_id(ticket) for ticket in self.lines)
        for previous_ticket, ticket in zip(all_tickets, all_tickets[1:]):
            if ticket - previous_ticket == 2:
                return previous_ticket + 1
