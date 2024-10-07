from unittest.mock import patch

from advent_of_code.year_2019.day05 import ComputerMemory, Day05


class Day09(Day05):
    def solution1(self):
        return ComputerMemory(self.parsed + [0] * 1000, input_buffer=[1]).run_program()[
            0
        ]

    def solution2(self):
        return ComputerMemory(self.parsed + [0] * 1000, input_buffer=[2]).run_program()[
            0
        ]


class Day09Test(Day09):
    @property
    def line(self):
        return self.data

    @property
    def parsed(self):
        return self.parse()

    def solution1(self):
        with patch.object(
            self, "data", "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
        ):
            original_result = super().solution1()
            assert ",".join(str(x) for x in original_result) == self.data

        with patch.object(self, "data", "1102,34915192,34915192,7,4,7,99,0"):
            assert self.data == "1102,34915192,34915192,7,4,7,99,0"
            original_result = super().solution1()
            assert len(original_result) == 1
            assert len(str(original_result[0])) == 16

        with patch.object(self, "data", "104,1125899906842624,99"):
            original_result = super().solution1()
            assert original_result == [1125899906842624]
