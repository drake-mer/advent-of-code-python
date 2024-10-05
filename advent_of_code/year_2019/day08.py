import enum

from advent_of_code.solution import Solution


class Pixel(str, enum.Enum):
    WHITE = "0"
    BLACK = "1"
    TRANSPARENT = "2"


class Layer(list[Pixel]):
    def __init__(self, width=25, height=6):
        super().__init__()
        self.width = width
        self.height = height

    def superpose(self, layer: "Layer"):
        output = Layer(width=self.width, height=self.height)
        for k, l in zip(self, layer):
            if k == Pixel.TRANSPARENT:
                output.append(l)
            else:
                output.append(k)
        return output

    def is_full(self):
        return self.count("2") == 0

    def display(self):
        for k in range(self.height):
            print(
                "".join(
                    ("*" if u == "1" else " " if u == "0" else " " if u == "2" else "N")
                    for u in self[k * self.width : (k + 1) * self.width]
                )
            )
        print()


class Day08(Solution):
    WIDTH = 25
    HEIGHT = 6

    def parse(self) -> list[Layer]:
        layers = []
        line = self.line
        k = 0
        frame = line[k * self.WIDTH * self.HEIGHT : (k + 1) * self.WIDTH * self.HEIGHT]
        while frame:
            layer = Layer(width=self.WIDTH, height=self.HEIGHT)
            layer.extend(frame)
            layers.append(layer)
            k += 1
            frame = line[
                k * self.WIDTH * self.HEIGHT : (k + 1) * self.WIDTH * self.HEIGHT
            ]
        return layers

    def solution1(self):
        frame_with_minimum_zeroes = min(self.parsed, key=lambda f: f.count("0"))
        return frame_with_minimum_zeroes.count("1") * frame_with_minimum_zeroes.count(
            "2"
        )

    def solution2(self):
        frame, *others = self.parsed
        while not frame.is_full() and others:
            next_frame, *others = others
            frame = frame.superpose(next_frame)
        frame.display()


class Day08Test(Day08):
    WIDTH = 2
    HEIGHT = 2

    @property
    def data(self):
        return "0222112222120000"
