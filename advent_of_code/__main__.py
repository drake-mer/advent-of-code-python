import argparse
import datetime
import functools
import importlib
import os
import pathlib
import sys
import urllib.request
import webbrowser
from typing import Literal

from .solution import Solution
from .utils import get_env

CURRENT_YEAR = (
    int(year) if (year := get_env("AOC_YEAR")) else datetime.datetime.now().year
)
CURRENT_DAY = datetime.datetime.now().day

parser = argparse.ArgumentParser(
    prog="AdventOfCode CLI",
    description="A small CLI program to handle advent of code problems",
)
parser.add_argument(
    "-y",
    "--year",
    choices=range(2015, CURRENT_YEAR + 1),
    default=CURRENT_YEAR,
    help="The year of the puzzle",
    type=int,
)
parser.add_argument(
    "-d",
    "--day",
    choices=range(1, 26),
    default=CURRENT_DAY,
    help="The day of the puzzle",
    type=int,
)
parser.add_argument(
    "--download",
    help="Force downloading (aka refresh) the input",
    action="store_true",
)
parser.add_argument(
    "-s",
    "--submit",
    help="Submit your solution to question 1 or 2",
    default=0,
    choices=(1, 2),
    type=int,
)
parser.add_argument(
    "-t",
    "--test",
    help="Run the class DayXXTest solution1 and solution2 methods (useful for test examples)",
    action="store_true",
)
parser.add_argument(
    "-n", "--new-tab", help="open a new browser page on the puzzle", action="store_true"
)
args = parser.parse_args()


class YearFolder(pathlib.Path):
    def __new__(cls, y: int):
        return pathlib.Path(__file__).parent / f"year_{y}"


class Day(str):
    def __new__(cls, d: int):
        return str(f"day{d:02}")


class DayInput(str):
    def __new__(cls, d: int):
        return str(Day(d) + ".txt")


class DayModule(str):
    def __new__(cls, d: int):
        return str(Day(d) + ".py")


def download_input_data(year: int, day: int):
    if not (session_token := get_env()):
        print("WARNING: could not download puzzle data, token is missing.")
        print(
            "Try putting AOC_TOKEN (session cookie value) into your environment or a '.env' file",
        )
        return
    request = urllib.request.Request(
        method="GET",
        headers={
            "cookie": f"session={session_token}",
        },
        url=f"https://adventofcode.com/{year}/day/{day}/input",
    )
    response = urllib.request.urlopen(request)
    if response.code != 200:
        print(f"response from server has status code {response.code}")
        print(f"response content: {response.read()}")
        print("you might need to refresh your token")
        sys.exit(1)
    content = response.read()
    response.close()
    with open(YearFolder(year) / "data" / DayInput(day), "w") as f:
        f.write(content.decode())
        print(
            "Successfully downloaded puzzle input, you can start solving the puzzle...",
        )


def push_solution(year=None, day=None, level=None, solution=None):
    if not (solution and year and day and level):
        raise ValueError("you must specify valid input parameters")
    if not (token := get_env()):
        raise ValueError("token is not set for answer submission")
    request = urllib.request.Request(
        method="POST",
        headers={
            "cookie": f"session={token}",
            "content-type": "application/x-www-form-urlencoded",
        },
        data=f"level={level}&answer={solution}".encode(),
        url=f"https://adventofcode.com/{year}/day/{day}/answer",
    )
    response = urllib.request.urlopen(request)
    print(
        f"solution for day {day=}, {year=} at {level=} submitted successfully with status HTTP {response.code=}",
    )
    content_response = response.read().decode()
    if (
        "you have to wait after submitting an answer before trying again"
        in content_response
    ):
        print("please wait before resubmitting")
    elif "That's the right answer!" in content_response:
        print(f"You seem to have found the correct answer to {level=}, {day=}, {year=}")
    elif "That's not the right answer." in content_response:
        print(
            f"you seem to have given an incorrect answer to {level=}, {day=}, {year=}",
        )
    elif (
        s := "That's not the right answer; your answer is too high."
    ) in content_response:
        print(f"Miss for {solution=}: \"{s}\"")
    elif (
        s := "That's not the right answer; your answer is too low."
    ) in content_response:
        print(s)
    elif (
        s := "You don't seem to be solving the right level.  Did you already complete it?"
    ) in content_response:
        print(s)
    else:
        print("<================ Unhandled response ===============>")
        print(content_response)


def prepare_puzzle_data_and_layout(
    year: int, day: int, refresh_input=False, new_tab=False
):
    if not (year_path := YearFolder(year)).exists():
        print("year folder does not exist, creating it")
        os.mkdir(year_path)

    if not (year_path / "__init__.py").exists():
        with open(year_path / "__init__.py", "w") as f:
            f.write(f'"""AOC year={year}\n"""')

    if not (year_path / DayModule(day)).exists():
        with open(year_path / DayModule(day), "w") as f:
            f.write(
                f"""from advent_of_code.solution import Solution


class {Day(day).title()}(Solution):
    def parse(self):
        raise NotImplementedError()

    def solution1(self):
        return "not implemented"

    def solution2(self):
        return "not implemented"


class {Day(day).title()}Test({Day(day).title()}):
    @property
    def data(self):
        return \"\"\"\"\"\"
""",
            )

    if not (data_path := (YearFolder(year) / "data")).exists():
        print("data folder does not exist, creating it")
        os.mkdir(data_path)
    if (not (YearFolder(year) / "data" / DayInput(day)).exists()) or new_tab:
        webbrowser.open(f"https://adventofcode.com/{year}/day/{day}")
    if not (YearFolder(year) / "data" / DayInput(day)).exists() or refresh_input:
        download_input_data(year=year, day=day)


def run_solution(year: int, day: int, submit: Literal[0, 1, 2] = 0, test: bool = False):
    day_module_name = f"day{day:02d}"
    year_module_name: str = f"year_{year:04d}"
    module = importlib.import_module(
        f"advent_of_code.{year_module_name}.{day_module_name}",
    )
    solution: Solution = getattr(
        module,
        day_module_name.title() + ("Test" if test else ""),
    )(day=day, year=year)
    print("solution 1:", (s1 := solution.solution1()))
    print("solution 2:", (s2 := solution.solution2()))
    push = functools.partial(push_solution, day=day, year=year)
    if submit == 1 and s1 and s1 != "not implemented" and not test:
        push(level=submit, solution=s1)
    elif submit == 2 and s2 and s2 != "not implemented" and not test:
        push(level=submit, solution=s2)


prepare_puzzle_data_and_layout(args.year, args.day, args.download, args.new_tab)
run_solution(args.year, args.day, args.submit, args.test)
