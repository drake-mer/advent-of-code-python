import argparse
import contextlib
import datetime
import importlib
import os
import pathlib
import sys
import urllib.request
import urllib

from .solution import Solution

CURRENT_YEAR = datetime.datetime.now().year
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

args = parser.parse_args()


@contextlib.contextmanager
def chdir(new_folder):
    current_folder = os.getcwd()
    print(f"moving from folder {current_folder} to folder {new_folder}")
    os.chdir(new_folder)
    yield
    os.chdir(current_folder)


class YearPath(pathlib.Path):
    def __new__(cls, y: int):
        return pathlib.Path( __file__).parent / f"year_{y}"


class DayInput(str):
    def __new__(cls, d: int):
        return str(f"day{d:02}.txt")


def get_puzzle_input(year: int, day: int):
    def get_token():
        env_file = pathlib.Path(os.getcwd()) / ".env"

        if token := os.getenv("AOC_TOKEN"):
            return token
        if env_file.exists():
            for line in open(env_file):
                line = line.strip()
                if not line:
                    return
                try:
                    key, value = line.split("=")
                    key = key.strip()
                    value = value.strip()
                    if key == "AOC_TOKEN":
                        return value
                except ValueError:
                    return

    if not (data_path := (YearPath(year) / "data")).exists():
        print("data folder does not exist, creating it")
        os.mkdir(data_path)
    if (YearPath(year) / "data" / DayInput(day)).exists():
        print("data already downloaded, skipping")
        return
    if not (session_token := get_token()):
        print("WARNING: could not download puzzle data")
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
        print(f"you might need to refresh your token")
        return
    content = response.read()
    response.close()
    with open(YearPath(year) / "data" / DayInput(day), "w") as f:
        f.write(content.decode())
        print("Successfully downloaded puzzle input, you can start answering...")


def run_solution(year: int, day: int, exit=False):
    day_module_name = f"day{day:02d}"
    year_module_name: str = f"year_{year:04d}"
    try:
        module = importlib.import_module(
            f"advent_of_code.{year_module_name}.{day_module_name}"
        )
    except ImportError as exc:
        if exit:
            print(exc)
            sys.exit(1)
        with open(
            pathlib.Path(__file__).parent / year_module_name / f"{day_module_name}.py",
            "w",
        ) as f:
            f.write(
                f"""
from advent_of_code.solution import Solution


class {day_module_name.title()}(Solution):
    def solution1(self):
        raise NotImplementedError(\"please implement solution 1\")

    def solution2(self):
        raise NotImplementedError(\"please implement solution 2\")
"""
            )
        run_solution(year, day, exit=True)

    solution: Solution = getattr(module, day_module_name.title())(day=day, year=year)
    print("solution 1: ", solution.solution1())
    print("solution 2: ", solution.solution2())


get_puzzle_input(args.year, args.day)
run_solution(args.year, args.day)
