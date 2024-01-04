import os
import pathlib


def tokenize(raw_line: str, wrapper=lambda _: _, separator=" "):
    raw_line = raw_line.strip()
    return [wrapper(token.strip()) for token in raw_line.split(separator) if token.strip()]


def get_env(name: str = "AOC_TOKEN"):
    env_file = pathlib.Path(os.getcwd()) / ".env"

    if token := os.getenv(name):
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
                if key == name:
                    return value
            except ValueError:
                return
