import dataclasses
import pathlib
from enum import Enum
from typing import List

basedir = pathlib.Path(__file__).parent


@dataclasses.dataclass
class File:
    size: int
    name: str


@dataclasses.dataclass
class Folder:
    name: str
    parent: "Folder" = None
    folders: list["Folder"] = dataclasses.field(default_factory=list)
    files: list[File] = dataclasses.field(default_factory=list)

    @property
    def size(self):
        return sum(f.size for f in self.files) + sum(d.size for d in self.folders)

    def get_folder(self, name):
        for folder in self.folders:
            if folder.name == name:
                return folder


class Command(str, Enum):
    ls = "ls"
    cd = "cd"


@dataclasses.dataclass
class Instruction:
    instruction: Command
    arguments: None | list[str]


def munch_data(payload: str) -> list[Instruction | Folder | File]:
    all_lines = []
    for line in payload.splitlines(keepends=False):
        if line.startswith("$ "):
            line = line.removeprefix("$ ")
            cmd, *args = line.split(" ")
            all_lines.append(
                Instruction(instruction=Command(cmd), arguments=list(args) or None),
            )
        elif line.startswith("dir "):
            all_lines.append(Folder(line.removeprefix("dir ")))
        else:
            size, name = line.split()
            if not size.isdigit():
                raise ValueError(f"incorrect file format {line}")
            all_lines.append(File(size=int(size), name=name))
    return all_lines


def all_folders(folder: Folder, acc: List[Folder] = None) -> List[Folder]:
    acc = [] if acc is None else acc
    acc.append(folder)
    for folder in folder.folders:
        all_folders(folder, acc=acc)
    return acc


def tree(data: list[Instruction | Folder | File]):
    assert data[0] == Instruction(Command("cd"), ["/"])
    root = Folder("/")
    current: Folder = root
    for d in data[1:]:
        match d:
            case Folder(_):
                current.folders.append(Folder(name=d.name, parent=current))
            case File(_):
                current.files.append(d)
            case Instruction(cmd, args):
                match cmd:
                    case Command.ls:
                        pass
                    case Command.cd:
                        directory = Folder(*args)
                        match directory:
                            case Folder(".."):
                                if current.parent is None:
                                    raise ValueError
                                current = current.parent
                            case Folder("/"):
                                current = root
                            case Folder(_):
                                if not (subfolder := current.get_folder(directory.name)):
                                    raise ValueError
                                current = subfolder
    return root


def day_1_first_puzzle(payload):
    t = tree(munch_data(payload))
    return sum(x.size for x in all_folders(t) if x.size <= 100000)


def day_1_second_puzzle(payload):
    t = tree(munch_data(payload))
    total_size = t.size
    free_memory = 70000000 - total_size
    needed = 30000000 - free_memory
    for folder in sorted(all_folders(t), key=lambda f: f.size):
        if needed <= folder.size:
            return folder.size
    raise


print(day_1_first_puzzle(open(basedir / "input.txt").read()))
print(day_1_second_puzzle(open(basedir / "input.txt").read()))
