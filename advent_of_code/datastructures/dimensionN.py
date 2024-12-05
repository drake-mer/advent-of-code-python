import dataclasses
from typing import Any, Generic, Iterable, Sized, TypeAlias, TypeVar

C: TypeAlias = TypeVar("C")
T: TypeAlias = TypeVar("T")


@dataclasses.dataclass
class GenericMap(Generic[C, T], Sized):
    default: Any = None
    content: dict[C, T] = None

    def __post_init__(self):
        if not self.content:
            self.content = dict()

    def __getitem__(self, c: C) -> T:
        return self.content.get(c, self.default)

    def __setitem__(self, c: C, v: T):
        return self.content.__setitem__(c, v)

    def __iter__(self):
        yield from self.content.__iter__()

    def __len__(self):
        return self.content.__len__()

    def __contains__(self, item):
        return self.content.__contains__(item)

    def values(self):
        yield from self.content.values()

    def items(self) -> Iterable[tuple[C, T]]:
        yield from self.content.items()
