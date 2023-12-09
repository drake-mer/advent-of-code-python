def tokenize(raw_line: str, wrapper=lambda _: _, separator=" "):
    raw_line = raw_line.strip()
    return [
        wrapper(token.strip()) for token in raw_line.split(separator) if token.strip()
    ]
