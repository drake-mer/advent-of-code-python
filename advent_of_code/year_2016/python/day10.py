

data = [ instruction for instruction in open('input_day_10.txt') ]

bot_indices = map(
    lambda x: int(x.split()[1]),
    filter(lambda x: x.startswith('bot'), data)
)

bots = {k:{'h': None, 'l': None} for k in range(1, max(bot_indices)+1)}



for instruction in data:
    if instruction.startswith('bot'):
        a, current_bot, b, c, d, e, low_bot, f, g, h, i, high_bot, _ = instruction.split()

        current_bot = int(current_bot)
        low_bot = int(low_bot)
        high_bot = int(high_bot)

        if bots[current_bot]['h']:
            if not bots[high_bot]['h'] and not bots[high_bot]['l']:
                bots[high_bot]['h'] = bots[index]['h']
                bots[index]['h']=None
            if bots[high_bot]['h'] or bots[high_bot]['l']:
                bots[high_bot]['l'], bots[low_bot]['h'] = sorted(filter(None, (bots[high_bot]['h'], bots[high_bot]['l'], bots[current_bot]['h'])))

        bots[index] = {'l': low_bot, 'h': high_bot}
