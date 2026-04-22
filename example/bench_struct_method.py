class Counter:
    __slots__ = ("val",)

    def __init__(self):
        self.val = 0

    def inc(self):
        self.val = self.val + 1

    def add(self, amount):
        self.val = self.val + amount

    def get(self):
        return self.val


def run(n):
    c = Counter()
    i = 0
    while i < n:
        c.inc()
        c.add(i)
        i = i + 1
    return c.get()


run(500_000)
