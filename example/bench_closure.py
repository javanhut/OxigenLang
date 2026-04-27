def make_adder(x):
    def inner(y):
        return x + y
    return inner


def run(n):
    add5 = make_adder(5)
    total = 0
    i = 0
    while i < n:
        total = total + add5(i)
        i = i + 1
    return total


run(500_000)
