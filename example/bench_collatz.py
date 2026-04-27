def collatz_steps(n):
    steps = 0
    while n > 1:
        if n % 2 == 0:
            n = n // 2
        else:
            n = n * 3 + 1
        steps = steps + 1
    return steps


def run(limit):
    total = 0
    n = 1
    while n <= limit:
        total = total + collatz_steps(n)
        n = n + 1
    return total


run(50000)
