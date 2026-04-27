def loop_sum(n):
    total = 0
    i = 1
    while i <= n:
        total = total + i * 2
        i = i + 1
    return total


loop_sum(1_000_000)
