def nested_sum(n):
    total = 0
    i = 1
    while i <= n:
        j = 1
        while j <= n:
            total = total + i * j
            j = j + 1
        i = i + 1
    return total


nested_sum(2000)
