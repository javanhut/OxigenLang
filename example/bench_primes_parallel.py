import multiprocessing


def count_primes(limit):
    count = 0
    n = 2
    while n < limit:
        is_p = 1
        d = 2
        while d * d <= n:
            if n % d == 0:
                is_p = 0
            d = d + 1
        if is_p == 1:
            count = count + 1
        n = n + 1
    return count


CHUNKS = [40000] * 16

if __name__ == "__main__":
    with multiprocessing.Pool() as pool:
        pool.map(count_primes, CHUNKS)
