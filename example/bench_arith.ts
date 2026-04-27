function work(n: number): number {
    if (n < 2) return n;
    return work(n - 1) + work(n - 2) * 3 - n;
}

work(28);
