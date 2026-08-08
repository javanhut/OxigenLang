function loopSum(n: number): number {
    let total = 0;
    let i = 1;
    while (i <= n) {
        total = total + i * 2;
        i = i + 1;
    }
    return total;
}

loopSum(20_000_000);
