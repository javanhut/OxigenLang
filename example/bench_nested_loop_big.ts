function nestedSum(n: number): number {
    let total = 0;
    let i = 1;
    while (i <= n) {
        let j = 1;
        while (j <= n) {
            total = total + i * j;
            j = j + 1;
        }
        i = i + 1;
    }
    return total;
}

nestedSum(2000);
