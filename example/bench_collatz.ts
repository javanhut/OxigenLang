function collatzSteps(n: number): number {
    let steps = 0;
    while (n > 1) {
        if (n % 2 === 0) {
            n = Math.floor(n / 2);
        } else {
            n = n * 3 + 1;
        }
        steps = steps + 1;
    }
    return steps;
}

function run(limit: number): number {
    let total = 0;
    let n = 1;
    while (n <= limit) {
        total = total + collatzSteps(n);
        n = n + 1;
    }
    return total;
}

run(50000);
