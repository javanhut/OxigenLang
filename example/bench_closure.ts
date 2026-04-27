function makeAdder(x: number): (y: number) => number {
    return function (y: number): number {
        return x + y;
    };
}

function run(n: number): number {
    const add5 = makeAdder(5);
    let total = 0;
    let i = 0;
    while (i < n) {
        total = total + add5(i);
        i = i + 1;
    }
    return total;
}

run(500_000);
