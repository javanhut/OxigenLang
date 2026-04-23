class Counter {
    val: number = 0;

    inc(): void {
        this.val = this.val + 1;
    }

    add(amount: number): void {
        this.val = this.val + amount;
    }

    get(): number {
        return this.val;
    }
}

function run(n: number): number {
    const c = new Counter();
    let i = 0;
    while (i < n) {
        c.inc();
        c.add(i);
        i = i + 1;
    }
    return c.get();
}

run(500_000);
