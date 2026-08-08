/* Phase 2.3: A/B of the two candidate lean-entry ABIs.
 *
 * Measures the ABI itself rather than a JIT integration, so the design choice
 * can be made before either one is built. Each variant runs the bench_arith
 * body (`work(34)`), which is statically infallible apart from recursion depth
 * — exactly the precondition design 2 requires.
 *
 *     cc -O2 -o /tmp/abi_ab example/controls/abi_ab.c && /tmp/abi_ab
 *
 * `noinline` is not a handicap, it is the point: in the JIT these are separately
 * compiled entries reached by a call, so letting the C compiler inline the
 * recursion away would measure something that cannot happen.
 *
 * All variants must print the same value (-646393729142) or the comparison is
 * meaningless — the arithmetic is done in uint64_t because signed overflow is
 * UB in C and at -O2 the optimizer may assume it never occurs, while Oxigen
 * specifies two's-complement wrap.
 */
#include <stdio.h>
#include <stdint.h>
#include <time.h>

#define BUDGET 16384

/* ── Baseline: what a native function costs with no VM contract at all. ── */
__attribute__((noinline)) static int64_t w_base(int64_t n) {
    if (n < 2) return n;
    uint64_t a = (uint64_t)w_base(n - 1);
    uint64_t b = (uint64_t)w_base(n - 2);
    return (int64_t)(a + b * 3u - (uint64_t)n);
}

/* ── Design 1: fn(vm, args..., budget) -> (status, i64) ──
 * Keeps status propagation. Every recursive call must test the callee's status
 * before using its value, so the error path stays a plain return.
 */
typedef struct { int32_t status; int64_t val; } r1_t;

__attribute__((noinline)) static r1_t w_d1(void *vm, int64_t n, int64_t budget) {
    if (__builtin_expect(budget <= 0, 0)) { r1_t r = {1, 0}; return r; }
    if (n < 2) { r1_t r = {0, n}; return r; }
    r1_t a = w_d1(vm, n - 1, budget - 1);
    if (__builtin_expect(a.status != 0, 0)) return a;
    r1_t b = w_d1(vm, n - 2, budget - 1);
    if (__builtin_expect(b.status != 0, 0)) return b;
    r1_t r = {0, (int64_t)((uint64_t)a.val + (uint64_t)b.val * 3u - (uint64_t)n)};
    return r;
}

/* ── Design 2: infallible fn(args..., budget) -> i64 ──
 * No vm pointer, no status. Recursion exhaustion branches to a cold adapter
 * that never returns to the fast path, so callers need no per-call check.
 */
static volatile int g_overflowed = 0;
static int64_t g_depth_result = 0;

__attribute__((noinline, cold)) static int64_t cold_overflow(void) {
    g_overflowed = 1;
    return 0;
}

__attribute__((noinline)) static int64_t w_d2(int64_t n, int64_t budget) {
    if (__builtin_expect(budget <= 0, 0)) return cold_overflow();
    if (n < 2) return n;
    uint64_t a = (uint64_t)w_d2(n - 1, budget - 1);
    uint64_t b = (uint64_t)w_d2(n - 2, budget - 1);
    return (int64_t)(a + b * 3u - (uint64_t)n);
}

/* ── Design 2 without the budget, to price the budget separately. ── */
__attribute__((noinline)) static int64_t w_d2_nobudget(int64_t n) {
    if (n < 2) return n;
    uint64_t a = (uint64_t)w_d2_nobudget(n - 1);
    uint64_t b = (uint64_t)w_d2_nobudget(n - 2);
    return (int64_t)(a + b * 3u - (uint64_t)n);
}

/* ── Design 2 + non-local cold exit ──
 * Design 2's "infallible" claim breaks at exactly one point: recursion
 * exhaustion has no status channel to report through. Returning a sentinel is
 * impossible (every i64 is a legal result) and per-call checking is design 1.
 * The remaining option is a non-local exit, which skips every intermediate
 * frame without costing the fast path anything.
 *
 * Safe here for the same reason it would be in the JIT: these frames own no
 * destructors. VM bookkeeping (`stack_view.len`, `jit_frame_view.len`) is
 * restored from a snapshot at the boundary — which is precisely the
 * reconstruction description Phase 2.4 already requires.
 */
#include <setjmp.h>
static jmp_buf g_unwind;

__attribute__((noinline, cold)) static void cold_overflow_unwind(void) {
    longjmp(g_unwind, 1);
}

__attribute__((noinline)) static int64_t w_d2u(int64_t n, int64_t budget) {
    if (__builtin_expect(budget <= 0, 0)) cold_overflow_unwind();
    if (n < 2) return n;
    uint64_t a = (uint64_t)w_d2u(n - 1, budget - 1);
    uint64_t b = (uint64_t)w_d2u(n - 2, budget - 1);
    return (int64_t)(a + b * 3u - (uint64_t)n);
}

/* Boundary: snapshot, run, restore on the cold path. Returns 1 on overflow. */
static int run_d2u(int64_t n, int64_t budget, int64_t *out) {
    if (setjmp(g_unwind) != 0) {
        return 1; /* cold: intermediate frames skipped entirely */
    }
    *out = w_d2u(n, budget);
    return 0;
}

static double ms_since(struct timespec t0) {
    struct timespec t1;
    clock_gettime(CLOCK_MONOTONIC, &t1);
    return (t1.tv_sec - t0.tv_sec) * 1e3 + (t1.tv_nsec - t0.tv_nsec) / 1e6;
}

#define REPS 7
#define BEST(expr, label, sink)                                                \
    do {                                                                       \
        double best = 1e9;                                                     \
        for (int i = 0; i < REPS; i++) {                                       \
            struct timespec t0;                                                \
            clock_gettime(CLOCK_MONOTONIC, &t0);                               \
            sink = (expr);                                                     \
            double e = ms_since(t0);                                           \
            if (e < best) best = e;                                            \
        }                                                                      \
        printf("%-34s %8.1f ms   result=%lld\n", label, best, (long long)sink); \
    } while (0)

int main(void) {
    int64_t s = 0;
    r1_t r;
    double best;

    BEST(w_base(34), "baseline  int64 f(n)", s);
    BEST(w_d2_nobudget(34), "design 2  int64 f(n)  [no budget]", s);
    BEST(w_d2(34, BUDGET), "design 2  int64 f(n, budget)", s);

    /* Design 1 returns a struct, so it needs its own timing loop. */
    best = 1e9;
    for (int i = 0; i < REPS; i++) {
        struct timespec t0;
        clock_gettime(CLOCK_MONOTONIC, &t0);
        r = w_d1((void *)0, 34, BUDGET);
        double e = ms_since(t0);
        if (e < best) best = e;
    }
    printf("%-34s %8.1f ms   result=%lld\n",
           "design 1  (status,i64) f(vm,n,budget)", best, (long long)r.val);

    /* Design 2 with the non-local cold exit, on the fast path (no overflow). */
    best = 1e9;
    int64_t uout = 0;
    for (int i = 0; i < REPS; i++) {
        struct timespec t0;
        clock_gettime(CLOCK_MONOTONIC, &t0);
        int of = run_d2u(34, BUDGET, &uout);
        double e = ms_since(t0);
        if (of) { printf("unexpected overflow\n"); return 1; }
        if (e < best) best = e;
    }
    printf("%-34s %8.1f ms   result=%lld\n",
           "design 2 + unwind cold exit", best, (long long)uout);

    /* And that the cold path actually works: budget 8 cannot reach depth 34. */
    int64_t dummy = 12345;
    int overflowed = run_d2u(34, 8, &dummy);
    printf("\ncold-exit check: overflow reported=%s, fast-path value untouched=%s\n",
           overflowed ? "yes" : "NO (BUG)",
           dummy == 12345 ? "yes" : "NO (BUG)");

    g_depth_result = s;
    if (g_overflowed) printf("note: budget exhausted (invalid run)\n");
    return 0;
}
