/* Native floor for bench_arith — the number Oxigen is being measured against.
 *
 * Not part of any automated report; build and run it by hand when you want to
 * know how much headroom is left:
 *
 *     cc -O2 -o /tmp/arith_ref example/controls/arith_reference.c
 *     /tmp/arith_ref
 *
 * Oxigen's integer arithmetic wraps, so this uses int64_t with -fwrapv-style
 * intent expressed via unsigned intermediates: signed overflow is UB in C, and
 * at -O2 the optimizer is entitled to assume it never happens. Doing the
 * arithmetic in uint64_t and casting back is the portable way to get the
 * two's-complement wrap Oxigen specifies.
 *
 * Must print -646393729142, matching bench_arith and arith_typed_param.oxi. A
 * different value means this is no longer the same computation and the
 * comparison is meaningless.
 */
#include <stdio.h>
#include <stdint.h>

static int64_t work(int64_t n) {
    if (n < 2) {
        return n;
    }
    uint64_t a = (uint64_t)work(n - 1);
    uint64_t b = (uint64_t)work(n - 2);
    return (int64_t)(a + b * 3u - (uint64_t)n);
}

int main(void) {
    printf("%lld\n", (long long)work(34));
    return 0;
}
