//! The time, for `time`, `datetime`, `log` and seeding `random`.
//!
//! Native builds ask the operating system. `wasm32-unknown-unknown` has no
//! clock to ask (`SystemTime::now` panics there), so a host built for it
//! installs one with `set_host_clock`; a web page passes `Date.now`. Until it
//! does, the time there is the Unix epoch.

use std::time::Duration;

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
static HOST_CLOCK: std::sync::OnceLock<fn() -> f64> = std::sync::OnceLock::new();

/// Use `now_ms`, milliseconds since the Unix epoch, as the clock. Only the
/// first call has an effect.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub fn set_host_clock(now_ms: fn() -> f64) {
    let _ = HOST_CLOCK.set(now_ms);
}

/// Time since the Unix epoch, or `None` when the clock is set before it.
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub fn since_epoch() -> Option<Duration> {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .ok()
}

/// Time since the Unix epoch, or `None` when the clock is set before it.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub fn since_epoch() -> Option<Duration> {
    let ms = HOST_CLOCK.get().map_or(0.0, |now| now());
    (ms >= 0.0).then(|| Duration::from_secs_f64(ms / 1000.0))
}

/// Nanoseconds since this thread first asked, never going backwards.
#[cfg(not(all(target_arch = "wasm32", target_os = "unknown")))]
pub fn monotonic_nanos() -> i64 {
    use std::time::Instant;
    thread_local! {
        static EPOCH: Instant = Instant::now();
    }
    EPOCH.with(|epoch| epoch.elapsed().as_nanos() as i64)
}

/// Nanoseconds since this thread first asked, never going backwards.
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
pub fn monotonic_nanos() -> i64 {
    use std::cell::Cell;
    thread_local! {
        static EPOCH: Duration = since_epoch().unwrap_or_default();
        // The host clock is wall time, which can be set back.
        static LAST: Cell<i64> = const { Cell::new(0) };
    }
    let now = since_epoch().unwrap_or_default();
    let elapsed = EPOCH.with(|epoch| now.saturating_sub(*epoch).as_nanos() as i64);
    LAST.with(|last| {
        let value = elapsed.max(last.get());
        last.set(value);
        value
    })
}
