use console::{style, Term};
use once_cell::sync::OnceCell;

use std::sync::{Arc, Mutex};
use std::time::Instant;

/// Time since startup
pub static TIMER: OnceCell<Arc<Instant>> = OnceCell::new();

pub static LOG_LEVEL: OnceCell<usize> = OnceCell::new();

pub fn set_log_level(verbose: bool, quiet: bool) {
    LOG_LEVEL
        .set(match (quiet, verbose) {
            (true, _) => 0,
            (_, false) => 1,
            (_, true) => 2,
        })
        .unwrap();
}

pub fn level_quiet() -> bool {
    *LOG_LEVEL.get().unwrap() == 0
}

pub fn level_normal() -> bool {
    *LOG_LEVEL.get().unwrap() >= 1
}

pub fn level_info() -> bool {
    *LOG_LEVEL.get().unwrap() >= 2
}

pub fn global_timer_start() {
    TIMER.set(Arc::new(Instant::now())).unwrap();
}

pub fn get_global_timer() -> &'static Arc<Instant> {
    TIMER.get().unwrap()
}

/// Print a log message to stdout in a single threaded context.
pub fn info(msg: &str) {
    if level_info() {
        let term = Term::stdout();
        let elapsed = get_global_timer().elapsed();
        let elapsed_str = format!("[{:.3?}s]", elapsed.as_secs_f64());
        term.write_line(format!("{} {}", style(elapsed_str).color256(7), msg).as_str())
            .unwrap();
    }
}

/// Print a log message to stdout in a multithreaded context.
/// If the expected value and the value of the mutex is the same (or lower) print `msg` to stdout.
pub fn info_mt(msg: &str, mutex: &Arc<Mutex<usize>>, expected: usize) {
    let mutex_lock = &mut *mutex.lock().unwrap();
    if *mutex_lock == expected {
        info(msg);
        *mutex_lock += 1;
    }
}
