#![allow(incomplete_features)]
#![feature(optin_builtin_traits)]
#![feature(const_generics)]
#![feature(untagged_unions)]
#![feature(const_fn)]
#![feature(specialization)]
#![feature(negative_impls)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(const_mut_refs)]
#![feature(trivial_bounds)]
#![feature(associated_type_bounds)]
#![feature(const_panic)]

use std::{
    sync::atomic::{AtomicUsize, Ordering},
    thread,
    time::Duration,
};
use sundial_gc::arena::*;

fn log_init() {
    let _ = env_logger::builder().is_test(true).try_init();
}

static COUNT: AtomicUsize = AtomicUsize::new(0);
struct Count(usize);

impl Count {
    pub fn new() -> Self {
        COUNT.fetch_add(1, Ordering::SeqCst);
        Count(1)
    }
}

impl Drop for Count {
    fn drop(&mut self) {
        COUNT.fetch_sub(1, Ordering::SeqCst);
    }
}

#[test]
fn drop_test() {
    log_init();
    for i in 0..1000 {
        let a = Arena::new();
        for _ in 0..1000 {
            a.gc(Count::new());
        }
        log::trace!("i: {}", i);
    }

    while COUNT.load(Ordering::SeqCst) != 0 {
        log::info!("DROP COUNT {}", COUNT.load(Ordering::SeqCst));
        thread::sleep(Duration::from_millis(100));
    }
}
