use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sundial_gc::*;

fn new_arena() -> u16 {
    let a = Arena::<usize>::new();
    std::sync::atomic::fence(std::sync::atomic::Ordering::SeqCst);
    a.capacity()
}

fn gc_alloc_usize() {
    let a = Arena::<usize>::new();
    a.gc_alloc(1);
}

fn gc_copy_usize() {
    let a = Arena::<usize>::new();
    a.gc_copy(&1);
}

fn gc_clone_usize() {
    let a = Arena::<usize>::new();
    a.gc_clone(&1);
}

fn usize_benchmark(c: &mut Criterion) {
    let _ = env_logger::builder().is_test(true).try_init();
    // FIXME new_arena is getting optimized out.
    c.bench_function("new_arena", |b| b.iter(|| black_box(new_arena())));
    c.bench_function("gc_alloc_usize", |b| b.iter(|| gc_alloc_usize()));
    c.bench_function("gc_copy_usize", |b| b.iter(|| gc_copy_usize()));
    c.bench_function("gc_clone_usize", |b| b.iter(|| gc_clone_usize()));
}

criterion_group!(benches, usize_benchmark);
criterion_main!(benches);
