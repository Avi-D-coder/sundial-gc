use criterion::{black_box, criterion_group, criterion_main, Criterion};
use sundial_gc::{collections::HashMap, *};

fn new_arenas() -> (u16, u16) {
    let arena_hm = Arena::<HashMap<usize, usize>>::new();
    let arena_kv = Arena::<(usize, usize)>::new();
    (arena_hm.capacity(), arena_kv.capacity())
}

fn gc_alloc_empty_hamt() {
    let arena_hm = Arena::<HashMap<usize, usize>>::new();
    arena_hm.gc_alloc(HashMap::default());
}

fn gc_copy_empty_hamt() {
    let arena_hm = Arena::<HashMap<usize, usize>>::new();
    arena_hm.gc_copy(&HashMap::default());
}

fn gc_clone_empty_hamt() {
    let arena_hm = Arena::<HashMap<usize, usize>>::new();
    arena_hm.gc_clone(&HashMap::default());
}

fn hamt_benchmark(c: &mut Criterion) {
    let _ = env_logger::builder().is_test(true).try_init();
    // FIXME new_arenas is getting optimized out.
    c.bench_function("new_arenas", |b| b.iter(|| {black_box(new_arenas())}));
    c.bench_function("gc_alloc_empty_hamt", |b| b.iter(|| gc_alloc_empty_hamt()));
    c.bench_function("gc_copy_empty_hamt", |b| b.iter(|| gc_copy_empty_hamt()));
    c.bench_function("gc_clone_empty_hamt", |b| b.iter(|| gc_clone_empty_hamt()));
}

criterion_group!(benches, hamt_benchmark);
criterion_main!(benches);
