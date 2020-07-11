use sundial_gc::*;

#[test]
fn gc_allocs_test() {
    let _ = env_logger::builder().is_test(true).try_init();
    for _ in 0..arena::ARENA_SIZE {
        let a: Arena<usize> = Arena::new();
        let n1 = unsafe { *a.next.get() } as usize;
        a.gc_alloc(1);
        let n2 = unsafe { *a.next.get() } as usize;
        if !a.full() {
            assert!(n1 == n2 + 8)
        }
    }
}

#[test]
fn gc_clones_test() {
    for _ in 0..arena::ARENA_SIZE {
        let a: Arena<usize> = Arena::new();
        let n1 = unsafe { *a.next.get() } as usize;
        a.gc_clone(&1);
        let n2 = unsafe { *a.next.get() } as usize;
        assert!(n1 == n2 + 8 || a.full())
    }
}

#[test]
fn gc_copys_test() {
    for _ in 0..arena::ARENA_SIZE {
        let a: Arena<usize> = Arena::new();
        let n1 = unsafe { *a.next.get() } as usize;
        a.gc_copy(&1);
        let n2 = unsafe { *a.next.get() } as usize;
        assert!(n1 == n2 + 8 || a.full())
    }
}

#[test]
fn capacity_test() {
    let a: Arena<u64> = Arena::new();
    let c1 = a.capacity();
    // size_of Header == 160
    assert_eq!((arena::ARENA_SIZE - 160) / 8, c1 as usize);
    a.gc_alloc(1);
    let c2 = a.capacity();
    assert!(c1 - 1 == c2)
}

#[test]
fn capacitys_test() {
    let _ = env_logger::builder().is_test(true).try_init();
    let (mut next, mut cap) = {
        let a = Arena::<usize>::new();
        (unsafe { *a.next.get() }, a.capacity())
    };
    for _ in 0.. {
        let a: Arena<usize> = Arena::new();
        let nxt = unsafe { *a.next.get() };
        assert_eq!(next, nxt);
        assert!(!a.full());
        a.gc_alloc(1);
        let c = a.capacity();
        assert!(cap - 1 == c);

        cap -= 1;
        next = unsafe { *a.next.get() };
        if a.full() {
            break;
        }
    }
}
