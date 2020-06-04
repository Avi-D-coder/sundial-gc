#![allow(incomplete_features)]
#![feature(optin_builtin_traits)]
#![feature(const_generics)]
#![feature(untagged_unions)]
#![feature(const_fn)]
#![feature(const_transmute)]
#![feature(specialization)]
#![feature(negative_impls)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(const_mut_refs)]
#![feature(trivial_bounds)]

use sundial_gc::arena::*;
use sundial_gc::auto_traits::*;
use sundial_gc::gc::*;
use sundial_gc::mark::*;
use sundial_gc::trace::*;

// #[derive(Trace, Mark)
struct List<'r, T> {
    _t: T,
    _next: Option<Gc<'r, List<'r, T>>>,
}

// These three impls will be derived with a procedural macro
unsafe impl<'r, T: 'r + Trace + Immutable> Trace for List<'r, T> {
    fn trace(_: usize) {}
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [
        Some(GcTypeInfo::new::<T>()),
        Some(GcTypeInfo::new::<Option<Gc<'r, List<'r, T>>>>()),
        None,
        None,
        None,
        None,
        None,
        None,
    ];

    fn trace_transitive_type_info(tti: &mut Tti) {
        tti.add_direct::<Self>();
        tti.add_trans(T::trace_transitive_type_info);
        tti.add_trans(Option::<Gc<'r, List<'r, T>>>::trace_transitive_type_info);
    }
}

unsafe impl<'o, 'n, O: Trace, N> Mark<'o, 'n, List<'o, O>, List<'n, N>> for ArenaGc<List<'n, N>> {
    default fn mark(&'n self, o: Gc<'o, List<'o, O>>) -> Gc<'n, List<'n, N>> {
        unsafe { std::mem::transmute(o) }
    }
}

#[test]
fn _churn_list() {
    let usizes: Arena<usize> = Arena::new();
    let gc_one = usizes.gc_alloc(1);

    let lists: ArenaGc<List<Gc<usize>>> = ArenaGc::new();
    let one_two = lists.gc_alloc(List {
        _t: gc_one,
        _next: Some(lists.gc_alloc(List {
            _t: usizes.gc_alloc(2),
            _next: None,
        })),
    });
    let one_two = lists.mark(one_two);
    drop(usizes);
    let _ = one_two._t;
}

struct Foo<'r> {
    _bar: Gc<'r, usize>,
}

unsafe impl<'r> Trace for Foo<'r> {
    fn trace(_: usize) {}
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = GcTypeInfo::one_child::<Gc<'r, usize>>();
    fn trace_transitive_type_info(tti: &mut Tti) {
        tti.add_direct::<Self>();
        tti.add_trans(Gc::<'r, usize>::trace_transitive_type_info);
    }
}

unsafe impl<'o, 'n> Mark<'o, 'n, Foo<'o>, Foo<'n>> for ArenaGc<Foo<'n>> {
    fn mark(&'n self, o: Gc<'o, Foo<'o>>) -> Gc<'n, Foo<'n>> {
        unsafe { std::mem::transmute(o) }
    }
}

#[test]
fn churn() {
    let usizes: Arena<usize> = Arena::new();
    let gced_usize = usizes.gc_alloc(1);

    let foos: ArenaGc<Foo> = ArenaGc::new();
    let foo = foos.gc_alloc(Foo { _bar: gced_usize });

    let foos2: ArenaGc<Foo> = ArenaGc::new();
    // mark extends foos lifetime to that of the new arena foos2
    // This does not copy foo into the new arena.
    // In this case it is simply transmuting a lifetime.
    //
    // If the Gc owned foos or usizes and wanted to free it,
    // mark would copy the head of it's structure to foos2.
    // The Gc would then copy the tail of the structure into a older generation.
    let foo2 = foos2.mark(foo);
    drop(foos);
    drop(usizes);
    let _ = *foo2._bar + 1usize;
}

#[test]
fn prevent_use_after_free() {
    let strings: Arena<String> = Arena::new();
    let gced = strings.gc_alloc(String::from("foo"));
    let strs: Arena<&String> = Arena::new();
    let str1 = strs.gc_alloc(&*gced);
    let strs2: Arena<&String> = Arena::new();
    let _str2 = strs2.mark(str1);
    drop(strings); //~ cannot move out of `strings` because it is borrowed
                   // let str3 = *str2;
}

#[test]
fn prevent_use_after_free_correct() {
    let strings: Arena<String> = Arena::new();
    let gced = strings.gc_alloc(String::from("foo"));
    let strs: Arena<String> = Arena::new();
    let str1 = strs.mark(gced);
    let strs2 = Arena::new();
    let str2 = strs2.mark(str1);
    drop(strings);
    let _str3 = &*str2;
}

// Why did I think this is unsound without GAT Mark?
#[test]
fn hidden_lifetime_test() {
    struct Bar<'b> {
        _b: &'b str,
    }
    struct Foo2<'a, 'b> {
        _bar: Gc<'a, Bar<'b>>,
    }

    // This may not be trivail to implement as a proc macro
    unsafe impl<'a, 'b: 'a> Trace for Foo2<'a, 'b> {
        fn trace(_: usize) {}
        const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
        const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] =
            GcTypeInfo::one_child::<Gc<'a, Bar<'b>>>();
        fn trace_transitive_type_info(tti: &mut Tti) {
            tti.add_direct::<Self>();
            tti.add_trans(Gc::<'a, Bar<'b>>::trace_transitive_type_info);
        }
    }

    unsafe impl<'o, 'n, 'b> Mark<'o, 'n, Foo2<'o, 'b>, Foo2<'n, 'b>> for ArenaGc<Foo2<'n, 'b>> {
        fn mark(&'n self, o: Gc<'o, Foo2<'o, 'b>>) -> Gc<'n, Foo2<'n, 'b>> {
            unsafe { std::mem::transmute(o) }
        }
    }

    let foos = ArenaGc::new();
    let bars = Arena::new();
    let string = String::from("bar");
    let b = &*string;
    let foo = foos.gc_alloc(Foo2 {
        _bar: bars.gc_alloc(Bar { _b: b }),
    });

    let foos2 = ArenaGc::new();
    let foo2 = foos2.mark(foo);
    // drop(string); //~ cannot move out of `string` because it is borrowed
    let _ = *foo2._bar._b;
}

#[test]
fn immutable_test() {
    // use std::sync::Mutex;
    //~ trait bound `std::cell::UnsafeCell<usize>: Immutable` is not satisfied
    // let mutexes: Arena<Mutex<usize>> = Arena::new();

    let _mutexes: Arena<Box<std::sync::Arc<usize>>> = Arena::new();
}
