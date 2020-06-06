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
#![feature(associated_type_bounds)]

use sundial_gc::arena::*;
use sundial_gc::auto_traits::*;
use sundial_gc::gc::*;
use sundial_gc::mark::*;
use sundial_gc::trace::*;

use std::ops::Range;

// #[derive(Trace, Mark)
struct List<'r, T: 'r> {
    t: T,
    next: Option<Gc<'r, List<'r, T>>>,
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

unsafe impl<'o, 'n, O: Trace + 'o, N: 'n> Mark<'o, 'n, List<'o, O>, List<'n, N>>
    for ArenaGc<List<'n, N>>
{
    default fn mark(&'n self, o: Gc<'o, List<'o, O>>) -> Gc<'n, List<'n, N>> {
        unsafe { std::mem::transmute(o) }
    }
}

struct ArenaList<'r, T: 'r>(ArenaGc<List<'r, T>>);

unsafe impl<'o, 'n, O: 'o, N: 'n> Mark2<'o, 'n, O> for ArenaList<'n, N> {
    type Old = List<'o, O>;
    type New = List<'n, N>;
    fn mark(&'n self, o: Gc<'o, Self::Old>) -> Gc<'n, Self::New> {
        unsafe { std::mem::transmute(o) }
    }
}

unsafe impl<'r, T: Condemned> Condemned for List<'r, T> {
    default fn feilds(x: &List<'r, T>, grey_feilds: u8, condemned: Range<usize>) -> u8 {
        let mut bloom = 0b0000000;
        if 0b1000_0000 == grey_feilds & 0b1000_0000 {
            bloom |= Condemned::feilds(&x.t, grey_feilds, condemned.clone())
        };

        // When a fields Gc is at the top level (not hidden in a generic),
        // the proc macro will generate specific code.
        // Here it's hidden in Option
        if 0b0100_0000 == grey_feilds & 0b0100_0000 {
            bloom |= Condemned::feilds(&x.next, grey_feilds, condemned)
        };
        bloom
    }
}

unsafe impl<'r, T: NoGc> Condemned for List<'r, T> {
    fn feilds(_: &List<'r, T>, _: u8, _: Range<usize>) -> u8 {
        0b0000000
    }
}

#[test]
fn churn_list() {
    let usizes: Arena<usize> = Arena::new();
    let gc_one = usizes.gc_alloc(1);

    let lists: ArenaGc<List<Gc<usize>>> = ArenaGc::new();
    let one_two = lists.gc_alloc(List {
        t: unsafe { std::mem::transmute(gc_one) },
        next: {
            // TODO use a macro or a proc maco to generate this until GAT
            // This preserves type while transmute liftimes
            // TODO remove transmute from mark
            let f: Option<Gc<List<Gc<usize>>>> = Some(lists.gc_alloc(List {
                t: usizes.gc_alloc(2),
                next: None,
            }));
            unsafe { std::mem::transmute(f) }
        },
    });

    let lists2: ArenaGc<List<Gc<usize>>> = ArenaGc::new();
    let one_two = lists2.mark(one_two);
    drop(lists);
    drop(usizes);
    let _ = one_two.t;
}

#[test]
fn churn_list2() {
    let usizes: Arena<usize> = Arena::new();
    let gc_one = usizes.gc_alloc(1);

    let lists: ArenaList<Gc<usize>> = ArenaList(ArenaGc::new());
    let one_two = lists.0.gc_alloc(List {
        t: gc_one,
        next: Some(lists.0.gc_alloc(List {
            t: usizes.gc_alloc(2),
            next: None,
        })),
    });
    let lists2: ArenaGc<List<Gc<usize>>> = ArenaGc::new();
    let one_two = lists2.mark(one_two);
    // drop(lists);
    drop(usizes);
    let _ = one_two.t;
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
        _bar: Option<Gc<'a, Bar<'b>>>,
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
        _bar: Some(bars.gc_alloc(Bar { _b: b })),
    });

    let foos2 = ArenaGc::new();
    let foo2 = foos2.mark(foo);
    drop(foos);
    drop(bars);
    // drop(string); //~ cannot move out of `string` because it is borrowed
    let _: Option<&str> = foo2._bar.as_ref().map(|b| b._b);
}

#[test]
fn immutable_test() {
    // use std::sync::Mutex;
    //~ trait bound `std::cell::UnsafeCell<usize>: Immutable` is not satisfied
    // let mutexes: Arena<Mutex<usize>> = Arena::new();

    let _mutexes: Arena<Box<std::sync::Arc<usize>>> = Arena::new();
}
