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
use sundial_gc::gc::Gc;
use sundial_gc::{mark::*, TRIGGER_MAJOR_GC};
use sundial_gc_derive::*;

fn log_init() {
    let _ = env_logger::builder().is_test(true).try_init();
}

#[derive(Trace)]
struct List<'r, T>
where
    T: 'r,
{
    t: T,
    _next: Option<Gc<'r, List<'r, T>>>,
}

#[test]
fn churn_list() {
    log_init();
    let usizes: Arena<usize> = Arena::new();
    let gc_one = usizes.gc_alloc(1);

    let lists: Arena<List<Gc<usize>>> = Arena::new();
    let one_two = lists.gc_alloc(List {
        t: gc_one,
        _next: Some(lists.gc_alloc(List {
            t: usizes.gc_alloc(2),
            _next: None,
        })),
    });

    let lists2: Arena<List<Gc<usize>>> = Arena::new();
    let one_two = lists2.mark(one_two);
    drop(lists);
    drop(usizes);
    let _ = one_two.t;
}

#[derive(Trace)]
struct Foo<'r> {
    _bar: Gc<'r, usize>,
}

#[test]
fn churn() {
    log_init();
    let usizes: Arena<usize> = Arena::new();
    let gced_usize = usizes.gc_alloc(1);

    let foos: Arena<Foo> = Arena::new();
    let foo = foos.gc_alloc(Foo { _bar: gced_usize });

    let foos2: Arena<Foo> = Arena::new();
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

// TODO &T: Trace cannot be added soundly until GAT lands.
// In The mean time use Gc::from(&'static T)

// #[test]
// fn prevent_use_after_free() {
//     let strings: Arena<String> = Arena::new();
//     let gced = strings.gc_alloc(String::from("foo"));
//     let strs: Arena<&String> = Arena::new();
//     let str1 = strs.gc_alloc(&*gced);
//     let strs2: Arena<&String> = Arena::new();
//     let _str2: Gc<&String> = strs2.mark(str1);
//     drop(strings); //~ cannot move out of `strings` because it is borrowed
//     let str3 = *_str2;
// }

// #[test]
// fn prevent_use_after_free_correct() {
//     let strings: Arena<String> = Arena::new();
//     let gced = strings.gc_alloc(String::from("foo"));
//     let strs: Arena<&String> = Arena::new();
//     let str1 = strs.gc_alloc(&*gced);
//     drop(strings);
//     let _str3 = &*str1;
// }

// #[test]
// fn hidden_lifetime_test() {
//     struct Bar<'b> {
//         _b: &'b str,
//     }
//     struct Foo2<'a, 'b> {
//         _bar: Option<Gc<'a, Bar<'b>>>,
//     }

//     // This may not be trivail to implement as a proc macro
//     unsafe impl<'a, 'b: 'a> Trace for Foo2<'a, 'b> {
//         fn trace(_: usize) {}
//         const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
//         const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] =
//             GcTypeInfo::one_child::<Gc<'a, Bar<'b>>>();
//         fn trace_transitive_type_info(tti: &mut Tti) {
//             tti.add_direct::<Self>();
//             tti.add_trans(Gc::<'a, Bar<'b>>::trace_transitive_type_info);
//         }
//     }

//     unsafe impl<'o, 'n, 'r: 'n, 'b> Mark<'o, 'n, 'r, Foo2<'o, 'b>, Foo2<'r, 'b>>
//         for Arena<Foo2<'r, 'b>>
//     {
//         fn mark(&'n self, o: Gc<'o, Foo2<'o, 'b>>) -> Gc<'r, Foo2<'r, 'b>> {
//             // TODO fillout
//             unsafe { std::mem::transmute(o) }
//         }
//     }

//     // There should be an error, triggered by drop
//     let string = String::from("bar");
//     let foos = Arena::new();
//     let bars = Arena::new();
//     let _b = &*string;
//     let foo = foos.gc_alloc(Foo2 {
//         _bar: Some(bars.gc_alloc(Bar { _b })),
//     });

//     let foos2 = Arena::new();
//     let foo2 = foos2.mark(foo);
//     drop(foos);
//     drop(bars);
//     // drop(string); //~ cannot move out of `string` because it is borrowed
//     let _: Option<&str> = foo2._bar.as_ref().map(|b| b._b);
// }

#[test]
fn immutable_test() {
    // use std::sync::Mutex;
    //~ trait bound `std::cell::UnsafeCell<usize>: Immutable` is not satisfied
    // let mutexes: Arena<Mutex<usize>> = Arena::new();

    let _mutexes: Arena<Box<std::sync::Arc<usize>>> = Arena::new();
}
