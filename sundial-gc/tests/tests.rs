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
#![feature(bindings_after_at)]
#![feature(generic_associated_types)]

use sundial_gc::arena::*;
use sundial_gc::gc::Gc;
use sundial_gc::{collections::Map, mark::*};
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
    let gc_one = usizes.gc(1);

    let lists: Arena<List<Gc<usize>>> = Arena::new();
    let one_two: Gc<List<Gc<usize>>> = lists.gc(List {
        t: gc_one,
        _next: Some(lists.gc(List {
            t: usizes.gc(2),
            _next: None,
        })),
    });

    let lists2: Arena<List<Gc<usize>>> = Arena::new();
    let one_two: Gc<List<Gc<usize>>> = lists2.mark(one_two);
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
    let gced_usize = usizes.gc(1);

    let foos: Arena<Foo> = Arena::new();
    let foo = foos.gc(Foo { _bar: gced_usize });

    let foos2: Arena<Foo> = Arena::new();
    // mark extends foos lifetime to that of the new arena foos2
    // This does not copy foo into the new arena.
    // In this case it is simply transmuting a lifetime.
    //
    // If the Gc owned foos or usizes and wanted to free it,
    // mark would copy the head of it's structure to foos2.
    // The Gc would then copy the tail of the structure into a older generation.
    let foo2: Gc<Foo> = foos2.mark(foo);
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

// It generates the correct errors.
// TODO test for errors like rustc does.
#[test]
fn use_after_free_test() {
    // let mut foo = None;
    let mut _map: Map<usize, usize> = Map::default();

    // {
    // foo = Some(&Arena::new().gc(1)) //~ Err
    // let a = Arena::new();
    // foo = Some(&a.gc(1)) //~ Err

    let arena = Arena::new();
    _map = _map.insert(1, 1, &arena);
    drop(arena);
    // }

    // eprintln!("{:?}", _map); // ~ ^^^ cannot move out of `arena` because it is borrowed \ move out of `arena` occurs here
}

// #[cfg(test)]
// mod tests {
//     use std::cell::UnsafeCell;
//     use std::{mem, ops::Deref};

//     pub trait Id {
//         type T<'l>;
//     }

//     impl<T> Id for T {
//         type T<'l> = T;
//     }

//     pub unsafe trait CoerceLifetime {
//         type Type<'l>: 'l + Sized;
//         unsafe fn coerce_lifetime<'o, 'n>(old: &'o Self::Type<'o>) -> &'n Self::Type<'n> {
//             todo!()
//         }
//     }
//     unsafe impl<'r, T: CoerceLifetime> CoerceLifetime for Gc<'r, T> {
//         type Type<'l> = Gc<'l, T::Type<'l>>;
//     }
//     unsafe impl CoerceLifetime for usize {
//         default type Type<'l> = usize;
//     }

//     pub struct Arena<T> {
//         vec: UnsafeCell<Vec<T>>,
//     }

//     #[derive(Debug, Ord, PartialOrd, Eq, PartialEq)]
//     pub struct Gc<'r, T>(&'r T, ());
//     impl<'r, T> Copy for Gc<'r, T> {}
//     impl<'r, T> Clone for Gc<'r, T> {
//         fn clone(&self) -> Self {
//             *self
//         }
//     }
//     impl<'r, T> Deref for Gc<'r, T> {
//         type Target = T;
//         fn deref(&self) -> &T {
//             self.0
//         }
//     }

//     impl<A: CoerceLifetime> Arena<A> {
//         fn new() -> Self {
//             Self {
//                 vec: UnsafeCell::new(Vec::new()),
//             }
//         }

//         pub fn gc<'r, 'a: 'r, 'o, T: Id<T = A::Type<'r>>>(
//             &'a self,
//             t: T,
//         ) -> Gc<'r, T> {
//             let vec = unsafe { &mut *self.vec.get() };
//             todo!()
//         }
//     }
//     impl<T> Drop for Arena<T> {
//         fn drop(&mut self) {}
//     }

//     #[test]
//     fn use_after_free_test() {
//         struct Foo<'r>(Gc<'r, usize>);
//         unsafe impl<'r> CoerceLifetime for Foo<'r> {
//             type Type<'l> = Foo<'l>;
//         }

//         let usizes: Arena<usize> = Arena::new();
//         let foos: Arena<Foo> = Arena::new();

//         let n = usizes.gc(1usize);
//         let foo = foos.gc(Foo(n));
//     }

//     fn foo<'r>(n: usize, usizes: &'r Arena<usize>) -> Foo<'r> {
//         let n = usizes.gc(n);
//         Foo(n)
//     }

//     #[derive(Copy, Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
//     enum List<'r, T: Copy> {
//         Cons(T, Gc<'r, List<'r, T>>),
//         Empty,
//     }

//     impl<'r, T: Copy> List<'r, T> {
//         fn cons(
//             head: T,
//             tail: Gc<'r, List<'r, T>>,
//             arena: &'r Arena<List<'r, T>>,
//         ) -> Gc<'r, List<'r, T>> {
//             arena.gc(List::Cons(head, tail))
//         }
//     }

//     let lists: Arena<List<u8>> = Arena::new();
//     // let lists: &Arena<List<u8>> = &lists;
//     // List::cons(1, lists.gc(List::Empty), &lists);
//     lists.gc(List::Cons(1, lists.gc(List::Empty)));

//     // let nodes: Arena<Node<u8, u8>> = Arena::new();
//     // let nodes: &Arena<Node<u8, u8>> = nodes;

//     // Map::default().insert(1, 1, &nodes);
// }
// }
