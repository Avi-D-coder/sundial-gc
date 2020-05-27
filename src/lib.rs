#![allow(incomplete_features)]
#![feature(optin_builtin_traits)]
#![feature(const_generics)]
#![feature(untagged_unions)]
#![feature(const_fn)]
#![feature(const_transmute)]
#![feature(specialization)]
#![feature(negative_impls)]
use std::cell::UnsafeCell;
use std::collections::HashSet;
use std::ops::Deref;

// GAT Mark
// pub unsafe trait Mark<'o, 'n, O> {
//     type Struct<'l>;
//     fn mark(&'n self, o: Gc<'o, Self::Struct<'o>>) -> Gc<'n, Self::Struct<'n>>;
// }

pub unsafe trait Mark<'o, 'n, O, N> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'n, N>;
}

pub unsafe trait Trace {
    fn trace(t: &Self);
    const TRACE_FIELD_COUNT: u8;
    const TRACE_TYPE_INFO: Option<GcTypeInfo>;
    fn trace_child_type_info() -> HashSet<GcTypeInfo>;
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcTypeInfo {
    has_drop: bool,
    drop_ptr: usize,
    size: u16,
    alignment: u16,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T> {
    ptr: &'r T,
}

impl<'r, T: Trace> Deref for Gc<'r, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.ptr
    }
}

pub struct Arena<T> {
    roots: usize,
    high_ptr: *const T,
}

impl<T: Trace> Arena<T> {
    pub fn new() -> Self {
        Self {
            roots: todo!(),
            high_ptr: todo!(),
        }
    }
    pub fn gc_alloc<'r>(&'r self, t: T) -> Gc<'r, T> {
        unimplemented!()
    }

    pub fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

// Auto traits
pub unsafe auto trait NoGc {}
impl<'r, T> !NoGc for Gc<'r, T> {}

pub unsafe auto trait Immutable {}
impl<T> !Immutable for &mut T {}
impl<T> !Immutable for UnsafeCell<T> {}

unsafe impl<T: NoGc> Trace for T {
    fn trace(_: &T) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: Option<GcTypeInfo> = None;
    fn trace_child_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl<'o, 'n, T: NoGc> Mark<'o, 'n, T, T> for Arena<T> {
    fn mark(&'n self, o: Gc<'o, T>) -> Gc<'n, T> {
        unsafe { std::mem::transmute(o) }
    }
}

unsafe impl<'r, T: Trace> Trace for Gc<'r, T> {
    fn trace(t: &Self) {
        Trace::trace(t.deref())
    }
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: Option<GcTypeInfo> = None;
    fn trace_child_type_info() -> HashSet<GcTypeInfo> {
        T::trace_child_type_info()
    }
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        T::trace_transitive_type_info()
    }
}

#[test]
fn list_test() {
    // #[derive(Trace, Mark)
    struct List<'r, T> {
        _t: T,
        _next: Option<Gc<'r, List<'r, T>>>,
    }

    // These three impls will be derived with a procedural macro

    unsafe impl<'r, T> Trace for List<'r, T> {
        fn trace(_: &List<'r, T>) {}
        const TRACE_FIELD_COUNT: u8 = 0;
        const TRACE_TYPE_INFO: Option<GcTypeInfo> = None;
        fn trace_child_type_info() -> HashSet<GcTypeInfo> {
            HashSet::default()
        }
        fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
            HashSet::default()
        }
    }

    unsafe impl<'o, 'n, T: NoGc> Mark<'o, 'n, List<'o, T>, List<'n, T>> for Arena<List<'n, T>> {
        fn mark(&'n self, o: Gc<'o, List<'o, T>>) -> Gc<'n, List<'n, T>> {
            unsafe { std::mem::transmute(o) }
        }
    }

    unsafe impl<'o, 'n, O: Trace, N> Mark<'o, 'n, List<'o, O>, List<'n, N>> for Arena<List<'n, N>> {
        default fn mark(&'n self, o: Gc<'o, List<'o, O>>) -> Gc<'n, List<'n, N>> {
            unsafe { std::mem::transmute(o) }
        }
    }

    #[test]
    fn _churn_list() {
        let usizes: Arena<usize> = Arena::new();
        let gc_one = usizes.gc_alloc(1);

        let lists: Arena<List<Gc<usize>>> = Arena::new();
        let one_two = lists.gc_alloc(List {
            _t: gc_one,
            _next: Some(lists.gc_alloc(List {
                _t: usizes.gc_alloc(2),
                _next: None,
            })),
        });
        let one_two = lists.mark(one_two);
        drop(usizes);
        let _ =one_two._t;
    }
}

struct Foo<'l> {
    _bar: Gc<'l, usize>,
}

unsafe impl<'r> Trace for Foo<'r> {
    fn trace(_: &Foo<'r>) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: Option<GcTypeInfo> = None;
    fn trace_child_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl<'o, 'n> Mark<'o, 'n, Foo<'o>, Foo<'n>> for Arena<Foo<'n>> {
    fn mark(&'n self, o: Gc<'o, Foo<'o>>) -> Gc<'n, Foo<'n>> {
        unsafe { std::mem::transmute(o) }
    }
}

#[test]
fn churn() {
    let usizes: Arena<usize> = Arena::new();
    let gced_usize = usizes.gc_alloc(1);

    let foos: Arena<Foo> = Arena::new();
    let foo = foos.gc_alloc(Foo {
        _bar: gced_usize,
    });

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
