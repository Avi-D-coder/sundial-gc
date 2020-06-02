#![allow(incomplete_features)]
#![feature(optin_builtin_traits)]
#![feature(const_generics)]
#![feature(untagged_unions)]
#![feature(const_fn)]
#![feature(const_transmute)]
#![feature(specialization)]
#![feature(negative_impls)]
#![feature(const_raw_ptr_to_usize_cast)]
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::mem::*;
use std::ops::Deref;
use std::ptr::drop_in_place;
use std::sync::Mutex;
use std::thread_local;

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
    const TRACE_TYPE_INFO: GcTypeInfo;
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo>;
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo>;
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcTypeInfo {
    trace_ptr: usize,
    drop_ptr: usize,
    needs_drop: bool,
    byte_size: u16,
    alignment: u16,
}

impl GcTypeInfo {
    pub const fn new<T: Trace>() -> Self {
        Self {
            trace_ptr: unsafe { T::trace as *const fn(&T) as usize },
            drop_ptr: unsafe { drop_in_place::<T> as *const fn(*mut T) as usize },
            needs_drop: needs_drop::<T>(),
            byte_size: size_of::<T>() as u16,
            alignment: align_of::<T>() as u16,
        }
    }
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

thread_local! {
    /// Map from type to GC communication bus.
    static GC_BUS: HashMap<GcTypeInfo, Box<Mutex<BusMsg>>> = HashMap::new();
}

// TODO replace with atomic 64 byte encoding.
struct BusMsg {
    from_gc: bool,
    high_ptr: usize,
    capacity: u16,
    /// The first 8 bits correspond to GCed fields.
    /// The last 8 bits are reserved.
    ///
    /// Only 8 GCed fields per struct are supported
    /// TODO do enums with GCed fields count as one (conservative), or N fields.
    grey_feilds: u16,
}

pub struct Arena<T> {
    // roots: usize,
    high_ptr: *const T,
    capacity: u16,
}

impl<T: Trace> Arena<T> {
    pub fn new() -> Self {
        // GC_BUS.with(|type_map| T::TRACE_TYPE_INFOtype_map.entry());
        Self {
            high_ptr: todo!(),
            capacity: 1000,
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

struct NGc<T: NoGc + Immutable>(T);

/// Shallow immutability
pub unsafe auto trait Immutable {}
impl<T> !Immutable for &mut T {}
impl<T> !Immutable for UnsafeCell<T> {}
unsafe impl<T> Immutable for Box<T> {}

// Needs negative impls
// unsafe impl<T: NoGc + Immutable> Trace for T {
//     fn trace(_: &T) {}
//     const TRACE_FIELD_COUNT: u8 = 0;
//     const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<T>();
//     fn trace_child_type_info() -> Vec<GcTypeInfo> {
//         Vec::new()
//     }
//     fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
//         HashSet::default()
//     }
// }

unsafe impl<T: NoGc + Immutable> Trace for NGc<T> {
    fn trace(_: &Self) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl<'o, 'n, T: NoGc + Immutable> Mark<'o, 'n, T, T> for Arena<T> {
    fn mark(&'n self, o: Gc<'o, T>) -> Gc<'n, T> {
        unsafe { std::mem::transmute(o) }
    }
}

unsafe impl<'r, T: Trace> Trace for Gc<'r, T> {
    fn trace(t: &Self) {
        Trace::trace(t.deref())
    }
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<T>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = vec![GcTypeInfo::new::<T>()];
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        T::trace_transitive_type_info()
    }
}

unsafe impl<'r, T: Trace> Trace for Option<T> {
    fn trace(t: &Self) {
        Trace::trace(t.deref())
    }
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<T>();
    fn trace_child_type_info() -> Vec<GcTypeInfo> {
        T::trace_child_type_info()
    }
    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        T::trace_transitive_type_info()
    }
}

unsafe impl Trace for usize {
    fn trace(_: &Self) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    fn trace_child_type_info() -> Vec<GcTypeInfo> {
        Vec::new()
    }

    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl Trace for String {
    fn trace(_: &Self) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();

    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl Trace for isize {
    fn trace(_: &Self) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();

    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl<T: Trace> Trace for &T {
    fn trace(_: &Self) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<&T>();
    fn trace_child_type_info() -> Vec<GcTypeInfo> {
        Vec::new()
    }

    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

unsafe impl<T: Trace> Trace for Box<T> {
    fn trace(_: &Self) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<&T>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();

    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        HashSet::default()
    }
}

// #[derive(Trace, Mark)
struct List<'r, T> {
    _t: T,
    _next: Option<Gc<'r, List<'r, T>>>,
}

// These three impls will be derived with a procedural macro

unsafe impl<'r, T: 'r + Trace> Trace for List<'r, T> {
    fn trace(_: &List<'r, T>) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> =
        vec![GcTypeInfo::new::<Option<Gc<'r, List<'r, T>>>>()];

    fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
        let mut tti = HashSet::from_iter(Self::TRACE_CHILD_TYPE_INFO.into_iter());
        tti.extend(T::trace_transitive_type_info().into_iter());
        tti
    }
}

// unsafe impl<'o, 'n, T: NoGc + Immutable> Mark<'o, 'n, List<'o, T>, List<'n, T>>
//     for Arena<List<'n, T>>
// {
//     fn mark(&'n self, o: Gc<'o, List<'o, T>>) -> Gc<'n, List<'n, T>> {
//         unsafe { std::mem::transmute(o) }
//     }
// }

unsafe impl<'o, 'n, O: Trace, N> Mark<'o, 'n, List<'o, O>, List<'n, N>> for Arena<List<'n, N>> {
    fn mark(&'n self, o: Gc<'o, List<'o, O>>) -> Gc<'n, List<'n, N>> {
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
    let _ = one_two._t;
}

struct Foo<'l> {
    _bar: Gc<'l, usize>,
}

unsafe impl<'r> Trace for Foo<'r> {
    fn trace(_: &Foo<'r>) {}
    const TRACE_FIELD_COUNT: u8 = 0;
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();

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
        fn trace(_: &Foo2<'a, 'b>) {}
        const TRACE_FIELD_COUNT: u8 = 0;
        const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
        const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();
        fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
            HashSet::default()
        }
    }

    unsafe impl<'o, 'n, 'b> Mark<'o, 'n, Foo2<'o, 'b>, Foo2<'n, 'b>> for Arena<Foo2<'n, 'b>> {
        fn mark(&'n self, o: Gc<'o, Foo2<'o, 'b>>) -> Gc<'n, Foo2<'n, 'b>> {
            unsafe { std::mem::transmute(o) }
        }
    }

    unsafe impl<'b> Trace for Bar<'b> {
        fn trace(_: &Self) {}
        const TRACE_FIELD_COUNT: u8 = 0;
        const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
        const TRACE_CHILD_TYPE_INFO: Vec<GcTypeInfo> = Vec::new();

        fn trace_transitive_type_info() -> HashSet<GcTypeInfo> {
            HashSet::default()
        }
    }

    let foos = Arena::new();
    let bars = Arena::new();
    let string = String::from("bar");
    let b = &*string;
    let foo = foos.gc_alloc(Foo2 {
        _bar: bars.gc_alloc(Bar { _b: b }),
    });

    let foos2 = Arena::new();
    let foo2 = foos2.mark(foo);
    // drop(string); //~ cannot move out of `string` because it is borrowed
    let _ = *foo2._bar._b;
}

#[test]
fn immutable_test() {
    use std::sync::Mutex;
    //~ trait bound `std::cell::UnsafeCell<usize>: Immutable` is not satisfied
    // let mutexes: Arena<Mutex<usize>> = Arena::new();

    let _mutexes: Arena<NGc<Box<Mutex<usize>>>> = Arena::new();
}
