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
    default fn trace(_: usize) {}
    default const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [
        Some(GcTypeInfo::new::<T>()),
        Some(GcTypeInfo::new::<Option<Gc<'r, List<'r, T>>>>()),
        None,
        None,
        None,
        None,
        None,
        None,
    ];

    default fn trace_transitive_type_info(tti: &mut Tti) {
        tti.add_direct::<Self>();
        tti.add_trans(T::trace_transitive_type_info);
        tti.add_trans(Option::<Gc<'r, List<'r, T>>>::trace_transitive_type_info);
    }
}

unsafe impl<'r, T: 'r + Trace + Immutable + NoGc> Trace for List<'r, T> {
    fn trace(_: usize) {}
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [
        Some(GcTypeInfo::new::<Option<Gc<'r, List<'r, T>>>>()),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
    ];

    fn trace_transitive_type_info(tti: &mut Tti) {
        tti.add_direct::<Self>();
        tti.add_trans(Option::<Gc<'r, List<'r, T>>>::trace_transitive_type_info);
    }
}

unsafe impl<'o, 'n, 'r: 'n, O: Trace, N: 'r + Trace> Mark<'o, 'n, 'r, List<'o, O>, List<'r, N>>
    for ArenaGc<List<'r, N>>
where
    List<'r, N>: Trace,
{
    fn mark(&'n self, o: Gc<'o, List<'o, O>>) -> Gc<'r, List<'r, N>> {
        let condemned_self = self.intern.grey_self
            && self
                .intern
                .white_region
                .contains(&(&*o as *const _ as usize));
        let grey_feilds = unsafe { &mut *self.intern.grey_feilds.get() };
        let cf = Condemned::feilds(&*o, *grey_feilds, self.intern.white_region.clone());
        *grey_feilds |= cf;
        if condemned_self || (0b0000_0000 != cf) {
            let next = self.intern.next.get();
            unsafe { std::ptr::copy(std::mem::transmute::<_, *const List<N>>(o), *next, 1) };
            let mut new_gc: *const List<'r, N> = next as _;
            let old_addr = &*o as *const _ as usize;
            let offset = old_addr % 16384;
            let old_header = unsafe { &*((old_addr - offset) as *const Header<List<'n, N>>) };
            let evacuated = old_header.evacuated.lock();
            evacuated
                .unwrap()
                .entry((offset / std::mem::size_of::<List<N>>()) as u16)
                .and_modify(|gc| new_gc = unsafe { std::mem::transmute(*gc) })
                .or_insert_with(|| {
                    unsafe {
                        *next = ((*next as usize) - std::mem::size_of::<List<N>>()) as *mut _
                    };
                    new_gc
                });
            Gc {
                ptr: unsafe { &*new_gc },
            }
        } else {
            unsafe { std::mem::transmute(o) }
        }
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
    let usizes: ArenaPrim<usize> = ArenaPrim::new();
    let gc_one = usizes.gc_alloc(1);

    let lists: ArenaGc<List<Gc<usize>>> = ArenaGc::new();
    let one_two = lists.gc_alloc(List {
        t: gc_one,
        next: Some(lists.gc_alloc(List {
            t: usizes.gc_alloc(2),
            next: None,
        })),
    });

    let lists2: ArenaGc<List<Gc<usize>>> = ArenaGc::new();
    let one_two = lists2.mark(one_two);
    drop(lists);
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

unsafe impl<'o, 'n, 'r: 'n> Mark<'o, 'n, 'r, Foo<'o>, Foo<'r>> for ArenaGc<Foo<'r>> {
    fn mark(&'n self, o: Gc<'o, Foo<'o>>) -> Gc<'r, Foo<'r>> {
        // TODO fill out
        unsafe { std::mem::transmute(o) }
    }
}

#[test]
fn churn() {
    let usizes: ArenaPrim<usize> = ArenaPrim::new();
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
    let strings: ArenaPrim<String> = ArenaPrim::new();
    let gced = strings.gc_alloc(String::from("foo"));
    let strs: ArenaPrim<&String> = ArenaPrim::new();
    let str1 = strs.gc_alloc(&*gced);
    let strs2: ArenaPrim<&String> = ArenaPrim::new();
    let _str2: Gc<&String> = strs2.mark(str1);
    // drop(strings); //~ cannot move out of `strings` because it is borrowed
    // let str3 = *str2;
}

#[test]
fn prevent_use_after_free_correct() {
    let strings: ArenaPrim<String> = ArenaPrim::new();
    let gced = strings.gc_alloc(String::from("foo"));
    let strs: ArenaPrim<String> = ArenaPrim::new();
    let str1 = strs.mark(gced);
    let strs2: ArenaPrim<&String> = ArenaPrim::new();
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

    unsafe impl<'o, 'n, 'r: 'n, 'b> Mark<'o, 'n, 'r, Foo2<'o, 'b>, Foo2<'r, 'b>>
        for ArenaGc<Foo2<'r, 'b>>
    {
        fn mark(&'n self, o: Gc<'o, Foo2<'o, 'b>>) -> Gc<'r, Foo2<'r, 'b>> {
            // TODO fillout
            unsafe { std::mem::transmute(o) }
        }
    }

    // TODO This errors now in an unexpectec place.
    // There should be an error, triggered by drop
    let string = String::from("bar");
    let foos = ArenaGc::new();
    let bars = ArenaPrim::new();
    let _b = &*string;
    let foo = foos.gc_alloc(Foo2 {
        _bar: Some(bars.gc_alloc(Bar { _b })),
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
    // let mutexes: ArenaPrim<Mutex<usize>> = ArenaPrim::new();

    let _mutexes: ArenaPrim<Box<std::sync::Arc<usize>>> = ArenaPrim::new();
}
