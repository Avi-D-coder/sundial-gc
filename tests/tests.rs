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
#![feature(const_if_match)]
#![feature(const_panic)]

use sundial_gc::arena::*;
use sundial_gc::auto_traits::*;
use sundial_gc::gc::*;
use sundial_gc::mark::*;

use std::ops::Range;

// #[derive(Trace, Mark)
struct List<'r, T: 'r> {
    t: T,
    next: Option<Gc<'r, List<'r, T>>>,
}

unsafe impl<'r, T: Immutable + Condemned + 'r> Condemned for List<'r, T> {
    default fn feilds(x: &Self, offset: u8, grey_feilds: u8, condemned: Range<usize>) -> u8 {
        assert!(Self::PRE_CONDTION);
        let mut bloom = 0b0000000;
        bloom |= Condemned::feilds(&x.t, offset, grey_feilds, condemned.clone());

        bloom |= Condemned::feilds(&x.next, offset + T::GC_COUNT, grey_feilds, condemned);
        bloom
    }

    default unsafe fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_feilds: u8,
        condemned: Range<usize>,
        handlers: &mut Handlers,
    ) {
        Condemned::evacuate(&s.t, offset, grey_feilds, condemned.clone(), handlers);
        Condemned::evacuate(&s.next, offset, grey_feilds, condemned, handlers);
    }

    default fn direct_gc_types(t: &mut std::collections::HashMap<GcTypeInfo, TypeRow>, offset: u8) {
        T::direct_gc_types(t, offset);
        Option::<Gc<'r, List<'r, T>>>::direct_gc_types(t, offset);
    }

    default fn transitive_gc_types(tti: *mut Tti) {
        T::transitive_gc_types(tti);
        Option::<Gc<'r, List<'r, T>>>::transitive_gc_types(tti);
    }

    default const GC_COUNT: u8 = T::GC_COUNT + Option::<Gc<'r, List<'r, T>>>::GC_COUNT;
    default const PRE_CONDTION: bool =
        if T::PRE_CONDTION && Option::<Gc<'r, List<'r, T>>>::PRE_CONDTION {
            true
        } else {
            panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
        };
}

unsafe impl<'r, T: Immutable + NoGc + 'r> Condemned for List<'r, T> {
    fn feilds(x: &Self, offset: u8, grey_feilds: u8, condemned: Range<usize>) -> u8 {
        assert!(Self::PRE_CONDTION);
        let mut bloom = 0b0000000;
        bloom |= Condemned::feilds(&x.next, offset, grey_feilds, condemned);
        bloom
    }

    fn direct_gc_types(t: &mut std::collections::HashMap<GcTypeInfo, TypeRow>, offset: u8) {
        T::direct_gc_types(t, offset);
        Option::<Gc<'r, List<'r, T>>>::direct_gc_types(t, offset);
    }

    fn transitive_gc_types(tti: *mut Tti) {
        Option::<Gc<'r, List<'r, T>>>::transitive_gc_types(tti);
    }

    const GC_COUNT: u8 = Option::<Gc<'r, List<'r, T>>>::GC_COUNT;
    const PRE_CONDTION: bool = if Option::<Gc<'r, List<'r, T>>>::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
    };
}

#[test]
fn churn_list() {
    env_logger::init();

    let usizes: Arena<usize> = Arena::new();
    let gc_one = usizes.gc_alloc(1);

    let lists: Arena<List<Gc<usize>>> = Arena::new();
    let one_two = lists.gc_alloc(List {
        t: gc_one,
        next: Some(lists.gc_alloc(List {
            t: usizes.gc_alloc(2),
            next: None,
        })),
    });

    let lists2: Arena<List<Gc<usize>>> = Arena::new();
    let one_two = lists2.mark(one_two);
    drop(lists);
    drop(usizes);
    let _ = one_two.t;
}

unsafe impl<'r> Condemned for Foo<'r> {
    fn feilds(s: &Self, offset: u8, grey_feilds: u8, condemned: Range<usize>) -> u8 {
        assert!(Self::PRE_CONDTION);
        let mut bloom = 0b0000000;
        bloom |= Condemned::feilds(&s._bar, offset, grey_feilds, condemned);
        bloom
    }

    fn direct_gc_types(t: &mut std::collections::HashMap<GcTypeInfo, TypeRow>, offset: u8) {
        Gc::<'r, usize>::direct_gc_types(t, offset);
    }

    fn transitive_gc_types(tti: *mut Tti) {
        Gc::<'r, usize>::transitive_gc_types(tti);
    }

    const GC_COUNT: u8 = Gc::<'r, usize>::GC_COUNT;
    const PRE_CONDTION: bool = if Gc::<'r, usize>::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
    };
}

struct Foo<'r> {
    _bar: Gc<'r, usize>,
}

#[test]
fn churn() {
    env_logger::init();
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

    env_logger::init();
    let _mutexes: Arena<Box<std::sync::Arc<usize>>> = Arena::new();
}
