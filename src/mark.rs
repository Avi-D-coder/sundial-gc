use super::gc::*;
use crate::{auto_traits::HasGc, trace::GcTypeInfo};
use smallvec::SmallVec;
use std::ops::Range;

/// This will be sound once GAT or const Eq &str lands.
/// The former will allow transmuting only lifetimes.
/// The latter will make `assert_eq!(type_name::<O>(), type_name::<N>())` const.
// https://github.com/rust-lang/rfcs/pull/2632.
pub unsafe trait Mark<'o, 'n, 'r: 'n, O: 'o, N: 'r> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'r, N>;
}

// Blanket Arena<T> impl is in src/arena.rs

pub struct Handlers<'e, E: FnMut(*const u8)> {
    // TODO benchmark sizes
    translation: &'e SmallVec<[u8; 16]>,
    effects: &'e mut SmallVec<[E; 4]>,
}

pub unsafe trait Condemned {
    fn feilds(s: &Self, grey_feilds: u8, region: Range<usize>) -> u8;
    fn evacuate<'e, E: FnMut(*const u8), const OFFSET: u8>(
        s: &Self,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: Handlers<'e, E>,
    );

    const PRE_CONDTION: bool;
}

unsafe impl<T> Condemned for T {
    default fn feilds(_: &Self, _: u8, _: Range<usize>) -> u8 {
        // This check is superfluous
        // TODO ensure if gets optimized out
        if !T::HAS_GC {
            0b0000_0000
        } else {
            panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>")
        }
    }

    default fn evacuate<'e, E: FnMut(*const u8), const OFFSET: u8>(
        _: &Self,
        _: u8,
        _: Range<usize>,
        _: Handlers<'e, E>,
    ) {
    }

    default const PRE_CONDTION: bool = if T::HAS_GC {
        // TODO When fmt is allowed in const s/your type/type_name::<T>()
        panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
    } else {
        true
    };
}

unsafe impl<'r, T> Condemned for Gc<'r, T> {
    fn feilds(_: &Self, _: u8, _: std::ops::Range<usize>) -> u8 {
        0b0000_0000
    }
    const PRE_CONDTION: bool = true;
    fn evacuate<'e, E: FnMut(*const u8), const OFFSET: u8>(
        s: &Self,
        grey_feilds: u8,
        region: std::ops::Range<usize>,
        handlers: Handlers<'e, E>,
    ) {
        todo!()
    }
}

// std impls

unsafe impl<T> Condemned for Option<T> {
    default fn feilds(s: &Self, grey_feilds: u8, region: Range<usize>) -> u8 {
        match s {
            Some(t) => Condemned::feilds(t, grey_feilds, region),
            None => 0b0000_0000,
        }
    }

    fn evacuate<'e, E: FnMut(*const u8), const OFFSET: u8>(
        s: &Self,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: Handlers<'e, E>,
    ) {
        match s {
            Some(t) => Condemned::evacuate::<E, OFFSET>(t, grey_feilds, region, handlers),
            None => (),
        }
    }

    default const PRE_CONDTION: bool = if T::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for T. Required due to a direct Gc<A> in Option<T>");
    };
}
