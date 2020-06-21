use super::gc::*;
use crate::{auto_traits::HasGc, trace::GcTypeInfo};
use smallvec::SmallVec;
use std::ops::Range;
use std::{mem, ptr};

/// This will be sound once GAT or const Eq &str lands.
/// The former will allow transmuting only lifetimes.
/// The latter will make `assert_eq!(type_name::<O>(), type_name::<N>())` const.
// https://github.com/rust-lang/rfcs/pull/2632.
pub unsafe trait Mark<'o, 'n, 'r: 'n, O: 'o, N: 'r> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'r, N>;
}

// Blanket Arena<T> impl is in src/arena.rs

/// Currently just holds Arenas
pub struct Handlers {
    // TODO benchmark sizes
    translation: SmallVec<[u8; 16]>,
    nexts: SmallVec<[*mut u8; 4]>,
    /// forward(old, new) -> already evacuated
    forward: fn(*const u8, *const u8) -> Option<*const u8>,
}

pub unsafe trait Condemned {
    fn feilds(s: &Self, grey_feilds: u8, region: Range<usize>) -> u8;
    fn evacuate<'e, const OFFSET: u8>(
        s: &Self,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
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

    default fn evacuate<'e, const OFFSET: u8>(_: &Self, _: u8, _: Range<usize>, _: &mut Handlers) {}

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
    fn evacuate<'e, const OFFSET: u8>(
        s: &Self,
        _: u8,
        region: std::ops::Range<usize>,
        handlers: &mut Handlers,
    ) {
        let ptr = s.ptr as *const T;
        let addr = ptr as usize;
        if region.contains(&addr) {
            let i = handlers.translation[OFFSET as usize];
            if let Some(next) = handlers.nexts.get_mut(i as usize) {
                unsafe { ptr::copy_nonoverlapping(ptr, *next as *mut T, 1) }
                let forward = handlers.forward;
                if let Some(pre_evac) = forward(ptr as *const u8, *next as *const u8) {
                    let r = s as *const Gc<T> as *mut *const T;
                    unsafe {
                        *r = pre_evac as *const T;
                    }
                } else {
                    let r = s as *const Gc<T> as *mut *const T;
                    unsafe {
                        *r = *next as *const T;
                    }
                    *next = (addr + mem::size_of::<T>()) as *mut u8;
                }
            }
        }
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

    fn evacuate<'e, const OFFSET: u8>(
        s: &Self,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
    ) {
        match s {
            Some(t) => Condemned::evacuate::<OFFSET>(t, grey_feilds, region, handlers),
            None => (),
        }
    }

    default const PRE_CONDTION: bool = if T::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for T. Required due to a direct Gc<A> in Option<T>");
    };
}
