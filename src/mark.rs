use super::gc::*;
use crate::{
    arena::alloc_arena,
    auto_traits::HasGc,
    gc_logic::{HeaderUnTyped, ARENA_SIZE},
    trace::GcTypeInfo,
};
use smallvec::SmallVec;
use std::ops::Range;
use std::{cmp::min, mem, ptr};

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
    filled: SmallVec<[SmallVec<[*mut HeaderUnTyped; 1]>; 4]>,
}

pub unsafe trait Condemned {
    fn feilds(s: &Self, grey_feilds: u8, region: Range<usize>) -> u8;
    fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
    );

    const GC_COUNT: u8;
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

    default fn evacuate<'e>(_: &Self, _: u8, _: u8, _: Range<usize>, _: &mut Handlers) {}

    default const GC_COUNT: u8 = 0;
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

    fn evacuate<'e>(
        s: &Self,
        offset: u8,
        _: u8,
        region: std::ops::Range<usize>,
        handlers: &mut Handlers,
    ) {
        let ptr = s.ptr as *const T;
        let addr = ptr as usize;
        if region.contains(&addr) {
            let i = handlers.translation[offset as usize];
            if let Some(next) = handlers.nexts.get_mut(i as usize) {
                let next_addr = *next as usize;
                let header_addr = next_addr - next_addr % ARENA_SIZE;

                // Get a new Arena if this ones full
                if HeaderUnTyped::last_offset(mem::align_of::<T>() as u16) as usize > next_addr {
                    handlers.filled[i as usize].push(header_addr as *mut HeaderUnTyped);
                    *next = alloc_arena::<T>() as *mut u8;
                };

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
                    // TODO fix overwrite when size_of<T> > size_of<Header> via min in other places
                    *next = min(addr - mem::size_of::<T>(), header_addr) as *mut u8;
                }
            }
        }
    }

    const GC_COUNT: u8 = 1;
    const PRE_CONDTION: bool = true;
}

// std impls

unsafe impl<T> Condemned for Option<T> {
    default fn feilds(s: &Self, grey_feilds: u8, region: Range<usize>) -> u8 {
        match s {
            Some(t) => Condemned::feilds(t, grey_feilds, region),
            None => 0b0000_0000,
        }
    }

    fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
    ) {
        match s {
            Some(t) => Condemned::evacuate(t, offset, grey_feilds, region, handlers),
            None => (),
        }
    }

    default const PRE_CONDTION: bool = if T::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for T. Required due to a direct Gc<A> in Option<T>");
    };
}

unsafe impl<A, B> Condemned for (A, B) {
    fn feilds((a, b): &Self, grey_feilds: u8, region: Range<usize>) -> u8 {
        let mut r = 0b0000_0000;
        if (grey_feilds & 0b1000_0000) == 0b1000_0000 {
            r |= Condemned::feilds(a, grey_feilds, region.clone());
        };

        if (grey_feilds & 0b0100_0000) == 0b0100_0000 {
            r |= Condemned::feilds(b, grey_feilds, region);
        };

        r
    }

    fn evacuate<'e>(
        (a, b): &Self,
        offset: u8,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
    ) {
        Condemned::evacuate(a, offset, grey_feilds, region.clone(), handlers);

        Condemned::evacuate(
            b,
            offset + A::GC_COUNT,
            grey_feilds,
            region.clone(),
            handlers,
        );
    }

    const GC_COUNT: u8 = A::GC_COUNT + B::GC_COUNT;
    const PRE_CONDTION: bool = if A::PRE_CONDTION && B::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for A & B. Required due to a direct Gc<T> in (A, B)");
    };
}
