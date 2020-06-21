use super::gc::*;
use crate::{
    arena::{alloc_arena, Header},
    auto_traits::{HasGc, Immutable},
    gc_logic::{HeaderUnTyped, ARENA_SIZE},
    trace::{GcTypeInfo, Trace},
};
use smallvec::SmallVec;
use std::ops::Range;
use std::{
    cmp::min,
    collections::{HashMap, HashSet},
    mem, ptr,
};

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

pub struct Tti {
    /// Holds fn ptr of trace_transitive_type_info calls
    parrents: HashSet<usize>,
    pub type_info: HashSet<GcTypeInfo>,
}

impl Tti {
    pub fn new() -> Self {
        Self {
            parrents: HashSet::new(),
            type_info: HashSet::new(),
        }
    }

    /// Gc is the only type that adds it's self.
    pub unsafe fn add_gc<T: Trace>(tti: *mut Tti) {
        (*tti).type_info.insert(GcTypeInfo::new::<T>());
    }

    pub unsafe fn add_trans<T: Condemned>(tti: *mut Tti) {
        // Prevent cycles by only calling each function once
        let tgt = T::transitive_gc_types;
        if (*tti).parrents.insert(tgt as *const fn(*mut Tti) as usize) {
            T::transitive_gc_types(tti)
        }
    }
}

pub unsafe trait Condemned {
    fn feilds(s: &Self, offset: u8, grey_feilds: u8, region: Range<usize>) -> u8;
    fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
    );

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, (SmallVec<[u8; 8]>, u8)>, offset: u8);
    fn transitive_gc_types(tti: *mut Tti);

    const GC_COUNT: u8;
    const PRE_CONDTION: bool;
}

unsafe impl<T: Immutable> Condemned for T {
    default fn feilds(_: &Self, _: u8, _: u8, _: Range<usize>) -> u8 {
        // This check is superfluous
        // TODO ensure if gets optimized out
        if !T::HAS_GC {
            0b0000_0000
        } else {
            panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>")
        }
    }

    default fn direct_gc_types(_: &mut HashMap<GcTypeInfo, (SmallVec<[u8; 8]>, u8)>, _: u8) {}

    default fn evacuate<'e>(_: &Self, _: u8, _: u8, _: Range<usize>, _: &mut Handlers) {}

    default fn transitive_gc_types(_: *mut Tti) {}

    default const GC_COUNT: u8 = 0;
    default const PRE_CONDTION: bool = if T::HAS_GC {
        // TODO When fmt is allowed in const s/your type/type_name::<T>()
        panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
    } else {
        true
    };
}

unsafe impl<'r, T: Immutable + Condemned> Condemned for Gc<'r, T> {
    /// Returns the bit associated with a condemned ptr
    fn feilds(s: &Self, offset: u8, grey_feilds: u8, condemned: std::ops::Range<usize>) -> u8 {
        let bit = 1 << (offset % 8);
        let ptr = s.ptr as *const T;
        if grey_feilds & bit == bit
            && condemned.contains(&(ptr as usize))
            && (unsafe { (&*Header::from(ptr)).condemned })
        {
            bit
        } else {
            0b0000_0000
        }
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

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, (SmallVec<[u8; 8]>, u8)>, offset: u8) {
        let (idxs, bits) = t
            .entry(GcTypeInfo::new::<T>())
            .or_insert((SmallVec::new(), 0b1111_1111));
        idxs.push(offset);
        *bits |= 1 << (offset % 8)
    }

    fn transitive_gc_types(tti: *mut Tti) {
        unsafe {
            Tti::add_gc::<T>(tti);
            Tti::add_trans::<T>(tti);
        }
    }

    const GC_COUNT: u8 = 1;
    const PRE_CONDTION: bool = true;
}

// std impls

unsafe impl<T: Immutable> Condemned for Option<T> {
    default fn feilds(s: &Self, offset: u8, grey_feilds: u8, region: Range<usize>) -> u8 {
        match s {
            Some(t) => Condemned::feilds(t, offset, grey_feilds, region),
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

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, (SmallVec<[u8; 8]>, u8)>, offset: u8) {
        T::direct_gc_types(t, offset)
    }

    fn transitive_gc_types(tti: *mut Tti) {
        unsafe { Tti::add_trans::<T>(tti) }
    }

    const GC_COUNT: u8 = T::GC_COUNT;
    default const PRE_CONDTION: bool = if T::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for T. Required due to a direct Gc<A> in Option<T>");
    };
}

unsafe impl<A: Immutable, B: Immutable> Condemned for (A, B) {
    fn feilds((a, b): &Self, offset: u8, grey_feilds: u8, region: Range<usize>) -> u8 {
        let mut r = 0b0000_0000;
        if (grey_feilds & 0b1000_0000) == 0b1000_0000 {
            r |= Condemned::feilds(a, offset, grey_feilds, region.clone());
        };

        if (grey_feilds & 0b0100_0000) == 0b0100_0000 {
            r |= Condemned::feilds(b, offset, grey_feilds, region);
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

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, (SmallVec<[u8; 8]>, u8)>, offset: u8) {
        A::direct_gc_types(t, offset);
        B::direct_gc_types(t, offset);
    }

    fn transitive_gc_types(tti: *mut Tti) {
        unsafe {
            Tti::add_trans::<A>(tti);
            Tti::add_trans::<B>(tti);
        }
    }

    const GC_COUNT: u8 = A::GC_COUNT + B::GC_COUNT;
    const PRE_CONDTION: bool = if A::PRE_CONDTION && B::PRE_CONDTION {
        true
    } else {
        panic!("You need to derive Condemned for A & B. Required due to a direct Gc<T> in (A, B)");
    };
}
