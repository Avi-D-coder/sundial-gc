use super::gc::*;
use crate::{
    arena::{Arena, Header, HeaderUnTyped},
    auto_traits::{HasGc, Immutable},
};
use smallvec::SmallVec;
use std::ops::{Index, Range};
use std::{
    collections::{HashMap, HashSet},
    iter, mem, ptr,
};

/// This will be sound once GAT or const Eq &str lands.
/// The former will allow transmuting only lifetimes.
/// The latter will make `assert_eq!(type_name::<O>(), type_name::<N>())` const.
// https://github.com/rust-lang/rfcs/pull/2632.
pub unsafe trait Mark<'o, 'n, 'r: 'n, O: 'o, N: 'r> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'r, N>;
}

// Blanket Arena<T> impl is in src/arena.rs

pub type Offset = u8;

pub(crate) struct Translator {
    offsets: SmallVec<[Offset; 16]>,
}

impl Translator {
    pub(crate) fn from<T: Condemned>(effs: &Vec<GcTypeInfo>) -> (Self, u8) {
        let mut types: HashMap<GcTypeInfo, TypeRow> = HashMap::with_capacity(16);
        T::direct_gc_types(&mut types, 0);

        let mut offsets = SmallVec::from([255; 16]);
        let mut bloom = 0b0000_0000;

        types
            .drain()
            .into_iter()
            .for_each(|(ti, (type_offs, bits))| {
                bloom |= bits;
                if let Some((i, _)) = effs.iter().enumerate().find(|(_, eff)| *eff == &ti) {
                    type_offs.iter().for_each(|off| {
                        if offsets.len() < *off as usize {
                            offsets
                                .extend(iter::repeat(255).take(1 + *off as usize - offsets.len()));
                        };

                        offsets[*off as usize] = i as u8;
                    });
                };
            });

        (Self { offsets }, bloom)
    }
}

impl Index<Offset> for Translator {
    type Output = Offset;
    fn index(&self, i: Offset) -> &Self::Output {
        unsafe { self.offsets.get_unchecked(i as usize) }
    }
}

/// Currently just holds Arenas
pub struct Handlers {
    // TODO benchmark sizes
    pub(crate) translator: Translator,
    /// Must contain a entry for each active allocator effect Offset
    /// A missing index will be treated as null;
    pub(crate) nexts: SmallVec<[*mut u8; 4]>,
    // forward(old, new) -> already evacuated
    // forward: fn(*const u8, *const u8) -> Option<*const u8>,
    /// Must contain a entry for each active Effect Offset
    /// A missing index will be treated as null;
    pub(crate) filled: SmallVec<[SmallVec<[*mut HeaderUnTyped; 1]>; 4]>,
    // TODO not safe once we trace in parallel
    /// Header must be uninitialized!
    pub(crate) free: *mut Vec<*mut HeaderUnTyped>,
}

impl Handlers {
    /// Returns a ptr if the condemned ptr has already been evacuated.
    unsafe fn forward_single_threaded<T: Immutable>(
        condemned: *const T,
        new: *const T,
    ) -> *const T {
        let header = &mut *(Header::from(condemned) as *mut Header<T>);
        // This will race if multiple threads are tracing!
        let evacuated = header.evacuated.get_mut().unwrap();

        // Add a forwarding ptr to the condemned Arena
        *evacuated.entry(Arena::index(condemned)).or_insert(new)
    }
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
    pub unsafe fn add_gc<T: Condemned>(tti: *mut Tti) {
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

/// `([Offsets], CompressedOffsets)`
/// Encodes the positions a type occurs in.
pub type TypeRow = (SmallVec<[u8; 8]>, u8);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcTypeInfo {
    /// `fn(*const u8, u8, u8, Range<usize>, *mut Handlers)`
    pub(crate) evacuate_fn: *const (),
    /// `fn(*mut Tti)`
    pub(crate) transitive_gc_types_fn: *const (),
    /// `direct_gc_types(&mut HashMap<GcTypeInfo, ([offset: u8], bits: u8)>, starting_offset: u8 = 0)`
    /// `fn(&mut HashMap<GcTypeInfo, TypeRow>, u8)`
    pub(crate) direct_gc_types_fn: *const (),
    /// `unsafe fn(*mut u8)`
    pub(crate) drop_in_place_fn: *const (),
    /// from(effs: &Vec<GcTypeInfo>) -> (Self, u8)
    pub(crate) translator_from_fn: *const (),
    pub(crate) needs_drop: bool,
    pub(crate) size: u16,
    pub(crate) align: u16,
}

impl GcTypeInfo {
    pub const fn new<T: Condemned>() -> Self {
        Self {
            evacuate_fn: T::evacuate as *const _,
            transitive_gc_types_fn: T::transitive_gc_types as *const _,
            direct_gc_types_fn: T::direct_gc_types as *const _,
            drop_in_place_fn: ptr::drop_in_place::<T> as *const _,
            translator_from_fn: Translator::from::<T> as *const _,
            needs_drop: mem::needs_drop::<T>(),
            size: mem::size_of::<T>() as u16,
            align: mem::align_of::<T>() as u16,
        }
    }
}

unsafe impl Send for GcTypeInfo {}
unsafe impl Sync for GcTypeInfo {}

pub unsafe trait Condemned: Immutable {
    fn feilds(s: &Self, offset: u8, grey_feilds: u8, region: Range<usize>) -> u8;
    unsafe fn evacuate<'e>(
        s: &Self,
        offset: Offset,
        grey_feilds: u8,
        region: Range<usize>,
        handlers: &mut Handlers,
    );

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8);
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

    default fn direct_gc_types(_: &mut HashMap<GcTypeInfo, TypeRow>, _: u8) {}

    default unsafe fn evacuate<'e>(_: &Self, _: u8, _: u8, _: Range<usize>, _: &mut Handlers) {}

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
        let ptr = s.0 as *const T;
        if grey_feilds & bit == bit
            && condemned.contains(&(ptr as usize))
            && (unsafe { (&*Header::from(ptr)).condemned })
        {
            bit
        } else {
            0b0000_0000
        }
    }

    unsafe fn evacuate<'e>(
        sellf: &Self,
        offset: u8,
        _: u8,
        region: std::ops::Range<usize>,
        handlers: &mut Handlers,
    ) {
        let ptr = sellf.0 as *const T;
        let addr = ptr as usize;
        if region.contains(&addr) {
            let i = handlers.translator[offset];
            if let Some(next_slot) = handlers.nexts.get_mut(i as usize) {
                let mut next = *next_slot as *mut T;
                let header_addr = Header::from(next);

                // Get a new Arena if this ones full
                let mut new_arena = false;
                if Arena::full_ptr(next) {
                    handlers
                        .filled
                        .get_mut(i as usize)
                        .map(|v| v.push(header_addr as *mut HeaderUnTyped));
                    let empty = &mut *handlers.free;
                    next = empty
                        .pop()
                        .map(|header| {
                            HeaderUnTyped::init(header);
                            header as *mut T
                        })
                        .unwrap_or(Arena::alloc());
                    new_arena = true;
                };

                ptr::copy_nonoverlapping(ptr, next, 1);
                let evacuated_ptr = Handlers::forward_single_threaded(ptr, next);

                // If we installed a forwarding ptr, bump next.
                if next == evacuated_ptr as *mut T {
                    *next_slot = Arena::bump_down_ptr(next_slot) as *mut u8;
                } else if new_arena {
                    let header = HeaderUnTyped::from(next as *const u8) as *mut HeaderUnTyped;
                    ptr::drop_in_place(header);

                    let empty = &mut *handlers.free;
                    empty.push(header)
                }
                let old_ptr = sellf as *const Gc<T> as *mut *const T;
                *old_ptr = next;
            }
        }
    }

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8) {
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

    unsafe fn evacuate<'e>(
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

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8) {
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
            r |= Condemned::feilds(b, offset + A::GC_COUNT, grey_feilds, region);
        };

        r
    }

    unsafe fn evacuate<'e>(
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

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8) {
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
