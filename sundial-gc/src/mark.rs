use super::gc::*;
pub use crate::life::*;
use crate::{
    arena::{Arena, Header, HeaderUnTyped},
    auto_traits::{HasGc, Immutable, NotDerived},
    gc_logic::{free_list::FreeList, type_state::TypeState},
};
use smallvec::{Array, SmallVec};
use std::ops::{Index, Range};
use std::{
    any,
    collections::{HashMap, HashSet},
    iter, mem, ptr,
};

pub unsafe trait Mark<'o, 'n, O, N: 'n> {
    /// Mark extends the lifetime of a GCed object.
    /// `O` and `N` must be the same type with diffrent lifetimes.
    fn mark<'a: 'n>(&'a self, o: Gc<'o, O>) -> Gc<'n, N>;
}

// Blanket impl Mark for Arena<T> is in src/arena.rs

pub type Offset = u8;

#[derive(Debug)]
pub(crate) struct Translator {
    offsets: SmallVec<[Offset; 16]>,
}

pub(crate) type EffTypes = SmallVec<[&'static TypeState; 5]>;

impl Translator {
    pub(crate) fn from<T: Trace>(
        direct_gc_types: &SmallVec<[(&'static TypeState, TypeRow); 3]>,
    ) -> (Translator, u8) {
        log::trace!(
            "Translator::from(effs: {:?})",
            direct_gc_types.iter().map(|(ts, _)| ts.type_info.type_name)
        );

        let mut offsets = SmallVec::from([255; 16]);
        let mut bloom = 0b0000_0000;

        direct_gc_types
            .iter()
            .enumerate()
            .for_each(|(i, (_, (type_offs, bits)))| {
                bloom |= bits;
                type_offs.iter().for_each(|off| {
                    if offsets.len() < *off as usize {
                        offsets.extend(iter::repeat(255).take(1 + *off as usize - offsets.len()));
                    };

                    offsets[*off as usize] = i as u8;
                });
            });

        let r = (Self { offsets }, bloom);
        log::trace!("Translator::from -> {:?}", r);
        r
    }
}

impl Index<Offset> for Translator {
    type Output = Offset;
    fn index(&self, i: Offset) -> &Self::Output {
        self.offsets
            .get(i as usize)
            .expect("Handlers does not exist")
    }
}

/// Currently just holds Arenas
#[derive(Debug)]
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
    pub(crate) free: *mut FreeList,
}

impl Handlers {
    /// Returns a ptr if the condemned ptr has already been evacuated.
    unsafe fn forward<T: Trace>(condemned: *const T, new: *const T) -> *const T {
        let header = &mut *(Header::from(condemned) as *mut Header<T>);
        // This will race if multiple threads are tracing!
        let mut evacuated = header.intern.evacuated.lock().unwrap();

        // Add a forwarding ptr to the condemned Arena
        *evacuated
            .entry(Arena::index(condemned))
            .or_insert(new as *const u8) as *const T
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
    pub unsafe fn add_gc<T: Trace>(tti: *mut Tti) {
        (*tti).type_info.insert(GcTypeInfo::new::<T>());
    }

    pub unsafe fn add_trans<T: Trace>(tti: *mut Tti) {
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
    /// `fn(*const u8, u8, u8, *Invariant, *mut Handlers)`
    evacuate_fn: *const (),
    /// `fn(*mut Tti)`
    transitive_gc_types_fn: *const (),
    /// `direct_gc_types(&mut HashMap<GcTypeInfo, ([offset: u8], bits: u8)>, starting_offset: u8 = 0)`
    /// `fn(&mut HashMap<GcTypeInfo, TypeRow>, u8)`
    direct_gc_types_fn: *const (),
    /// `unsafe fn(*mut u8)`
    drop_in_place_fn: *const (),
    /// from(effs: &SmallVec<[(&'static TypeState, TypeRow); 3]>) -> (Self, u8)
    translator_from_fn: *const (),
    pub(crate) needs_drop: bool,
    pub(crate) size: u16,
    pub(crate) align: u16,
    pub(crate) gc_count: u8,
    pub(crate) type_name: &'static str,
}

impl GcTypeInfo {
    pub const fn new<T: Trace>() -> Self {
        Self {
            evacuate_fn: T::evacuate as *const _,
            transitive_gc_types_fn: T::transitive_gc_types as *const _,
            direct_gc_types_fn: T::direct_gc_types as *const _,
            drop_in_place_fn: ptr::drop_in_place::<T> as *const _,
            translator_from_fn: Translator::from::<T> as *const _,
            needs_drop: mem::needs_drop::<T>(),
            size: mem::size_of::<T>() as u16,
            align: mem::align_of::<T>() as u16,
            gc_count: T::GC_COUNT,
            type_name: any::type_name::<T>(),
        }
    }

    pub(crate) const unsafe fn evacuate_fn(
        &self,
    ) -> fn(
        *const u8,
        offset: Offset,
        grey_fields: u8,
        invariant: *const Invariant,
        handlers: *mut Handlers,
    ) {
        mem::transmute(self.evacuate_fn)
    }

    pub(crate) const fn transitive_gc_types_fn(&self) -> fn(*mut Tti) {
        unsafe { mem::transmute(self.transitive_gc_types_fn) }
    }

    pub(crate) const fn direct_gc_types_fn(&self) -> fn(*mut HashMap<GcTypeInfo, TypeRow>, u8) {
        unsafe { mem::transmute(self.direct_gc_types_fn) }
    }

    pub(crate) const unsafe fn drop_in_place_fn(&self) -> fn(*mut u8) {
        mem::transmute(self.drop_in_place_fn)
    }

    pub(crate) const fn translator_from_fn(
        &self,
    ) -> fn(&SmallVec<[(&'static TypeState, TypeRow); 3]>) -> (Translator, u8) {
        unsafe { mem::transmute(self.translator_from_fn) }
    }
}

unsafe impl Send for GcTypeInfo {}
unsafe impl Sync for GcTypeInfo {}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Invariant {
    pub(crate) white_start: usize,
    pub(crate) white_end: usize,
    pub(crate) id: u8,
    pub(crate) grey_fields: u8,
    pub(crate) grey_self: bool,
}

impl Invariant {
    pub(crate) const fn none(id: u8) -> Self {
        Self {
            grey_self: false,
            grey_fields: 0b0000_0000,
            id,
            white_start: 0,
            white_end: 0,
        }
    }

    pub(crate) const fn all(id: u8) -> Self {
        Invariant {
            white_start: 0,
            white_end: usize::MAX,
            grey_fields: 0b1111_1111,
            grey_self: true,
            id,
        }
    }

    pub(crate) fn condemned<T>(&self, ptr: *const T) -> bool {
        (self.white_start..self.white_end).contains(&(ptr as usize))
            && unsafe { &*HeaderUnTyped::from(ptr as *const _) }.condemned
    }

    pub(crate) fn _contains(&self, r: Range<usize>) -> bool {
        self.white_start <= r.start && self.white_end >= r.end
    }
}

/// Derive `Trace` when ever possible.
/// Writing your own instance is likely to result in a use after free!
///
/// If you are using unions,`UnsafeCell`, etc. you will have to write your own instance.
/// Please consult the HAMT impl for an example.
///
/// Your type must only have one lifetime `'r`, the life time of all transitive GCed values `Gc<'r, _>`.
/// You must implement all methods, do not use defaults.
///
/// You must also implement `CoerceLifetime`.
pub unsafe trait Trace {
    // type Arena: 'static + Sized;
    fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &Invariant) -> u8;
    unsafe fn evacuate<'e>(
        s: &Self,
        offset: Offset,
        grey_fields: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    );

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8);
    fn transitive_gc_types(tti: *mut Tti);

    const GC_COUNT: u8;
}

unsafe impl<T> Trace for T {
    default fn fields(_: &Self, _: u8, _: u8, _: &Invariant) -> u8 {
        // This check is superfluous
        // TODO ensure if gets optimized out
        if !T::HAS_GC {
            0b0000_0000
        } else {
            panic!("You need to derive Trace for your type. Required due to a direct Gc<T>")
        }
    }

    default fn direct_gc_types(_: &mut HashMap<GcTypeInfo, TypeRow>, _: u8) {}

    default unsafe fn evacuate<'e>(_: &Self, _: u8, _: u8, _: &Invariant, _: &mut Handlers) {}

    default fn transitive_gc_types(_: *mut Tti) {}

    default const GC_COUNT: u8 = 0;
}

unsafe impl<'r, T: Trace> Trace for Gc<'r, T> {
    /// Returns the bit associated with a condemned ptr
    fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &Invariant) -> u8 {
        let bit = 1 << (offset % 8);
        if grey_fields & bit == bit && invariant.condemned(s.0 as *const T as *const _) {
            bit
        } else {
            0b0000_0000
        }
    }

    unsafe fn evacuate<'e>(
        sellf: &Self,
        offset: u8,
        _: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    ) {
        let ptr = sellf.0 as *const T;
        let addr = ptr as usize;
        if invariant.condemned(&addr) {
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
                    let free = &mut *handlers.free;
                    log::trace!("Needs new next");
                    next = (free.alloc() as *mut _ as usize + Header::<T>::high_offset() as usize)
                        as *mut T;
                    new_arena = true;
                };

                ptr::copy_nonoverlapping(ptr, next, 1);
                let evacuated_ptr = Handlers::forward(ptr, next);

                // If we installed a forwarding ptr, bump next.
                if next == evacuated_ptr as *mut T {
                    *next_slot = Arena::bump_down_ptr(next_slot) as *mut u8;
                } else if new_arena {
                    let header = HeaderUnTyped::from(next as *const u8) as *mut HeaderUnTyped;
                    let empty = &mut *handlers.free;
                    empty.dealloc(header)
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
}

// std impls

unsafe impl<A: AsStatic + smallvec::Array> AsStatic for SmallVec<A>
where
    <A as AsStatic>::Static: Array,
{
    type Static = SmallVec<A::Static>;
}

unsafe impl<A: GC + smallvec::Array> Trace for SmallVec<A> {
    default fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &Invariant) -> u8 {
        s.iter().fold(0b0000_0000, |b, t| {
            b | Trace::fields(t, offset, grey_fields, invariant)
        })
    }

    unsafe fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_fields: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    ) {
        s.iter()
            .for_each(|t| Trace::evacuate(t, offset, grey_fields, invariant, handlers))
    }

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8) {
        A::direct_gc_types(t, offset)
    }

    fn transitive_gc_types(tti: *mut Tti) {
        unsafe { Tti::add_trans::<A>(tti) }
    }

    const GC_COUNT: u8 = A::GC_COUNT;
}

unsafe impl<T: AsStatic> AsStatic for Vec<T> {
    type Static = Vec<T::Static>;
}

unsafe impl<T: AsStatic> Trace for Vec<T> {
    default fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &Invariant) -> u8 {
        s.iter().fold(0b0000_0000, |b, t| {
            b | Trace::fields(t, offset, grey_fields, invariant)
        })
    }

    unsafe fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_fields: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    ) {
        s.iter()
            .for_each(|t| Trace::evacuate(t, offset, grey_fields, invariant, handlers))
    }

    fn direct_gc_types(t: &mut HashMap<GcTypeInfo, TypeRow>, offset: u8) {
        T::direct_gc_types(t, offset)
    }

    fn transitive_gc_types(tti: *mut Tti) {
        unsafe { Tti::add_trans::<T>(tti) }
    }

    const GC_COUNT: u8 = T::GC_COUNT;
}

unsafe impl<T: GC> AsStatic for [T; 32] {
    type Static = [T::Static; 32];
}

unsafe impl<T: GC> Trace for [T; 32] {
    // FIXME
}

unsafe impl<T: AsStatic> AsStatic for Option<T> {
    type Static = Option<T::Static>;
}

unsafe impl<T: AsStatic> Trace for Option<T> {
    default fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &Invariant) -> u8 {
        match s {
            Some(t) => Trace::fields(t, offset, grey_fields, invariant),
            None => 0b0000_0000,
        }
    }

    unsafe fn evacuate<'e>(
        s: &Self,
        offset: u8,
        grey_fields: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    ) {
        match s {
            Some(t) => Trace::evacuate(t, offset, grey_fields, invariant, handlers),
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
}

unsafe impl<A: Trace, B: Trace> Trace for (A, B) {
    // type Arena = Arena<(A::L<'static>, B::L<'static>)>;
    fn fields((a, b): &Self, offset: u8, grey_fields: u8, invariant: &Invariant) -> u8 {
        let mut r = 0b0000_0000;
        r |= Trace::fields(a, offset, grey_fields, invariant);
        r |= Trace::fields(b, offset + A::GC_COUNT, grey_fields, invariant);
        r
    }

    unsafe fn evacuate<'e>(
        (a, b): &Self,
        offset: u8,
        grey_fields: u8,
        invariant: &Invariant,
        handlers: &mut Handlers,
    ) {
        Trace::evacuate(a, offset, grey_fields, invariant, handlers);
        Trace::evacuate(b, offset + A::GC_COUNT, grey_fields, invariant, handlers);
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
}
