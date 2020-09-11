use crate::{gc::{self, Gc}, life::{TyEq, Life}};
use crate::{
    gc_logic::{
        bus::{self, Bus, Msg},
        GcThreadBus,
    },
    mark::{GcTypeInfo, Invariant, Mark, Trace},
};
use bus::WorkerMsg;
use gc::RootIntern;
use smallvec::SmallVec;
use std::alloc::{GlobalAlloc, Layout, System};
use std::any::type_name;
use std::cell::{Cell, UnsafeCell};
use std::collections::HashMap;
use std::mem;
use std::ptr;
use std::sync::Mutex;
use std::{fmt::Debug, thread_local};

pub const ARENA_SIZE: usize = 16384;
#[repr(align(16384))]
pub(crate) struct Mem {
    _mem: [u8; ARENA_SIZE],
}

pub struct Arena<T> {
    // TODO compact representation of arenas
    // TODO make all these private by wrapping up needed functionality.
    // TODO derive header from next
    /// The index of this Arenas start message.
    /// Currently infallible
    bus_idx: u8,
    new_allocation: Cell<bool>,
    invariant: Invariant,
    marked_grey_fields: Cell<u8>,
    // TODO pub(crate)
    next: Cell<*mut T>,
    /// (next, new_allocation, grey_fields)
    full: UnsafeCell<SmallVec<[(*mut T, bool, u8); 2]>>,
}

pub struct ArenaDyn {
    type_info: GcTypeInfo,
    // TODO compact representation of arenas
    // TODO make all these private by wrapping up needed functionality.
    // TODO derive header from next
    /// The index of this Arenas start message.
    /// Currently infallible
    bus_idx: u8,
    new_allocation: Cell<bool>,
    invariant: Invariant,
    marked_grey_fields: Cell<u8>,
    // TODO pub(crate)
    next: Cell<*mut ()>,
    /// (next, new_allocation, grey_fields)
    full: UnsafeCell<SmallVec<[(*mut (), bool, u8); 2]>>,
}

impl<T: Life> Debug for Arena<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Arena<{}>", type_name::<T>()))
            .field("header", &(self.header() as *const Header<T>))
            .field("next", &self.next.get())
            .field("new_allocation", &self.new_allocation.get())
            .field("marked_grey_fields", &self.marked_grey_fields.get())
            .field("full", unsafe { &*self.full.get() })
            .field("invariant", &self.invariant)
            .finish()
    }
}

impl Debug for ArenaDyn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Arena<{}>", self.type_info.type_name))
            .field("header", &(self.header() as *const HeaderUnTyped))
            .field("next", &self.next.get())
            .field("new_allocation", &self.new_allocation.get())
            .field("marked_grey_fields", &self.marked_grey_fields.get())
            .field("full", unsafe { &*self.full.get() })
            .field("invariant", &self.invariant)
            .finish()
    }
}

impl<T: Life> Arena<T> {
    /// Returns the pointer of the next object to be allocated.
    /// Returns `None` when the `Arena` is full.
    pub fn next_ptr(&self) -> Option<*const T> {
        if !self.full() {
            Some(self.next.get() as _)
        } else {
            None
        }
    }

    pub fn header(&self) -> &Header<T> {
        unsafe { &*Header::from(self.next.get() as *const _) }
    }

    pub fn capacity(&self) -> u16 {
        capacity(
            self.next.get() as *const u8,
            mem::size_of::<T>() as u16,
            mem::align_of::<T>() as u16,
        )
    }

    #[inline(always)]
    pub fn capacity_ptr(next: *const T) -> u16 {
        capacity(
            next as *const u8,
            mem::size_of::<T>() as u16,
            mem::align_of::<T>() as u16,
        )
    }

    pub fn full(&self) -> bool {
        full(self.next.get() as *const u8, mem::align_of::<T>() as u16)
    }

    #[inline(always)]
    pub fn full_ptr(next: *const T) -> bool {
        full(next as *const u8, mem::align_of::<T>() as u16)
    }

    /// Returns a ptr to the slot after `self.next`.
    /// When `self.next` points to the lowest slot in it's Arena block
    /// the block is full, and a ptr to header will be returned.
    /// `self.next` must be aligned!
    pub fn bump_down(&self) -> *const T {
        bump_down(
            self.next.get() as *const u8,
            mem::size_of::<T>() as u16,
            mem::align_of::<T>() as u16,
        ) as *const T
    }

    /// Returns a ptr to the slot after `next`.
    /// When `next` points to the lowest slot in it's Arena block
    /// the block is full, and a ptr to header will be returned.
    /// `next` must be aligned!
    pub fn bump_down_ptr(next: *const T) -> *const T {
        bump_down(
            next as *const u8,
            mem::size_of::<T>() as u16,
            mem::align_of::<T>() as u16,
        ) as *const T
    }

    /// The index of `ptr` in it's Arena block.
    pub fn index(ptr: *const T) -> u16 {
        index(
            ptr as *const u8,
            mem::size_of::<T>() as u16,
            mem::align_of::<T>() as u16,
        )
    }

    /// To allocate a struct `T<'l>` you should use the derived `T::gc(args, Arena<T>)`.
    /// To allocate a struct `T: 'static` you should use the derived `Arena::gc(T)`.
    ///
    /// You must initialize the returned pointer!
    pub unsafe fn alloc_unit(&self) -> *mut T {
        if self.full() {
            self.new_block();
        };
        let ptr = self.next.get() as _;
        self.next.set(self.bump_down() as _);
        ptr
    }
}

impl ArenaDyn {
    /// Returns the pointer of the next object to be allocated.
    /// Returns `None` when the `Arena` is full.
    pub fn next_ptr(&self) -> Option<*const ()> {
        if !self.full() {
            Some(self.next.get() as _)
        } else {
            None
        }
    }

    fn header(&self) -> &HeaderUnTyped {
        unsafe { &*HeaderUnTyped::from(self.next.get() as *const _) }
    }

    pub fn capacity(&self) -> u16 {
        capacity(
            self.next.get() as *const u8,
            self.type_info.size,
            self.type_info.align,
        )
    }

    #[inline(always)]
    pub fn capacity_ptr(&self, next: *const ()) -> u16 {
        capacity(next as *const u8, self.type_info.size, self.type_info.align)
    }

    pub fn full(&self) -> bool {
        full(self.next.get() as *const u8, self.type_info.align)
    }

    #[inline(always)]
    pub fn full_ptr(&self, next: *const ()) -> bool {
        full(next as *const u8, self.type_info.align)
    }

    /// Returns a ptr to the slot after `self.next`.
    /// When `self.next` points to the lowest slot in it's Arena block
    /// the block is full, and a ptr to header will be returned.
    /// `self.next` must be aligned!
    pub fn bump_down(&self) -> *const () {
        bump_down(
            self.next.get() as *const u8,
            self.type_info.size,
            self.type_info.align,
        ) as *const ()
    }

    /// Returns a ptr to the slot after `next`.
    /// When `next` points to the lowest slot in it's Arena block
    /// the block is full, and a ptr to header will be returned.
    /// `next` must be aligned!
    pub fn bump_down_ptr(&self, next: *const ()) -> *const () {
        bump_down(next as *const u8, self.type_info.size, self.type_info.align) as *const ()
    }

    /// The index of `ptr` in it's Arena block.
    pub fn index(&self, ptr: *const ()) -> u16 {
        index(ptr as *const u8, self.type_info.size, self.type_info.align)
    }
}

#[inline(always)]
pub(crate) fn capacity(next: *const u8, size: u16, align: u16) -> u16 {
    let next_addr = next as usize;
    let low = HeaderUnTyped::from(next) as usize + HeaderUnTyped::low_offset(align) as usize;
    if next_addr < low {
        0
    } else {
        ((next_addr - low) as u16 / size) + 1
    }
}

#[inline(always)]
pub(crate) fn full(next: *const u8, align: u16) -> bool {
    let low_offset = HeaderUnTyped::low_offset(align) as usize;
    let header = HeaderUnTyped::from(next) as usize;
    let next_addr = next as usize;
    next_addr < header + low_offset
}

#[inline(always)]
/// Returns a ptr to the slot after `next`.
/// When `next` points to the lowest slot in it's Arena block
/// the block is full, and a ptr to header will be returned.
/// `next` must be aligned!
pub(crate) fn bump_down(next: *const u8, size: u16, align: u16) -> *const u8 {
    // TODO inspect generated code to ensure `next % align` is only being calculated once,
    // when caller compares `bump_down(next)` to `Header::from(next)`.
    let header = HeaderUnTyped::from(next);
    let low = header as usize + HeaderUnTyped::low_offset(align) as usize;
    // TODO does this generate better ASM than min(next', low)?
    if low == next as usize {
        header as *const u8
    } else {
        ((next as usize) - size as usize) as *const u8
    }
}

/// The index of `ptr` in it's Arena block.
pub(crate) fn index(ptr: *const u8, size: u16, align: u16) -> u16 {
    let header = HeaderUnTyped::from(ptr);
    let low = header as usize + HeaderUnTyped::low_offset(align) as usize;
    ((ptr as usize - low) / size as usize) as u16
}

// unsafe impl<'o, 'n, A: Life, T: Life + TyEq<A>> Mark<'o, 'n, T> for Arena<A> {
//     fn mark<'a: 'n>(&'a self, old: Gc<'o, T>) -> Gc<'n, T::L<'n>> {
//         let old_ptr = old.0 as *const _;
//         let condemned_self =
//             self.invariant.grey_self && self.invariant.condemned(old.0 as *const _);

//         let cf = Trace::fields(old.0, 0, self.marked_grey_fields.get(), &self.invariant);

//         // Worker encountered fields marked in cf.
//         self.marked_grey_fields
//             .set(self.marked_grey_fields.get() | cf);

//         if condemned_self || (0b0000_0000 != cf) {
//             let next = self.next.get();
//             if Self::full_ptr(next) {
//                 self.new_block()
//             };

//             unsafe {
//                 self.next.set(Self::bump_down_ptr(next) as *mut A);
//                 std::ptr::copy(old_ptr, next as _, 1);
//             };

//             let mut new_ptr = next;
//             let old_header = unsafe { &*Header::from(old_ptr) };
//             let evacuated = old_header.intern.evacuated.lock();
//             evacuated
//                 .unwrap()
//                 .entry(Arena::index(old.0 as *const _))
//                 .and_modify(|evacuated_ptr| new_ptr = *evacuated_ptr as _)
//                 .or_insert(next as *const u8);

//             unsafe { Gc::new(&*(new_ptr as *const _)) }
//         } else {
//             // old contains no direct condemned ptrs
//             unsafe { std::mem::transmute(old) }
//         }
//     }
// }

impl<T: Life> Drop for Arena<T> {
    fn drop(&mut self) {
        log::trace!("WORKER: droping: {:?}", self);

        GC_BUS.with(|tm| {
            let last_next = self.next.get();
            let last_new_allocation = self.new_allocation.get();
            let last_grey_fields = self.marked_grey_fields.get();
            let last_header = HeaderUnTyped::from(last_next as _);

            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).and_modify(|bus| {
                let BusState {
                    cached_arenas, bus, ..
                } = bus;
                let bus = &mut bus.lock().expect("Worker could not unlock bus").1;
                // A good candidate for https://github.com/rust-lang/rust/issues/53667

                let last_release_to_gc = match cached_arenas.put(
                    last_next as _,
                    mem::size_of::<T>() as _,
                    mem::align_of::<T>() as _,
                ) {
                    CacheStatus::Cached(cached_next) => {
                        log::trace!(
                            "WORKER: releasing cached Arena block, header: {:?}, next: {:?}",
                            last_header,
                            last_next
                        );
                        bus::send(bus, Msg::Worker(WorkerMsg::Release(cached_next as _)));

                        debug_assert!(!self.full());
                        false
                    }
                    CacheStatus::Err => {
                        // We didn't cache it so the arena is released to the GC.
                        true
                    }
                    CacheStatus::Stored => false,
                };

                let invariant_id = self.invariant.id;
                let full = unsafe { &mut *self.full.get() };
                // Get the original arena block
                let (header, next, release_to_gc, new_allocation, grey_fields) = full
                    .first()
                    .cloned()
                    .map(|(n, na, gf)| {
                        *full.first_mut().unwrap() =
                            (last_next, last_new_allocation, last_grey_fields);

                        full.iter().cloned().enumerate().for_each(
                            |(i, (next, new_allocation, grey_fields))| {
                                let release_to_gc = i != 0 || last_release_to_gc;

                                let end = Msg::Worker(WorkerMsg::End {
                                    release_to_gc,
                                    new_allocation,
                                    next: next as *mut u8,
                                    grey_fields,
                                    invariant_id,
                                    transient: true,
                                });

                                log::trace!(
                                    "WORKER: sending: {:?}, header: {:?}",
                                    end,
                                    HeaderUnTyped::from(next as _)
                                );
                                bus::send(bus, end);
                            },
                        );

                        (HeaderUnTyped::from(n as _), n, true, na, gf)
                    })
                    .unwrap_or((
                        last_header,
                        last_next,
                        last_release_to_gc,
                        last_new_allocation,
                        last_grey_fields,
                    ));

                let end = |transient| {
                    Msg::Worker(WorkerMsg::End {
                        release_to_gc,
                        new_allocation,
                        next: next as *mut u8,
                        grey_fields,
                        invariant_id,
                        transient,
                    })
                };
                let closing_end = |bus| {
                    let end = end(false);
                    log::trace!("WORKER: sending: {:?}, header: {:?}", end, header);
                    bus::send(bus, end);
                };

                let msg = bus.get_mut(self.bus_idx as usize);
                match msg {
                    Some(Msg::Worker(WorkerMsg::Start { next: nxt, .. })) => {
                        if header == HeaderUnTyped::from(*nxt) {
                            debug_assert!(next <= *nxt as _);
                            let end = end(true);

                            log::trace!(
                                "WORKER: sending merged: {:?} => {:?}, header: {:?}",
                                msg,
                                end,
                                header
                            );
                            *msg.unwrap() = end;
                        } else {
                            closing_end(bus)
                        }
                    }
                    _ => closing_end(bus),
                };
            });
        });
    }
}

// TODO reduplicate
impl Drop for ArenaDyn {
    fn drop(&mut self) {
        log::trace!("WORKER: droping: {:?}", self);

        GC_BUS.with(|tm| {
            let last_next = self.next.get();
            let last_new_allocation = self.new_allocation.get();
            let last_grey_fields = self.marked_grey_fields.get();
            let last_header = HeaderUnTyped::from(last_next as _);

            let tm = unsafe { &mut *tm.get() };
            tm.entry(
                (self.type_info, unsafe { self.type_info.drop_in_place_fn() }
                    as usize),
            )
            .and_modify(|bus| {
                let BusState {
                    cached_arenas, bus, ..
                } = bus;
                let bus = &mut bus.lock().expect("Worker could not unlock bus").1;
                // A good candidate for https://github.com/rust-lang/rust/issues/53667

                let last_release_to_gc = match cached_arenas.put(
                    last_next as _,
                    self.type_info.size,
                    self.type_info.align,
                ) {
                    CacheStatus::Cached(cached_next) => {
                        log::trace!(
                            "WORKER: releasing cached Arena block, header: {:?}, next: {:?}",
                            last_header,
                            last_next
                        );
                        bus::send(bus, Msg::Worker(WorkerMsg::Release(cached_next as _)));

                        debug_assert!(!self.full());
                        false
                    }
                    CacheStatus::Err => {
                        // We didn't cache it so the arena is released to the GC.
                        true
                    }
                    CacheStatus::Stored => false,
                };

                let invariant_id = self.invariant.id;
                let full = unsafe { &mut *self.full.get() };
                // Get the original arena block
                let (header, next, release_to_gc, new_allocation, grey_fields) = full
                    .first()
                    .cloned()
                    .map(|(n, na, gf)| {
                        *full.first_mut().unwrap() =
                            (last_next, last_new_allocation, last_grey_fields);

                        full.iter().cloned().enumerate().for_each(
                            |(i, (next, new_allocation, grey_fields))| {
                                let release_to_gc = i != 0 || last_release_to_gc;

                                let end = Msg::Worker(WorkerMsg::End {
                                    release_to_gc,
                                    new_allocation,
                                    next: next as *mut u8,
                                    grey_fields,
                                    invariant_id,
                                    transient: true,
                                });

                                log::trace!(
                                    "WORKER: sending: {:?}, header: {:?}",
                                    end,
                                    HeaderUnTyped::from(next as _)
                                );
                                bus::send(bus, end);
                            },
                        );

                        (HeaderUnTyped::from(n as _), n, true, na, gf)
                    })
                    .unwrap_or((
                        last_header,
                        last_next,
                        last_release_to_gc,
                        last_new_allocation,
                        last_grey_fields,
                    ));

                let end = |transient| {
                    Msg::Worker(WorkerMsg::End {
                        release_to_gc,
                        new_allocation,
                        next: next as *mut u8,
                        grey_fields,
                        invariant_id,
                        transient,
                    })
                };
                let closing_end = |bus| {
                    let end = end(false);
                    log::trace!("WORKER: sending: {:?}, header: {:?}", end, header);
                    bus::send(bus, end);
                };

                let msg = bus.get_mut(self.bus_idx as usize);
                match msg {
                    Some(Msg::Worker(WorkerMsg::Start { next: nxt, .. })) => {
                        if header == HeaderUnTyped::from(*nxt) {
                            debug_assert!(next <= *nxt as _);
                            let end = end(true);

                            log::trace!(
                                "WORKER: sending merged: {:?} => {:?}, header: {:?}",
                                msg,
                                end,
                                header
                            );
                            *msg.unwrap() = end;
                        } else {
                            closing_end(bus)
                        }
                    }
                    _ => closing_end(bus),
                };
            });
        });
    }
}

// TODO add specialized Mark impl for N: 'static.

impl<A: Life> Arena<A> {
    pub fn new() -> Arena<A> {
        // Register a bus for this thread type combo.
        let bus = GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<A>()).or_insert_with(|| {
                let bus = Box::leak(Box::new(BusState {
                    cached_arenas: CachedArenas([CachedArena::null(); 2]),
                    known_invariant: Invariant::none(0),
                    bus: Mutex::new((None, SmallVec::new())),
                }));

                let mut reg = GcThreadBus::get().lock().expect("Could not unlock RegBus");
                reg.register::<A>(&(bus.bus));
                bus
            })
        });

        let BusState {
            bus,
            known_invariant,
            cached_arenas,
        } = bus;
        let bus = &mut bus.lock().expect("Could not unlock bus").1;

        // Update invariant
        bus.iter_mut().for_each(|msg| {
            if let Msg::Invariant(inv) = msg {
                log::trace!("WORKER knows of {:?}", inv);
                *known_invariant = *inv;
                *msg = Msg::Slot;
            };
        });

        let (next, new_allocation) = if let Some(n) = cached_arenas.get::<A>() {
            (n, false)
        } else if let Some(next) = bus
            .iter_mut()
            .filter_map(|msg| match *msg {
                Msg::Next(next) => {
                    *msg = Msg::Slot;
                    Some(next)
                }
                _ => None,
            })
            .next()
        {
            (next as _, false)
        } else {
            (Arena::alloc() as _, true)
        };

        // send start message to gc
        let start = WorkerMsg::Start {
            next: next as *const u8,
            invariant_id: known_invariant.id,
        };
        log::trace!(
            "WORKER sending: {:?}, header: {:?}",
            start,
            HeaderUnTyped::from(next as _)
        );
        let slot = bus::send(bus, Msg::Worker(start));

        Arena {
            bus_idx: slot as u8,
            new_allocation: Cell::new(new_allocation),
            invariant: *known_invariant,
            next: Cell::new(next),
            marked_grey_fields: Cell::new(0b0000_0000),
            full: UnsafeCell::new(SmallVec::new()),
        }
    }

    fn new_block(&self) {
        unsafe { self.full.get().as_mut().unwrap() }.push((
            self.next.get(),
            self.new_allocation.get(),
            self.marked_grey_fields.get(),
        ));
        self.next.set(Self::alloc());
        self.new_allocation.set(true);
    }

    /// Create a new `Gc<T>`.
    /// If `T : Copy` & `size_of::<T>() > 8`, you should use `self.gc_copy(&T)` instead.
    pub fn gc<'r, 'a: 'r, T: Life + TyEq<A>>(&'a self, t: T) -> Gc<'r, T::L<'r>> {
        unsafe {
            if self.full() {
                self.new_block();
            };
            let ptr = self.next.get() as *mut T;
            self.next.set(self.bump_down() as *mut A);
            ptr::write(ptr, t);
            self.mark(Gc::new(&*ptr))
        }
    }

    pub fn box_alloc<'a, 'r: 'a>(&'a self, t: A) -> gc::Box<'r, A> {
        unsafe {
            if self.full() {
                self.new_block()
            }
            let ptr = self.next.get();
            self.next.set(self.bump_down() as *mut A);
            ptr::write(ptr, t);
            gc::Box::new(&mut *ptr)
        }
    }

    // fn advance(&mut self) -> Self {
    //     unimplemented!()
    // }

    /// Allocates & initializes a new Arena block.
    /// Returns highest ptr.
    pub(crate) fn alloc() -> *mut A {
        // Get more memory from system allocator
        let header = unsafe { System.alloc(Layout::new::<Mem>()) };
        debug_assert!(
            (header as usize)
                < unsafe { &std::mem::transmute::<_, &Mem>(header)._mem[ARENA_SIZE - 1] }
                    as *const _ as usize
        );

        HeaderUnTyped::init(header as *mut _);

        (header as usize + Header::<A>::high_offset() as usize) as *mut A
    }
}

impl<'l, A: Trace + Life + Copy> Arena<A> {
    /// Directly copies T instead of reading it onto the stack first.
    pub fn gc_copy<'a, 'r: 'a, T: Copy>(&'a self, t: &T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                self.new_block()
            }
            let ptr = self.next.get() as _;
            self.next.set(self.bump_down() as *mut A);
            ptr::copy(t, ptr, 1);
            Gc::new(&*ptr)
        }
    }

    /// Directly copies T instead of reading it onto the stack first.
    pub fn box_copy<'a, 'r: 'a, T: Copy>(&'a self, t: &T) -> gc::Box<'r, T> {
        unsafe {
            if self.full() {
                self.new_block()
            }
            let ptr = self.next.get() as _;
            self.next.set(self.bump_down() as *mut A);
            ptr::copy(t, ptr, 1);
            gc::Box::new(&mut *ptr)
        }
    }
}

#[test]
fn gc_alloc_test() {
    let a: Arena<usize> = Arena::new();
    let n1 = a.next.get() as usize;
    a.gc::<usize>(1);
    let n2 = a.next.get() as usize;
    assert!(n1 == n2 + 8)
}

#[test]
fn gc_clone_test() {
    let a: Arena<usize> = Arena::new();
    let n1 = a.next.get() as usize;
    a.gc_clone(&1);
    let n2 = a.next.get() as usize;
    assert!(n1 == n2 + 8)
}

#[test]
fn gc_copy_test() {
    let a: Arena<usize> = Arena::new();
    let n1 = a.next.get() as usize;
    a.gc_copy(&1);
    let n2 = a.next.get() as usize;
    assert!(n1 == n2 + 8)
}

impl<'l, A: Trace + Life + Clone> Arena<A> {
    pub fn gc_clone<'r, 'a: 'r, T: Clone>(&'a self, t: &T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                self.new_block()
            }
            let ptr = self.next.get() as _;
            self.next.set(self.bump_down() as *mut A);
            ptr::write(ptr, t.clone());
            Gc::new(&*ptr)
        }
    }

    pub fn box_clone<'a, 'r: 'a, T: Clone>(&'a self, t: &T) -> gc::Box<'r, T> {
        unsafe {
            if self.full() {
                self.new_block()
            }
            let ptr = self.next.get() as _;
            self.next.set(self.bump_down() as *mut A);
            ptr::write(ptr, t.clone());
            gc::Box::new(&mut *ptr)
        }
    }
}

pub(crate) struct HeaderUnTyped {
    pub(crate) evacuated: Mutex<HashMap<u16, *const u8>>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
    pub(crate) roots: Mutex<HashMap<u16, *const RootIntern<u8>>>,
    /// Called before the evacuation of an arena block, directly after the block is condemned.
    /// If you return `true` then the callback will be moved to the next Arena.
    pub(crate) pre_move: Mutex<HashMap<u16, SmallVec<[Box<dyn Fn(*const u8) -> bool>; 3]>>>,
    pub(crate) condemned: bool,
}

pub struct Header<T> {
    pub(crate) intern: HeaderUnTyped,
    t: std::marker::PhantomData<*const T>,
}

impl HeaderUnTyped {
    #[inline(always)]
    pub fn from(ptr: *const u8) -> *const HeaderUnTyped {
        let addr = ptr as usize;
        let offset = addr % ARENA_SIZE;
        (addr - offset) as *const _
    }

    #[inline(always)]
    /// The offset of the last T (closest to Header).
    pub(crate) const fn low_offset(align: u16) -> u16 {
        let header = mem::size_of::<HeaderUnTyped>() as u16;
        header + ((align - (header % align)) % align)
    }

    #[inline(always)]
    // TODO is this right?
    /// The offset of the first T in the Arena.
    pub(crate) const fn high_offset(align: u16, size: u16) -> u16 {
        let cap = ((ARENA_SIZE as u16 - HeaderUnTyped::low_offset(align)) / size) - 1;
        HeaderUnTyped::low_offset(align) + (cap * size)
    }

    pub(crate) fn init(ptr: *mut HeaderUnTyped) {
        // log::trace!("HeaderUnTyped::init {:?}", ptr);
        unsafe {
            ptr::write(
                ptr,
                HeaderUnTyped {
                    evacuated: Mutex::new(HashMap::new()),
                    roots: Mutex::new(HashMap::new()),
                    pre_move: Mutex::new(HashMap::new()),
                    condemned: false,
                },
            )
        }
        // log::trace!("HeaderUnTyped::init {:?} Complete", ptr);
    }
}

#[test]
fn low_offset_test() {
    assert_eq!(mem::size_of::<HeaderUnTyped>(), 200);
    assert_eq!(Header::<usize>::low_offset(), 200)
}

#[test]
fn header_from_test() {
    assert_ne!(
        HeaderUnTyped::from(0x7fcc200dd918u64 as _),
        HeaderUnTyped::from(0x7fcc200d7778u64 as _)
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use quickcheck_macros::*;

    #[quickcheck]
    fn header_from_stable_test(next: usize) {
        let h = HeaderUnTyped::from(next as *const u8);
        assert_eq!(h, HeaderUnTyped::from(h as _))
    }
}

impl<T> Header<T> {
    #[inline(always)]
    pub fn from(ptr: *const T) -> *const Header<T> {
        let offset = ptr as usize % ARENA_SIZE;
        (ptr as usize - offset) as *const _
    }

    #[inline(always)]
    pub const fn low_offset() -> u16 {
        HeaderUnTyped::low_offset(mem::align_of::<T>() as u16)
    }

    #[inline(always)]
    pub const fn high_offset() -> u16 {
        HeaderUnTyped::high_offset(mem::align_of::<T>() as u16, mem::size_of::<T>() as u16)
    }
}

#[derive(Debug)]
struct BusState {
    /// (next, new_allocation)
    cached_arenas: CachedArenas,
    known_invariant: Invariant,
    bus: Bus,
}

#[derive(Debug)]
struct CachedArenas([CachedArena; 2]);

#[derive(Debug)]
enum CacheStatus<T> {
    Cached(*mut T),
    Stored,
    Err,
}

impl CachedArenas {
    /// Try to cache an Arena.
    /// If the Cache is full and the new Arena will be cached the old Arena with the least capacity will be returned.
    fn put<T>(&mut self, next: *mut T, size: u16, align: u16) -> CacheStatus<T> {
        let header = HeaderUnTyped::from(next as _);
        // log::trace!(
        //     "CachedArenas::put(header: {:?}, next: {:?}, new_allocation: {:?})",
        //     header,
        //     next,
        //     new_allocation
        // );
        // log::trace!("CachedArenas::put {:?}", self);

        debug_assert!(self
            .0
            .iter()
            .find(|ca| HeaderUnTyped::from(ca.next()) == header)
            .is_none());

        let (cached_arena, cap) = self
            .0
            .iter_mut()
            .map(|a| {
                let cap = capacity(a.next() as _, size, align);
                (a, cap)
            })
            .min_by(|(_, ac), (_, bc)| ac.cmp(bc))
            .unwrap();

        if cap < capacity(next as _, size, align) {
            let r = if cached_arena.is_null() {
                CacheStatus::Stored
            } else {
                CacheStatus::Cached(cached_arena.next() as _)
            };

            *cached_arena = CachedArena::new(next as _);
            log::trace!("WORKER: CachedArenas::put modified {:?}", self);
            r
        } else {
            CacheStatus::Err
        }
    }

    fn get<T: Life>(&mut self) -> Option<*mut T> {
        self.0
            .iter_mut()
            .filter(|a| !CachedArena::is_null(**a))
            .map(|a| {
                let cap = Arena::<T>::capacity_ptr(a.next() as _);
                (a, cap)
            })
            .max_by(|(_, ac), (_, bc)| ac.cmp(bc))
            .map(|(a, _)| {
                let next = a.next() as _;
                *a = CachedArena::null();
                next
            })
    }
}

#[derive(Copy, Clone)]
struct CachedArena {
    /// May be null.
    next: *const u8,
}

impl Debug for CachedArena {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CachedArena")
            .field("header", &HeaderUnTyped::from(self.next()))
            .field("next", &self.next())
            .finish()
    }
}

impl CachedArena {
    fn new(next: *mut u8) -> CachedArena {
        CachedArena { next }
    }

    fn next(self) -> *mut u8 {
        self.next as _
    }

    fn null() -> CachedArena {
        CachedArena { next: ptr::null() }
    }
    fn is_null(self) -> bool {
        self.next == ptr::null()
    }
}

thread_local! {
    /// Map from type to GC communication bus.
    /// `Arena.next` from an `Msg::End` with excise capacity.
    /// Cached invariant from last `Msg::Gc`.
    static GC_BUS: UnsafeCell<HashMap<(GcTypeInfo, usize), &'static mut BusState>> = UnsafeCell::new(HashMap::new());

    // FIXME upon drop/thread end send End for cached_next and Msg::Gc.next
}

fn key<T: Life>() -> (GcTypeInfo, usize) {
    (GcTypeInfo::new::<T>(), ptr::drop_in_place::<T> as usize)
}

#[test]
fn pre_condition_gc_newtype() {
    use crate as sundial_gc;
    use sundial_gc_derive::*;

    // Without a Trace impl errors are generated.
    // TODO can we improve the missleading error message?
    #[derive(Trace)]
    struct Foo<'r>(Gc<'r, usize>);

    let u: Arena<usize> = Arena::new();

    let a: Arena<Foo> = Arena::new();
    let b: Arena<Foo<'static>> = Arena::new();

    // a.gc::<Foo>(Foo(u.gc(1)));
    // b.gc::<Foo>(Foo(u.gc(1)));
}

#[test]
fn pre_condition_no_gc_newtype() {
    use crate as sundial_gc;
    use sundial_gc::*;
    use sundial_gc_derive::*;
    // Without a Trace impl errors are generated.
    #[derive(Trace)]
    struct Foo<T>(T);

    // let a: Arena<Foo<Gc<usize>>> = Arena::new();
    let b: Arena<Foo<usize>> = Arena::new();
    let c: Arena<Foo<String>> = Arena::new();

    // a.gc(Foo(u.gc(1))); //~ [rustc E0597] [E] `u` does not live long enough borrowed value does not live long enough
    b.gc::<Foo<usize>>(Foo(1));
    c.gc::<Foo<String>>(Foo(String::from("foo")));
}

// #[test]
// fn use_test() {
//     let a: &'static usize = unsafe { mark::coerce_lifetime(&1usize)};
// }
