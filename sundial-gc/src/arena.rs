use crate::auto_traits::*;
use crate::gc::{self, Gc};
use crate::{
    gc_logic::GcThreadBus,
    mark::{GcTypeInfo, Invariant, Mark, Trace},
};
use gc::RootIntern;
use smallvec::SmallVec;
use std::alloc::{GlobalAlloc, Layout, System};
use std::any::type_name;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::mem::{self, transmute};
use std::ptr;
use std::sync::Mutex;
use std::{
    fmt::Debug,
    thread, thread_local,
    time::{Duration, Instant},
};

pub const ARENA_SIZE: usize = 16384;
#[repr(align(16384))]
pub(crate) struct Mem {
    _mem: [u8; ARENA_SIZE],
}

pub struct Arena<T: Trace + Immutable> {
    // TODO compact representation of arenas
    // TODO make all these private by wrapping up needed functionality.
    // TODO derive header from next
    /// The index of this Arenas start message.
    /// Currently infallible
    bus_idx: u8,
    new_allocation: bool,
    invariant: Invariant,
    marked_grey_fields: UnsafeCell<u8>,
    // TODO pub(crate)
    pub next: UnsafeCell<*mut T>,
    /// (next, new_allocation, grey_fields)
    full: UnsafeCell<SmallVec<[(*mut T, bool, u8); 2]>>,
}

impl<T: Immutable + Trace> Arena<T> {
    pub fn header(&self) -> &Header<T> {
        unsafe { &*Header::from(*self.next.get() as *const _) }
    }

    pub fn capacity(&self) -> u16 {
        capacity(
            unsafe { *self.next.get() } as *const u8,
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
        full(
            unsafe { *self.next.get() } as *const u8,
            mem::align_of::<T>() as u16,
        )
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
            unsafe { *self.next.get() } as *const u8,
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

unsafe impl<'o, 'n, 'r: 'n, O: Trace + 'o, N: Trace + 'r> Mark<'o, 'n, 'r, O, N> for Arena<N> {
    default fn mark(&self, old: Gc<'o, O>) -> Gc<'r, N> {
        if type_name::<O>() != type_name::<N>() {
            // TODO once Eq for &str is const make this const
            panic!("O is a different type then N, mark only changes lifetimes")
        };

        let old_ptr = old.0 as *const O as *const N;
        let condemned_self =
            self.invariant.grey_self && self.invariant.condemned(old.0 as *const O as *const _);

        let grey_fields = unsafe { &mut *self.marked_grey_fields.get() };
        let cf = Trace::fields(old.0, 0, *grey_fields, &self.invariant);

        // Worker encountered fields marked in cf.
        unsafe { *self.marked_grey_fields.get() |= cf }

        if condemned_self || (0b0000_0000 != cf) {
            let next_slot = self.next.get();
            let next = unsafe { *next_slot };
            if Self::full_ptr(next) {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            };

            unsafe {
                *next_slot = Self::bump_down_ptr(next) as *mut N;
                std::ptr::copy(old_ptr, next, 1);
            };

            let mut new_ptr = next;
            let old_header = unsafe { &*Header::from(old_ptr) };
            let evacuated = old_header.intern.evacuated.lock();
            evacuated
                .unwrap()
                .entry(Arena::index(old.0 as *const O))
                .and_modify(|evacuated_ptr| new_ptr = *evacuated_ptr as *mut N)
                .or_insert(next as *const u8);

            Gc::new(unsafe { &*new_ptr })
        } else {
            // old contains no direct condemned ptrs
            unsafe { std::mem::transmute(old) }
        }
    }
}

impl<T: Immutable + Trace> Drop for Arena<T> {
    fn drop(&mut self) {
        GC_BUS.with(|tm| {
            let next = unsafe { *self.next.get() } as *const u8;
            let full = unsafe { &*self.full.get() };
            let grey_fields = unsafe { *self.marked_grey_fields.get() };
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).and_modify(|bus| {
                // A good candidate for https://github.com/rust-lang/rust/issues/53667
                let release_to_gc = if self.full() {
                    // log::trace!("Worker releasing full Arena: {:?}", next);
                    true
                } else if let Some((cached_next, new_allocation)) =
                    bus.cached_arenas.put::<T>(next as _, self.new_allocation)
                {
                    // log::trace!("Worker releasing cached Arena {:?}",);
                    send(
                        &bus.bus,
                        Msg::End {
                            next: cached_next as _,
                            release_to_gc: true,
                            new_allocation,
                            grey_fields,
                            white_start: self.invariant.white_start,
                            white_end: self.invariant.white_end,
                        },
                    );

                    debug_assert!(!self.full());
                    false
                } else {
                    debug_assert!(!self.full());
                    // We didn't cache it so the arena is released to the GC.
                    true
                };

                let end = Msg::End {
                    release_to_gc,
                    new_allocation: self.new_allocation,
                    next,
                    grey_fields: unsafe { *self.marked_grey_fields.get() },
                    white_start: self.invariant.white_start,
                    white_end: self.invariant.white_end,
                };

                // locked bus
                {
                    let mut msgs = bus
                        .bus
                        .lock()
                        .expect("Could not unlock bus while dropping Arena");
                    let msg = &mut msgs[self.bus_idx as usize];

                    match msg {
                        Msg::Slot | Msg::Start { .. } => *msg = end,
                        _ => {
                            drop(msgs);
                            send(&bus.bus, end);
                        }
                    };
                }

                full.into_iter()
                    .cloned()
                    .for_each(|(next, new_allocation, grey_fields)| {
                        send(
                            &bus.bus,
                            Msg::End {
                                release_to_gc: true,
                                new_allocation,
                                next: next as *mut u8,
                                grey_fields,
                                white_start: self.invariant.white_start,
                                white_end: self.invariant.white_end,
                            },
                        );
                    });
            });
        });
    }
}

unsafe impl<'o, 'n, 'r: 'n, O: NoGc + Immutable + 'o, N: NoGc + Immutable + 'r>
    Mark<'o, 'n, 'r, O, N> for Arena<N>
{
    #[inline(always)]
    default fn mark(&'n self, o: Gc<'o, O>) -> Gc<'r, N> {
        // TODO make const https://github.com/rust-lang/rfcs/pull/2632
        assert_eq!(type_name::<O>(), type_name::<N>());
        if self.invariant.grey_self && self.invariant.condemned(o.0) {
            let next = self.next.get();
            unsafe { ptr::copy(transmute(o), *next, 1) };
            let mut new_gc = next as *const N;
            let old_addr = &*o as *const O as usize;
            let offset = old_addr % ARENA_SIZE;
            let old_header = unsafe { &*((old_addr - offset) as *const Header<N>) };
            let evacuated = old_header.intern.evacuated.lock();
            evacuated
                .unwrap()
                .entry((offset / mem::size_of::<N>()) as u16)
                .and_modify(|gc| new_gc = *gc as *const N)
                .or_insert_with(|| {
                    // FIXME overrun
                    unsafe { *next = ((*next as usize) - mem::size_of::<N>()) as *mut N };
                    new_gc as *const u8
                });
            Gc::new(unsafe { &*new_gc })
        } else {
            unsafe { std::mem::transmute(o) }
        }
    }
}

impl<T: Immutable + Trace> Arena<T> {
    pub fn new() -> Arena<T> {
        if !T::PRE_CONDTION {
            panic!("You need to derive Trace for T")
        };

        // Register a bus for this thread type combo.
        let bus = GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).or_insert_with(|| {
                let bus = Box::leak(Box::new(BusState {
                    cached_arenas: CachedArenas([CachedArena::null(); 2]),
                    known_invariant: Invariant::none(),
                    bus: Mutex::new([Msg::Slot; 8]),
                }));

                let mut reg = GcThreadBus::get().lock().expect("Could not unlock RegBus");
                reg.register::<T>(&(bus.bus));
                bus
            })
        });

        let BusState {
            bus,
            known_invariant,
            cached_arenas,
        } = bus;
        let mut msgs = bus.lock().expect("Could not unlock bus");
        msgs.iter_mut()
            .enumerate()
            .filter(|(_, msg)| match msg {
                Msg::Invariant(inv) => {
                    *known_invariant = *inv;
                    true
                }
                _ => false,
            })
            .map(|(i, msg)| {
                *msg = Msg::Slot;
                i
            })
            .next();

        let (next, new_allocation) = if let Some(n) = cached_arenas.get::<T>() {
            n
        } else if let Some(next) = msgs
            .iter_mut()
            .filter_map(|msg| match msg {
                Msg::Next(next) => Some(*next as _),
                _ => None,
            })
            .next()
        {
            (next, false)
        } else {
            (Arena::alloc(), true)
        };

        drop(msgs);
        // send start message to gc
        let slot = send(
            &bus,
            Msg::Start {
                next: next as *const u8,
                white_start: 0,
                white_end: 1,
            },
        );

        Arena {
            bus_idx: slot as u8,
            new_allocation,
            invariant: *known_invariant,
            next: UnsafeCell::new(next),
            marked_grey_fields: UnsafeCell::new(0b0000_0000),
            full: UnsafeCell::new(SmallVec::new()),
        }
    }

    fn new_block(&self) {
        unsafe {
            self.full.get().as_mut().unwrap().push((
                *self.next.get(),
                self.new_allocation,
                *self.marked_grey_fields.get(),
            ));
            *self.next.get() = Self::alloc();
        }
    }

    /// Create a new `Gc<T>`.
    /// If `T : Copy` & `size_of::<T>() > 8`, you should use `self.gc_copy(&T)` instead.
    pub fn gc_alloc<'a, 'r: 'a>(&'a self, t: T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            }
            let ptr = *self.next.get();
            *self.next.get() = self.bump_down() as *mut T;
            ptr::write(ptr, t);
            Gc::new(&*ptr)
        }
    }

    pub fn box_alloc<'a, 'r: 'a>(&'a self, t: T) -> gc::Box<'r, T> {
        unsafe {
            if self.full() {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            }
            let ptr = *self.next.get();
            *self.next.get() = self.bump_down() as *mut T;
            ptr::write(ptr, t);
            gc::Box::new(&mut *ptr)
        }
    }

    // fn advance(&mut self) -> Self {
    //     unimplemented!()
    // }

    /// Allocates & initializes a new Arena block.
    /// Returns highest ptr.
    pub(crate) fn alloc() -> *mut T {
        // Get more memory from system allocator
        let header = unsafe { System.alloc(Layout::new::<Mem>()) };
        debug_assert!(
            (header as usize)
                < unsafe { &std::mem::transmute::<_, &Mem>(header)._mem[ARENA_SIZE - 1] }
                    as *const _ as usize
        );

        HeaderUnTyped::init(header as *mut _);

        (header as usize + Header::<T>::high_offset() as usize) as *mut T
    }
}

impl<T: Immutable + Trace + Copy> Arena<T> {
    /// Directly copies T instead of reading it onto the stack first.
    pub fn gc_copy<'a, 'r: 'a>(&'a self, t: &T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            }
            let ptr = *self.next.get();
            *self.next.get() = self.bump_down() as *mut T;
            ptr::copy(t, ptr, 1);
            Gc::new(&*ptr)
        }
    }

    /// Directly copies T instead of reading it onto the stack first.
    pub fn box_copy<'a, 'r: 'a>(&'a self, t: &T) -> gc::Box<'r, T> {
        unsafe {
            if self.full() {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            }
            let ptr = *self.next.get();
            *self.next.get() = self.bump_down() as *mut T;
            ptr::copy(t, ptr, 1);
            gc::Box::new(&mut *ptr)
        }
    }
}

#[test]
fn gc_alloc_test() {
    let a: Arena<usize> = Arena::new();
    let n1 = unsafe { *a.next.get() } as usize;
    a.gc_alloc(1);
    let n2 = unsafe { *a.next.get() } as usize;
    assert!(n1 == n2 + 8)
}

#[test]
fn gc_clone_test() {
    let a: Arena<usize> = Arena::new();
    let n1 = unsafe { *a.next.get() } as usize;
    a.gc_clone(&1);
    let n2 = unsafe { *a.next.get() } as usize;
    assert!(n1 == n2 + 8)
}

#[test]
fn gc_copy_test() {
    let a: Arena<usize> = Arena::new();
    let n1 = unsafe { *a.next.get() } as usize;
    a.gc_copy(&1);
    let n2 = unsafe { *a.next.get() } as usize;
    assert!(n1 == n2 + 8)
}

impl<T: Immutable + Trace + Clone> Arena<T> {
    pub fn gc_clone<'a, 'r: 'a>(&'a self, t: &T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            }
            let ptr = *self.next.get();
            *self.next.get() = self.bump_down() as *mut T;
            ptr::write(ptr, t.clone());
            Gc::new(&*ptr)
        }
    }

    pub fn box_clone<'a, 'r: 'a>(&'a self, t: &T) -> gc::Box<'r, T> {
        unsafe {
            if self.full() {
                log::info!("Growing an Arena. This is slow!");
                self.new_block()
            }
            let ptr = *self.next.get();
            *self.next.get() = self.bump_down() as *mut T;
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
    _finalizers: usize,
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
        log::trace!("HeaderUnTyped::init {:?}", ptr);
        unsafe {
            ptr::write(
                ptr,
                HeaderUnTyped {
                    evacuated: Mutex::new(HashMap::new()),
                    roots: Mutex::new(HashMap::new()),
                    _finalizers: 0,
                    condemned: false,
                },
            )
        }
        log::trace!("HeaderUnTyped::init {:?} Complete", ptr);
    }
}

#[test]
fn low_offset_test() {
    assert_eq!(mem::size_of::<HeaderUnTyped>(), 160);
    assert_eq!(Header::<usize>::low_offset(), 160)
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

pub(crate) type Bus = Mutex<[Msg; 8]>;

/// FIXME(extend_bus) This is a pause on the worker!
/// Returns index of sent message.
fn send(bus: &Bus, msg: Msg) -> usize {
    let mut waiting_since: Option<Instant> = None;
    let mut sleep = 2;
    loop {
        let mut msgs = bus.lock().expect("Could not unlock bus");
        if let Some((i, slot)) = msgs
            .iter_mut()
            .enumerate()
            .filter(|(_, o)| o.is_slot())
            .next()
        {
            *slot = msg;
            drop(msgs);
            break i;
        } else {
            drop(msgs);
            let since = waiting_since.unwrap_or_else(|| {
                let n = Instant::now();
                waiting_since = Some(n);
                n
            });
            let elapsed = since.elapsed();
            thread::sleep(Duration::from_millis(sleep));
            log::info!("Worker has waited for {}ms", elapsed.as_millis());
            sleep *= 2;
        };
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

impl CachedArenas {
    /// Try to cache an Arena.
    /// If the Cache is full and the new Arena will be cached the old Arena with the least capacity will be returned.
    fn put<T: Trace>(&mut self, next: *mut T, new_allocation: bool) -> Option<(*mut T, bool)> {
        let (cached_arena, cap) = self
            .0
            .iter_mut()
            .map(|a| {
                let cap = Arena::<T>::capacity_ptr(a.next() as _);
                (a, cap)
            })
            .min_by(|(_, ac), (_, bc)| ac.cmp(bc))
            .unwrap();

        if cap < Arena::<T>::capacity_ptr(next) {
            let r = if cached_arena.is_null() {
                None
            } else {
                Some((cached_arena.next() as _, cached_arena.new_allocation()))
            };

            *cached_arena = CachedArena::new(next as _, new_allocation);
            r
        } else {
            None
        }
    }

    fn get<T: Trace>(&mut self) -> Option<(*mut T, bool)> {
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
                let new_allocation = a.new_allocation();
                *a = CachedArena::null();
                (next, new_allocation)
            })
    }
}

#[derive(Copy, Clone)]
struct CachedArena {
    tagged_next: usize,
}

impl Debug for CachedArena {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CachedArena")
            .field("header", &HeaderUnTyped::from(self.next()))
            .field("next", &self.next())
            .field("new_allocation", &self.new_allocation())
            .finish()
    }
}

impl CachedArena {
    fn new(next: *mut u8, new_allocation: bool) -> CachedArena {
        CachedArena {
            tagged_next: if new_allocation {
                next as usize | 1
            } else {
                next as usize
            },
        }
    }

    fn next(self) -> *mut u8 {
        (self.tagged_next ^ 1) as _
    }

    fn new_allocation(self) -> bool {
        self.tagged_next & 1 == 1
    }

    fn null() -> CachedArena {
        CachedArena { tagged_next: 0 }
    }
    fn is_null(self) -> bool {
        self.tagged_next == 0
    }
}

thread_local! {
    /// Map from type to GC communication bus.
    /// `Arena.next` from an `Msg::End` with excise capacity.
    /// Cached invariant from last `Msg::Gc`.
    static GC_BUS: UnsafeCell<HashMap<(GcTypeInfo, usize), &'static mut BusState>> = UnsafeCell::new(HashMap::new());

    // FIXME upon drop/thread end send End for cached_next and Msg::Gc.next
}

fn key<T: Trace>() -> (GcTypeInfo, usize) {
    (
        GcTypeInfo::new::<T>(),
        ptr::drop_in_place::<T> as *const fn(*mut T) as usize,
    )
}

// TODO replace with atomic 64 byte header encoding.
// Plus variable length condemned region messages.
#[derive(Debug, Copy, Clone)]
pub(crate) enum Msg {
    Slot,
    /// The `mem_addr` of the area
    Start {
        /// The condemned ptr range
        white_start: usize,
        white_end: usize,
        /// TODO cache on GC side per Bus
        next: *const u8,
    },
    End {
        /// Worker will not finish filling the Arena
        release_to_gc: bool,
        new_allocation: bool,
        /// Address of the 16 kB arena
        next: *const u8,
        grey_fields: u8,
        /// The condemned ptr range
        /// Will be replaced with epoch
        white_start: usize,
        white_end: usize,
    },
    Invariant(Invariant),
    Next(*mut u8),
}

impl Msg {
    #[inline(always)]
    pub fn is_slot(&self) -> bool {
        match self {
            Msg::Slot => true,
            _ => false,
        }
    }

    pub fn is_invariant(&self) -> bool {
        match self {
            Msg::Invariant { .. } => true,
            _ => false,
        }
    }
}
