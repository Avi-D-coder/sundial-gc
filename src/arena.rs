use crate::auto_traits::*;
use crate::gc::{self, Gc};
use crate::{
    gc_logic::GcThreadBus,
    mark::{Condemned, GcTypeInfo, Mark},
};
use std::alloc::{GlobalAlloc, Layout, System};
use std::any::type_name;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::mem::{self, transmute};
use std::ops::Range;
use std::ptr;
use std::sync::Mutex;
use std::{thread, thread_local, time::Duration};

pub const ARENA_SIZE: usize = 16384;

pub struct Arena<T: Condemned + Immutable> {
    // TODO compact representation of arenas
    // TODO make all these private by wrapping up needed functionality.
    // TODO derive header from next
    /// The index of this Arenas start message.
    /// Currently infallible
    bus_idx: u8,
    new_allocation: bool,
    pub grey_self: bool,
    pub grey_feilds: UnsafeCell<u8>,
    pub white_region: Range<usize>,
    pub next: UnsafeCell<*mut T>,
}

impl<T: Immutable + Condemned> Arena<T> {
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
    (next_addr - low) as u16 / size
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

#[repr(align(16384))]
pub(crate) struct Mem {
    _mem: [u8; ARENA_SIZE],
}

unsafe impl<'o, 'n, 'r: 'n, O: Immutable + 'o, N: Immutable + 'r> Mark<'o, 'n, 'r, O, N>
    for Arena<N>
{
    default fn mark(&self, old: Gc<'o, O>) -> Gc<'r, N> {
        if type_name::<O>() != type_name::<N>() {
            // TODO once Eq for &str is const make this const
            panic!("O is a different type then N, mark only changes lifetimes")
        };

        let old_ptr = old.0 as *const O as *const N;
        let condemned_self = self.grey_self && self.white_region.contains(&(old_ptr as usize));

        let grey_feilds = unsafe { &mut *self.grey_feilds.get() };
        let cf = Condemned::feilds(old.0, 0, *grey_feilds, self.white_region.clone());

        if condemned_self || (0b0000_0000 != cf) {
            let next_slot = self.next.get();
            let next = unsafe { *next_slot };
            if Self::full_ptr(next) {
                // TODO
                panic!("Allocating additional memory for an arena not yet supported");
            };

            if Self::full_ptr(next) {
                // TODO
                panic!("Allocating additional memory for an arena not yet supported");
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

impl<T: Immutable + Condemned> Drop for Arena<T> {
    fn drop(&mut self) {
        GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).and_modify(|bus| {
                let next = unsafe { *self.next.get() } as *const u8;
                let capacity = self.capacity();
                let cached_next = bus.cached_next;

                let release_to_gc = if capacity != 0
                    && capacity
                        > cached_next
                            .map(|cn| Self::capacity_ptr(cn as *const T))
                            .unwrap_or(0)
                {
                    bus.cached_next = Some(next as *mut _);
                    false
                } else {
                    true
                };
                let mut msgs = bus
                    .bus
                    .lock()
                    .expect("Could not unlock bus while dropping Arena");
                msgs[self.bus_idx as usize] = Msg::End {
                    release_to_gc,
                    new_allocation: self.new_allocation,
                    next,
                    grey_feilds: unsafe { *self.grey_feilds.get() },
                    white_start: self.white_region.start,
                    white_end: self.white_region.end,
                };
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
        if self.grey_self && self.white_region.contains(&(&*o as *const _ as usize)) {
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

impl<T: Immutable + Condemned> Arena<T> {
    pub fn new() -> Arena<T> {
        if !T::PRE_CONDTION {
            panic!("You need to derive Condemned for T")
        };

        // Register a bus for this thread type combo.
        let bus = GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).or_insert_with(|| {
                let bus = Box::leak(Box::new(CacheBus {
                    cached_next: None,
                    known_invariant: GcInvariant::none(),
                    bus: Mutex::new([Msg::Slot; 8]),
                }));

                let mut reg = GcThreadBus::get().lock().expect("Could not unlock RegBus");
                reg.register::<T>(&(bus.bus));
                bus
            })
        });

        // FIXME(extend_bus) This is a pause on the worker!
        loop {
            let msgs = bus.bus.lock().expect("Could not unlock bus");
            let slot = msgs.iter().filter(|o| o.is_gc() || o.is_slot()).next();

            if slot.is_some() {
                break;
            } else {
                log::info!("Worker sleeping 5ms");
                thread::sleep(Duration::from_millis(5));
            }
        }

        let mut msgs = bus.bus.lock().expect("Could not unlock bus");
        let slot = msgs
            .iter_mut()
            .enumerate()
            .filter(|(_, o)| o.is_gc())
            .next();

        // First look for a Gc msg then a empty Slot msg.
        if let Some((
            bus_idx,
            &mut Msg::Gc {
                next,
                grey_self,
                grey_feilds,
                white_start,
                white_end,
            },
        )) = slot
        {
            // Found a Gc msg.
            let mut new_allocation = false;
            let next = UnsafeCell::new(if let Some(n) = next {
                n as *mut T
            } else if let Some(n) = bus.cached_next {
                n as *mut T
            } else {
                new_allocation = true;
                Arena::alloc()
            });

            msgs[bus_idx] = Msg::Slot;
            Arena {
                bus_idx: bus_idx as u8,
                new_allocation,
                next,
                grey_self,
                grey_feilds: UnsafeCell::new(grey_feilds),
                white_region: white_start..white_end,
            }
        } else {
            let inv = bus.known_invariant;

            let mut new_allocation = false;
            let next = if let Some(n) = bus.cached_next {
                n as *mut T
            } else {
                new_allocation = true;
                Arena::alloc()
            };

            // TODO(extend_bus)
            let (bus_idx, slot) = msgs
                .iter_mut()
                .enumerate()
                .filter(|(_, o)| o.is_slot())
                .next()
                .expect("No space on bus left. Extending the bus not yet supported.");

            // send start message to gc
            *slot = Msg::Start {
                next: next as *const u8,
                white_start: 0,
                white_end: 1,
            };

            Arena {
                bus_idx: bus_idx as u8,
                new_allocation,
                next: UnsafeCell::new(next),
                grey_self: inv.grey_self,
                grey_feilds: UnsafeCell::new(inv.grey_feilds),
                white_region: inv.white_start..inv.white_end,
            }
        }
    }

    /// Create a new `Gc<T>`.
    /// If `T : Copy` & `size_of::<T>() > 8`, you should use `self.gc_copy(&T)` instead.
    pub fn gc_alloc<'a, 'r: 'a>(&'a self, t: T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                log::error!("Growing Arenas not yet implemented");
                panic!("Growing Arenas not yet implemented")
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
                log::error!("Growing Arenas not yet implemented");
                panic!("Growing Arenas not yet implemented")
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

impl<T: Immutable + Condemned + Copy> Arena<T> {
    /// Directly copies T instead of reading it onto the stack first.
    pub fn gc_copy<'a, 'r: 'a>(&'a self, t: &T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                log::error!("Growing Arenas not yet implemented");
                panic!("Growing Arenas not yet implemented")
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
                log::error!("Growing Arenas not yet implemented");
                panic!("Growing Arenas not yet implemented")
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

// #[test]
// fn gc_allocs_test() {
//     for _ in 0..ARENA_SIZE {
//         let a: Arena<usize> = Arena::new();
//         let n1 = unsafe { *a.next.get() } as usize;
//         a.gc_alloc(1);
//         let n2 = unsafe { *a.next.get() } as usize;
//         assert!(n1 == n2 + 8)
//     }
// }

// #[test]
// fn gc_clones_test() {
//     for _ in 0..ARENA_SIZE {
//         let a: Arena<usize> = Arena::new();
//         let n1 = unsafe { *a.next.get() } as usize;
//         a.gc_clone(&1);
//         let n2 = unsafe { *a.next.get() } as usize;
//         assert!(n1 == n2 + 8)
//     }
// }

// #[test]
// fn gc_copys_test() {
//     for _ in 0..ARENA_SIZE {
//         let a: Arena<usize> = Arena::new();
//         let n1 = unsafe { *a.next.get() } as usize;
//         a.gc_copy(&1);
//         let n2 = unsafe { *a.next.get() } as usize;
//         assert!(n1 == n2 + 8)
//     }
// }

#[test]
fn capacity_test() {
    let a: Arena<usize> = Arena::new();
    let c1 = a.capacity();
    a.gc_alloc(1);
    let c2 = a.capacity();
    assert!(c1 - 1 == c2)
}

#[test]
fn capacitys_test() {
    let (mut next, mut cap) = {
        let a = Arena::<usize>::new();
        (unsafe { *a.next.get() }, a.capacity())
    };
    for _ in 0..ARENA_SIZE {
        let a: Arena<usize> = Arena::new();
        assert_eq!(next, unsafe { *a.next.get() });
        assert!(!a.full());
        a.gc_alloc(1);
        let c = a.capacity();
        dbg!(cap, c);
        assert!(cap - 1 == c);

        cap -= 1;
        next = unsafe { *a.next.get() };
        if a.full() {
            break;
        }
    }
}

impl<T: Immutable + Condemned + Clone> Arena<T> {
    pub fn gc_clone<'a, 'r: 'a>(&'a self, t: &T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                log::error!("Growing Arenas not yet implemented");
                panic!("Growing Arenas not yet implemented")
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
                log::error!("Growing Arenas not yet implemented");
                panic!("Growing Arenas not yet implemented")
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
    pub(crate) roots: Mutex<HashMap<u16, *mut u8>>,
    finalizers: usize,
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
        unsafe {
            ptr::write(
                ptr,
                HeaderUnTyped {
                    evacuated: Mutex::new(HashMap::new()),
                    roots: Mutex::new(HashMap::new()),
                    finalizers: 0,
                    condemned: false,
                },
            )
        }
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

struct CacheBus {
    // TODO remove Option,
    // it does not get optized away.
    cached_next: Option<*mut u8>,
    known_invariant: GcInvariant,
    bus: Bus,
}

thread_local! {
    /// Map from type to GC communication bus.
    /// `Arena.next` from an `Msg::End` with excise capacity.
    /// Cached invariant from last `Msg::Gc`.
    static GC_BUS: UnsafeCell<HashMap<(GcTypeInfo, usize), &'static mut CacheBus>> = UnsafeCell::new(HashMap::new());

    // FIXME upon drop/thread end send End for cached_next and Msg::Gc.next
}

fn key<T: Condemned>() -> (GcTypeInfo, usize) {
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
        grey_feilds: u8,
        /// The condemned ptr range
        /// Will be replaced with epoch
        white_start: usize,
        white_end: usize,
    },
    Gc {
        next: Option<*mut u8>,
        grey_feilds: u8,
        grey_self: bool,
        /// The condemned ptr range
        white_start: usize,
        white_end: usize,
    },
}

impl Msg {
    #[inline(always)]
    pub fn is_slot(&self) -> bool {
        match self {
            Msg::Slot => true,
            _ => false,
        }
    }

    pub fn is_gc(&self) -> bool {
        match self {
            Msg::Gc { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct GcInvariant {
    grey_feilds: u8,
    grey_self: bool,
    /// The condemned ptr range
    white_start: usize,
    white_end: usize,
}

impl GcInvariant {
    const fn none() -> Self {
        Self {
            grey_self: false,
            grey_feilds: 0b0000_0000,
            white_start: 0,
            white_end: 0,
        }
    }
}
