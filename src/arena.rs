use crate::auto_traits::*;
use crate::gc::*;
use crate::mark::{Condemned, GcTypeInfo, Mark};
use std::alloc::{GlobalAlloc, Layout, System};
use std::any::type_name;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::mem::{self, transmute};
use std::ops::Range;
use std::ptr;
use std::sync::Mutex;
use std::{cmp::min, thread_local};

pub const ARENA_SIZE: usize = 16384;

pub struct Arena<T: Condemned + Immutable> {
    // TODO compact representation of arenas
    // TODO make all these private by wrapping up needed functionality.
    // TODO derive header from next
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

    pub fn capacity(&self) -> usize {
        let next = unsafe { *self.next.get() } as usize;
        (next - (self.header() as *const _ as usize + Header::<T>::low_offset() as usize))
            / mem::size_of::<T>()
    }

    pub fn full(&self) -> bool {
        let next = unsafe { *self.next.get() } as usize;
        next < self.header() as *const _ as usize + Header::<T>::low_offset() as usize
    }
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
        let condemned_self =
            self.grey_self && self.white_region.contains(&(&*old as *const _ as usize));

        let grey_feilds = unsafe { &mut *self.grey_feilds.get() };
        let cf = Condemned::feilds(&*old, 0, *grey_feilds, self.white_region.clone());

        if condemned_self || (0b0000_0000 != cf) {
            let next = self.next.get();
            if (next as usize)
                < self.header() as *const _ as usize + Header::<N>::low_offset() as usize
            {
                // TODO
                panic!("Allocating additional memory for an arena not yet supported");
            };

            unsafe { std::ptr::copy(std::mem::transmute::<_, *const N>(old), *next, 1) };
            let mut new_gc: *const N = next as _;
            let old_addr = &*old as *const _ as usize;
            let offset = old_addr % ARENA_SIZE;
            let old_header = unsafe { &*((old_addr - offset) as *const Header<N>) };
            let evacuated = old_header.evacuated.lock();
            evacuated
                .unwrap()
                .entry((offset / mem::size_of::<N>()) as u16)
                .and_modify(|gc| new_gc = unsafe { std::mem::transmute(*gc) })
                .or_insert_with(|| {
                    unsafe {
                        // FIXME overrun
                        *next = min(
                            (*next as usize) - mem::size_of::<N>(),
                            self.header() as *const _ as usize,
                        ) as *mut _
                    };
                    new_gc
                });
            Gc {
                ptr: unsafe { &*new_gc },
            }
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
                let cached_next = bus.0 .0;

                let release_to_gc = if capacity != 0
                    && cached_next.is_some()
                    && capacity
                        > (cached_next.unwrap() as usize
                            - (Header::from(cached_next.unwrap() as *mut _) as usize
                                + mem::size_of::<Header<T>>()))
                            / mem::size_of::<T>()
                {
                    bus.0 .0 = Some(next as *mut _);
                    false
                } else {
                    true
                };
                let mut msgs = bus.1.lock().unwrap();
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
            let evacuated = old_header.evacuated.lock();
            evacuated
                .unwrap()
                .entry((offset / mem::size_of::<N>()) as u16)
                .and_modify(|gc| new_gc = *gc)
                .or_insert_with(|| {
                    // FIXME overrun
                    unsafe { *next = ((*next as usize) - mem::size_of::<N>()) as *mut N };
                    new_gc
                });
            Gc {
                ptr: unsafe { &*new_gc },
            }
        } else {
            unsafe { std::mem::transmute(o) }
        }
    }
}

/// Returns `next`.
pub fn alloc_arena<T>() -> *mut T {
    // Get more memory from system allocator
    let mem_ptr = unsafe { System.alloc(Layout::new::<Mem>()) };
    let mem_addr = mem_ptr as usize;
    debug_assert!(
        mem_addr
            < unsafe { &std::mem::transmute::<_, &Mem>(mem_ptr)._mem[16383] } as *const _ as usize
    );

    // FIXME animalizes Header

    // FIXME this seems wrong! Make sure it's in sync with ArenaUntyped
    let capacity = (ARENA_SIZE - Header::<T>::low_offset() as usize) / mem::size_of::<T>();
    (mem_addr + (capacity * mem::size_of::<T>())) as *mut T
}

impl<T: Immutable + Condemned> Arena<T> {
    pub fn new() -> Arena<T> {
        // if !T::PRE_CONDITION {

        // };
        let bus = GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).or_insert_with(|| {
                Box::new(((None, GcInvariant::none()), Mutex::new([Msg::Slot; 8])))
            })
        });

        let mut msgs = bus.1.lock().expect("Could not unlock bus");
        let slot = msgs
            .iter_mut()
            .enumerate()
            .filter(|(_, o)| o.is_gc())
            .next();

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
            let mut new_allocation = false;
            let next = UnsafeCell::new(if let Some(n) = next {
                n as *mut T
            } else if let Some(n) = bus.0 .0 {
                n as *mut T
            } else {
                new_allocation = true;
                alloc_arena::<T>()
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
            let inv = bus.0 .1;

            let mut new_allocation = false;
            let next = if let Some(n) = bus.0 .0 {
                n as *mut T
            } else {
                new_allocation = true;
                alloc_arena::<T>()
            };

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

    pub fn gc_alloc<'a, 'r: 'a>(&'a self, t: T) -> Gc<'r, T> {
        unsafe {
            if self.full() {
                panic!("Growing `Arena`s not yet implemented")
            }
            let ptr = *self.next.get();
            *self.next.get() = (ptr as usize - mem::size_of::<T>()) as *mut _;
            ptr::write(ptr, t);
            Gc { ptr: &*ptr }
        }
    }

    // fn advance(&mut self) -> Self {
    //     unimplemented!()
    // }
}

pub(crate) struct HeaderUnTyped {
    // TODO private
    pub evacuated: Mutex<HashMap<u16, *const u8>>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
    roots: usize,
    finalizers: usize,
    condemned: bool,
}

impl HeaderUnTyped {
    #[inline(always)]
    /// The offset of the last T (closest to Header).
    pub(crate) fn low_offset(align: u16) -> u16 {
        let header = mem::size_of::<HeaderUnTyped>() as u16;
        header + ((align - (header % align)) % align)
    }

    #[inline(always)]
    // TODO is this right?
    /// The offset of the first T in the Arena.
    pub(crate) fn high_offset(align: u16, size: u16) -> u16 {
        let cap = (ARENA_SIZE as u16 - HeaderUnTyped::low_offset(align)) / size;
        HeaderUnTyped::low_offset(align) + (cap * size)
    }
}

pub struct Header<T> {
    // TODO private
    pub evacuated: Mutex<HashMap<u16, *const T>>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
    roots: usize,
    finalizers: usize,
    pub condemned: bool,
}

impl<T> Header<T> {
    #[inline(always)]
    pub fn from(ptr: *const T) -> *const Header<T> {
        let offset = ptr as usize % ARENA_SIZE;
        (ptr as usize - offset) as *const _
    }

    #[inline(always)]
    fn low_offset() -> u16 {
        HeaderUnTyped::low_offset(mem::align_of::<T>() as u16)
    }
}

pub(crate) type Bus = Mutex<[Msg; 8]>;

thread_local! {
    /// Map from type to GC communication bus.
    /// `Arena.next` from an `Msg::End` with excise capacity.
    /// Cached invariant from last `Msg::Gc`.
    static GC_BUS: UnsafeCell<
        HashMap<(GcTypeInfo, usize), Box<((Option<*mut u8>, GcInvariant), Bus)>>,
    > = UnsafeCell::new(HashMap::new());

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
