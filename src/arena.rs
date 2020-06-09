use super::auto_traits::*;
use super::gc::*;
use super::trace::*;
use super::Mark;

use std::alloc::{GlobalAlloc, Layout, System};
use std::any::type_name;
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::mem::{align_of, size_of, transmute};
use std::ops::Range;
use std::ptr;
use std::sync::Mutex;
use std::thread_local;

pub struct ArenaPrim<T: Trace> {
    intern: ArenaInternals<T>,
}

/// An arena allocator for allocating GCed objects containing GCed fields.
pub struct ArenaGc<T: Trace> {
    pub intern: ArenaInternals<T>,
}

pub struct ArenaInternals<T: Trace> {
    // TODO compact representation of arenas
    // TODO make all these private by wrapping up needed functionality.
    pub header: *const Header<T>,
    pub grey_self: bool,
    pub grey_feilds: UnsafeCell<u8>,
    pub white_region: Range<usize>,
    pub next: UnsafeCell<*mut T>,
}

#[repr(align(16384))]
struct Mem {
    _mem: [u8; 16384],
}

pub trait Arena<T> {
    fn new() -> Self;
    fn advance(&mut self) -> Self;
    fn gc_alloc<'a, 'r: 'a>(&'a self, _t: T) -> Gc<'r, T>;
}

impl<T: NoGc + Trace> Arena<T> for ArenaPrim<T> {
    fn new() -> Self {
        Self {
            intern: Arena::new(),
        }
    }
    fn gc_alloc<'a, 'r: 'a>(&'a self, t: T) -> Gc<'r, T> {
        self.intern.gc_alloc(t)
    }

    fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

impl<T: Trace> Arena<T> for ArenaGc<T> {
    fn new() -> Self {
        Self {
            intern: Arena::new(),
        }
    }
    fn gc_alloc<'a, 'r: 'a>(&'a self, t: T) -> Gc<'r, T> {
        self.intern.gc_alloc(t)
    }

    fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

impl<T: Trace> Drop for ArenaInternals<T> {
    fn drop(&mut self) {
        GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            tm.entry(key::<T>()).and_modify(|bus| {
                let mut msg = bus.lock().unwrap();
                msg.from_gc = false;
                msg.worker_read = false;
                msg.grey_feilds = unsafe { *self.grey_feilds.get() };
            });
        });
    }
}

unsafe impl<'o, 'n, 'r: 'n, O: NoGc + Immutable, N: NoGc + Immutable + 'r> Mark<'o, 'n, 'r, O, N>
    for ArenaPrim<N>
{
    #[inline(always)]
    default fn mark(&'n self, o: Gc<'o, O>) -> Gc<'r, N> {
        // TODO make const https://github.com/rust-lang/rfcs/pull/2632
        assert_eq!(type_name::<O>(), type_name::<N>());
        if self.intern.grey_self
            && self
                .intern
                .white_region
                .contains(&(&*o as *const _ as usize))
        {
            let next = self.intern.next.get();
            unsafe { ptr::copy(transmute(o), *next, 1) };
            let mut new_gc = next as *const N;
            let old_addr = &*o as *const O as usize;
            let offset = old_addr % 16384;
            let old_header = unsafe { &*((old_addr - offset) as *const Header<N>) };
            let evacuated = old_header.evacuated.lock();
            evacuated
                .unwrap()
                .entry((offset / size_of::<N>()) as u16)
                .and_modify(|gc| new_gc = *gc)
                .or_insert_with(|| {
                    unsafe { *next = ((*next as usize) - size_of::<N>()) as *mut N };
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

const fn header_size<T>() -> usize {
    let align = align_of::<T>();
    let header = size_of::<Header<T>>();
    header + ((align - (header % align)) % align)
}

impl<T: Trace> Arena<T> for ArenaInternals<T> {
    fn new() -> ArenaInternals<T> {
        let bus = GC_BUS.with(|tm| {
            let tm = unsafe { &mut *tm.get() };
            let mem_ptr = unsafe { System.alloc_zeroed(Layout::new::<Mem>()) };
            let mem_addr = mem_ptr as usize;
            debug_assert!(
                mem_addr
                    < unsafe { &std::mem::transmute::<_, &Mem>(mem_ptr)._mem[16383] } as *const _
                        as usize
            );
            tm.entry(key::<T>()).or_insert_with(|| {
                Box::new(Mutex::new(BusMsg {
                    from_gc: true,
                    worker_read: false,
                    mem_addr,
                    grey_self: false,
                    grey_feilds: 0b0000_0000,
                    white_region: 0..1,
                }))
            })
        });

        let mut msg = bus.lock().unwrap();
        if msg.from_gc && !msg.worker_read {
            msg.worker_read = true;
            // TODO const assert layout details
            let capacity = (16384 - header_size::<T>()) / size_of::<T>();
            ArenaInternals {
                header: msg.mem_addr as *const Header<T>,
                next: UnsafeCell::new((capacity * size_of::<T>()) as *mut T),
                grey_self: msg.grey_self,
                grey_feilds: UnsafeCell::new(msg.grey_feilds),
                white_region: msg.white_region.clone(),
            }
        } else {
            todo!()
        }
    }

    fn gc_alloc<'a, 'r: 'a>(&'a self, t: T) -> Gc<'r, T> {
        unsafe {
            let ptr = *self.next.get();
            let next = self.next.get();
            *next = (ptr as usize - size_of::<T>()) as *mut _;
            let ptr = &mut *ptr;
            *ptr = t;
            Gc { ptr }
        }
    }

    fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

pub struct Header<T> {
    // TODO private
    pub evacuated: Mutex<HashMap<u16, *const T>>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
    roots: usize,
    finalizers: usize,
}

thread_local! {
    /// Map from type to GC communication bus.
    static GC_BUS: UnsafeCell<HashMap<(GcTypeInfo, usize), Box<Mutex<BusMsg>>>> = UnsafeCell::new(HashMap::new());
}

fn key<T: Trace>() -> (GcTypeInfo, usize) {
    (
        T::TRACE_TYPE_INFO,
        ptr::drop_in_place::<T> as *const fn(*mut T) as usize,
    )
}

// TODO replace with atomic 64 byte header encoding.
// Plus variable length condemned region messages.
// TODO allow multiple messages and therefore Arenas of T.
pub struct BusMsg {
    pub from_gc: bool,
    pub worker_read: bool,
    pub mem_addr: usize,
    /// Is a Self Arena being condemned.
    grey_self: bool,
    /// The 8 bits correspond to GCed fields.
    /// Only 8 GCed fields per struct are supported.
    /// TODO do enums with GCed fields count as one (conservative), or N fields.
    /// TODO unpack bits from composite types tuples, inline structs, enums.
    pub grey_feilds: u8,
    /// TODO support multiple field specific regions in future encoding
    pub white_region: Range<usize>,
}
