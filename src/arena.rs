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

pub struct ArenaPrim<T> {
    intern: ArenaInternals<T>,
}

/// An arena allocator for allocating GCed objects containing GCed fields.
pub struct ArenaGc<T> {
    pub intern: ArenaInternals<T>,
}

pub struct ArenaInternals<T> {
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
        Self { intern: new() }
    }
    fn gc_alloc<'a, 'r: 'a>(&'a self, _t: T) -> Gc<'r, T> {
        unimplemented!()
    }

    fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

impl<T> Drop for ArenaInternals<T> {
    fn drop(&mut self) {}
}

unsafe impl<'n, O: NoGc + Immutable, N: NoGc + Immutable> Mark<'n, O, N> for ArenaPrim<N> {
    #[inline(always)]
    default unsafe fn ptr(a: *const Self, o: *const O) -> *const N {
        // TODO make const https://github.com/rust-lang/rfcs/pull/2632
        assert_eq!(type_name::<O>(), type_name::<N>());
        let a = unsafe { &*a };
        if a.intern.grey_self && a.intern.white_region.contains(&(o as *const _ as usize)) {
            let next = a.intern.next.get();
            unsafe { ptr::copy(transmute(o), *next, 1) };
            let mut new_gc = next as *const N;
            let old_addr = o as *const O as usize;
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
            new_gc
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

fn new<T: Trace>() -> ArenaInternals<T> {
    let bus = GC_BUS.with(|tm| {
        let tm = unsafe { &mut *tm.get() };
        let key = (
            T::TRACE_TYPE_INFO,
            ptr::drop_in_place::<T> as *const fn(*mut T) as usize,
        );
        let mem_ptr = unsafe { System.alloc_zeroed(Layout::new::<Mem>()) as usize };
        tm.entry(key).or_insert_with(|| {
            Box::new(Mutex::new(BusMsg {
                from_gc: true,
                worker_read: false,
                mem_ptr,
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
        // FIXME attempt to subtract with overflow
        let capacity = (16384 - (msg.mem_ptr + header_size::<T>())) / size_of::<T>();
        ArenaInternals {
            header: msg.mem_ptr as *const Header<T>,
            next: UnsafeCell::new((capacity * size_of::<T>()) as *mut T),
            grey_self: msg.grey_self,
            grey_feilds: UnsafeCell::new(msg.grey_feilds),
            white_region: msg.white_region.clone(),
        }
    } else {
        todo!()
    }
}

impl<T: Trace> Arena<T> for ArenaGc<T> {
    fn new() -> Self {
        Self { intern: new() }
    }
    fn gc_alloc<'a, 'r: 'a>(&'a self, _t: T) -> Gc<'r, T> {
        todo!()
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

// TODO replace with atomic 64 byte header encoding.
// Plus variable length condemned region messages.
// TODO allow multiple messages and therefore Arenas of T.
pub struct BusMsg {
    pub from_gc: bool,
    pub worker_read: bool,
    pub mem_ptr: usize,
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
