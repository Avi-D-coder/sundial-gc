use super::auto_traits::*;
use super::gc::*;
use super::trace::*;
use super::Mark;

use std::alloc::{GlobalAlloc, Layout, System};
use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::ops::Range;
use std::ptr;
use std::sync::Mutex;
use std::thread_local;

pub struct ArenaPrim<T> {
    intern: ArenaInternals<T>,
}

/// An arena allocator for allocating GCed objects containing GCed fields.
pub struct ArenaGc<T> {
    intern: ArenaInternals<T>,
}

struct ArenaInternals<T> {
    // TODO compact representation of arenas
    header: *const Header,
    arr_ptr: *const T,
    grey_self: bool,
    grey_feilds: u8,
    white_region: Range<usize>,
    next: UnsafeCell<*mut T>,
}

pub trait Arena<T> {
    fn new() -> Self;
    fn advance(&mut self) -> Self;
    fn gc_alloc<'r>(&'r self, _t: T) -> Gc<'r, T>
    where
        T: 'r;
}

type ArenaLayout<T> = (Header, [T; 1000]);

impl<T: NoGc + Trace> Arena<T> for ArenaPrim<T> {
    fn new() -> Self {
        Self { intern: new() }
    }
    fn gc_alloc<'r>(&'r self, _t: T) -> Gc<'r, T>
    where
        T: 'r,
    {
        unimplemented!()
    }

    fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

unsafe impl<'o, 'n, T: NoGc + Immutable> Mark<'o, 'n, T, T> for ArenaPrim<T> {
    #[inline(always)]
    default fn mark(&'n self, o: Gc<'o, T>) -> Gc<'n, T> {
        if self.intern.grey_self
            && self
                .intern
                .white_region
                .contains(&(o.ptr as *const _ as usize))
        {}
        unsafe { std::mem::transmute(o) }
    }
}

fn new<T: Trace>() -> ArenaInternals<T> {
    let bus = GC_BUS.with(|tm| {
        let tm = unsafe { &mut *tm.get() };
        let key = (
            T::TRACE_TYPE_INFO,
            ptr::drop_in_place::<T> as *const fn(*mut T) as usize,
        );
        let mem_ptr = unsafe { System.alloc_zeroed(Layout::new::<ArenaLayout<T>>()) as usize };
        tm.entry(key).or_insert_with(|| {
            Box::new(Mutex::new(BusMsg {
                from_gc: true,
                worker_read: false,
                mem_ptr,
                capacity: 1000,
                grey_self: false,
                grey_feilds: 0b0000_0000,
                white_region: 0..1,
            }))
        })
    });

    let mut msg = bus.lock().unwrap();
    if msg.from_gc && !msg.worker_read {
        msg.worker_read = true;
        ArenaInternals {
            arr_ptr: todo!(),
            header: todo!(),
            next: todo!(),
            grey_self: msg.grey_self,
            grey_feilds: msg.grey_feilds,
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
    fn gc_alloc<'r>(&'r self, _t: T) -> Gc<'r, T>
    where
        T: 'r,
    {
        todo!()
    }

    fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

struct Header {
    evacuated: HashMap<u16, usize>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
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
    pub capacity: u16,
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
