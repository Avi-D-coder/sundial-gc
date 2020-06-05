use super::auto_traits::*;
use super::gc::*;
use super::trace::*;
use super::Mark;

use std::ptr;
use std::collections::HashMap;
use std::sync::Mutex;
use std::thread_local;

pub struct Arena<T> {
    // roots: usize,
    high_ptr: *const T,
    capacity: u16,
}

impl<T: NoGc + Trace> Arena<T> {
    pub fn new() -> Self {
        // GC_BUS.with(|type_map| T::TRACE_TYPE_INFOtype_map.entry());
        Self {
            high_ptr: ptr::null(),
            capacity: 1000,
        }
    }
    pub fn gc_alloc<'r>(&'r self, _t: T) -> Gc<'r, T>
    where
        T: 'r,
    {
        unimplemented!()
    }

    pub fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

unsafe impl<'o, 'n, T: NoGc + Immutable> Mark<'o, 'n, T, T> for Arena<T> {
    default fn mark(&'n self, o: Gc<'o, T>) -> Gc<'n, T> {
        unsafe { std::mem::transmute(o) }
    }
}

pub struct ArenaGc<T> {
    // roots: usize,
    high_ptr: *const T,
    capacity: u16,
    grey_types: u8,
}

impl<T: Trace> ArenaGc<T> {
    pub fn new() -> Self {
        // GC_BUS.with(|type_map| T::TRACE_TYPE_INFOtype_map.entry());
        Self {
            high_ptr: ptr::null(),
            capacity: 1000,
            grey_types: 0,
        }
    }
    pub fn gc_alloc<'r>(&'r self, _t: T) -> Gc<'r, T>
    where
        T: 'r,
    {
        unimplemented!()
    }

    pub fn advance(&mut self) -> Self {
        unimplemented!()
    }
}

thread_local! {
    /// Map from type to GC communication bus.
    static GC_BUS: HashMap<GcTypeInfo, Box<Mutex<BusMsg>>> = HashMap::new();
}

// TODO replace with atomic 64 byte encoding.
struct BusMsg {
    from_gc: bool,
    high_ptr: usize,
    capacity: u16,
    /// The 8 bits correspond to GCed fields.
    /// Only 8 GCed fields per struct are supported
    /// TODO do enums with GCed fields count as one (conservative), or N fields.
    /// TODO unpack bits from composite types tuples, inline structs, enums
    grey_feilds: u8,
}

