use super::auto_traits::*;
use super::gc::*;
use super::trace::Trace;
use super::Mark;
use std::ptr;

pub struct Arena<T> {
    // roots: usize,
    high_ptr: *const T,
    capacity: u16,
}

impl<T: Trace> Arena<T> {
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
