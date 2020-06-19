use crate::trace::*;

use crate::mark::Condemned;
use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T> {
    // TODO pub(crate)
    pub ptr: &'r T,
}

impl<'r, T> Copy for Gc<'r, T> {}

impl<'r, T> Clone for Gc<'r, T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<'r, T> Deref for Gc<'r, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.ptr
    }
}

impl<'r, T> From<&'static T> for Gc<'r, T> {
    fn from(t: &'static T) -> Self {
        Gc { ptr: &*t }
    }
}

unsafe impl<'r, T: 'r + Trace> Trace for Gc<'r, T> {
    fn trace(_: usize) {}
    // A Gc<Gc<T>> is equvlent to Gc<T>
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    const TRACE_DIRECT_FIELDS: u8 = 1;
    fn trace_transitive_type_info(tti: *mut Tti) {
        let tti = unsafe { &mut *tti };
        tti.add_gc::<Self>();
        T::trace_transitive_type_info(tti)
    }
}

unsafe impl<'r, T> Condemned for Gc<'r, T> {
    fn feilds(_: &Self, _: u8, _: std::ops::Range<usize>) -> u8 {
        0b0000_0000
    }
    fn evacuate(_: &Self, _: u8, _: std::ops::Range<usize>, _: *const fn(*const u8, GcTypeInfo)) {}
    const PRE_CONDTION: bool = true;
}
