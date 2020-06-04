use super::auto_traits::*;
use super::trace::*;

use std::ops::Deref;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T> {
    ptr: &'r T,
}

impl<'r, T: Trace> Deref for Gc<'r, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.ptr
    }
}

unsafe impl<'r, T: 'r + Immutable + Trace> Trace for Gc<'r, T> {
    fn trace(_: usize) {}
    // A Gc<Gc<T>> is equvlent to Gc<T>
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<T>();
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [
        Some(GcTypeInfo::new::<T>()),
        None,
        None,
        None,
        None,
        None,
        None,
        None,
    ];
    fn trace_transitive_type_info(tti: &mut Tti) {
        tti.add_direct::<Self>();
        T::trace_transitive_type_info(tti)
    }
}
