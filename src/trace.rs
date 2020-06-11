use super::auto_traits::*;

use std::collections::HashSet;
use std::mem::*;
use std::ops::Deref;

pub unsafe trait Trace {
    fn trace(t: usize);
    const TRACE_TYPE_INFO: GcTypeInfo;
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8];
    fn trace_transitive_type_info(tti: *mut Tti);
}

// Ideally would use negative impls
// Blanket impl seems to be safe, since Mark's blanket requires NoGc
// If you only implement Mark and not Trace CHILD_TYPE_INFO will all be const None
unsafe impl<T: Immutable> Trace for T {
    default fn trace(_: usize) {}
    default const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    default fn trace_transitive_type_info(_: *mut Tti) {}
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcTypeInfo {
    trace_ptr: fn(usize),
    tti_ptr: fn(*mut Tti),
    needs_drop: bool,
    byte_size: u16,
    alignment: u16,
}

impl GcTypeInfo {
    pub const fn new<T: Trace>() -> Self {
        Self {
            trace_ptr: T::trace,
            tti_ptr: T::trace_transitive_type_info,
            needs_drop: needs_drop::<T>(),
            byte_size: size_of::<T>() as u16,
            alignment: align_of::<T>() as u16,
        }
    }

    pub const fn one_child<T: Trace>() -> [Option<GcTypeInfo>; 8] {
        [
            Some(GcTypeInfo::new::<T>()),
            None,
            None,
            None,
            None,
            None,
            None,
            None,
        ]
    }
}

pub struct Tti {
    /// Holds fn ptr of trace_transitive_type_info calls
    parrents: HashSet<usize>,
    type_info: HashSet<GcTypeInfo>,
}

impl Tti {
    pub fn new() -> Self {
        Self {
            parrents: HashSet::new(),
            type_info: HashSet::new(),
        }
    }

    pub fn add_direct<T: Trace>(&mut self) {
        self.type_info
            .extend(T::TRACE_CHILD_TYPE_INFO.iter().filter_map(|o| *o));
    }

    pub fn add_trans(&mut self, tti: fn(*mut Tti)) {
        if self.parrents.insert(tti as *const fn(&mut Tti) as usize) {
            tti(self)
        }
    }
}

/// # std impls

unsafe impl<'r, T: Immutable + Trace> Trace for Option<T> {
    default fn trace(o: usize) {
        if let Some(t) = unsafe { &*(o as *const Self) } {
            T::trace(t as *const T as usize)
        }
    }
    default const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = GcTypeInfo::one_child::<T>();
    default fn trace_transitive_type_info(tti: *mut Tti) {
        let tti = unsafe { &mut *tti };
        tti.add_direct::<Self>();
        tti.add_trans(T::trace_transitive_type_info);
    }
}

unsafe impl<'r, T: Immutable + Trace + NoGc> Trace for Option<T> {
    fn trace(_: usize) {}
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    fn trace_transitive_type_info(_: *mut Tti) {}
}

unsafe impl<T: Immutable + Trace> Trace for Box<T> {
    default fn trace(b: usize) {
        let t = unsafe { &*(b as *const Self) }.deref();
        T::trace(t as *const T as usize)
    }
    default const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    default fn trace_transitive_type_info(_: *mut Tti) {}
}

unsafe impl<T: Immutable + Trace + NoGc> Trace for Box<T> {
    fn trace(_: usize) {}
    const TRACE_TYPE_INFO: GcTypeInfo = GcTypeInfo::new::<Self>();
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = GcTypeInfo::one_child::<T>();
    fn trace_transitive_type_info(tti: *mut Tti) {
        let tti = unsafe { &mut *tti };
        tti.add_direct::<Self>();
        tti.add_trans(T::trace_transitive_type_info);
    }
}
