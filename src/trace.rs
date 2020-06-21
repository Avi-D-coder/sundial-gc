use super::auto_traits::*;

use std::collections::{HashMap, HashSet};
use std::mem::*;
use std::ops::Deref;

pub unsafe trait Trace {
    fn trace(t: usize);
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8];
    const TRACE_DIRECT_FIELDS: u8;
    fn trace_transitive_type_info(tti: *mut Tti);
}

// Ideally would use negative impls
// Blanket impl seems to be safe, since Mark's blanket requires NoGc
// If you only implement Mark and not Trace CHILD_TYPE_INFO will all be const None
unsafe impl<T: Immutable> Trace for T {
    default fn trace(_: usize) {}
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    default const TRACE_DIRECT_FIELDS: u8 = 0;
    default fn trace_transitive_type_info(_: *mut Tti) {
        eprintln!("Trace T::HAS_GC: {}", T::HAS_GC);
        if T::HAS_GC {
            panic!("You need to derive Trace")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GcTypeInfo {
    pub(crate) trace_ptr: fn(usize),
    pub(crate) tti_ptr: fn(*mut Tti),
    pub(crate) child_gc_info: *const [Option<GcTypeInfo>; 8],
    pub(crate) needs_drop: bool,
    pub(crate) is_gc: bool,
    pub(crate) byte_size: u16,
    pub(crate) alignment: u16,
}

impl GcTypeInfo {
    pub const fn new<T: Trace>() -> Self {
        Self {
            trace_ptr: T::trace,
            tti_ptr: T::trace_transitive_type_info,
            child_gc_info: &T::TRACE_CHILD_TYPE_INFO as *const _,
            needs_drop: needs_drop::<T>(),
            is_gc: T::IS_GC,
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
    pub type_info: HashSet<GcTypeInfo>,
}

impl Tti {
    pub fn new() -> Self {
        Self {
            parrents: HashSet::new(),
            type_info: HashSet::new(),
        }
    }

    /// Gc is the only type that adds it's self.
    pub fn add_gc<T: Trace>(&mut self) {
        self.type_info.insert(GcTypeInfo::new::<T>());
    }

    pub fn add_trans(&mut self, tti: fn(*mut Tti)) {
        if self.parrents.insert(tti as *const fn(&mut Tti) as usize) {
            tti(self)
        }
    }
}

// TODO test direct_gced

trait IsGc {
    const IS_GC: bool;
}

impl<T> IsGc for T {
    default const IS_GC: bool = false;
}

impl<'r, T> IsGc for super::Gc<'r, T> {
    const IS_GC: bool = false;
}

/// # std impls

unsafe impl<'r, T: Immutable + Trace> Trace for Option<T> {
    default fn trace(o: usize) {
        if let Some(t) = unsafe { &*(o as *const Self) } {
            T::trace(t as *const T as usize)
        }
    }
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = GcTypeInfo::one_child::<T>();
    default const TRACE_DIRECT_FIELDS: u8 = T::TRACE_DIRECT_FIELDS;
    default fn trace_transitive_type_info(tti: *mut Tti) {
        let tti = unsafe { &mut *tti };
        tti.add_trans(T::trace_transitive_type_info);
    }
}

unsafe impl<'r, T: Immutable + Trace + NoGc> Trace for Option<T> {
    default fn trace(_: usize) {}
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    default const TRACE_DIRECT_FIELDS: u8 = 0;
    default fn trace_transitive_type_info(_: *mut Tti) {}
}

unsafe impl<T: Immutable + Trace> Trace for Box<T> {
    default fn trace(b: usize) {
        let t = unsafe { &*(b as *const Self) }.deref();
        T::trace(t as *const T as usize)
    }
    default const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    default const TRACE_DIRECT_FIELDS: u8 = T::TRACE_DIRECT_FIELDS;
    default fn trace_transitive_type_info(tti: *mut Tti) {
        let tti = unsafe { &mut *tti };
        tti.add_trans(T::trace_transitive_type_info);
    }
}

unsafe impl<T: Immutable + Trace + NoGc> Trace for Box<T> {
    fn trace(_: usize) {}
    const TRACE_CHILD_TYPE_INFO: [Option<GcTypeInfo>; 8] = [None; 8];
    const TRACE_DIRECT_FIELDS: u8 = 0;
    fn trace_transitive_type_info(_: *mut Tti) {}
}
