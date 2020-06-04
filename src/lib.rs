#![allow(incomplete_features)]
#![feature(optin_builtin_traits)]
#![feature(const_generics)]
#![feature(untagged_unions)]
#![feature(const_fn)]
#![feature(const_transmute)]
#![feature(specialization)]
#![feature(negative_impls)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(const_mut_refs)]
#![feature(trivial_bounds)]

pub mod arena;
pub mod auto_traits;
pub mod gc;
pub mod mark;
pub mod trace;

use gc::*;
use mark::*;
use trace::*;

use std::collections::HashMap;
use std::sync::Mutex;
use std::thread_local;

thread_local! {
    /// Map from type to GC communication bus.
    static GC_BUS: HashMap<GcTypeInfo, Box<Mutex<BusMsg>>> = HashMap::new();
}

// TODO replace with atomic 64 byte encoding.
struct BusMsg {
    from_gc: bool,
    high_ptr: usize,
    capacity: u16,
    /// The first 8 bits correspond to GCed fields.
    /// The last 8 bits are reserved.
    ///
    /// Only 8 GCed fields per struct are supported
    /// TODO do enums with GCed fields count as one (conservative), or N fields.
    grey_feilds: u16,
}
