#![feature(optin_builtin_traits)]
#![feature(untagged_unions)]
#![feature(specialization)]
#![feature(negative_impls)]
#![allow(incomplete_features)]
#![feature(const_generics)]
#![feature(const_fn)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(const_mut_refs)]
#![feature(const_panic)]
#![feature(const_type_name)]
#![feature(trivial_bounds)]
#![feature(bindings_after_at)]
#![feature(map_first_last)]
#![feature(backtrace)]
#![feature(arbitrary_self_types)]
#![feature(generic_associated_types)]
#![feature(associated_type_bounds)]
#![feature(marker_trait_attr)]
#![feature(dropck_eyepatch)]
#![feature(const_fn_fn_ptr_basics)]
#![feature(const_fn_transmute)]

pub mod arena;
pub use arena::{Alloc, Arena};
pub mod auto_traits;
pub use auto_traits::{Immutable, NoGc};
pub mod collections;
pub mod gc;
pub use gc::{Gc, Root};
mod gc_logic;
pub mod mark;
pub use mark::{Mark, Trace};
pub mod life;
pub use life::*;
// pub mod thunk;
// pub use thunk::Thunk;
