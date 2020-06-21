#![feature(optin_builtin_traits)]
#![feature(untagged_unions)]
#![feature(specialization)]
#![feature(negative_impls)]
#![allow(incomplete_features)]
#![feature(const_generics)]
#![feature(const_fn)]
#![feature(const_transmute)]
#![feature(const_raw_ptr_to_usize_cast)]
#![feature(const_mut_refs)]
#![feature(const_if_match)]
#![feature(const_panic)]
#![feature(const_type_name)]
#![feature(trivial_bounds)]
#![feature(bindings_after_at)]
#![feature(map_first_last)]

pub mod arena;
pub mod auto_traits;
pub mod gc;
mod gc_logic;
pub mod mark;
