use std::cell::UnsafeCell;
use super::gc::*;

pub unsafe auto trait NoGc {}
impl<'r, T> !NoGc for Gc<'r, T> {}
unsafe impl<'r, T: NoGc> NoGc for Box<T> {}

/// Shallow immutability
pub unsafe auto trait Immutable {}
impl<T> !Immutable for &mut T {}
impl<T> !Immutable for UnsafeCell<T> {}
unsafe impl<T> Immutable for Box<T> {}