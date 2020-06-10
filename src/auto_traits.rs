use super::gc::*;
use std::cell::UnsafeCell;

pub unsafe auto trait NoGc {}
impl<'r, T> !NoGc for Gc<'r, T> {}
unsafe impl<'r, T: NoGc> NoGc for Box<T> {}

/// Shallow immutability
pub unsafe auto trait Immutable {}
impl<T> !Immutable for &mut T {}
impl<'r, T> !Immutable for &'r T {}
impl<T> !Immutable for UnsafeCell<T> {}
unsafe impl<T> Immutable for Box<T> {}
unsafe impl<'r, T> Immutable for Gc<'r, T> {}
