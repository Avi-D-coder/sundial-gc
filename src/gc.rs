use std::ops::{Deref, DerefMut};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T>(pub &'r T, pub P);

impl<'r, T> Gc<'r, T> {
    #[inline(always)]
    pub(crate) fn new(t: &'r T) -> Self {
        Gc(t, P(()))
    }
}

/// Just here to prevent construction of `Gc` & `Box`.
/// Use `_` to pattern match against `Gc` & `Box`.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct P(());

impl<'r, T> Copy for Gc<'r, T> {}

impl<'r, T> Clone for Gc<'r, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'r, T> Deref for Gc<'r, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
    }
}

impl<'r, T> From<Box<'r, T>> for Gc<'r, T> {
    fn from(t: Box<'r, T>) -> Self {
        Gc::new(t.0)
    }
}

/// A temporarily owned mutable value allocated in an `Arena`.
/// Mutability is only available while the Arena it was allocated into is live.
/// Can be turned into a immutable `Gc<T>`, in order to extend it's lifetime.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Box<'r, T>(pub &'r mut T, pub P);

impl<'r, T> Box<'r, T> {
    #[inline(always)]
    pub(crate) fn new(t: &'r mut T) -> Self {
        Box(t, P(()))
    }
}

impl<'r, T> Deref for Box<'r, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.0
    }
}

impl<'r, T> DerefMut for Box<'r, T> {
    fn deref_mut(&mut self) -> &mut T {
        self.0
    }
}
