use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T>(pub &'r T);

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
