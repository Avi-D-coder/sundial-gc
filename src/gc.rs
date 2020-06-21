use std::ops::Deref;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T> {
    // TODO pub(crate)
    pub ptr: &'r T,
}

impl<'r, T> Copy for Gc<'r, T> {}

impl<'r, T> Clone for Gc<'r, T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<'r, T> Deref for Gc<'r, T> {
    type Target = T;
    fn deref(&self) -> &T {
        self.ptr
    }
}

impl<'r, T> From<&'static T> for Gc<'r, T> {
    fn from(t: &'static T) -> Self {
        Gc { ptr: &*t }
    }
}
