use super::gc::*;
use std::ops::Range;

/// This will be sound once GAT or const Eq &str lands.
/// The former will allow transmuting only lifetimes.
/// The latter will make `assert_eq!(type_name::<O>(), type_name::<N>())` const.
// https://github.com/rust-lang/rfcs/pull/2632.
pub unsafe trait Mark<'o, 'n, 'r: 'n, O: 'o, N: 'r> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'r, N>;
}

// Blanket Arena<T> impl is in src/arena.rs

pub unsafe trait Condemned {
    fn feilds(s: &Self, grey_feilds: u8, region: Range<usize>) -> u8;
}

unsafe impl<T> Condemned for T {
    default fn feilds(_: &Self, _: u8, _: Range<usize>) -> u8 {
        0b0000_0000
    }
}
