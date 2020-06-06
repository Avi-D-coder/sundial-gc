use super::Gc;
use std::ops::Range;

pub unsafe trait Mark<'o, 'n, O, N> {
    fn mark(&'n self, o: Gc<'o, O>) -> Gc<'n, N>;
}

// GAT Mark
pub unsafe trait Mark2<'o, 'n, O: 'o> {
    type Old: 'o;
    type New: 'n;
    fn mark(&'n self, o: Gc<'o, Self::Old>) -> Gc<'n, Self::New>;
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
