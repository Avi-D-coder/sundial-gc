use std::ops::Range;

pub unsafe trait Mark<'n, O, N> {
    unsafe fn ptr(a: *const Self, o: *const O) -> *const N;
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
