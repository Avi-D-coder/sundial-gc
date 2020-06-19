use super::gc::*;
use crate::{auto_traits::HasGc, trace::GcTypeInfo};
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
    fn evacuate(
        s: &Self,
        grey_feilds: u8,
        region: Range<usize>,
        on_condemed: *const fn(*const u8, GcTypeInfo),
    );

    const PRE_CONDTION: bool;
}

unsafe impl<T> Condemned for T {
    default fn feilds(_: &Self, _: u8, _: Range<usize>) -> u8 {
        // This check is superfluous
        // TODO ensure if gets optimized out
        if !T::HAS_GC {
            0b0000_0000
        } else {
            panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>")
        }
    }

    default fn evacuate(_: &Self, _: u8, _: Range<usize>, _: *const fn(*const u8, GcTypeInfo)) {}

    default const PRE_CONDTION: bool = if T::HAS_GC {
        // TODO When fmt is allowed in const s/your type/type_name::<T>()
        panic!("You need to derive Condemned for your type. Required due to a direct Gc<T>");
    } else {
        true
    };
}
