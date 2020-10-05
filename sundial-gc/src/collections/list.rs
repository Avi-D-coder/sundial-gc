use crate::*;
use crate as sundial_gc;
use std::{fmt::Debug, iter, ops::Index};
use sundial_gc_derive::*;

#[derive(Trace, Ord, PartialOrd, Eq, PartialEq)]
struct List<'r, T>(Option<Gc<'r, Elem<'r, T>>>)
where
    T: 'r;

#[derive(Trace, Clone, Ord, PartialOrd, Eq, PartialEq)]
struct Elem<'r, T>
where
    T: 'r,
{
    next: List<'r, T>,
    value: T,
}

impl<'r, T: Copy> Copy for Elem<'r, T> {}
impl<'r, T> Copy for List<'r, T> {}
impl<'r, T> Clone for List<'r, T> {
    fn clone(&self) -> Self {
        *self
    }
}

// unsafe impl<'r, T> sundial_gc::Trace for List<'r, T>
// where
//     T: GC,
// {
//     default fn fields(
//         s: &Self,
//         offset: u8,
//         grey_fields: u8,
//         invariant: &sundial_gc::mark::Invariant,
//     ) -> u8 {
//         let mut bloom = 0b0000000;
//         let Self(f0) = s;
//         bloom |= Trace::fields(f0, offset, grey_fields, invariant);
//         bloom
//     }
//     default unsafe fn evacuate<'e>(
//         s: &Self,
//         offset: sundial_gc::mark::Offset,
//         grey_fields: u8,
//         invariant: &sundial_gc::mark::Invariant,
//         handlers: &mut sundial_gc::mark::Handlers,
//     ) {
//         let Self(f0) = s;
//         Trace::evacuate(f0, offset, grey_fields, invariant, handlers);
//     }
//     default fn direct_gc_types(
//         t: &mut std::collections::HashMap<sundial_gc::mark::GcTypeInfo, sundial_gc::mark::TypeRow>,
//         offset: u8,
//     ) {
//         <Option<Gc<'r, Elem<'r, T::Static>>>>::direct_gc_types(t, offset);
//     }
//     default fn transitive_gc_types(tti: *mut sundial_gc::mark::Tti) {
//         <Option<Gc<'r, Elem<'r, T::Static>>>>::transitive_gc_types(tti);
//     }
//     default const GC_COUNT: u8 = <Option<Gc<'r, Elem<'r, T::Static>>>>::GC_COUNT;
// }
// unsafe impl<'r, T> sundial_gc::life::AsStatic for List<'r, T>
// where
//     T: sundial_gc::life::AsStatic,
// {
//     type Static = List<'static, T::Static>;
// }

// unsafe impl<'r, T: GC> sundial_gc::Trace for Elem<'r, T> {
//     default fn fields(
//         s: &Self,
//         offset: u8,
//         grey_fields: u8,
//         invariant: &sundial_gc::mark::Invariant,
//     ) -> u8 {
//         let mut bloom = 0b0000000;
//         let Self { next, value } = s;
//         bloom |= Trace::fields(next, offset, grey_fields, invariant);
//         bloom |= Trace::fields(
//             value,
//             offset + <List<'r, T>>::GC_COUNT,
//             grey_fields,
//             invariant,
//         );
//         bloom
//     }
//     default unsafe fn evacuate<'e>(
//         s: &Self,
//         offset: sundial_gc::mark::Offset,
//         grey_fields: u8,
//         invariant: &sundial_gc::mark::Invariant,
//         handlers: &mut sundial_gc::mark::Handlers,
//     ) {
//         let Self { next, value } = s;
//         Trace::evacuate(next, offset, grey_fields, invariant, handlers);
//         Trace::evacuate(
//             value,
//             offset + <List<'r, T>>::GC_COUNT,
//             grey_fields,
//             invariant,
//             handlers,
//         );
//     }
//     default fn direct_gc_types(
//         t: &mut std::collections::HashMap<sundial_gc::mark::GcTypeInfo, sundial_gc::mark::TypeRow>,
//         offset: u8,
//     ) {
//         <List<'r, T::Static>>::direct_gc_types(t, offset);
//         <T>::direct_gc_types(t, offset + <List<'r, T::Static>>::GC_COUNT);
//     }
//     default fn transitive_gc_types(tti: *mut sundial_gc::mark::Tti) {
//         <List<'r, T::Static>>::transitive_gc_types(tti);
//         <T::Static>::transitive_gc_types(tti);
//     }
//     default const GC_COUNT: u8 = <List<'r, T::Static>>::GC_COUNT + <T::Static>::GC_COUNT;
// }
// unsafe impl<'r, T> sundial_gc::life::AsStatic for Elem<'r, T>
// where
//     T: sundial_gc::life::GC,
// {
//     type Static = Elem<'static, T::Static>;
// }

impl<'r, T: Debug> Debug for Elem<'r, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
        // f.debug_list()
        //     .entries(iter::once(&self.value).chain(self.next.iter()))
        //     .finish()
    }
}

impl<'r, T: Debug> Debug for List<'r, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(e) => e.fmt(f),
            None => f.write_str("[]"),
        }
    }
}

impl<'r, T: GC> From<Gc<'r, Elem<'r, T>>> for List<'r, T> {
    fn from(e: Gc<'r, Elem<'r, T>>) -> Self {
        List(Some(e))
    }
}

impl<'r, T: GC> Default for List<'r, T> {
    fn default() -> Self {
        List(None)
    }
}

impl<'r, T: GC> Index<usize> for List<'r, T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        self.iter().nth(index).unwrap()
    }
}

// TODO not sure if this is worth it.
// impl<'r, T: GC + Clone> Add<List<'r, T>> for gc::Box<'r, Elem<'r, T>> {
//     type Output = List<'r, T>;
//     fn add(self, rhs: List<'r, T>) -> Self::Output {
//         match self.next.0 {
//             None => {self.next = rhs; List::from(Gc::from(self))}
//             Some(_) => {
//                 let arena = Arena::new();
//             }
//         }
//     }
// }

impl<'r, T: GC> List<'r, T> {
    pub fn iter(self) -> Iter<'r, T> {
        Iter { cursor: self }
    }
}

// impl<'r, T: GC<T> + Clone> List<'r, T> {
//     /// Prepend `value` to a list.
//     /// The arguments are in reverse order.
//     pub fn cons<'a: 'r>(self, value: impl GC<T>, arena: &'a Arena<Elem<T>>) -> List<'r, T::L<'r>> {
//         List::from(arena.gc(Elem { value, next: self }))
//     }
// }

impl<'r, T: 'r + GC + Clone> List<'r, T> {
    /// Inserts an element at position `index`.
    /// This is equivalent `Vec::insert` not Haskell's `insert :: Ord a => a -> [a] -> [a]`.
    ///
    /// # Panics
    ///
    /// Panics if `index > len`.
    /// This function is recursive and may cause a stack overflow.
    ///
    /// TODO Replace with non recursive variant.
    pub fn insert<'a: 'r>(
        self,
        index: usize,
        t: T,
        arena: &'a Arena<Elem<T>>,
    ) -> List<'r, T> {
        let Gc(Elem { next, value }, _) = self.0.unwrap();
        let value: T = value.clone();

        let next: List<'r, T> = next.insert(index - 1, t, arena);

        List(Some(arena.gc(Elem { next, value })))
    }

    pub fn insert_mark<'n, 'a: 'n, N>(
        self,
        index: usize,
        t: T,
        arena: &'a Arena<Elem<T>>,
    ) -> List<'n, N> {
        let Gc(Elem { next, value }, _) = self.0.unwrap();
        let value: T = value.clone();

        let next = next.insert(index - 1, t, arena);

        // List(Some(arena.gc(Elem { next, value })))
        todo!()
    }
}

#[derive(Trace, Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Iter<'r, T>
where
    T: 'r,
{
    cursor: List<'r, T>,
}

impl<'r, T: GC> Iterator for Iter<'r, T> {
    type Item = &'r T;
    fn next(&mut self) -> Option<Self::Item> {
        match self.cursor.0 {
            Some(Gc(Elem { next, value }, _)) => {
                self.cursor = *next;
                Some(value)
            }
            None => None,
        }
    }
}

#[test]
fn size_of_list() {
    use std::mem::size_of;
    assert_eq!(size_of::<usize>(), size_of::<List<usize>>());
}
