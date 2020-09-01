use self::sundial_gc::{
    mark::{Life, ID},
    *,
};
use crate as sundial_gc;
use std::{fmt::Debug, iter, ops::Index};
use sundial_gc_derive::*;

#[derive(Trace, Ord, PartialOrd, Eq, PartialEq)]
struct List<'r, T>(Option<Gc<'r, Elem<'r, T>>>)
where
    T: Life;

#[derive(Trace, Ord, PartialOrd, Eq, PartialEq)]
struct Elem<'r, T>
where T: Life,
{
    next: List<'r, T>,
    value: T,
}

impl<'r, T: Debug + Life> Debug for Elem<'r, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(iter::once(&self.value).chain(self.next.iter()))
            .finish()
    }
}

impl<'r, T: Debug + Life> Debug for List<'r, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(e) => e.fmt(f),
            None => f.write_str("[]"),
        }
    }
}

impl<'r, T: Life> From<Gc<'r, Elem<'r, T>>> for List<'r, T> {
    fn from(e: Gc<'r, Elem<'r, T>>) -> Self {
        List(Some(e))
    }
}

impl<'r, T: Life+ Copy> Copy for Elem<'r, T> {}
impl<'r, T: Life> Clone for Elem<'r, T> {
    fn clone(&self) -> Self {
        *self
    }
}


impl<'r, T: Life +  Copy> Copy for List<'r, T> {}
impl<'r, T: Life> Clone for List<'r, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'r, T: Life> Default for List<'r, T> {
    fn default() -> Self {
        List(None)
    }
}

impl<'r, T: Life> Index<usize> for List<'r, T> {
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

impl<'r, T: Life> List<'r, T> {
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

/// Inserts an element at position `index`.
/// This is equivalent `Vec::insert` not Haskell's `insert :: Ord a => a -> [a] -> [a]`.
///
/// # Panics
///
/// Panics if `index > len`.
/// This function is recursive and may cause a stack overflow.
///
/// TODO Replace with non recursive variant.
// pub fn insert<'l, 'r, 'a: 'r, T: >(
//     list: List<'l, T>,
//     index: usize,
//     arena: &'a Arena<<Elem<T> as Life>::L<'a>>,
// ) -> <List<'r, T> as Life>::L<'r>
// where
//     // T::L<'static>: GC + Clone + Sized + ID<<T as Life>::L<'static>>,
//     // T::L<'a>: GC + Sized,
//     // T::L<'r>: GC + Sized,
// {
//     let Gc(Elem { value, next }, _) = list.0.unwrap();
//     let next: List<'r, <T as Life>::L<'r>> = next.insert(index - 1, arena);
//     let next = next.insert(index - 1, arena);

//     List::from(arena.gc(Elem {
//         value: value.clone(),
//         next,
//     }))
// }

#[derive(Trace, Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
pub struct Iter<'r, T> where T: Life
{
    cursor: List<'r, T>,
}

impl<'r, T: Life> Iterator for Iter<'r, T> {
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
