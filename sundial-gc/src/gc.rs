use crate::{arena::Header, Arena, Mark, Trace};
use std::boxed;
use std::ops::{Deref, DerefMut};
use std::{
    any::type_name,
    sync::atomic::{AtomicPtr, AtomicUsize, Ordering},
};

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T: 'r>(pub &'r T, pub P);

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

pub struct Root<T> {
    intern: *const RootIntern<T>,
}

impl<T: Trace> Root<T> {
    pub fn to_gc<'a>(&self, arena: &'a Arena<T>) -> Gc<'a, T> {
        let root = unsafe { &*self.intern };
        let t = unsafe { &*root.gc_ptr.load(Ordering::Acquire) };
        arena.mark(Gc(t, P(())))
    }
}

// Gc<'o, T> -> Gc<'n, T>

// impl<'r, T: Condemned> From<Gc<'r, T>> for Root<T> {
//     fn from(gc: Gc<'r, T>) -> Root<T> {
//         let header = unsafe { &*Header::from(gc.0) };
//         let mut roots = header.intern.roots.lock().unwrap();
//         let root = roots.entry(Arena::<T>::index(gc.0)).or_insert_with(|| {
//             boxed::Box::leak(boxed::Box::new(RootIntern {
//                 ref_count: AtomicUsize::new(1),
//                 gc_ptr: AtomicPtr::new(gc.0 as *const T as *mut u8),
//             }))
//         });

//         Root {
//             intern: *root as *const RootIntern<T>,
//         }
//     }
// }

impl<'r, O: Trace, N: Trace> From<Gc<'r, O>> for Root<N> {
    fn from(gc: Gc<'r, O>) -> Self {
        if type_name::<O>() != type_name::<N>() {
            // TODO once Eq for &str is const make this const
            panic!("O is a different type then N, mark only changes lifetimes")
        };
        let header = unsafe { &*Header::from(gc.0) };
        let mut roots = header.intern.roots.lock().unwrap();
        let root = roots
            .entry(Arena::<N>::index(gc.0 as *const _ as *const _))
            .or_insert_with(|| {
                boxed::Box::leak(boxed::Box::new(RootIntern {
                    ref_count: AtomicUsize::new(1),
                    gc_ptr: AtomicPtr::new(gc.0 as *const _ as *const N as *mut u8),
                }))
            });

        Root {
            intern: *root as *const RootIntern<N>,
        }
    }
}

impl<T: Trace> Clone for Root<T> {
    fn clone(&self) -> Root<T> {
        unsafe { &*self.intern }
            .ref_count
            .fetch_add(1, Ordering::Relaxed);
        Root {
            intern: self.intern,
        }
    }
}

impl<T> Drop for Root<T> {
    fn drop(&mut self) {
        // Relaxed is safe, since the GC frees RootIntern.
        // Note the diffrence with Arc.
        unsafe { &*self.intern }
            .ref_count
            .fetch_sub(1, Ordering::Relaxed);
    }
}

pub(crate) struct RootIntern<T> {
    pub ref_count: AtomicUsize,
    /// Pointer is only live while an Arena<T> exists.
    pub gc_ptr: AtomicPtr<T>,
}

// FIXME
// #[test]
// fn root_gc_moved_test() {
//     let a = Arena::new();
//     let foo = a.gc_alloc(String::from("foo"));
//     let ptr = foo.0 as *const String;
//     let root: Root<String> = Root::from(foo);
//     drop(a);
//     while ptr == unsafe { &*root.intern }.gc_ptr.load(Ordering::Relaxed) {}
//     assert_eq!(
//         unsafe { &*(&*root.intern).gc_ptr.load(Ordering::Relaxed) },
//         "foo"
//     )
// }
