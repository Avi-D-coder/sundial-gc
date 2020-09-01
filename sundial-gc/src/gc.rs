use crate::{arena::Header, Arena, Life, Mark, Trace, GC};
use std::boxed;
use std::ops::{Deref, DerefMut};
use std::{
    any::Any,
    borrow::Borrow,
    fmt::Debug,
    marker::PhantomData,
    sync::atomic::{AtomicPtr, AtomicUsize, Ordering},
};

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Gc<'r, T>(pub &'r T, pub P);

impl<'r, T> Gc<'r, T> {
    #[inline(always)]
    pub unsafe fn new(t: &'r T) -> Self {
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
        unsafe { Gc::new(t.0) }
    }
}

impl<'r, T> AsRef<T> for Gc<'r, T> {
    fn as_ref(&self) -> &T {
        self.0
    }
}

impl<'r, T> Borrow<T> for Gc<'r, T> {
    fn borrow(&self) -> &T {
        self.0
    }
}

impl<'r, T: Debug> Debug for Gc<'r, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Gc").field(self.0).finish()
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

impl<'l, T: Life> Root<T> {
    pub fn into_gc<'r, 'a: 'r>(&self, arena: &'a Arena<impl GC<T>>) -> Gc<'r, T::L<'r>> {
        let root = unsafe { &*self.intern };
        let t = unsafe { &*root.gc_ptr.load(Ordering::Acquire) };
        arena.mark(Gc(t, P(())))
    }

    // TODO requires deep evac prior to installing new pointer.
    // /// Block collection of `T` and it's transitive child types.
    // fn guard(&self) -> Guard<Gc<T>> {}
}

impl<'r, T: Life> From<Gc<'r, T>> for Root<T::L<'static>> {
    fn from(gc: Gc<'r, T>) -> Root<T::L<'static>> {
        todo!()
        // let header = unsafe { &*Header::from(gc.0) };
        // let mut roots = header.intern.roots.lock().unwrap();
        // let root = roots.entry(Arena::<T>::index(gc.0)).or_insert_with(|| {
        //     boxed::Box::leak(boxed::Box::new(RootIntern {
        //         ref_count: AtomicUsize::new(1),
        //         gc_ptr: AtomicPtr::new(gc.0 as *const T as *mut u8),
        //     }))
        // });

        // Root {
        //     intern: *root as *const RootIntern<T>,
        // }
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
        // Note the difference with Arc.
        unsafe { &*self.intern }
            .ref_count
            .fetch_sub(1, Ordering::Relaxed);
    }
}

pub(crate) struct RootIntern<T> {
    /// Pointer is only live while an Arena<T> exists.
    pub gc_ptr: AtomicPtr<T>,
    pub ref_count: AtomicUsize,
    // pub guard_count: AtomicUsize,
}

/// A polymorphic GCed reference.
/// Gc<dyn Trace>.
pub struct Dyn<'r, D> {
    // TODO there may be a trick to remove the box.
    gc_ref: boxed::Box<AtomicPtr<*const D>>,
    life: PhantomData<&'r D>,
}

#[test]
fn function_name_test() {
    let a: Arena<usize> = Arena::new();
    let one = a.gc(1);
}

impl<'o, D> Dyn<'o, D> {
    pub fn try_into_gc<'r, 'a: 'r, T: Life>(
        &self,
        arena: &'a Arena<impl GC<T>>,
    ) -> Option<Gc<'r, T::L<'r>>>
// D: 'o + GC<A::L<'o>>,
        // N: 'r + GC<A::L<'r>>,
    {
        todo!()
        // if todo!() {
        //     None
        // } else {
        //     let t = unsafe { &*(self.gc_ref.load(Ordering::Acquire) as *const O) };
        //     Some(arena.mark(Gc(t, P(()))))
        // }
    }

    pub fn with_dyn<F, R>(&self, f: F) -> R
    where
        F: FnOnce(D) -> R,
    {
        todo!()
    }
}

impl<'r, T: Trace> From<Gc<'r, T>> for Dyn<'r, &'r dyn Any> {
    fn from(gc: Gc<'r, T>) -> Dyn<'r, &'r dyn Any> {
        // let d: Dyndyn D = *gc;
        todo!()
        // let header = unsafe { &*Header::from(gc.0) };
        // let mut roots = header.intern.roots.lock().unwrap();
        // let root = roots.entry(Arena::<T>::index(gc.0)).or_insert_with(|| {
        //     boxed::Box::leak(boxed::Box::new(RootIntern {
        //         ref_count: AtomicUsize::new(1),
        //         gc_ptr: AtomicPtr::new(gc.0 as *const T as *mut u8),
        //     }))
        // });

        // Root {
        //     intern: *root as *const RootIntern<T>,
        // }
    }
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
