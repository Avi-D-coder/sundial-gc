use crate::{Gc, Immutable, Trace};
use std::ops::Deref;
use std::{
    any::type_name,
    fmt::Debug,
    ptr,
    sync::atomic::{AtomicPtr, Ordering},
};

// TODO(polymorphism)
// struct Lazy<'r, T>(*const (), PhantomData<Gc<'r, T>>);

// impl<'r, T: 'r> Deref for Lazy<'r, T> {
//     type Target = Gc<'r, T>;
//     fn deref(&self) -> &Self::Target {
//         todo!()
//     }
// }

pub struct Thunk<'r, A, T>
where
    A: 'r,
    T: 'r,
{
    /// If `strict.is_null()` then the Thunk is unevaluated.
    /// If `strict == 1` then we are in a recursive initialization closure.
    strict: AtomicPtr<T>,
    args: *const A,
    /// TODO(optimization) Small Copy structs should be unboxed, Such that `GC<T: Copy>> === T`.
    func: fn(Gc<'r, A>) -> Gc<'r, T>,
}

impl<'r, A, T> Thunk<'r, A, T> {
    pub fn new(args: Gc<'r, A>, func: fn(Gc<'r, A>) -> Gc<'r, T>) -> Thunk<'r, A, T> {
        Thunk {
            strict: AtomicPtr::new(ptr::null_mut()),
            args: args.0 as _,
            func,
        }
    }

    // pub fn new_rec<R: FnOnce(Lazy<'r, T>) -> Gc<'r, A>>(
    //     recursive: R,
    //     func: fn(Gc<'r, A>) -> Gc<'r, T>,
    // ) {
    //     let thunk = Thunk {
    //         strict: AtomicPtr::from(1 as *mut _),
    //         func,
    //         args: ptr::null(),
    //     };
    //
    //     todo!()
    // }
}

impl<'r, A, T> Deref for Thunk<'r, A, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        let strict = self.strict.load(Ordering::Relaxed);

        if self.args.is_null() {
            panic!(
                "Dereferenced self referential Thunk in sundial_gc::Thunk::new_rec.\n
                 Recursive Thunk cannot be dereferenced until initialization is complete."
            )
        } else if strict.is_null() {
            let func = self.func;
            let b = func(unsafe { Gc::new(&*self.args) });
            self.strict
                .store(b.0 as *const T as *mut T, Ordering::Relaxed);
            b.0
        } else {
            unsafe { &*strict }
        }
    }
}

impl<'r, A, T> AsRef<T> for Thunk<'r, A, T> {
    fn as_ref(&self) -> &T {
        self.deref()
    }
}

impl<'r, A, T> From<Thunk<'r, A, T>> for Gc<'r, T> {
    fn from(thunk: Thunk<'r, A, T>) -> Self {
        let ptr = thunk.deref() as *const _;
        unsafe { Gc::new(&*ptr) }
    }
}

impl<'r, A: Debug, T: Debug> Debug for Thunk<'r, A, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut f = f.debug_struct(&format!(
            "Thunk<{:?}, {:?}>",
            type_name::<A>(),
            type_name::<T>()
        ));

        let strict = self.strict.load(Ordering::Relaxed);

        if !strict.is_null() {
            f.field("strict", unsafe { &*strict });
        } else if !self.args.is_null() {
            f.field("args", unsafe { &*self.args });
        };

        f.finish()
    }
}

unsafe impl<'r, A: Immutable, T: Immutable> Immutable for Thunk<'r, A, T> {}

unsafe impl<'r, A: Trace, T: Trace> Trace for Thunk<'r, A, T> {
    fn fields(s: &Self, offset: u8, grey_fields: u8, invariant: &crate::mark::Invariant) -> u8 {
        todo!()
    }
    unsafe fn evacuate<'e>(
        s: &Self,
        offset: crate::mark::Offset,
        grey_fields: u8,
        invariant: &crate::mark::Invariant,
        handlers: &mut crate::mark::Handlers,
    ) {
        let strict = s.strict.load(Ordering::Relaxed);

        if !strict.is_null() {
            T::evacuate(
                unsafe { &*strict },
                offset,
                grey_fields,
                invariant,
                handlers,
            )
        } else {
            A::evacuate(
                unsafe { &*s.args },
                offset + 1,
                grey_fields,
                invariant,
                handlers,
            )
        };
    }
    fn direct_gc_types(
        t: &mut std::collections::HashMap<crate::mark::GcTypeInfo, crate::mark::TypeRow>,
        offset: u8,
    ) {
        Gc::<T>::direct_gc_types(t, offset);
        Gc::<A>::direct_gc_types(t, offset + 1);
    }
    fn transitive_gc_types(tti: *mut crate::mark::Tti) {
        A::transitive_gc_types(tti);
        T::transitive_gc_types(tti);
    }
    const GC_COUNT: u8 = 2;
    // FIXME
    const PRE_CONDITION: bool = true;
}
