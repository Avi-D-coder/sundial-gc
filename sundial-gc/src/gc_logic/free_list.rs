use crate::arena::{HeaderUnTyped, Mem};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    fmt::Debug,
    ptr,
};

pub(crate) struct FreeList {
    free: Vec<*mut Mem>,
    /// More Arenas may be allocated then this count, due to workers allocating and freeing Arenas.
    pub allocated: usize,
}

impl Debug for FreeList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FreeList")
            .field("free", &self.free)
            .field("free count", &self.free.len())
            .field("allocated", &self.allocated)
            .finish()
    }
}

impl Default for FreeList {
    fn default() -> Self {
        FreeList {
            free: Vec::new(),
            allocated: 0,
        }
    }
}

impl FreeList {
    pub fn alloc(&mut self) -> &mut HeaderUnTyped {
        log::trace!("FreeList: {:?}", self);
        self.allocated += 1;
        if let Some(header) = self.free.pop() {
            log::trace!("Reusing Arena: {:?}", header);
            HeaderUnTyped::init(header as *mut HeaderUnTyped);
            let h = unsafe { &mut *(header as *mut HeaderUnTyped) };
            h.roots.lock().unwrap();
            log::trace!("Reusing Arena 2: {:?}", header);
            h
        } else {
            let header = unsafe { System.alloc(Layout::new::<Mem>()) } as *mut HeaderUnTyped;
            log::trace!("Allocating Arena: {:?}", header);
            HeaderUnTyped::init(header as *mut HeaderUnTyped);
            let h: &mut HeaderUnTyped = unsafe { &mut *header };
            h.roots.lock().unwrap();
            log::trace!("Reusing Arena 2: {:?}", header);
            h
        }
    }

    // TODO return memory to operating system.
    pub fn dealloc(&mut self, arena: *mut HeaderUnTyped) {
        log::trace!("FreeList:dealloc: {:?}", arena);
        unsafe { ptr::drop_in_place(arena) };
        // More Arenas may be allocated then this count.
        self.allocated = self.allocated.saturating_sub(1);
        unsafe { System.dealloc(arena as *mut u8, Layout::new::<Mem>()) };
        // self.free.push(arena as *mut Mem);
    }

    pub fn merge(&mut self, with: &mut FreeList) {
        log::trace!("FreeList::merge {:?} with {:?}", self, with);
        self.free.extend(with.free.iter());
        self.allocated += with.allocated;
        with.free.clear();
        with.allocated = 0;
    }

    pub fn len(&self) -> usize {
        self.free.len()
    }
}
