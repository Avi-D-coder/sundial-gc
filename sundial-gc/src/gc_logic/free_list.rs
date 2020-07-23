use crate::arena::{HeaderUnTyped, Mem};
use std::alloc::{GlobalAlloc, Layout, System};

pub(crate) struct FreeList {
    free: Vec<*mut Mem>,
}

impl Default for FreeList {
    fn default() -> Self {
        FreeList { free: Vec::new() }
    }
}

impl FreeList {
    pub fn alloc(&mut self) -> &mut HeaderUnTyped {
        if let Some(header) = self.free.pop() {
            HeaderUnTyped::init(header as *mut _);
            unsafe { &mut *(header as *mut _) }
        } else {
            let header = unsafe { System.alloc(Layout::new::<Mem>()) } as *mut _;
            HeaderUnTyped::init(header as *mut _);
            unsafe { &mut *header }
        }
    }

    // TODO return memory to operating system.
    pub fn dealloc(&mut self, arena: *mut Mem) {
        self.free.push(arena);
    }
}
