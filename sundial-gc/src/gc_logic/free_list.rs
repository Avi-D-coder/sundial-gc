use crate::arena::{HeaderUnTyped, Mem};
use std::alloc::{GlobalAlloc, Layout, System};

pub(crate) struct FreeList {
    pub free: Vec<*mut Mem>,
    pub allocated: usize,
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
        if let Some(header) = self.free.pop() {
            HeaderUnTyped::init(header as *mut _);
            unsafe { &mut *(header as *mut _) }
        } else {
            self.allocated += 1;
            let header = unsafe { System.alloc(Layout::new::<Mem>()) } as *mut _;
            HeaderUnTyped::init(header as *mut _);
            unsafe { &mut *header }
        }
    }

    // TODO return memory to operating system.
    pub fn dealloc(&mut self, arena: *mut Mem) {
        self.allocated -= 1;
        self.free.push(arena);
    }
}
