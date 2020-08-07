use crate::{arena::HeaderUnTyped, mark::Invariant};
use smallvec::SmallVec;
use std::ops::Deref;
use std::{cmp, sync::Mutex};

pub type Bus = Mutex<SmallVec<[Msg; 8]>>;

/// Returns index of sent message.
pub fn send(bus: &mut SmallVec<[Msg; 8]>, msg: Msg) -> usize {
    if let Some((i, slot)) = bus
        .iter_mut()
        .enumerate()
        .filter(|(_, o)| o.is_slot())
        .next()
    {
        *slot = msg;
        i
    } else {
        bus.push(msg);
        bus.len() - 1
    }
}

/// Removes `Msg::Slot`s and `Msg::Worker`s.
/// Returns merged `WorkerMsg`s.
pub fn reduce(bus: &mut SmallVec<[Msg; 8]>) -> SmallVec<[WorkerMsg; 8]> {
    let mut wms: SmallVec<[WorkerMsg; 8]> = SmallVec::new();

    *bus = bus
        .into_iter()
        .filter_map(|msg| match msg {
            Msg::Worker(m) => {
                wms.push(*m);
                None
            }
            Msg::Slot => None,
            m => Some(*m),
        })
        .collect();

    bus.sort();
    wms.sort();

    let mut v = SmallVec::new();
    wms.into_iter().for_each(|b: WorkerMsg| {
        if let Some(Ok(m)) = v.last().map(|a: &WorkerMsg| (*a).try_merge(b)) {
            *v.last_mut().unwrap() = m;
        } else {
            v.push(b)
        }
    });
    v
}

#[derive(Copy, Clone)]
pub(crate) struct BusPtr(&'static Bus);

impl BusPtr {
    pub unsafe fn new(ptr: *const Bus) -> Self {
        BusPtr(&*ptr)
    }
}

impl Deref for BusPtr {
    type Target = Bus;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

unsafe impl Send for BusPtr {}
unsafe impl Sync for BusPtr {}

// TODO replace with atomic 64 byte header encoding.
// Plus variable length condemned region messages.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Msg {
    Slot,
    Invariant(Invariant),
    Next(*mut u8),
    Worker(WorkerMsg),
}

impl PartialOrd for Msg {
    /// Sort on `next` ptr.
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Msg {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (Msg::Worker(a), Msg::Worker(b)) => a.cmp(b),
            (Msg::Worker { .. }, _) => cmp::Ordering::Greater,

            (Msg::Slot, Msg::Slot) => cmp::Ordering::Equal,
            (Msg::Slot, _) => cmp::Ordering::Greater,
            (_, Msg::Slot) => cmp::Ordering::Less,

            (Msg::Invariant(_), Msg::Invariant(_)) => cmp::Ordering::Equal,
            (Msg::Invariant(_), _) => cmp::Ordering::Less,

            (Msg::Next(a), Msg::Next(b)) => a.cmp(b),
            (Msg::Next(_), _) => cmp::Ordering::Less,
        }
    }
}

impl Msg {
    #[inline(always)]
    pub fn is_slot(&self) -> bool {
        match self {
            Msg::Slot => true,
            _ => false,
        }
    }

    pub fn is_invariant(&self) -> bool {
        match self {
            Msg::Invariant { .. } => true,
            _ => false,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum WorkerMsg {
    /// The `mem_addr` of the area
    Start {
        /// The last invariant the worker knew of.
        invariant_id: u8,
        /// TODO cache on GC side per Bus
        next: *const u8,
    },
    End {
        /// Worker will not finish filling the Arena
        release_to_gc: bool,
        new_allocation: bool,
        /// Address of the 16 kB arena
        next: *const u8,
        grey_fields: u8,
        invariant_id: u8,
    },
}

impl PartialOrd for WorkerMsg {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WorkerMsg {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (WorkerMsg::End { next: a, .. }, WorkerMsg::End { next: b, .. }) => a.cmp(b),
            (WorkerMsg::End { next: a, .. }, WorkerMsg::Start { next: b, .. }) => a.cmp(b),
            (WorkerMsg::Start { next: a, .. }, WorkerMsg::End { next: b, .. }) => a.cmp(b),
            (WorkerMsg::Start { next: a, .. }, WorkerMsg::Start { next: b, .. }) => a.cmp(b),
        }
    }
}

impl WorkerMsg {
    pub fn try_merge(self, b: WorkerMsg) -> Result<WorkerMsg, (WorkerMsg, WorkerMsg)> {
        match (self, b) {
            (
                WorkerMsg::Start { invariant_id, next },
                WorkerMsg::Start {
                    invariant_id: id,
                    next: nxt,
                },
            ) => {
                if invariant_id == id && HeaderUnTyped::from(next) == HeaderUnTyped::from(nxt) {
                    Ok(WorkerMsg::Start {
                        invariant_id,
                        next: cmp::min(next, nxt),
                    })
                } else {
                    Err((self, b))
                }
            }

            (
                WorkerMsg::Start { invariant_id, next },
                WorkerMsg::End {
                    new_allocation,
                    release_to_gc,
                    grey_fields,
                    invariant_id: id,
                    next: nxt,
                },
            )
            | (
                WorkerMsg::End {
                    new_allocation,
                    release_to_gc,
                    grey_fields,
                    invariant_id: id,
                    next: nxt,
                },
                WorkerMsg::Start { invariant_id, next },
            ) => {
                if invariant_id == id && HeaderUnTyped::from(next) == HeaderUnTyped::from(nxt) {
                    Ok(WorkerMsg::End {
                        release_to_gc,
                        new_allocation,
                        next,
                        grey_fields,
                        invariant_id,
                    })
                } else {
                    Err((self, b))
                }
            }

            (
                WorkerMsg::End {
                    release_to_gc,
                    new_allocation,
                    next,
                    grey_fields,
                    invariant_id,
                },
                WorkerMsg::End {
                    release_to_gc: rtg,
                    new_allocation: na,
                    next: nxt,
                    grey_fields: gf,
                    invariant_id: id,
                },
            ) => {
                if invariant_id == id && HeaderUnTyped::from(next) == HeaderUnTyped::from(nxt) {
                    debug_assert_eq!(new_allocation, na);
                    Ok(WorkerMsg::End {
                        release_to_gc: release_to_gc || rtg,
                        new_allocation,
                        next: cmp::min(next, nxt),
                        grey_fields: grey_fields | gf,
                        invariant_id,
                    })
                } else {
                    Err((self, b))
                }
            }
        }
    }
}
