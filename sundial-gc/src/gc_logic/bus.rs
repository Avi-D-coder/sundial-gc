use crate::{arena::HeaderUnTyped, mark::Invariant};
use smallvec::SmallVec;
use std::ops::Deref;
use std::{cmp, sync::Mutex};

pub type Bus = Mutex<SmallVec<[Msg; 8]>>;

/// Returns index of sent message.
pub fn send(bus: &mut SmallVec<[Msg; 8]>, msg: Msg) -> usize {
    bus.push(msg);
    bus.len() - 1
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

    log::trace!("Worker messages: {:?}", wms);

    let mut v = SmallVec::new();
    wms.iter().for_each(|b: &WorkerMsg| {
        if let Some(Ok(m)) = v.last().map(|a: &WorkerMsg| (*a).try_merge(*b)) {
            log::trace!(
                "Merged:\n  a: {:?},\n  b: {:?},\n=>{:?}",
                v.last().unwrap(),
                b,
                m
            );
            *v.last_mut().unwrap() = m;
        } else {
            v.push(*b)
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
        /// Worker will not finish filling the Arena.
        release_to_gc: bool,
        /// A Worker allocated Arena.
        new_allocation: bool,
        /// `true` when a start and a End message were merged.
        /// This will occur when the GC does not read the bus
        /// between the start and end of a Arena lifetime.
        transient: bool,
        /// Address of the 16 kB arena.
        next: *const u8,
        grey_fields: u8,
        invariant_id: u8,
    },
    Release(*mut u8),
}

impl PartialOrd for WorkerMsg {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for WorkerMsg {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match (self, other) {
            (
                WorkerMsg::End {
                    next: a,
                    invariant_id: ai,
                    transient: tb,
                    ..
                },
                WorkerMsg::End {
                    next: b,
                    invariant_id: bi,
                    transient: ta,
                    ..
                },
            ) => match cmp::Reverse(a).cmp(&cmp::Reverse(b)) {
                cmp::Ordering::Equal => match (ta, tb) {
                    (true, false) => cmp::Ordering::Less,
                    (false, true) => cmp::Ordering::Greater,
                    _ => ai.cmp(bi),
                },
                o => o,
            },
            (
                WorkerMsg::End {
                    next: a, transient, ..
                },
                WorkerMsg::Start { next: b, .. },
            ) => match cmp::Reverse(a).cmp(&cmp::Reverse(b)) {
                cmp::Ordering::Equal => {
                    if *transient {
                        cmp::Ordering::Less
                    } else {
                        cmp::Ordering::Greater
                    }
                }
                o => o,
            },
            (
                WorkerMsg::Start { next: a, .. },
                WorkerMsg::End {
                    next: b, transient, ..
                },
            ) => match cmp::Reverse(a).cmp(&cmp::Reverse(b)) {
                cmp::Ordering::Equal => {
                    if *transient {
                        cmp::Ordering::Greater
                    } else {
                        cmp::Ordering::Less
                    }
                }
                o => o,
            },
            (
                WorkerMsg::Start {
                    next: a,
                    invariant_id: ai,
                    ..
                },
                WorkerMsg::Start {
                    next: b,
                    invariant_id: bi,
                    ..
                },
            ) => match cmp::Reverse(a).cmp(&cmp::Reverse(b)) {
                cmp::Ordering::Equal => ai.cmp(bi),
                o => o,
            },

            (WorkerMsg::Release(a), WorkerMsg::Release(b)) => cmp::Reverse(a).cmp(&cmp::Reverse(b)),
            (WorkerMsg::Release(_), _) => cmp::Ordering::Greater,
            (_, WorkerMsg::Release(_)) => cmp::Ordering::Less,
        }
    }
}

impl WorkerMsg {
    pub fn try_merge(self, b: WorkerMsg) -> Result<WorkerMsg, (WorkerMsg, WorkerMsg)> {
        match (self, b) {
            (
                WorkerMsg::Start {
                    invariant_id,
                    next: nxt,
                },
                WorkerMsg::End {
                    new_allocation,
                    release_to_gc,
                    grey_fields,
                    invariant_id: id,
                    next,
                    transient: false,
                    ..
                },
            ) => {
                if invariant_id == id && HeaderUnTyped::from(next) == HeaderUnTyped::from(nxt) {
                    Ok(WorkerMsg::End {
                        release_to_gc,
                        new_allocation,
                        next,
                        grey_fields,
                        invariant_id,
                        transient: true,
                    })
                } else {
                    Err((self, b))
                }
            }

            _ => Err((self, b)),
        }
    }
}
