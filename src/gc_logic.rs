use super::arena::*;
use super::trace::*;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::mem;
use std::sync::{mpsc::*, Mutex};
use std::thread::{self, ThreadId};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    ptr,
    time::Duration,
};

pub(crate) static mut REGISTER: Option<Sender<RegMsg>> = None;

pub const ARENA_SIZE: usize = 16384;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct TypeInfo {
    info: GcTypeInfo,
    // TODO work around const limitation and merge these
    drop_ptr: fn(*mut u8),
}

unsafe impl Send for TypeInfo {}

#[derive(Copy, Clone)]
pub(crate) struct BusPtr(&'static Bus);

unsafe impl Send for BusPtr {}
unsafe impl Sync for BusPtr {}

#[derive(Copy, Clone)]
pub(crate) enum RegMsg {
    Reg(ThreadId, TypeInfo, BusPtr),
    Un(ThreadId, TypeInfo),
}

#[derive(Copy, Clone)]
struct Invariant {
    white_start: usize,
    white_end: usize,
    grey_feilds: u8,
    grey_self: bool,
}

#[derive(Clone)]
struct Parents {
    direct: HashMap<GcTypeInfo, u8>,
    transitive: HashSet<GcTypeInfo>,
}

impl Default for Parents {
    fn default() -> Self {
        Parents {
            direct: HashMap::new(),
            transitive: HashSet::new(),
        }
    }
}

/// Keep in sync with Header<T>
pub(crate) struct HeaderUnTyped {
    // TODO private
    pub evacuated: Mutex<HashMap<u16, *const u8>>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
    roots: usize,
    finalizers: usize,
    condemned: bool,
}

impl HeaderUnTyped {
    /// last is closest to Header, since we bump down.
    pub(crate) fn last_offset(align: u16) -> u16 {
        let header = mem::size_of::<HeaderUnTyped>() as u16;
        header + ((align - (header % align)) % align)
    }

    // TODO is this right?
    pub(crate) fn first_offset(align: u16, size: u16) -> u16 {
        let cap = (ARENA_SIZE as u16 - HeaderUnTyped::last_offset(align)) / size;
        HeaderUnTyped::last_offset(align) + (cap * size)
    }
}

/// This whole struct is unsafe!
struct TypeState {
    type_info: TypeInfo,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    /// Arena owned by GC, that have not been completely filled.
    gc_arenas: BTreeSet<*const u8>,
    gc_arenas_full: BTreeSet<*const u8>,
    /// Count of Arenas started before transitive_epoch.
    pending_known_transitive_grey: usize,
    /// Count of Arenas started before `invariant`.
    pending_known_grey: usize,
    /// `Arena.Header` started after `epoch`.
    /// `pending` is mutually exclusive with `pending_known_grey`.
    /// A arena `*const u8` cannot have a entry in `pending` and `pending_known_grey`.
    pending: HashMap<*const u8, usize>,
    latest_grey: usize,
    /// `epoch` is not synchronized across threads.
    /// 2 epochs delineate Arena age.
    /// 2 epoch are needed, since thread A may receive a GCed object from thread B in between the
    ///   GC reading A & B's messages.
    /// A's `Msg::Start` may be received in the next epoch after B's `Msg::End`.
    /// `epoch` counts from 1, and resets when free is accomplished.
    epoch: usize,
    transitive_epoch: usize,
    /// Waiting for transitive parents to clear their stack
    phase2: bool,
    invariant: Option<Invariant>,
    waiting_on_transitive_parents: usize,
}

impl TypeState {
    /// Handle messages of Type.
    /// Returns true when no condemned pointers from Type exist.
    fn step(&mut self, free: &mut Vec<*mut HeaderUnTyped>) -> bool {
        let Self {
            type_info,
            buses,
            gc_arenas_full,
            gc_arenas,
            pending_known_transitive_grey,
            pending_known_grey,
            pending,
            latest_grey,
            epoch,
            transitive_epoch,
            phase2,
            invariant,
            ..
        } = self;
        let last_offset = HeaderUnTyped::last_offset(type_info.info.alignment) as usize;
        let first_offset =
            HeaderUnTyped::first_offset(type_info.info.alignment, type_info.info.byte_size)
                as usize;
        *epoch += 1;

        // TODO lift allocation
        let mut grey = Vec::with_capacity(20);
        buses.iter().for_each(|(_, bus)| {
            let mut has_new = false;
            let mut bus = bus.0.lock().expect("Could not unlock bus");
            // TODO fill gc_arenas by sending them to workers.
            grey.extend(bus.iter_mut().filter_map(|msg| match msg {
                Msg::Start {
                    white_start,
                    white_end,
                    next,
                } => {
                    if *epoch < *transitive_epoch {
                        *pending_known_transitive_grey += 1;
                    }
                    if invariant
                        .map(|ci| ci.white_start == *white_start && ci.white_end == *white_end)
                        .unwrap_or(true)
                    {
                        let next_addr = *next as usize;
                        let header = (next_addr - (next_addr % ARENA_SIZE)) as *const u8;
                        pending.insert(header, *epoch);
                    } else {
                        *latest_grey = *epoch;
                        *pending_known_grey += 1;
                    }
                    *msg = Msg::Slot;
                    None
                }
                Msg::End {
                    release_to_gc,
                    new_allocation,
                    next,
                    grey_feilds,
                    white_start,
                    white_end,
                } => {
                    let ret = if invariant
                        .map(|i| i.white_start == *white_start && i.white_end == *white_end)
                        .unwrap_or(true)
                    {
                        let next_addr = *next as usize;
                        let header = (next_addr - (next_addr % ARENA_SIZE)) as *const u8;
                        if let Some(e) = pending.remove(&header) {
                            if e < *transitive_epoch {
                                *pending_known_transitive_grey =
                                    pending_known_transitive_grey.saturating_sub(1);
                            }
                        } else if !*new_allocation {
                            *pending_known_grey -= 1;
                        };
                        // Nothing was marked.
                        // Allocated objects contain no condemned pointers into the white set.
                        if *grey_feilds == 0b0000_0000 {
                            if *release_to_gc {
                                // if full
                                let last = (next_addr - (next_addr % ARENA_SIZE)) + last_offset;
                                if next_addr >= last {
                                    gc_arenas.insert(*next);
                                } else {
                                    gc_arenas_full.insert(*next);
                                };
                            } else {
                                // TODO store active worker arenas
                            };
                            None
                        } else {
                            *latest_grey = *epoch;
                            Some((*next, *grey_feilds))
                        }
                    } else {
                        *pending_known_grey -= 1;
                        *latest_grey = *epoch;
                        Some((*next, 0b1111_1111))
                    };

                    *msg = Msg::Slot;
                    ret
                }
                Msg::Gc { next, .. } => {
                    has_new = true;
                    if next.is_none() {
                        *next = Some(0 as *mut u8);
                    }
                    None
                }
                _ => None,
            }));

            if !has_new {
                bus.iter_mut().filter(|m| m.is_slot()).next().map(|slot| {
                    let next = gc_arenas
                        .pop_first()
                        .map(|p| p as *mut _)
                        .or(free.pop().map(|h| (h as usize + first_offset) as *mut u8));

                    if let Some(Invariant {
                        grey_feilds,
                        grey_self,
                        white_start,
                        white_end,
                    }) = *invariant
                    {
                        *slot = Msg::Gc {
                            next,
                            grey_feilds,
                            grey_self,
                            white_start,
                            white_end,
                        };
                    } else {
                        *slot = Msg::Gc {
                            next,
                            grey_feilds: 0b0000_0000,
                            grey_self: false,
                            white_start: 0,
                            white_end: 1,
                        };
                    }
                });
            }
        });

        // TODO trace grey, updating refs

        if *pending_known_grey == 0
            && *epoch >= (*latest_grey + 2)
            && (*phase2
                || (pending
                    .iter()
                    .all(|(_, start_epoch)| *start_epoch >= *latest_grey)))
        {
            // No direct pointers into the white region exist from this type!
            if *phase2 {
                false
            } else {
                *phase2 = true;
                true
            }
        } else {
            // This cannot happen at the moment
            // phase2 = false
            false
        }
    }

    /// Call when `Self` is a transitive parent of an `Arena<T>`,
    /// once `T`'s direct parents are clear of condemned ptrs.
    /// Must be proceed by a call to `is_stack_clear`.
    fn request_clear_stack(&mut self) {
        // TODO are 2 epochs needed?
        self.transitive_epoch = self.epoch + 2;
        self.pending_known_transitive_grey = self.pending.len();
    }

    /// Must be proceed by a call to `request_clear_stack`.
    fn is_stack_clear(&self) -> bool {
        self.pending_known_transitive_grey == 0
            && self.pending_known_grey == 0
            && self.transitive_epoch != 0
    }

    fn set_invariant(&mut self, inv: Invariant) {
        self.phase2 = false;
        self.pending_known_transitive_grey = 0;
        self.pending_known_grey = self.pending.len();
        // It will be one upon first `step()`
        self.epoch = 0;
        self.latest_grey = 1;
        self.pending = HashMap::new();
        self.invariant = Some(inv);
    }
}

struct TypeRelations {
    active: HashMap<GcTypeInfo, TypeState>,
    parents: HashMap<GcTypeInfo, Parents>,
    /// Key is 16 kB aligned Arena, value is `Arena.next`.
    /// TODO This will not work once we are freeing regions containing multiple arenas.
    /// Regions must be contiguous, since Gc.ptr may be a &'static T.
    /// To fix this, we should probably differentiate between Gc<T> and &'static
    mem: BTreeMap<usize, (*const u8, TypeInfo)>,
}

impl TypeRelations {
    pub fn new() -> Self {
        Self {
            active: HashMap::new(),
            parents: HashMap::new(),
            mem: BTreeMap::new(),
        }
    }

    fn reg(&mut self, type_info: TypeInfo, t_id: ThreadId, bus: BusPtr) {
        let Self {
            active,
            parents,
            mem,
        } = self;
        let ts = active.entry(type_info.info).or_insert_with(|| {
            let mut direct_children: HashMap<GcTypeInfo, u8> = HashMap::new();
            direct_gced(type_info.info, &mut direct_children);
            direct_children.iter().for_each(|(child, bit)| {
                let cp = parents.entry(*child).or_default();
                cp.direct.insert(type_info.info, *bit);
            });

            let mut tti = Tti::new();
            let tti_fn = type_info.info.tti_ptr;
            tti_fn(&mut tti as *mut Tti);
            tti.type_info.iter().for_each(|child| {
                let cp = parents.entry(*child).or_default();
                cp.transitive.insert(type_info.info);
            });

            TypeState {
                type_info,
                buses: HashMap::new(),
                gc_arenas: BTreeSet::new(),
                gc_arenas_full: BTreeSet::new(),
                pending_known_transitive_grey: 0,
                pending_known_grey: 0,
                pending: HashMap::new(),
                latest_grey: 0,
                epoch: 0,
                transitive_epoch: 0,
                phase2: false,
                invariant: None,
                waiting_on_transitive_parents: 0,
            }
        });

        ts.buses.insert(t_id, bus);
    }
}

fn start() -> Sender<RegMsg> {
    let (s, r) = channel();
    thread::spawn(move || gc_loop(r));
    s
}

fn gc_loop(r: Receiver<RegMsg>) {
    let mut types = TypeRelations::new();
    let mut free: Vec<*mut HeaderUnTyped> = Vec::with_capacity(100);

    loop {
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => types.reg(ty, id, bus),
                RegMsg::Un(id, ty) => {
                    // The thread was dropped
                    let type_state = types.active.get_mut(&ty.info).unwrap();
                    type_state.step(&mut free);
                    type_state.buses.remove(&id);
                }
            }
        }

        let TypeRelations {
            parents,
            active,
            mem,
        } = &mut types;

        let mut stack_cleared = HashSet::with_capacity(20);
        let mut clear_stack_old: HashMap<GcTypeInfo, HashSet<GcTypeInfo>> =
            HashMap::with_capacity(20);
        let mut clear_stack_new: HashMap<GcTypeInfo, HashSet<GcTypeInfo>> =
            HashMap::with_capacity(20);
        active.iter_mut().for_each(|(ti, ts)| {
            if ts.step(&mut free) {
                if let Some(ps) = parents.get(ti) {
                    // FIXME there's a race here when a parent Type is registered after active.iter
                    ts.waiting_on_transitive_parents = ps.transitive.len();

                    ps.transitive.iter().for_each(|pti| {
                        let requesters = clear_stack_new.entry(*pti).or_insert(HashSet::new());
                        requesters.insert(*ti);
                    });
                }
            }

            // TODO This is slower than it could be like everything
            if ts.is_stack_clear() && clear_stack_old.contains_key(ti) {
                stack_cleared.insert(*ti);
            };
        });

        stack_cleared.drain().for_each(|ti| {
            clear_stack_old.remove(&ti).unwrap().iter().for_each(|cti| {
                let ts = active.get_mut(cti).unwrap();
                ts.waiting_on_transitive_parents -= 1;
                if ts.waiting_on_transitive_parents == 0 {
                    let inv = ts.invariant.unwrap();
                    // Free memory
                    ts.gc_arenas_full
                        .range((inv.white_start as *const u8)..(inv.white_end as *const u8))
                        .into_iter()
                        .for_each(|next| {
                            let next_addr = *next as usize;
                            let header =
                                (next_addr - (next_addr % ARENA_SIZE)) as *mut HeaderUnTyped;
                            unsafe { ptr::drop_in_place(header) }
                            if free.len() < mem.len() / 3 {
                                free.push(header);
                            } else {
                                unsafe { System.dealloc(header as *mut u8, Layout::new::<Mem>()) };
                            }
                        });
                    ts.invariant = None;
                }
            })
        });

        clear_stack_new.drain().for_each(|(pti, mut ctis)| {
            active.get_mut(&pti).unwrap().request_clear_stack();
            clear_stack_old.entry(pti).and_modify(|s| {
                ctis.drain().for_each(|c| {
                    s.insert(c);
                });
            });
        });

        thread::sleep(Duration::from_millis(100))
    }
}
