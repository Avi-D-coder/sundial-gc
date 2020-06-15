use super::arena::*;
use super::trace::*;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::mem;
use std::sync::{mpsc::*, Mutex};
use std::thread::{self, ThreadId};
use std::time::Duration;

pub(crate) static mut REGISTER: Option<Sender<RegMsg>> = None;

const ARENA_SIZE: usize = 16384;

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
}

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
struct HeaderUnTyped {
    // TODO private
    pub evacuated: Mutex<HashMap<u16, *const u8>>,
    // roots: HashMap<u16, *const Box<UnsafeCell<*const T>>>,
    // finalizers: TODO
    roots: usize,
    finalizers: usize,
}

impl HeaderUnTyped {
    /// last is closest to Header, since we bump down.
    fn last_offset(align: u16) -> u16 {
        let header = mem::size_of::<HeaderUnTyped>() as u16;
        header + ((align - (header % align)) % align)
    }
}

/// This whole struct is unsafe!
struct TypeState {
    type_info: TypeInfo,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    /// Arena owned by GC, that have not been completely filled.
    gc_arenas: HashSet<*const u8>,
    /// Count of Arenas started before transitive_epoch.
    pending_known_transitive_grey: usize,
    /// Count of Arenas started before `invariant`.
    pending_known_grey: usize,
    /// `Arena.Header` started after `epoch`.
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
    invariant: Option<Invariant>,
}

impl TypeState {
    /// Handle messages of Type.
    /// Returns true when no condemed pointers from Type exist.
    fn step(&mut self) -> bool {
        let Self {
            type_info,
            buses,
            gc_arenas,
            pending_known_transitive_grey,
            pending_known_grey,
            pending,
            latest_grey,
            epoch,
            transitive_epoch,
            invariant,
        } = self;
        *epoch += 1;

        // TODO lift allocation
        let mut grey = Vec::with_capacity(20);
        buses.iter().for_each(|(_, bus)| {
            let bus = bus.0.lock().expect("Could not unlock bus");
            // TODO fill gc_arenas by sending them to workers.
            grey.extend(bus.iter().filter_map(|msg| match msg {
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
                    None
                }
                Msg::End {
                    release_to_gc,
                    next,
                    grey_feilds,
                    white_start,
                    white_end,
                } => {
                    if invariant
                        .map(|i| i.white_start == *white_start && i.white_end == *white_end)
                        .unwrap_or(true)
                    {
                        let next_addr = *next as usize;
                        let header = (next_addr - (next_addr % ARENA_SIZE)) as *const u8;

                        // Nothing was marked.
                        // Allocated objects contain no condemned pointers into the white set.
                        if *grey_feilds == 0b0000_0000 {
                            if !release_to_gc {
                                let remaining = pending
                                    .entry(header)
                                    .and_modify(|e| {
                                        if *e < *transitive_epoch {
                                            *pending_known_transitive_grey =
                                                pending_known_transitive_grey.saturating_sub(1);
                                        }
                                    })
                                    .or_insert(0);
                                *remaining = *epoch;
                                None
                            } else {
                                if let Some(e) = pending.remove(&header) {
                                    if e < *transitive_epoch {
                                        *pending_known_transitive_grey =
                                            pending_known_transitive_grey.saturating_sub(1);
                                    }
                                };

                                // if full
                                let last = (next_addr - (next_addr % ARENA_SIZE))
                                    + HeaderUnTyped::last_offset(type_info.info.alignment) as usize;
                                if next_addr >= last {
                                    gc_arenas.insert(*next);
                                };
                                None
                            }
                        } else {
                            *latest_grey = *epoch;
                            Some((*next, *grey_feilds))
                        }
                    } else {
                        *latest_grey = *epoch;
                        Some((*next, 0b1111_1111))
                    }
                }
                _ => None,
            }));
        });

        // TODO trace grey, updating refs

        if *pending_known_grey == 0
            && *epoch >= (*latest_grey + 2)
            && pending
                .iter()
                .all(|(_, start_epoch)| *start_epoch >= *latest_grey)
        {
            // No direct pointers into the white region exist from this type!
            true
        } else {
            false
        }
    }

    fn request_transitive_parrent_stack_clear(&mut self) {
        // TODO are 2 epochs needed?
        self.transitive_epoch = self.epoch + 2;
        self.pending_known_transitive_grey = self.pending.len();
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
                gc_arenas: HashSet::new(),
                pending_known_transitive_grey: 0,
                pending_known_grey: 0,
                pending: HashMap::new(),
                latest_grey: 0,
                epoch: 0,
                transitive_epoch: 0,
                invariant: None,
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

    loop {
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => types.reg(ty, id, bus),
                RegMsg::Un(id, ty) => {
                    // The thread was dropped
                    // let buses = types
                    //     .get_mut(&ty)
                    //     .expect("Impossible: Freed bus of type without an buses");
                    // let bus = buses
                    //     .get(&id)
                    //     .expect("Impossible: Freed thread's bus that does not exist");
                    // let bus = bus.0.lock().expect("Could not unlock freed bus");

                    // let inv = arena_invariants.entry(ty);

                    // for m in bus.iter() {
                    //     if let Msg::End {
                    //         next,
                    //         grey_feilds,
                    //         white_start,
                    //         white_end,
                    //         ..
                    //     } = m
                    //     {
                    //         mem.insert(Header::from(next) as usize, (*next, ty));
                    //         // TODO
                    //     }
                    // }

                    // buses.remove(&id);
                }
            }
        }

        types.active.iter_mut().map(|(_l, ts)| {});

        thread::sleep(Duration::from_millis(100))
    }
}
