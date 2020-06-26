use crate::arena::*;
use crate::mark::*;
use lazy_static::lazy_static;
use smallvec::{smallvec, SmallVec};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::sync::{
    atomic::{AtomicPtr, Ordering},
    mpsc::*,
    RwLock,
};
use std::thread::{self, ThreadId};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    mem,
    ops::Range,
    ptr,
    time::Duration,
};

pub(crate) static mut REGISTER: AtomicPtr<Sender<RegMsg>> = AtomicPtr::new(ptr::null_mut());

#[derive(Copy, Clone)]
pub(crate) struct BusPtr(&'static Bus);

impl BusPtr {
    pub unsafe fn new(ptr: *mut Bus) -> Self {
        BusPtr(&*ptr)
    }
}

unsafe impl Send for BusPtr {}
unsafe impl Sync for BusPtr {}

#[derive(Copy, Clone)]
pub(crate) enum RegMsg {
    Reg(ThreadId, GcTypeInfo, BusPtr),
    Un(ThreadId, GcTypeInfo),
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

/// This whole struct is unsafe!
struct TypeState {
    type_info: GcTypeInfo,
    handlers: Option<Handlers>,
    handlers_types: Vec<GcTypeInfo>,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    /// The last seen `next`. ptr + size..high_offset is owned by gc
    /// Will not include all Arenas owned by workers.
    /// Only contains Arenas that the gc owns some of.
    worker_arenas: HashMap<*const HeaderUnTyped, *const u8>,
    /// Arena owned by GC, that have not been completely filled.
    gc_arenas: BTreeSet<*mut u8>,
    gc_arenas_full: BTreeSet<*mut HeaderUnTyped>,
    /// Count of Arenas started before transitive_epoch.
    pending_known_transitive_grey: usize,
    /// Count of Arenas started before `invariant`.
    pending_known_grey: usize,
    /// `Arena.Header` started after `epoch`.
    /// `pending` is mutually exclusive with `pending_known_grey`.
    /// A arena `*const u8` cannot have a entry in `pending` and `pending_known_grey`.
    pending: HashMap<*const HeaderUnTyped, usize>,
    /// The last epoch we saw a grey in.
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
    /// `free` must be uninitialized.
    fn step(&mut self, free: &mut Vec<*mut HeaderUnTyped>) -> bool {
        let Self {
            type_info,
            buses,
            worker_arenas,
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
            handlers,
            ..
        } = self;
        *epoch += 1;

        // TODO lift allocation
        let mut grey: SmallVec<[(*const u8, u8); 8]> = SmallVec::new();
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
                        let next = *next;
                        pending.insert(HeaderUnTyped::from(next), *epoch);
                    } else {
                        *latest_grey = *epoch;
                        *pending_known_grey += 1;
                    }
                    // *msg = Msg::Slot;
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
                    let next = *next;
                    let ret = invariant
                        .map(|i| {
                            if i.white_start == *white_start && i.white_end == *white_end {
                                if let Some(e) = pending.remove(&HeaderUnTyped::from(next)) {
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
                                    None
                                } else {
                                    *latest_grey = *epoch;
                                    Some((next, *grey_feilds))
                                }
                            } else {
                                *latest_grey = *epoch;
                                Some((next, 0b1111_1111))
                            }
                        })
                        .flatten();

                    if *release_to_gc {
                        if full(next, type_info.align) {
                            gc_arenas.insert(next as *mut _);
                        } else {
                            let header = HeaderUnTyped::from(next) as *mut _;
                            gc_arenas_full.insert(header);
                        };
                    } else {
                        // store active worker arenas
                        worker_arenas.insert(HeaderUnTyped::from(next), next);
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
                        .or(free.pop().map(|header| {
                            HeaderUnTyped::init(header);
                            (header as usize
                                + HeaderUnTyped::high_offset(type_info.align, type_info.size)
                                    as usize) as *mut u8
                        }));

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

        // trace grey, updating refs
        if let Some(inv) = *invariant {
            let handlers = handlers
                .as_mut()
                .expect("handlers must be Some if an invariant is Some");

            grey.iter().for_each(|(next, bits)| {
                let GcTypeInfo {
                    align,
                    size,
                    evacuate_fn,
                    ..
                } = *type_info;
                let next = *next;
                let mut ptr = HeaderUnTyped::from(next) as usize
                    + HeaderUnTyped::high_offset(align, size) as usize;

                while ptr != next as usize {
                    let t = unsafe { &*(ptr as *const u8) };
                    unsafe {
                        mem::transmute::<_, fn(*const u8, u8, u8, Range<usize>, *mut Handlers)>(
                            evacuate_fn,
                        )(
                            t, 0, *bits, inv.white_start..inv.white_end, handlers
                        )
                    };
                    ptr -= size as usize;
                }
            });
        }

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
        self.transitive_epoch = self.epoch + 2;
        self.pending_known_transitive_grey = self.pending.len();
    }

    /// Must be proceed by a call to `request_clear_stack`.
    fn is_stack_clear(&self) -> bool {
        self.pending_known_transitive_grey == 0
            && self.pending_known_grey == 0
            && self.transitive_epoch != 0
    }

    fn set_invariant(&mut self, inv: Invariant, handlers: Handlers) {
        self.phase2 = false;
        self.pending_known_transitive_grey = 0;
        self.pending_known_grey = self.pending.len();
        // It will be one upon first `step()`
        self.epoch = 0;
        self.latest_grey = 1;
        self.pending = HashMap::new();
        self.invariant = Some(inv);
        self.handlers = Some(handlers);
    }
}

struct TypeRelations {
    active: HashMap<GcTypeInfo, TypeState>,
    parents: HashMap<GcTypeInfo, Parents>,
}

impl TypeRelations {
    pub fn new() -> Self {
        Self {
            active: HashMap::new(),
            parents: HashMap::new(),
        }
    }

    fn reg(&mut self, type_info: GcTypeInfo, t_id: ThreadId, bus: BusPtr) {
        let Self {
            active, parents, ..
        } = self;
        let ts = active.entry(type_info).or_insert_with(|| {
            let mut direct_children: HashMap<GcTypeInfo, TypeRow> = HashMap::new();
            let direct_gc_types = unsafe {
                mem::transmute::<_, fn(&mut HashMap<GcTypeInfo, TypeRow>, u8)>(
                    type_info.direct_gc_types_fn,
                )
            };
            direct_gc_types(&mut direct_children, 0);
            direct_children.iter().for_each(|(child, row)| {
                let cp = parents.entry(*child).or_default();
                cp.direct.insert(type_info, row.1);
            });

            let mut tti = Tti::new();
            let tti_fn =
                unsafe { mem::transmute::<_, fn(*mut Tti)>(type_info.transitive_gc_types_fn) };
            tti_fn(&mut tti as *mut Tti);
            tti.type_info.iter().for_each(|child| {
                let cp = parents.entry(*child).or_default();
                cp.transitive.insert(type_info);
            });

            TypeState {
                type_info,
                handlers: None,
                handlers_types: Vec::new(),
                buses: HashMap::new(),
                worker_arenas: HashMap::new(),
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

/// Start the Gc thread if needed and return type registration bus.
pub(crate) fn get_sender() -> Sender<RegMsg> {
    let reg = unsafe { REGISTER.load(Ordering::Relaxed) };
    if !reg.is_null() {
        let s = unsafe { &*reg };
        s.clone()
    } else {
        let (mut s, r) = channel();
        let pre = unsafe { REGISTER.compare_and_swap(ptr::null_mut(), &mut s, Ordering::AcqRel) };
        if pre.is_null() {
            thread::spawn(move || gc_loop(r));
            s
        } else {
            get_sender()
        }
    }
}

fn gc_loop(r: Receiver<RegMsg>) {
    let mut free: Vec<*mut HeaderUnTyped> = Vec::with_capacity(100);
    let mut types = TypeRelations::new();
    let mut pre_arena_count: usize = 1;

    loop {
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => types.reg(ty, id, bus),
                RegMsg::Un(id, ty) => {
                    // The thread was dropped
                    let type_state = types.active.get_mut(&ty).unwrap();
                    type_state.step(&mut free);
                    type_state.buses.remove(&id);
                }
            }
        }

        let TypeRelations { parents, active } = &mut types;

        let mut stack_cleared = HashSet::with_capacity(20);
        let mut clear_stack_old: HashMap<GcTypeInfo, HashSet<GcTypeInfo>> =
            HashMap::with_capacity(20);
        let mut clear_stack_new: HashMap<GcTypeInfo, HashSet<GcTypeInfo>> =
            HashMap::with_capacity(20);

        let mut gc_in_progress = false;
        let mut arena_count: usize = 0;

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

            if ts.invariant.is_some() {
                gc_in_progress = true;
            };

            arena_count += ts.gc_arenas_full.len();
        });

        let mut gen2_arenas: HashMap<
            GcTypeInfo,
            (SmallVec<[*mut HeaderUnTyped; 1]>, Vec<*mut u8>),
        > = HashMap::new();
        stack_cleared.drain().for_each(|ti| {
            clear_stack_old.remove(&ti).unwrap().iter().for_each(|cti| {
                let ts = active.get_mut(cti).unwrap();
                ts.waiting_on_transitive_parents -= 1;
                if ts.waiting_on_transitive_parents == 0 {
                    let inv = ts.invariant.unwrap();
                    // Free memory
                    ts.gc_arenas_full
                        .range((inv.white_start as *mut _)..(inv.white_end as *mut _))
                        .into_iter()
                        .for_each(|header| {
                            let header = *header;

                            if cti.needs_drop {
                                let GcTypeInfo {
                                    align,
                                    size,
                                    drop_in_place_fn,
                                    ..
                                } = ti;
                                let low =
                                    header as usize + HeaderUnTyped::low_offset(align) as usize;
                                let mut ptr =
                                    low as usize + HeaderUnTyped::high_offset(align, size) as usize;

                                while ptr >= low {
                                    unsafe {
                                        mem::transmute::<_, fn(*mut u8)>(drop_in_place_fn)(
                                            ptr as *mut u8,
                                        )
                                    }
                                    ptr -= size as usize;
                                }
                            };

                            unsafe { ptr::drop_in_place(header) }
                            if free.len() < arena_count {
                                free.push(header);
                            } else {
                                unsafe { System.dealloc(header as *mut u8, Layout::new::<Mem>()) };
                            }
                        });

                    ts.invariant = None;
                    ts.handlers.take().map(|h| {
                        h.filled
                            .into_iter()
                            .enumerate()
                            .into_iter()
                            .for_each(|(i, headers)| {
                                gen2_arenas
                                    .entry(ts.handlers_types[i])
                                    .and_modify(|(hs, _)| hs.extend(headers.iter().cloned()))
                                    .or_insert((headers, Vec::new()));
                            });

                        h.nexts
                            .into_iter()
                            .enumerate()
                            .into_iter()
                            .for_each(|(i, next)| {
                                if HeaderUnTyped::from(next) == next as *const _ {
                                    gen2_arenas
                                        .entry(ts.handlers_types[i])
                                        .and_modify(|(hs, _)| hs.push(next as *mut HeaderUnTyped))
                                        .or_insert((
                                            smallvec![next as *mut HeaderUnTyped],
                                            Vec::new(),
                                        ));
                                } else {
                                    gen2_arenas
                                        .entry(ts.handlers_types[i])
                                        .and_modify(|(_, nx)| nx.push(next))
                                        .or_insert((SmallVec::new(), vec![next]));
                                }
                            });
                    });
                }
            })
        });

        // Reset TypeState by adding contents of Handlers
        gen2_arenas
            .drain()
            .into_iter()
            .for_each(|(ti, (headers, next))| {
                let ts = active.get_mut(&ti).unwrap();
                ts.gc_arenas_full.extend(headers.into_iter());
                ts.gc_arenas.extend(next.into_iter());
            });

        clear_stack_new.drain().for_each(|(pti, mut ctis)| {
            active.get_mut(&pti).unwrap().request_clear_stack();
            clear_stack_old.entry(pti).and_modify(|s| {
                ctis.drain().for_each(|c| {
                    s.insert(c);
                });
            });
        });

        if !gc_in_progress && arena_count > pre_arena_count {
            pre_arena_count = arena_count;
            active.iter_mut().for_each(|(ti, ts)| {
                let inv = Invariant {
                    white_start: 0,
                    white_end: usize::MAX,
                    grey_feilds: 0b1111_1111,
                    grey_self: true,
                };

                let direct_gc_types = unsafe {
                    mem::transmute::<_, fn(&mut HashMap<GcTypeInfo, TypeRow>, u8)>(
                        ti.direct_gc_types_fn,
                    )
                };
                let translator_from = unsafe {
                    mem::transmute::<_, fn(&Vec<GcTypeInfo>) -> (Translator, u8)>(
                        ti.translator_from_fn,
                    )
                };

                let mut dgt = HashMap::with_capacity(20);
                direct_gc_types(&mut dgt, 0);
                let children: Vec<_> = dgt.into_iter().map(|(i, _)| i).collect();

                let mut nexts = SmallVec::with_capacity(children.len());
                let mut filled = SmallVec::new();
                for _ in 0..children.len() {
                    filled.push(SmallVec::new());
                    nexts.push(
                        free.pop()
                            .map(|header| {
                                HeaderUnTyped::init(header);
                                (header as usize
                                    + HeaderUnTyped::high_offset(ti.align, ti.size) as usize)
                                    as *mut u8
                            })
                            .unwrap_or(Arena::alloc()),
                    );
                }

                let handlers = Handlers {
                    translator: translator_from(&children).0,
                    nexts,
                    filled,
                    free: &mut free as *mut _,
                };
                ts.set_invariant(inv, handlers);

                ts.gc_arenas_full.iter().for_each(|header| {
                    let header = unsafe { &mut **header };
                    header.condemned = true;
                });
            });

            active.iter_mut().for_each(|(ti, ts)| {
                let inv = ts.invariant.unwrap();
                let handlers = ts
                    .handlers
                    .as_mut()
                    .expect("handlers must be Some if an invariant is Some");

                ts.gc_arenas.iter().for_each(|next| {
                    // trace grey, updating refs
                    let GcTypeInfo {
                        align,
                        size,
                        evacuate_fn,
                        ..
                    } = *ti;
                    let next = *next;
                    let mut ptr = HeaderUnTyped::from(next) as usize
                        + HeaderUnTyped::high_offset(align, size) as usize;

                    while ptr != next as usize {
                        let t = unsafe { &*(ptr as *const u8) };
                        unsafe {
                            mem::transmute::<_, fn(*const u8, u8, u8, Range<usize>, *mut Handlers)>(
                                evacuate_fn,
                            )(
                                t,
                                0,
                                inv.grey_feilds,
                                inv.white_start..inv.white_end,
                                handlers,
                            )
                        };
                        ptr -= size as usize;
                    }
                });
            });
        }

        thread::sleep(Duration::from_millis(100))
    }
}
