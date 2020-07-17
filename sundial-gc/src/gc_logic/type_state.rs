use crate::*;
use arena::{bump_down, full, index, HeaderUnTyped, Msg, ARENA_SIZE};
use gc::RootIntern;
use gc_logic::{free_list::FreeList, BusPtr};
use mark::{EffTypes, GcTypeInfo, Handlers, Invariant, Tti, TypeRow};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::{
    cell::UnsafeCell, cmp::max, collections::HashSet, ops::Range, ptr, sync::atomic::Ordering,
    thread::ThreadId,
};

/// Each `Gc<T>` has a `TypeState` object.
/// Related `TypeState`s must be owned by the same thread.
///
/// Once we add multiple GC threads types,
/// related `TypeState`s will be bundled into a `RelatedTypeStates` object.
///
/// `RelatedTypeStates` will implement `Send`
pub(crate) struct TypeState {
    pub(crate) type_info: GcTypeInfo,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    arenas: UnsafeCell<Arenas>,
    // TODO support multiple simultaneous cycles.
    state: UnsafeCell<Option<Cycle>>,
    relations: UnsafeCell<ActiveRelations>,
}

impl TypeState {
    fn step(&self, free: &mut FreeList) {
        let state = unsafe { &mut *self.state.get() };
        let arenas = unsafe { &mut *self.arenas.get() };

        if let Some(s) = state {
            s.epoch += 1;
        };

        let mut grey: SmallVec<[(*const u8, u8); 16]> = SmallVec::new();

        self.buses.iter().for_each(|(_, bus)| {
            let mut has_gc_msg = false;
            let mut bus = bus.0.lock().expect("Could not unlock bus");
            log::trace!("GC Locked bus");
            grey.extend(bus.iter_mut().filter_map(|msg| match msg {
                Msg::Start {
                    white_start,
                    white_end,
                    next,
                } => {
                    log::trace!(
                        "GC received: {:?}",
                        Msg::Start {
                            white_start: *white_start,
                            white_end: *white_end,
                            next: *next
                        }
                    );
                    if let Some(state) = state {
                        state.start(*next, *white_start..*white_end);
                    };
                    *msg = Msg::Slot;
                    None
                }

                Msg::End {
                    release_to_gc,
                    new_allocation,
                    next,
                    grey_fields,
                    white_start,
                    white_end,
                } => {
                    let next = *next;

                    let ret = if let Some(state) = state {
                        if state.handler.invariant.contains(*white_start..*white_end) {
                            if !*new_allocation {
                                state.resolve(next)
                            };

                            // Nothing was marked.
                            // Allocated objects contain no condemned pointers into the white set.
                            if *grey_fields == 0b0000_0000 {
                                None
                            } else {
                                state.latest_grey = state.epoch;
                                Some((next, *grey_fields))
                            }
                        } else {
                            state.latest_grey = state.epoch;
                            Some((next, 0b1111_1111))
                        }
                    } else {
                        None
                    };

                    if *release_to_gc {
                        let header = HeaderUnTyped::from(next) as *mut _;
                        if full(next, self.type_info.align) {
                            log::trace!("Full Arena released");
                            arenas.full.insert(header);
                        } else {
                            log::trace!("Non full Arena released");
                            arenas.partial.0.insert(next as *mut _);
                        };

                        arenas.worker.remove(&(header as *const _));
                    } else {
                        debug_assert!(!full(next, self.type_info.align));
                        // store active worker arenas
                        arenas.worker.insert(HeaderUnTyped::from(next), next);
                    };

                    *msg = Msg::Slot;
                    ret
                }
                Msg::Gc { next, invariant } => {
                    has_gc_msg = true;
                    if next.is_none() {
                        *next = Some(arenas.partial.get_arena(&self.type_info, free));
                        log::trace!("GC sent Arena {:?}", *next);
                    };

                    if let Some(state) = state {
                        if state.handler.invariant != *invariant {
                            *invariant = state.handler.invariant
                        }
                    };

                    None
                }
                _ => None,
            }));

            // Add a GC message to the bus if it does not yet have one.
            if !has_gc_msg {
                log::trace!("Adding Msg::GC to bus");
                bus.iter_mut().filter(|m| m.is_slot()).next().map(|slot| {
                    let next = Some(arenas.partial.get_arena(&self.type_info, free));
                    if let Some(Cycle {
                        handler: HandlerManager { invariant, .. },
                        ..
                    }) = *state
                    {
                        *slot = Msg::Gc { next, invariant };
                    } else {
                        *slot = Msg::Gc {
                            next,
                            invariant: Invariant::none(),
                        };
                    }
                });
            };

            log::trace!("\n\n GC sent:\n {:?}\n\n", bus);
            drop(bus);
        });

        // trace grey, updating refs
        if let Some(Cycle {
            ref mut handler, ..
        }) = state
        {
            grey.iter().for_each(|(next, bits)| {
                handler.root(*next as *mut u8, self.type_info.align, self.type_info.size)
            });

            arenas.evac_roots(&self.type_info, handler, free);

            while handler.root_grey_children() {}
        }

        // TODO manage state and free memory
    }
}

struct Pending {
    /// Count of Arenas started before transitive_epoch.
    known_transitive_grey: usize,
    /// Count of Arenas started before `invariant`.
    known_grey: usize,
    /// `Arena.Header` started after `epoch`.
    /// `pending` is mutually exclusive with `pending_known_grey`.
    /// A arena `*const u8` cannot have a entry in `pending` and `pending_known_grey`.
    arenas: HashMap<*const HeaderUnTyped, usize>,
}

struct Cycle {
    handler: HandlerManager,
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
    pending: Pending,
    /// Direct parents that may contain a condemned ptr.
    waiting_direct: Vec<&'static TypeState>,
    /// Transitive parents that may contain a condemned ptr on a workers stack.
    /// A map of `TypeState` -> `epoch: usize`
    waiting_trans: Vec<(&'static TypeState, usize)>,
}

impl Cycle {
    /// `condemned` is the range the worker knew about.
    fn start(&mut self, next: *const u8, white: Range<usize>) {
        if self.handler.invariant.contains(white) {
            self.pending
                .arenas
                .insert(HeaderUnTyped::from(next), self.epoch);
        } else {
            self.latest_grey = self.epoch;
            self.pending.known_grey += 1;
        }
    }

    /// resolve must be called on every Arena.
    /// It's the dual of `TypeState.start`.
    fn resolve(&mut self, next: *const u8) {
        if let Some(e) = self.pending.arenas.remove(&HeaderUnTyped::from(next)) {
            if e < self.transitive_epoch {
                self.pending.known_transitive_grey -= 1;
            }
        } else {
            self.pending.known_grey -= 1;
        };
    }

    fn try_free(&self) -> bool {
        self.waiting_trans.is_empty()
    }
}

struct Arenas {
    /// The last seen `next`. ptr + size..high_offset is owned by gc
    /// Will not include all Arenas owned by workers.
    /// Only contains Arenas that the gc owns some of.
    worker: HashMap<*const HeaderUnTyped, *const u8>,
    /// Arena owned by GC, that have not been completely filled.
    partial: Partial,
    full: HashSet<*mut HeaderUnTyped>,
    condemned: HashSet<*mut u8>,
}

struct Partial(HashSet<*mut u8>);

impl Default for Partial {
    fn default() -> Self {
        Partial(HashSet::new())
    }
}

impl Default for Arenas {
    fn default() -> Self {
        Arenas {
            worker: HashMap::new(),
            partial: Partial::default(),
            full: HashSet::new(),
            condemned: HashSet::new(),
        }
    }
}

impl Partial {
    /// Reuse or allocate a new Arena
    fn get_arena(&mut self, type_info: &GcTypeInfo, free: &mut FreeList) -> *mut u8 {
        self.0.iter().cloned().next().unwrap_or(
            (free.alloc() as *mut _ as usize
                + HeaderUnTyped::high_offset(type_info.align, type_info.size) as usize)
                as *mut u8,
        )
    }
}

impl Arenas {
    fn evac_roots(
        &mut self,
        type_info: &GcTypeInfo,
        handler: &mut HandlerManager,
        free: &mut FreeList,
    ) {
        let Arenas {
            full,
            partial,
            condemned,
            ..
        } = self;
        condemned.iter().cloned().for_each(|next| {
            let header = unsafe { &*HeaderUnTyped::from(next) };
            let mut roots = header
                .roots
                .lock()
                .expect("Could not unlock roots, in TypeState::step");
            roots.drain().into_iter().for_each(|(idx, root_ptr)| {
                let GcTypeInfo { align, size, .. } = *type_info;
                let high =
                    header as *const _ as usize + HeaderUnTyped::high_offset(align, size) as usize;
                let gc = (high - ((idx as usize) * size as usize)) as *const u8;
                let HandlerManager {
                    invariant,
                    handlers,
                    evacuate_fn,
                    ..
                } = handler;
                evacuate_fn(gc, 0, invariant.grey_fields, invariant, handlers);

                let root = unsafe { &mut *(root_ptr as *mut RootIntern<u8>) };
                if root.ref_count.load(Ordering::Relaxed) == 0 {
                    unsafe { ptr::drop_in_place(root) }
                } else {
                    let next = partial.get_arena(type_info, free);

                    unsafe { ptr::copy(gc, next, 1) }

                    let mut roots =
                        unsafe { &mut *(HeaderUnTyped::from(next) as *mut HeaderUnTyped) }
                            .roots
                            .lock()
                            .unwrap();

                    if !roots.insert(index(next, size, align), root_ptr).is_none() {
                        panic!("Not possible");
                    };

                    let ptr = bump_down(next, size, align);

                    if ptr as usize % ARENA_SIZE == 0 {
                        full.insert(ptr as *mut _);
                    } else {
                        partial.0.insert(ptr as *mut _);
                    }
                };
            });
            assert_eq!(0, roots.len());
        });
    }
}

struct HandlerManager {
    handlers: Handlers,
    eff_types: EffTypes,
    invariant: Invariant,
    /// fn( *const u8, offset: Offset, grey_fields: u8, invariant: *const Invariant, handlers: *mut Handlers )
    evacuate_fn: fn(*const u8, u8, u8, *const Invariant, *mut Handlers),
    /// A snapshot of nexts at last call to `unclean_children`.
    last_nexts: SmallVec<[*mut u8; 4]>,
}

impl HandlerManager {
    /// `next` may point to the next free slot or if the arena is full the Header.
    fn root(&mut self, next: *mut u8, align: u16, size: u16) {
        let HandlerManager {
            handlers,
            invariant,
            evacuate_fn,
            ..
        } = self;

        let header = HeaderUnTyped::from(next) as usize;
        let low = header + HeaderUnTyped::low_offset(align) as usize;
        let last = max(low, size as usize + next as usize);
        let mut ptr = header + HeaderUnTyped::high_offset(align, size) as usize;

        while ptr >= last {
            let t = unsafe { &*(ptr as *const u8) };
            // TODO(mark_evac) There's redundancy in the signature of evacuate.
            evacuate_fn(t, 0, invariant.grey_fields, invariant, handlers);
            ptr -= size as usize;
        }
    }

    /// Returns `true` when new values were evacuated.
    fn root_grey_children(&mut self) -> bool {
        let HandlerManager {
            handlers,
            eff_types,
            last_nexts,
            ..
        } = self;

        if handlers.nexts == *last_nexts {
            return false;
        } else {
            log::trace!("last_nexts: {:?}", last_nexts);
            log::trace!("this_nexts: {:?}", handlers.nexts);
        };

        handlers
            .nexts
            .iter()
            .enumerate()
            .zip(last_nexts.iter())
            // We only care about change next ptrs
            .filter_map(|(n, o)| if n.1 == o { None } else { Some(n) })
            .for_each(|(i, next)| {
                let child_ts = eff_types[i];

                // Root partial Arenas if child is undergoing a GC.
                if let Some(state) = unsafe { &mut *child_ts.state.get() } {
                    state
                        .handler
                        .root(*next, child_ts.type_info.align, child_ts.type_info.size);
                };
            });

        handlers
            .filled
            .iter_mut()
            .enumerate()
            .for_each(|(i, headers)| {
                let child_ts = eff_types[i];
                let child_arenas = unsafe { &mut *child_ts.arenas.get() };

                headers.iter().for_each(|header| {
                    let b = child_arenas.full.insert(*header);
                    // Assert full will never have `header`.
                    debug_assert!(b);
                });

                // Root filled Arenas if child is undergoing a GC.
                if let Some(child_state) = unsafe { &mut *child_ts.state.get() } {
                    headers.iter().for_each(|header| {
                        child_state.handler.root(
                            *header as *mut u8,
                            child_ts.type_info.align,
                            child_ts.type_info.size,
                        );
                    });
                };
            });

        *last_nexts = handlers.nexts.clone();
        true
    }
}

/// All known relations between GCed types.
/// New relationships are first known upon `TotalRelations::register`.
/// `TotalRelations::register` is triggered by a worker thread's first `Arena::<T>::new()`
struct TotalRelations {
    direct_parents: HashMap<GcTypeInfo, Vec<(GcTypeInfo, TypeRow)>>,
    transitive_parents: HashMap<GcTypeInfo, Vec<GcTypeInfo>>,
    direct_children: HashMap<GcTypeInfo, Vec<(GcTypeInfo, TypeRow)>>,
    transitive_children: HashMap<GcTypeInfo, Vec<GcTypeInfo>>,
}

impl Default for TotalRelations {
    fn default() -> Self {
        Self {
            direct_parents: HashMap::new(),
            transitive_parents: HashMap::new(),
            direct_children: HashMap::new(),
            transitive_children: HashMap::new(),
        }
    }
}

impl TotalRelations {
    fn register(
        &mut self,
        active: &mut HashMap<&'static GcTypeInfo, &'static TypeState>,
        type_info: GcTypeInfo,
        t_id: ThreadId,
        bus: BusPtr,
    ) {
        let ts = active.get(&type_info).cloned().unwrap_or_else(|| {
            let ts = Box::leak(Box::new(TypeState {
                type_info,
                buses: Default::default(),
                arenas: UnsafeCell::new(Arenas::default()),
                state: UnsafeCell::new(None),
                relations: UnsafeCell::new(self.new_active(&type_info, active)),
            }));
            let ts = &*ts;
            active.entry(&ts.type_info).or_insert(ts);

            // Add references to the new `TypeState` to related `TypeState`s
            let relations = unsafe { &*ts.relations.get() };
            relations
                .direct_parents
                .iter()
                .for_each(|(parent_ts, type_row)| {
                    let parent_relations = unsafe { &mut *parent_ts.relations.get() };
                    debug_assert!(parent_relations
                        .direct_children
                        .iter()
                        .find(|cts| cts.0.type_info == ts.type_info)
                        .is_none());
                    parent_relations
                        .direct_children
                        .push((ts, type_row.clone()))
                });

            relations.transitive_parents.iter().for_each(|parent_ts| {
                let parent_relations = unsafe { &mut *parent_ts.relations.get() };
                debug_assert!(parent_relations
                    .transitive_children
                    .iter()
                    .find(|cts| cts.type_info == ts.type_info)
                    .is_none());
                parent_relations.transitive_children.push(ts)
            });

            relations
                .direct_children
                .iter()
                .for_each(|(child_ts, type_row)| {
                    let parent_relations = unsafe { &mut *child_ts.relations.get() };
                    debug_assert!(parent_relations
                        .direct_parents
                        .iter()
                        .find(|cts| cts.0.type_info == ts.type_info)
                        .is_none());
                    parent_relations.direct_parents.push((ts, type_row.clone()));
                });
            ts
        });
    }
}

/// Relations between GCed types with a live `TypeState` object.
/// In the future `TypeState` may be deallocated, if an Arena<T> has not existed for a time.
struct ActiveRelations {
    // TODO revisit sizes
    direct_parents: SmallVec<[(&'static TypeState, TypeRow); 3]>,
    transitive_parents: SmallVec<[&'static TypeState; 7]>,
    direct_children: SmallVec<[(&'static TypeState, TypeRow); 3]>,
    transitive_children: SmallVec<[&'static TypeState; 7]>,
}

impl TotalRelations {
    pub fn new_active(
        &mut self,
        type_info: &GcTypeInfo,
        active: &HashMap<&'static GcTypeInfo, &'static TypeState>,
    ) -> ActiveRelations {
        let TotalRelations {
            direct_parents,
            transitive_parents,
            direct_children,
            transitive_children,
        } = self;
        let active_dc = direct_children.entry(*type_info).or_insert_with(|| {
            let mut dc = HashMap::new();
            type_info.direct_gc_types_fn()(&mut dc, 0);
            dc.into_iter()
                .map(|(child_type_info, tr)| {
                    let dp = direct_parents.entry(child_type_info).or_default();
                    dp.push((*type_info, tr.clone()));
                    (child_type_info, tr)
                })
                .collect()
        });

        let active_tc = transitive_children.entry(*type_info).or_insert_with(|| {
            let mut tc = Tti::new();
            type_info.transitive_gc_types_fn()(&mut tc);
            tc.type_info
                .into_iter()
                .map(|child_type_info| {
                    let tp = transitive_parents.entry(child_type_info).or_default();
                    tp.push(*type_info);
                    child_type_info
                })
                .collect()
        });

        ActiveRelations {
            direct_children: active_dc
                .into_iter()
                .filter_map(|(child_type_info, tr)| {
                    active.get(child_type_info).map(|ti| (*ti, tr.clone()))
                })
                .collect(),

            transitive_children: active_tc
                .into_iter()
                .filter_map(|child_type_info| active.get(child_type_info))
                .cloned()
                .collect(),

            direct_parents: direct_parents
                .get(type_info)
                .map(|dp| {
                    dp.iter()
                        .filter_map(|(parent_type_info, tr)| {
                            active.get(parent_type_info).map(|ts| (*ts, tr.clone()))
                        })
                        .collect()
                })
                .unwrap_or_default(),
            transitive_parents: transitive_parents
                .get(type_info)
                .map(|tp| {
                    tp.iter()
                        .filter_map(|parent_type_info| active.get(parent_type_info))
                        .cloned()
                        .collect()
                })
                .unwrap_or_default(),
        }
    }
}
