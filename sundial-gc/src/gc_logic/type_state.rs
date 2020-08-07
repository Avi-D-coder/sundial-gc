use crate::*;
use arena::{bump_down, full, index, HeaderUnTyped, ARENA_SIZE};
use bus::WorkerMsg;
use gc::RootIntern;
use gc_logic::{
    bus::{self, Msg},
    free_list::FreeList,
    BusPtr,
};
use mark::{EffTypes, GcTypeInfo, Handlers, Invariant, Tti, TypeRow};
use smallvec::SmallVec;
use std::{
    cell::{Cell, UnsafeCell},
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::{Deref, DerefMut},
    ptr,
    sync::atomic::Ordering,
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
    /// Map worker thread => (Bus, worker arena count).
    buses: UnsafeCell<HashMap<ThreadId, (BusPtr, usize)>>,
    /// Has the invariant be sent to workers.
    /// TODO(optimization) delay sending invariant.
    pub sent_invariant: Cell<bool>,
    pub invariant_id: Cell<u8>,
    pub arenas: UnsafeCell<Arenas>,
    pending: UnsafeCell<Pending>,
    // TODO support multiple simultaneous cycles.
    pub state: UnsafeCell<Option<Collection>>,
    pub relations: UnsafeCell<ActiveRelations>,
}

impl Debug for TypeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("TypeState::{}", self.type_info.type_name))
            .field("arenas", unsafe { &*self.arenas.get() })
            .field("pending", unsafe { &*self.pending.get() })
            .field("collection", unsafe { &*self.state.get() })
            .finish()
    }
}

impl PartialEq for TypeState {
    /// Ptr equality for `TypeState`.
    fn eq(&self, other: &Self) -> bool {
        self as *const _ == other as *const _
    }
}

impl TypeState {
    /// For safe usage See `TypeState` docs.
    /// Returns true when collection is complete.
    pub(crate) unsafe fn step(&self, free: &mut FreeList) -> bool {
        log::info!("{:?}", self);
        let mut done = false;
        let state = &mut *self.state.get();
        let arenas: &mut Arenas = &mut *self.arenas.get();
        let pending = &mut *self.pending.get();
        let buses = &mut *self.buses.get();

        pending.epoch += 1;

        let mut grey: SmallVec<[(*const u8, u8); 16]> = SmallVec::new();

        buses.iter_mut().for_each(|(thread_id, (bus, count))| {
            let msgs = bus::reduce(&mut bus.lock().expect("Could not unlock bus"));
            log::trace!("Thread: {:?}, received: {:?}", thread_id, msgs);

            grey.extend(msgs.into_iter().filter_map(|msg| match msg {
                WorkerMsg::Start { invariant_id, next } => {
                    log::info!(
                        "GC received from thread: {:?}: {:?}",
                        thread_id,
                        WorkerMsg::Start { invariant_id, next }
                    );
                    if let Some(cycle) = state {
                        if cycle.handler.invariant.id == invariant_id {
                            pending
                                .arenas
                                .insert(HeaderUnTyped::from(next), pending.epoch);
                        } else {
                            cycle.latest_grey = pending.epoch;
                            pending.known_grey += 1;
                        }
                    } else {
                        if invariant_id != 0 {
                            pending.arenas.insert(HeaderUnTyped::from(next), 0);
                        } else {
                            pending.known_grey += 1;
                        };
                    };

                    None
                }

                WorkerMsg::End {
                    release_to_gc,
                    new_allocation,
                    next,
                    grey_fields,
                    invariant_id,
                } => {
                    let header = HeaderUnTyped::from(next);

                    log::info!(
                        "GC received from thread: {:?}: {:?}, header: {:?}",
                        thread_id,
                        WorkerMsg::End {
                            next,
                            new_allocation,
                            release_to_gc,
                            grey_fields,
                            invariant_id,
                        },
                        header
                    );

                    // Option::map makes the borrow checker unhappy.
                    let ret = if let Some(cycle) = state {
                        debug_assert_eq!(cycle.handler.invariant.id, self.invariant_id.get());
                        if cycle.handler.invariant.id == invariant_id || invariant_id == 0 {
                            if !new_allocation {
                                pending.arenas.remove(&header);
                            };

                            // Nothing was marked.
                            // Allocated objects contain no condemned pointers into the white set.
                            if grey_fields == 0b0000_0000 {
                                None
                            } else {
                                Some((next, grey_fields))
                            }
                        } else {
                            Some((next, 0b1111_1111))
                        }
                    } else {
                        None
                    };

                    if !new_allocation
                        && invariant_id != self.invariant_id.get()
                        && invariant_id != 0
                        && arenas.worker.contains_key(&header)
                    {
                        pending.known_grey -= 1;
                    };

                    if release_to_gc {
                        let top = arenas
                            .worker
                            .remove(&header)
                            .map(|(_, t)| t)
                            .unwrap_or_else(|| {
                                debug_assert!(new_allocation);
                                header as usize
                                    + HeaderUnTyped::high_offset(
                                        self.type_info.align,
                                        self.type_info.size,
                                    ) as usize
                            });

                        if full(next, self.type_info.align) {
                            log::trace!("Full Arena released: header: {:?}", header);
                            arenas.full.insert(header, top);
                        } else {
                            log::trace!(
                                "Non full Arena released: header {:?}, next: {:?}",
                                header,
                                next
                            );
                            arenas.partial.insert(header, (next, top));
                        };

                        if !new_allocation {
                            // FIXME this should not be needed
                            *count = count.saturating_sub(1);
                        };
                    } else {
                        // store active worker arenas
                        arenas
                            .worker
                            .entry(HeaderUnTyped::from(next))
                            .and_modify(|(n, _)| *n = next)
                            .or_insert({
                                *count += 1;
                                (
                                    next,
                                    HeaderUnTyped::from(next) as usize
                                        + HeaderUnTyped::high_offset(
                                            self.type_info.align,
                                            self.type_info.size,
                                        ) as usize,
                                )
                            });
                    };

                    ret
                }
            }));

            if !self.sent_invariant.get() {
                let inv = Msg::Invariant(
                    state
                        .as_ref()
                        .map(|cycle| cycle.handler.invariant)
                        .unwrap_or_else(|| Invariant::none(0)),
                );

                let mut bus = bus.lock().expect("Could not unlock bus");
                // Send invariant replacing invariant bus if it exists.
                if let Some(slot) = bus.iter_mut().find(|msg| msg.is_invariant()) {
                    *slot = inv
                } else {
                    bus.push(inv)
                };

                log::trace!("GC sent thread {:?}, {:?}", thread_id, inv);
            };

            if *count < 2 {
                *count += 1;
                let (next, top) = arenas.partial.remove_arena(&self.type_info, free);
                let header = HeaderUnTyped::from(next);

                arenas.worker.insert(header, (next, top));
                log::trace!("GC sent header: {:?}, next: {:?}", header, next);
                let mut bus = bus.lock().expect("Could not unlock bus");
                bus.push(Msg::Next(next as _))
            };

            // log::trace!("\n\n GC sent:\n {:?}\n\n", bus);
        });

        self.sent_invariant.set(true);

        // trace grey, updating refs
        if let Some(cycle) = state {
            let relations = &mut *self.relations.get();

            grey.iter().for_each(|(next, bits)| {
                // Grey worker marked types.
                relations
                    .direct_children
                    .iter()
                    .for_each(|(cts, (_, offsets))| {
                        let epoch = (&mut *cts.pending.get()).epoch;
                        let state = &mut *cts.state.get();

                        state.iter_mut().for_each(|cycle| {
                            if *offsets & *bits == 0b0 {
                                cycle.latest_grey = epoch;
                            };
                        });
                    });

                cycle
                    .handler
                    .root(*next as *mut u8, self.type_info.align, self.type_info.size)
            });

            cycle.evac_roots(&self.type_info, arenas, free);

            while cycle.handler.root_grey_children() {}

            if cycle.safe_to_free(pending, relations) {
                log::trace!("safe_to_free: {}", self.type_info.type_name);

                cycle.condemned.full.drain().for_each(|(header, top)| {
                    self.drop_arena(
                        (header as usize + HeaderUnTyped::low_offset(self.type_info.align) as usize)
                            as *mut u8,
                        top,
                        free,
                    )
                });

                cycle
                    .condemned
                    .partial
                    .0
                    .drain()
                    .into_iter()
                    .for_each(|(_, (next, top))| self.drop_arena(next as _, top, free));

                if self.type_info.needs_drop {
                    cycle.condemned.worker.drain().for_each(|(_, range)| {
                        self.drop_objects(HeaderUnTyped::from(range.0) as *mut _, range)
                    });
                };

                let children_complete = relations.direct_children.iter().all(|(cts, _)| {
                    { &*cts.state.get() }
                        .as_ref()
                        .map(|cycle| cycle.condemned.is_empty())
                        .unwrap_or(true)
                });

                if children_complete {
                    log::trace!("Children complete Collection cleared!");
                    // This will trigger the Invariant::none to be sent to the workers.
                    self.sent_invariant.set(false);
                    debug_assert_eq!(pending.known_grey, 0);
                    pending.known_grey = pending.arenas.len();
                    pending.arenas.clear();
                    *state = None;
                    done = true;
                };
            };
        };

        done
    }

    unsafe fn drop_arena(&self, next: *mut u8, top: usize, free: &mut FreeList) {
        let header = HeaderUnTyped::from(next) as *mut HeaderUnTyped;
        log::trace!("drop_arena(header: {:?}, next: {:?})", header, next);

        let roots = { &*header }
            .roots
            .lock()
            .expect("Could not unlock roots while freeing Arena");
        if roots.len() != 0 {
            // FIXME should reset latest_grey and try again latter.
            panic!("Old Arena Rooted");
        }

        if self.type_info.needs_drop {
            self.drop_objects(header, (next, top));
        };

        free.dealloc(header)
    }

    /// Runs destructor on objects from the top of the `Arena` to next;
    unsafe fn drop_objects(&self, header: *mut HeaderUnTyped, range: (*const u8, usize)) {
        debug_assert!(self.type_info.needs_drop);

        let GcTypeInfo { align, size, .. } = self.type_info;
        let low = max(
            range.0 as usize + size as usize,
            header as usize + HeaderUnTyped::low_offset(align) as usize,
        );
        let mut ptr = range.1 as usize;

        let drop_in_place = self.type_info.drop_in_place_fn();
        let evacuated = { &mut *header }.evacuated.lock().unwrap();

        while ptr >= low {
            // TODO sort and iterate between evacuated
            if !evacuated.contains_key(&index(ptr as *const u8, size, align)) {
                // log::trace!("drop ptr: {:?}", ptr as *mut u8);
                drop_in_place(ptr as *mut u8)
            };
            ptr -= size as usize;
        }
    }

    pub unsafe fn reset(&self) {
        debug_assert!(self.state.get().as_ref().unwrap().is_none());

        let pending = &mut *self.pending.get();
        pending.epoch = 0;
        pending.arenas.iter_mut().for_each(|(_, epoch)| *epoch = 1);
    }
}

#[derive(Debug)]
struct Pending {
    /// `epoch` is not synchronized across threads.
    /// 2 epochs delineate Arena age.
    /// 2 epoch are needed, since thread A may receive a GCed object from thread B in between the
    ///   GC reading A & B's messages.
    /// A's `Msg::Start` may be received in the next epoch after B's `Msg::End`.
    /// `epoch` counts from 1, and resets when free is accomplished.
    epoch: usize,
    // transitive_epoch: usize,
    /// Count of Arenas started before transitive_epoch.
    // known_transitive_grey: usize,
    /// Count of Arenas that are not a part of a `Collection` cycle.
    /// Known grey do not uphold any `Collection` variant.
    known_grey: usize,
    /// `Arena.Header` started after `epoch`.
    /// `pending` is mutually exclusive with `pending_known_grey`.
    /// A arena `*const u8` cannot have a entry in `pending` and `pending_known_grey`.
    arenas: HashMap<*const HeaderUnTyped, usize>,
}

/// A single collection cycle.
pub(crate) struct Collection {
    handler: HandlerManager,
    /// The last epoch a condemned ptr of this type was seen in.
    latest_grey: usize,
    /// Arenas being freed.
    pub condemned: Arenas,
    /// Transitive parents that may contain a condemned ptr on a workers stack.
    /// A map of `TypeState` -> `epoch: usize`
    waiting_transitive_parents: SmallVec<[(&'static TypeState, usize); 5]>,
    waiting_transitive: bool,
}

impl Debug for Collection {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Collection")
            .field("latest_grey", &self.latest_grey)
            .field("condemned", &self.condemned)
            .finish()
    }
}

impl Collection {
    // TODO add granular cycles.
    pub fn new(
        type_info: &GcTypeInfo,
        relations: &ActiveRelations,
        free: &mut FreeList,
        invariant_id: u8,
    ) -> Collection {
        let direct_children: EffTypes = relations
            .direct_children
            .iter()
            .map(|(ts, _)| *ts)
            .collect();

        let last_nexts: SmallVec<_> = direct_children
            .iter()
            .map(|ts| {
                (free.alloc() as *mut _ as usize
                    + HeaderUnTyped::high_offset(ts.type_info.align, ts.type_info.size) as usize)
                    as *mut u8
            })
            .collect();

        Collection {
            handler: HandlerManager {
                handlers: Handlers {
                    translator: type_info.translator_from_fn()(&direct_children).0,
                    nexts: last_nexts.clone(),
                    filled: last_nexts.iter().map(|_| SmallVec::new()).collect(),
                    free,
                },
                eff_types: direct_children,
                invariant: Invariant::all(invariant_id),
                evacuate_fn: unsafe { type_info.evacuate_fn() },
                last_nexts,
            },
            latest_grey: 0,
            condemned: Default::default(),
            waiting_transitive_parents: SmallVec::new(),
            waiting_transitive: false,
        }
    }

    /// Ensures no ptrs exist in to the `TypeState`'s condemned arenas.
    fn safe_to_free(&mut self, pending: &Pending, relations: &ActiveRelations) -> bool {
        // TODO(drop) ensure transitive_parents drop order.
        // Or panic on Arena::new in a destructor.
        if self.no_grey_arenas(pending) {
            log::trace!("no_grey_arenas: true");
            if !self.waiting_transitive {
                self.waiting_transitive_parents = relations
                    .transitive_parents
                    .iter()
                    .map(|ts| (*ts, unsafe { &*ts.pending.get() }.epoch + 1))
                    .collect();
                self.waiting_transitive = true;

                false
            } else {
                self.waiting_transitive_parents
                    .iter()
                    .all(|(ts, e)| *e < unsafe { &*ts.pending.get() }.epoch)
            }
        } else {
            false
        }
    }

    fn no_grey_arenas(&self, pending: &Pending) -> bool {
        log::trace!(
            "pending.known_grey: {}, latest_grey: {}, pending.epoch: {}",
            pending.known_grey,
            self.latest_grey,
            pending.epoch
        );
        pending.known_grey == 0
            && self.latest_grey < pending.epoch - 2
            && pending
                .arenas
                .values()
                .cloned()
                .all(|epoch| epoch > self.latest_grey + 2)
    }

    /// Evacuate rooted objects from condemned `Arenas`.
    fn evac_roots(&mut self, type_info: &GcTypeInfo, arenas: &mut Arenas, free: &mut FreeList) {
        log::trace!("evac_roots: {}", type_info.type_name);
        let Collection {
            handler, condemned, ..
        } = self;

        fn er(
            header: *const HeaderUnTyped,
            type_info: &GcTypeInfo,
            handler: &mut HandlerManager,
            arenas: &mut Arenas,
            free: &mut FreeList,
        ) {
            let Arenas { full, partial, .. } = arenas;
            log::trace!("header: {:?}", header);
            let mut roots = unsafe { &*header }
                .roots
                .lock()
                .expect("Could not unlock roots, in TypeState::step");

            roots.drain().for_each(|(idx, root_ptr)| {
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
                    let (next, top) = partial.remove_arena(type_info, free);

                    unsafe { ptr::copy(gc, next as _, 1) }

                    let to_header =
                        unsafe { &mut *(HeaderUnTyped::from(next) as *mut HeaderUnTyped) };
                    let mut roots = to_header.roots.lock().unwrap();

                    if !roots.insert(index(next, size, align), root_ptr).is_none() {
                        panic!("Not possible");
                    };

                    let ptr = bump_down(next, size, align);

                    if arena::full(ptr, align) {
                        full.insert(header, top);
                    } else {
                        log::trace!(
                            "Evacuated root to Arena {{header: {:?}, next: {:?}}}",
                            to_header as *const _,
                            next
                        );
                        partial.0.insert(header, (ptr, top));
                    }
                };
            });
            assert_eq!(0, roots.len());
        };

        condemned
            .partial
            .0
            .keys()
            .cloned()
            .for_each(|header| er(header, type_info, handler, arenas, free));

        log::trace!("evaced root's in partial: {:?}", condemned.partial.0);

        condemned
            .full
            .keys()
            .cloned()
            .map(|h| h as *const _)
            .for_each(|header| er(header, type_info, handler, arenas, free));

        log::trace!("evaced root's in full: {:?}", condemned.full);

        condemned
            .worker
            .keys()
            .cloned()
            .for_each(|header| er(header, type_info, handler, arenas, free));
        log::trace!("evaced root's in worker: {:?}", condemned.worker);
    }
}

#[derive(Debug)]
pub(crate) struct Arenas {
    /// The last seen `next`..end.
    /// `end+size..=high_offset` is already freed.
    ///
    /// Will not include all Arenas owned by workers.
    /// Only contains Arenas that the gc owns some of.
    pub worker: ArenaBounds,
    /// Arena owned by GC, that have not been completely filled.
    pub partial: Partial,
    /// header => top
    pub full: HashMap<*const HeaderUnTyped, usize>,
}

impl Default for Arenas {
    fn default() -> Self {
        Arenas {
            worker: Default::default(),
            partial: Default::default(),
            full: Default::default(),
        }
    }
}

impl Arenas {
    fn is_empty(&self) -> bool {
        self.worker.is_empty() && self.full.is_empty() && self.partial.is_empty()
    }
}

#[derive(Debug)]
pub(crate) struct Partial(ArenaBounds);

impl Deref for Partial {
    type Target = <ArenaBounds as Deref>::Target;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Partial {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Default for Partial {
    fn default() -> Self {
        Partial(Default::default())
    }
}

impl Partial {
    /// Reuse or allocate a new Arena
    fn remove_arena(&mut self, type_info: &GcTypeInfo, free: &mut FreeList) -> (*const u8, usize) {
        if let Some((header, (next, top))) = self.iter().next().map(|(h, (n, t))| (*h, (*n, *t))) {
            self.remove(&header);
            (next as _, top)
        } else {
            let next = free.alloc() as *mut _ as usize
                + HeaderUnTyped::high_offset(type_info.align, type_info.size) as usize;
            (next as *mut u8, next)
        }
    }
}

type Droped = (*const u8, usize);

#[derive(Debug, Clone)]
/// Header => (next, top)
/// top..ARENA_SIZE has already been droped.
pub(crate) struct ArenaBounds(HashMap<*const HeaderUnTyped, Droped>);

impl Default for ArenaBounds {
    fn default() -> Self {
        ArenaBounds(HashMap::with_capacity(100))
    }
}

impl Deref for ArenaBounds {
    type Target = HashMap<*const HeaderUnTyped, Droped>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ArenaBounds {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
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

impl Drop for HandlerManager {
    fn drop(&mut self) {
        self.handlers
            .nexts
            .iter()
            .enumerate()
            .for_each(|(i, next)| {
                let child_ts = self.eff_types[i];
                let child_arenas = unsafe { &mut *child_ts.arenas.get() };
                let header = HeaderUnTyped::from(*next);
                let top = header as usize
                    + HeaderUnTyped::high_offset(child_ts.type_info.align, child_ts.type_info.size)
                        as usize;
                if *next as usize % ARENA_SIZE == 0 {
                    child_arenas.full.insert(header as _, top);
                } else {
                    child_arenas.partial.0.insert(header, (*next, top));
                }
            });

        assert_eq!(self.handlers.filled.iter().flatten().count(), 0);
    }
}

impl HandlerManager {
    /// Marks an `Arena` as black.
    /// `next` may point to the next free slot or the `Header` if the arena is full.
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

                let child_pending = unsafe { &mut *child_ts.pending.get() };
                // Root partial Arenas if child is undergoing a GC.
                if let Some(state) = unsafe { &mut *child_ts.state.get() } {
                    state.latest_grey = child_pending.epoch;

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
                    let b = child_arenas.full.insert(
                        *header,
                        *header as usize
                            + HeaderUnTyped::high_offset(
                                child_ts.type_info.align,
                                child_ts.type_info.size,
                            ) as usize,
                    );
                    // Assert full will never have `header`.
                    debug_assert!(b.is_none());
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
///

/// All known relations between GCed types.
/// New relationships are first known upon `TotalRelations::register`.
/// `TotalRelations::register` is triggered by a worker thread's first `Arena::<T>::new()`
pub(crate) struct TotalRelations {
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
    pub(crate) fn register(
        &mut self,
        active: &mut HashMap<&'static GcTypeInfo, &'static TypeState>,
        type_info: GcTypeInfo,
        thread_id: ThreadId,
        bus_ptr: BusPtr,
    ) -> Option<&'static TypeState> {
        let active_ts = active.get(&type_info).cloned();
        let ts = active_ts.unwrap_or_else(|| {
            let ts = Box::leak(Box::new(TypeState {
                type_info,
                buses: Default::default(),
                sent_invariant: Cell::from(true),
                invariant_id: Cell::from(1),
                arenas: UnsafeCell::new(Arenas::default()),
                pending: UnsafeCell::new(Pending {
                    epoch: 0,
                    // transitive_epoch: 0,
                    // known_transitive_grey: 0,
                    known_grey: 0,
                    arenas: Default::default(),
                }),
                state: UnsafeCell::new(None),
                relations: UnsafeCell::new(ActiveRelations {
                    direct_parents: Default::default(),
                    transitive_parents: Default::default(),
                    direct_children: Default::default(),
                    transitive_children: Default::default(),
                }),
            }));

            let ts = &*ts;
            let b = active.insert(&ts.type_info, ts);
            debug_assert!(b.is_none());

            let relations = unsafe { &mut *ts.relations.get() };

            // The new `TypeState` is added to active before we generate active relations,
            // so self referential types contain &TypeState and TypeRow back to them selfs.
            *relations = self.new_active(&type_info, active);

            // Add references to the new `TypeState` to related `TypeState`s
            relations
                .direct_parents
                .iter()
                .for_each(|(parent_ts, type_row)| {
                    let parent_relations = unsafe { &mut *parent_ts.relations.get() };
                    parent_relations
                        .direct_children
                        .push((ts, type_row.clone()))
                });

            relations.transitive_parents.iter().for_each(|parent_ts| {
                let parent_relations = unsafe { &mut *parent_ts.relations.get() };
                parent_relations.transitive_children.push(ts)
            });

            relations
                .direct_children
                .iter()
                .for_each(|(child_ts, type_row)| {
                    let parent_relations = unsafe { &mut *child_ts.relations.get() };
                    parent_relations.direct_parents.push((ts, type_row.clone()));
                });
            ts
        });

        let buses = unsafe { &mut *ts.buses.get() };
        buses.insert(thread_id, (bus_ptr, 0));
        if active_ts.is_none() {
            Some(ts)
        } else {
            None
        }
    }
}

/// Relations between GCed types with a live `TypeState` object.
/// In the future `TypeState` may be deallocated, if an Arena<T> has not existed for a time.
pub(crate) struct ActiveRelations {
    // TODO revisit sizes
    direct_parents: SmallVec<[(&'static TypeState, TypeRow); 3]>,
    transitive_parents: SmallVec<[&'static TypeState; 5]>,
    direct_children: SmallVec<[(&'static TypeState, TypeRow); 3]>,
    transitive_children: SmallVec<[&'static TypeState; 5]>,
}

impl ActiveRelations {
    pub fn active_relations(&self) -> HashSet<&'static GcTypeInfo> {
        let mut r: HashSet<&'static GcTypeInfo> =
            HashSet::with_capacity(self.transitive_parents.len() + self.transitive_children.len());
        r.extend(self.transitive_parents.iter().map(|ts| &ts.type_info));
        r.extend(self.transitive_children.iter().map(|ts| &ts.type_info));
        r
    }
}

impl TotalRelations {
    fn new_active(
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
