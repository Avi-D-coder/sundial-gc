use crate::{
    arena::{self, bump_down, full, index, HeaderUnTyped, ARENA_SIZE},
    gc::RootIntern,
    gc_logic::{
        bus::{self, Msg, WorkerMsg},
        free_list::FreeList,
        BusPtr,
    },
    mark::{EffTypes, GcTypeInfo, Handlers, Invariant, Tti, TypeRow},
};
use smallvec::SmallVec;
use std::{
    cell::{Cell, UnsafeCell},
    cmp::max,
    collections::{HashMap, HashSet},
    fmt::Debug,
    ops::{Deref, DerefMut},
    ptr,
    sync::atomic::{AtomicUsize, Ordering},
    thread::ThreadId,
};

/// Each `Gc<T>` has a `TypeState` object.
/// Related `TypeState`s must be owned by the same thread.
///
/// Related `TypeState`s are be bundled into a `TypeGroup`.
/// A `TypeGroup` is the unit of GC parallelization.
/// All related `TypeState`s are guaranteed to be on the same GC thread.
/// `TypeGroup: Send + !Sync`.
///
/// As such methods on `TypeState` may mutate related `TypeState`s.
pub(crate) struct TypeState {
    pub type_info: GcTypeInfo,
    /// Wrapping
    pub major_cycle: AtomicUsize,
    /// Map worker thread => (Bus, worker arena count, invariant_id).
    buses: UnsafeCell<HashMap<ThreadId, (BusPtr, usize, u8)>>,
    /// Has the invariant be sent to workers.
    /// TODO(optimization) delay sending invariant.
    pub invariant_id: Cell<u8>,
    pub arenas: UnsafeCell<Arenas>,
    pub pending: UnsafeCell<Pending>,
    // TODO support multiple simultaneous cycles.
    pub state: UnsafeCell<Option<Collection>>,
    pub relations: UnsafeCell<ActiveRelations>,
    // FIXME #[cfg(debug)]
    /// object ptr => live
    /// live will only be true when `needs_drop` is.
    pub slots: UnsafeCell<HashMap<*const u8, bool>>,
}

impl Debug for TypeState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("TypeState::{}", self.type_info.type_name))
            .field("invariant_id", &self.invariant_id.get())
            .field("pending", unsafe { &*self.pending.get() })
            .field("collection", unsafe { &*self.state.get() })
            .field("arenas", unsafe { &*self.arenas.get() })
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

        buses
            .iter_mut()
            .for_each(|(thread_id, (bus, _count, worker_known_invariant))| {
                let msgs = {
                    let mut bus = &mut bus.lock().expect("Could not unlock bus").1;
                    let msgs = bus::reduce(&mut bus);

                    if *worker_known_invariant != self.invariant_id.get() {
                        *worker_known_invariant = self.invariant_id.get();

                        let inv = Msg::Invariant(
                            state
                                .as_mut()
                                .map(|cycle| {
                                    cycle.latest_grey = pending.epoch;
                                    cycle.handler.invariant
                                })
                                .unwrap_or_else(|| Invariant::none(self.invariant_id.get())),
                        );

                        // Send invariant replacing invariant currently on bus if it exists.
                        if let Some(slot) = bus.iter_mut().find(|msg| msg.is_invariant()) {
                            *slot = inv
                        } else {
                            bus.push(inv)
                        };

                        log::trace!("GC: sent thread {:?}, {:?}", thread_id, inv);
                    };

                    msgs
                };

                grey.extend(
                    msgs.iter().filter_map(|msg| {
                        self.book_keeping(*msg, *thread_id, state, pending, arenas)
                    }),
                );

                log::trace!("GC: Messages handled");

                log::trace!(
                    "GC: {:?}, worker_known_invariant: {}, self.invariant_id: {}",
                    thread_id,
                    worker_known_invariant,
                    self.invariant_id.get()
                );

                // if *count < 2 {
                //     *count += 1;
                //     let (next, top) = arenas.partial.remove_arena(&self.type_info, free);
                //     let header = HeaderUnTyped::from(next);

                //     arenas.worker.insert(header, (next, top));
                //     log::trace!("GC sent header: {:?}, next: {:?}", header, next);
                //     let mut bus = bus.lock().expect("Could not unlock bus");
                //     bus.push(Msg::Next(next as _))
                // };
            });

        log::trace!("grey: {:?}", grey);

        debug_assert!(if !grey.is_empty() {
            state.is_some()
        } else {
            true
        });

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

            log::trace!("cycle.evac_roots");

            // FIXME
            // cycle.evac_roots(&self.type_info, arenas, free);

            log::trace!("while cycle.handler.root_grey_children");
            while cycle.handler.root_grey_children() {}

            log::trace!("cycle.safe_to_free");
            if cycle.safe_to_free(pending, relations) {
                log::trace!("safe_to_free: {}", self.type_info.type_name);

                cycle
                    .condemned
                    .full
                    .drain()
                    .for_each(|(header, top)| self.drop_arena(header as *mut u8, top, free));

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

                let children_complete = relations
                    .direct_children
                    .iter()
                    .filter(|(cts, _)| *cts != self)
                    .all(|(cts, _)| {
                        { &*cts.state.get() }
                            .as_ref()
                            .map(|cycle| cycle.condemned.is_empty())
                            .unwrap_or(true)
                    });

                if children_complete {
                    log::trace!("Children complete Collection cleared!");
                    // This will trigger the Invariant::none to be sent to the workers.
                    log::trace!("pending.known_grey: {}", pending.known_grey);
                    debug_assert_eq!(pending.known_grey, 0);
                    log::trace!(
                        "pending.arenas: len: {}, {:?}",
                        pending.arenas.len(),
                        pending.arenas
                    );

                    self.bump_invariant_id(pending);

                    log::trace!("clearing state");
                    cycle.handler.drop();
                    *state = None;
                    log::trace!("state cleared");
                    done = true;
                    self.major_cycle.fetch_add(1, Ordering::Release);
                };
            };
        };

        log::trace!("step::done: {:?}", done);
        done
    }

    unsafe fn book_keeping(
        &self,
        msg: WorkerMsg,
        thread_id: ThreadId,
        state: &mut Option<Collection>,
        pending: &mut Pending,
        arenas: &mut Arenas,
    ) -> Option<(*const u8, u8)> {
        fn small<K, V>(s: &HashMap<K, V>) -> Option<&HashMap<K, V>> {
            if s.len() < 20 {
                Some(s)
            } else {
                None
            }
        }

        log::trace!(
            "GC: prior:\n
            lastest_grey: {:?},\n
            pending.known_grey: {},\n
            pending.known_grey_arenas.len: {},\n
            pending.known_grey_arenas: {:?},\n
            pending.arenas.len: {},\n
            pending.arenas: {:?}",
            state.as_ref().map(|c| c.latest_grey),
            pending.known_grey,
            pending.known_grey_arenas.len(),
            small(&pending.known_grey_arenas),
            pending.arenas.len(),
            small(&pending.arenas),
        );
        let ret = match msg {
            WorkerMsg::Start { invariant_id, next } => {
                log::trace!(
                    "GC received from thread: {:?}: {:?}, header: {:?}",
                    thread_id,
                    WorkerMsg::Start { invariant_id, next },
                    HeaderUnTyped::from(next),
                );

                // If a collection is in progress, we keep track of pending arenas.
                if let Some(cycle) = state {
                    debug_assert_eq!(cycle.handler.invariant.id, self.invariant_id.get());
                    if invariant_id != cycle.handler.invariant.id {
                        // Arena was started prior to the worker hearing about the new invariant.
                        cycle.latest_grey = pending.epoch;
                    }
                };

                if invariant_id != self.invariant_id.get() {
                    let epoch = pending.epoch;
                    pending.known_grey += 1;
                    pending
                        .known_grey_arenas
                        .entry(HeaderUnTyped::from(next))
                        .and_modify(|em| {
                            em.0 = epoch;
                            em.1 += 1
                        })
                        .or_insert((epoch, 1));
                    log::trace!("Added to pending.known_grey: {}", pending.known_grey);
                } else if invariant_id == self.invariant_id.get() {
                    let epoch = pending.epoch;
                    pending
                        .arenas
                        .entry(HeaderUnTyped::from(next))
                        .and_modify(|em| {
                            em.0 = epoch;
                            em.1 += 1
                        })
                        .or_insert((epoch, 1));
                };

                None
            }

            m
            @
            WorkerMsg::End {
                release_to_gc,
                next,
                grey_fields,
                invariant_id,
                transient,
                ..
            } => {
                let header = HeaderUnTyped::from(next);

                log::trace!(
                    "GC received from thread: {:?}: {:?}, header: {:?}",
                    thread_id,
                    m,
                    header
                );

                // fn debug() {}
                // #[cfg(debug)]
                let debug = || {
                    let slots = &mut *self.slots.get();
                    let lowest = if next == header as _ {
                        header as usize + HeaderUnTyped::low_offset(self.type_info.align) as usize
                    } else {
                        next as usize + self.type_info.size as usize
                    };
                    let high = header as usize
                        + HeaderUnTyped::high_offset(self.type_info.align, self.type_info.size)
                            as usize;
                    for ptr in (lowest..=high).step_by(self.type_info.size as usize) {
                        slots.insert(ptr as _, self.type_info.needs_drop);
                    }
                };

                debug();

                // Option::map makes the borrow checker unhappy.
                let ret = if let Some(cycle) = state {
                    debug_assert_eq!(cycle.handler.invariant.id, self.invariant_id.get());
                    if cycle.handler.invariant.id == invariant_id {
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

                if !transient {
                    if invariant_id == self.invariant_id.get() {
                        let (_, multiplicity) = pending.arenas.get_mut(&header).unwrap();
                        *multiplicity -= 1;

                        if *multiplicity == 0 {
                            pending.arenas.remove(&header);
                        };
                    } else {
                        pending.known_grey -= 1;
                        let (_, multiplicity) = pending.known_grey_arenas.get_mut(&header).unwrap();
                        *multiplicity -= 1;

                        log::trace!("multiplicity: {}", multiplicity);
                        if *multiplicity == 0 {
                            let r = pending.known_grey_arenas.remove(&header);
                            debug_assert!(r.is_some())
                        };
                    };
                };

                if release_to_gc {
                    let top = arenas
                        .worker
                        .remove(&header)
                        .map(|(_, t)| t)
                        .unwrap_or_else(|| {
                            //  FIXME this failed once.
                            debug_assert!(transient);
                            (header as usize
                                + HeaderUnTyped::high_offset(
                                    self.type_info.align,
                                    self.type_info.size,
                                ) as usize) as _
                        });

                    if full(next, self.type_info.align) {
                        arenas.full.insert(header, top);
                    } else {
                        arenas.partial.insert(header, (next, top));
                    };
                } else {
                    // store active worker arenas
                    arenas
                        .worker
                        .entry(HeaderUnTyped::from(next))
                        .and_modify(|(n, _)| *n = next)
                        .or_insert({
                            (
                                next,
                                (HeaderUnTyped::from(next) as usize
                                    + HeaderUnTyped::high_offset(
                                        self.type_info.align,
                                        self.type_info.size,
                                    ) as usize) as _,
                            )
                        });
                };

                ret
            }
            // These are formerly cached arenas.
            WorkerMsg::Release(next) => {
                let header = HeaderUnTyped::from(next);
                let (nxt, top) = arenas.worker.remove(&header).unwrap_or_else(|| {
                    panic!("Cached Arena: {:?} not found in arenas.worker", next)
                });
                debug_assert_eq!(next, nxt as _);

                let p = arenas.partial.insert(header, (nxt, top));
                debug_assert!(p.is_none());

                None
            }
        };

        log::trace!(
            "GC: after:\n
            lastest_grey: {:?},\n
            pending.known_grey: {},\n
            pending.known_grey_arenas.len: {},\n
            pending.known_grey_arenas: {:?},\n
            pending.arenas.len: {},\n
            pending.arenas: {:?}",
            state.as_ref().map(|c| c.latest_grey),
            pending.known_grey,
            pending.known_grey_arenas.len(),
            small(&pending.known_grey_arenas),
            pending.arenas.len(),
            small(&pending.arenas),
        );

        ret
    }

    pub fn bump_invariant_id(&self, pending: &mut Pending) -> u8 {
        let invariant_id = self.invariant_id.get().checked_add(1).unwrap_or(1);
        self.invariant_id.set(invariant_id);

        pending.known_grey = pending.arenas.len();
        let Pending {
            arenas,
            known_grey_arenas,
            ..
        } = pending;
        arenas.drain().for_each(|(header, em)| {
            let had = known_grey_arenas.insert(header, em);
            debug_assert!(had.is_none());
        });
        invariant_id
    }

    unsafe fn drop_arena(&self, next: *mut u8, top: *const u8, free: &mut FreeList) {
        let header = HeaderUnTyped::from(next) as *mut HeaderUnTyped;
        log::trace!("drop_arena(header: {:?}, next: {:?})", header, next);

        // let roots = { &*header }
        //     .roots
        //     .lock()
        //     .expect("Could not unlock roots while freeing Arena");
        // if roots.len() != 0 {
        //     // FIXME should reset latest_grey and try again latter.
        //     panic!("Old Arena Rooted");
        // }

        if self.type_info.needs_drop {
            self.drop_objects(header, (next, top));
        };

        free.dealloc(header)
    }

    /// Runs destructor on objects from the top of the `Arena` to next;
    unsafe fn drop_objects(&self, header: *mut HeaderUnTyped, range: DropedBounds) {
        debug_assert!(self.type_info.needs_drop);

        let GcTypeInfo { align, size, .. } = self.type_info;
        let low = if header == range.0 as _ {
            header as usize + HeaderUnTyped::low_offset(align) as usize
        } else {
            range.0 as usize + size as usize
        };

        let mut ptr = range.1 as usize;

        let drop_in_place = self.type_info.drop_in_place_fn();
        let evacuated = { &mut *header }.evacuated.lock().unwrap();

        log::trace!(
            "drop_objects(header: {:?}, range: {:?}): {:?}..={:?}",
            header,
            range,
            low as *const u8,
            ptr as *const u8
        );

        while ptr >= low {
            // TODO sort and iterate between evacuated
            if !evacuated.contains_key(&index(ptr as *const u8, size, align)) {
                // log::trace!("drop ptr: {:?}", ptr as *mut u8);
                // fn debug() {}
                // FIXME #[cfg(debug)]
                let debug = || {
                    let slots = &mut *self.slots.get();
                    let live = slots
                        .get_mut(&(ptr as _))
                        .unwrap_or_else(|| panic!("Droping invalid ptr: {:?}", ptr as *mut u8));
                    if !*live {
                        panic!("Double freeing ptr: {:?}", ptr as *mut u8);
                    } else {
                        *live = false
                    }
                };

                debug();
                drop_in_place(ptr as *mut u8)
            };
            ptr -= size as usize;
        }
    }

    pub unsafe fn reset(&self) {
        debug_assert!(self.state.get().as_ref().unwrap().is_none());

        let pending = &mut *self.pending.get();
        pending.epoch = 0;
        pending
            .arenas
            .iter_mut()
            .for_each(|(_, em)| *em = (1, em.1));
    }
}

#[derive(Debug)]
pub(crate) struct Pending {
    /// `epoch` is not synchronized across threads.
    /// 2 epochs delineate Arena age.
    /// 2 epoch are needed, since thread A may receive a GCed object from thread B in between the
    ///   GC reading A & B's messages.
    /// A's `Msg::Start` may be received in the next epoch after B's `Msg::End`.
    /// `epoch` counts from 1, and resets when free is accomplished.
    pub epoch: usize,
    // transitive_epoch: usize,
    /// Count of Arenas started before transitive_epoch.
    // known_transitive_grey: usize,
    /// Count of Arenas that are not a part of a `Collection` cycle.
    /// Known grey do not uphold any `Collection` variant.
    known_grey: usize,
    /// This should not be needed.
    /// Header => (epoch, multiplicity)
    known_grey_arenas: HashMap<*const HeaderUnTyped, (usize, usize)>,
    /// `Arena.Header` started after `epoch`.
    /// A arena `*const u8` cannot have a entry in `pending` and `known_grey`.
    /// Header => (epoch, multiplicity)
    arenas: HashMap<*const HeaderUnTyped, (usize, usize)>,
}

/// A single collection cycle.
pub(crate) struct Collection {
    pub handler: HandlerManager,
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
        epoch: usize,
        type_info: &GcTypeInfo,
        relations: &ActiveRelations,
        free: &mut FreeList,
        invariant_id: u8,
    ) -> Collection {
        log::trace!(
            "Collection::new {}, relations: {:?}",
            type_info.type_name,
            relations
        );

        log::trace!("Making last_nexts");
        let last_nexts: SmallVec<_> = relations
            .direct_children
            .iter()
            .map(|t| {
                (free.alloc() as *mut _ as usize
                    + HeaderUnTyped::high_offset(t.0.type_info.align, t.0.type_info.size) as usize)
                    as *mut u8
            })
            .collect();

        log::trace!("Making eff_types");
        let eff_types = relations
            .direct_children
            .iter()
            .map(|(ts, _)| *ts)
            .collect();

        log::trace!("Making evac_fn");
        let evacuate_fn = unsafe { type_info.evacuate_fn() };

        log::trace!("Making translator");
        let translator = type_info.translator_from_fn()(&relations.direct_children).0;

        log::trace!("Making filled");
        let filled = last_nexts.iter().map(|_| SmallVec::new()).collect();

        let nexts = last_nexts.clone();

        log::trace!("Making condemned");
        let condemned = Default::default();

        log::trace!("Making Invariant::all(invariant_id: {})", invariant_id);
        let invariant = Invariant::all(invariant_id);

        log::trace!("Making Collection");
        let r = Collection {
            handler: HandlerManager {
                handlers: Handlers {
                    translator,
                    nexts,
                    filled,
                    free,
                },
                eff_types,
                invariant,
                evacuate_fn,
                last_nexts,
            },
            latest_grey: epoch,
            condemned,
            waiting_transitive_parents: SmallVec::new(),
            waiting_transitive: false,
        };
        log::trace!("Collection::new {:?}", r);

        r
    }

    /// Ensures no ptrs exist in to the `TypeState`'s condemned arenas.
    fn safe_to_free(&mut self, pending: &Pending, relations: &ActiveRelations) -> bool {
        log::trace!("safe_to_free");
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
        if pending.known_grey != pending.known_grey_arenas.len() {
            log::warn!(
                "no_grey_arenas: pending.known_grey: {} != pending.known_grey_arenas.len: {}",
                pending.known_grey,
                pending.known_grey_arenas.len()
            );
        };

        let safe = self.latest_grey + 2 < pending.epoch
            && pending.known_grey_arenas.is_empty()
            && pending
                .arenas
                .values()
                .cloned()
                .all(|(epoch, _)| epoch > self.latest_grey + 2);

        if !(self.latest_grey + 2 < pending.epoch) {
            log::info!(
                "no_grey_arenas: latest_grey: {}, pending.epoch: {},",
                self.latest_grey,
                pending.epoch
            );
        } else if !pending.known_grey_arenas.is_empty() {
            log::info!(
                "no_grey_arenas: pending.known_grey_arenas: {:?}",
                pending.known_grey_arenas
            );
        } else {
            log::info!(
                "no_grey_arenas: pending.arenas: {:?}",
                pending
                    .arenas
                    .iter()
                    .filter(|(_, (epoch, _))| *epoch > self.latest_grey + 2)
                    .collect::<HashMap<_, _>>()
            );
        };

        safe
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
    pub full: HashMap<*const HeaderUnTyped, *const u8>,
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
    fn remove_arena(&mut self, type_info: &GcTypeInfo, free: &mut FreeList) -> DropedBounds {
        if let Some((header, (next, top))) = self.iter().next().map(|(h, (n, t))| (*h, (*n, *t))) {
            self.remove(&header);
            (next as _, top)
        } else {
            let next = free.alloc() as *mut _ as usize
                + HeaderUnTyped::high_offset(type_info.align, type_info.size) as usize;
            (next as *mut u8, next as _)
        }
    }
}

type DropedBounds = (*const u8, *const u8);

#[derive(Debug, Clone)]
/// Header => (next, top)
/// top..ARENA_SIZE has already been droped.
pub(crate) struct ArenaBounds(HashMap<*const HeaderUnTyped, DropedBounds>);

impl Default for ArenaBounds {
    fn default() -> Self {
        ArenaBounds(HashMap::new())
    }
}

impl Deref for ArenaBounds {
    type Target = HashMap<*const HeaderUnTyped, DropedBounds>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for ArenaBounds {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub(crate) struct HandlerManager {
    handlers: Handlers,
    eff_types: EffTypes,
    invariant: Invariant,
    /// fn( *const u8, offset: Offset, grey_fields: u8, invariant: *const Invariant, handlers: *mut Handlers )
    evacuate_fn: fn(*const u8, u8, u8, *const Invariant, *mut Handlers),
    /// A snapshot of nexts at last call to `unclean_children`.
    last_nexts: SmallVec<[*mut u8; 4]>,
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
        log::trace!("root_grey_children");
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
                        (*header as usize
                            + HeaderUnTyped::high_offset(
                                child_ts.type_info.align,
                                child_ts.type_info.size,
                            ) as usize) as _,
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

    /// This panics when in the Drop trait
    pub fn drop(&mut self) {
        log::trace!("HandlerManager::Drop: {:?}", self.handlers);
        self.handlers
            .nexts
            .iter()
            .enumerate()
            .for_each(|(i, next)| {
                let child_ts = self.eff_types[i];
                let child_arenas = unsafe { &mut *child_ts.arenas.get() };
                let header = HeaderUnTyped::from(*next);
                let top = (header as usize
                    + HeaderUnTyped::high_offset(child_ts.type_info.align, child_ts.type_info.size)
                        as usize) as _;

                if *next as usize % ARENA_SIZE == 0 {
                    child_arenas.full.insert(header as _, top);
                } else {
                    child_arenas.partial.insert(header, (*next, top));
                }
            });

        assert_eq!(self.handlers.filled.iter().flatten().count(), 0);
        log::trace!("HandlerManager::Drop done");
    }
}

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
                major_cycle: AtomicUsize::new(0),
                buses: Default::default(),
                invariant_id: Cell::from(1),
                arenas: UnsafeCell::new(Arenas::default()),
                pending: UnsafeCell::new(Pending {
                    epoch: 0,
                    // transitive_epoch: 0,
                    // known_transitive_grey: 0,
                    known_grey: 0,
                    known_grey_arenas: Default::default(),
                    arenas: Default::default(),
                }),
                state: UnsafeCell::new(None),
                relations: UnsafeCell::new(ActiveRelations {
                    direct_parents: Default::default(),
                    transitive_parents: Default::default(),
                    direct_children: Default::default(),
                    transitive_children: Default::default(),
                }),
                slots: Default::default(),
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
        buses.insert(thread_id, (bus_ptr, 0, 0));
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

impl Debug for ActiveRelations {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ActiveRelations")
            .field(
                "direct_parents",
                &self
                    .direct_parents
                    .iter()
                    .map(|(ts, tr)| (ts.type_info.type_name, tr))
                    .collect::<Vec<_>>(),
            )
            .field(
                "direct_children",
                &self
                    .direct_children
                    .iter()
                    .map(|(ts, tr)| (ts.type_info.type_name, tr))
                    .collect::<Vec<_>>(),
            )
            .field("transitive_parents", &self.transitive_parents)
            .field("transitive_children", &self.transitive_children)
            .finish()
    }
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
