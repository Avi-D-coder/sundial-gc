use crate::arena::*;
use crate::mark::*;
use smallvec::SmallVec;
use log::info;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::{
    atomic::{AtomicPtr, Ordering},
    mpsc::*,
};
use std::thread::{self, ThreadId};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    cmp::max,
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

struct HandlerManager {
    handlers: Handlers,
    handlers_types: Vec<GcTypeInfo>,
    invariant: Invariant,
    evacuate_fn: fn(*const u8, u8, u8, Range<usize>, *mut Handlers),
    /// A snapshot of nexts at last call to `unclean_children`.
    last_nexts: SmallVec<[*mut u8; 4]>,
}

impl HandlerManager {
    /// Roots next..high_offset for this GC.
    /// Returns unclean *nexts of child types
    fn root(&mut self, next: *mut u8, align: u16, size: u16) {
        let HandlerManager {
            handlers,
            invariant,
            evacuate_fn,
            ..
        } = self;
        let Invariant {
            white_start,
            white_end,
            grey_feilds,
            ..
        } = invariant;

        let mut ptr =
            HeaderUnTyped::from(next) as usize + HeaderUnTyped::high_offset(align, size) as usize;

        while ptr != next as usize {
            let t = unsafe { &*(ptr as *const u8) };
            evacuate_fn(t, 0, *grey_feilds, *white_start..*white_end, handlers);
            ptr -= size as usize;
        }
    }

    fn unclean_children(
        &mut self,
        align: u16,
    ) -> SmallVec<[(&GcTypeInfo, SmallVec<[*mut u8; 4]>); 4]> {
        let HandlerManager {
            handlers,
            handlers_types,
            last_nexts,
            ..
        } = self;
        let mut unclean: SmallVec<[(&GcTypeInfo, SmallVec<[*mut u8; 4]>); 4]> = handlers_types
            .iter()
            .map(|ti| (ti, SmallVec::new()))
            .collect();

        handlers
            .filled
            .iter_mut()
            .enumerate()
            .for_each(|(i, headers)| {
                headers.into_iter().for_each(|header| {
                    let full_next = *header as usize + HeaderUnTyped::low_offset(align) as usize;
                    unclean[i].1.push(full_next as *mut u8)
                });
                headers.clear();
            });

        handlers
            .nexts
            .iter()
            .enumerate()
            .zip(last_nexts.iter())
            .filter_map(|(n, o)| if n.1 == o { None } else { Some(n) })
            .for_each(|(i, next)| unclean[i].1.push(*next));

        unclean
    }

    fn new_arenas(
        &mut self,
        align: u16,
        new_arenas: &mut HashMap<GcTypeInfo, SmallVec<[*mut u8; 4]>>,
    ) {
        self.unclean_children(align)
            .into_iter()
            .for_each(|(t, nxts)| {
                let nexts = new_arenas.entry(*t).or_default();
                nexts.extend(nxts);
            });
    }
}

/// This whole struct is unsafe!
struct TypeState {
    type_info: GcTypeInfo,
    handler: Option<HandlerManager>,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    /// The last seen `next`. ptr + size..high_offset is owned by gc
    /// Will not include all Arenas owned by workers.
    /// Only contains Arenas that the gc owns some of.
    worker_arenas: HashMap<*const HeaderUnTyped, *const u8>,
    /// Arena owned by GC, that have not been completely filled.
    gc_arenas: BTreeSet<*mut u8>,
    gc_arenas_full: HashSet<*mut HeaderUnTyped>,
    condemned_arenas: HashSet<*mut u8>,
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
            handler,
            ..
        } = self;
        *epoch += 1;

        let inv = handler.as_ref().map(|h| h.invariant).clone();
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
                    if inv
                        .map(|inv| inv.white_start == *white_start && inv.white_end == *white_end)
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
                    let ret = if let Some(inv) = inv {
                        if inv.white_start == *white_start && inv.white_end == *white_end {
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
                    } else {
                        None
                    };

                    if *release_to_gc {
                        if full(next, type_info.align) {
                            let header = HeaderUnTyped::from(next) as *mut _;
                            gc_arenas_full.insert(header);
                        } else {
                            gc_arenas.insert(next as *mut _);
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
                    }) = inv
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
        if let Some(handler) = handler {
            // TODO bring back granular tracing using bits
            grey.iter().for_each(|(next, bits)| {
                let GcTypeInfo { align, size, .. } = *type_info;
                handler.root(*next as *mut u8, align, size)
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

    fn set_invariant(
        &mut self,
        inv: Invariant,
        condemned_children: Vec<GcTypeInfo>,
        free: &mut Vec<*mut HeaderUnTyped>,
    ) {
        self.phase2 = false;
        self.pending_known_transitive_grey = 0;
        self.pending_known_grey = self.pending.len();
        // It will be one upon first `step()`
        self.epoch = 0;
        self.latest_grey = 1;
        self.pending = HashMap::new();

        let evacuate_fn = unsafe {
            mem::transmute::<*const (), fn(*const u8, u8, u8, Range<usize>, *mut Handlers)>(
                self.type_info.evacuate_fn,
            )
        };
        let translator_from = unsafe {
            mem::transmute::<*const (), fn(&Vec<GcTypeInfo>) -> (Translator, u8)>(
                self.type_info.translator_from_fn,
            )
        };

        let mut nexts = SmallVec::with_capacity(condemned_children.len());
        let mut filled = SmallVec::new();
        for _ in 0..condemned_children.len() {
            filled.push(SmallVec::new());
            nexts.push(
                free.pop()
                    .map(|header| {
                        HeaderUnTyped::init(header);
                        (header as usize
                            + HeaderUnTyped::high_offset(self.type_info.align, self.type_info.size)
                                as usize) as *mut u8
                    })
                    .unwrap_or(Arena::alloc()),
            );
        }
        let handlers = Handlers {
            translator: translator_from(&condemned_children).0,
            nexts,
            filled,
            free,
        };

        self.handler = Some(HandlerManager {
            last_nexts: SmallVec::from_elem(ptr::null_mut(), condemned_children.len()),
            handlers_types: condemned_children,
            handlers,
            invariant: inv,
            evacuate_fn,
        });
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
                handler: None,
                buses: HashMap::new(),
                worker_arenas: HashMap::new(),
                gc_arenas: BTreeSet::new(),
                gc_arenas_full: HashSet::new(),
                condemned_arenas: HashSet::new(),
                pending_known_transitive_grey: 0,
                pending_known_grey: 0,
                pending: HashMap::new(),
                latest_grey: 0,
                epoch: 0,
                transitive_epoch: 0,
                phase2: false,
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
        info!("Starting GC thread");
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
        info!("New loop");
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => {
                    info!("Thread: {:?} registered for GcTypeInfo: {:?}", id, ty);
                    types.reg(ty, id, bus)
                },
                RegMsg::Un(id, ty) => {
                    // The thread was dropped
                    // TODO Currently `Un` is never sent.
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

        let mut new_arenas: HashMap<GcTypeInfo, SmallVec<[*mut u8; 4]>> = HashMap::new();


        info!("calling step on active");
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

            ts.handler
                .as_mut()
                .map(|hm| hm.new_arenas(ti.align, &mut new_arenas));

            // TODO This is slower than it could be like everything
            if ts.is_stack_clear() && clear_stack_old.contains_key(ti) {
                stack_cleared.insert(*ti);
            };

            if ts.handler.is_some() {
                gc_in_progress = true;
            };

            arena_count += ts.gc_arenas_full.len();
        });

        stack_cleared.drain().for_each(|ti| {
            clear_stack_old.remove(&ti).unwrap().iter().for_each(|cti| {
                let ts = active.get_mut(cti).unwrap();
                ts.waiting_on_transitive_parents -= 1;
                if ts.waiting_on_transitive_parents == 0 {
                    let inv = ts.handler.as_ref().expect("Removed").invariant;
                    // Free memory
                    ts.condemned_arenas.drain().into_iter().for_each(|next| {
                        let header = HeaderUnTyped::from(next) as *mut HeaderUnTyped;
                        if cti.needs_drop {
                            let GcTypeInfo {
                                align,
                                size,
                                drop_in_place_fn,
                                ..
                            } = ti;

                            let low = max(
                                next as usize,
                                header as usize + HeaderUnTyped::low_offset(align) as usize,
                            );
                            let mut ptr =
                                low as usize + HeaderUnTyped::high_offset(align, size) as usize;

                            let drop_in_place =
                                unsafe { mem::transmute::<_, fn(*mut u8)>(drop_in_place_fn) };

                            while ptr >= low {
                                unsafe {
                                    let header = &mut *header;
                                    // TODO sort and iterate between evacuated
                                    if header.evacuated.get_mut().unwrap().contains_key(&index(
                                        ptr as *const u8,
                                        ti.size,
                                        ti.align,
                                    )) {
                                        drop_in_place(ptr as *mut u8)
                                    };
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

                    // TODO replace with Drop for HandlerManager
                    debug_assert!(ts.handler.is_some());
                    ts.handler.take().map(
                        |HandlerManager {
                             handlers,
                             handlers_types,
                             ..
                         }| {
                            debug_assert_eq!(handlers.filled.iter().flatten().count(), 0);
                            debug_assert_eq!(handlers.nexts.len(), handlers_types.len());
                            handlers
                                .nexts
                                .into_iter()
                                .zip(handlers_types.into_iter())
                                .for_each(|(next, ti)| {
                                    let ts = active.get_mut(&ti).unwrap();
                                    // Add arenas left in Handler to the pool;
                                    if next as usize % ARENA_SIZE == 0 {
                                        ts.gc_arenas_full.insert(next as *mut HeaderUnTyped);
                                    } else {
                                        ts.gc_arenas.insert(next);
                                    }
                                })
                        },
                    );
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

        if !gc_in_progress && arena_count > pre_arena_count {
            info!("Starting major GC");
            pre_arena_count = arena_count;

            // Set the invariants
            // Since this is a major GC / two space everything is condemned
            active.iter_mut().for_each(|(ti, ts)| {
                let inv = Invariant {
                    white_start: 0,
                    white_end: usize::MAX,
                    grey_feilds: 0b1111_1111,
                    grey_self: true,
                };

                let direct_gc_types = unsafe {
                    mem::transmute::<*const (), fn(&mut HashMap<GcTypeInfo, TypeRow>, u8)>(
                        ti.direct_gc_types_fn,
                    )
                };

                let mut dgt = HashMap::with_capacity(20);
                direct_gc_types(&mut dgt, 0);
                let condemned_children: Vec<_> = dgt.into_iter().map(|(i, _)| i).collect();

                ts.set_invariant(inv, condemned_children, &mut free);

                // Mark all the GC's arenas as condemned.
                ts.gc_arenas_full.iter().for_each(|header| {
                    let header = unsafe { &mut **header };
                    header.condemned = true;
                });

                debug_assert_eq!(ts.condemned_arenas.len(), 0);
                unsafe {
                    ptr::swap(
                        &mut ts.gc_arenas_full as *mut HashSet<*mut HeaderUnTyped> as *mut HashSet<*mut u8>,
                        &mut ts.condemned_arenas,
                    )
                }

                ts.gc_arenas.iter().cloned().for_each(|next| {
                    let header = unsafe { &mut *(HeaderUnTyped::from(next) as *mut HeaderUnTyped) };
                    header.condemned = true;
                });

                ts.gc_arenas = BTreeSet::new();
            });

            active.iter_mut().for_each(
                |(
                    ti,
                    TypeState {
                        worker_arenas,
                        handler,
                        ..
                    },
                )| {
                    if let Some(hm) = handler {
                        worker_arenas
                            .iter()
                            .for_each(|(_, next)| hm.root(*next as *mut u8, ti.align, ti.size));

                        hm.new_arenas(ti.align, &mut new_arenas);
                    };
                },
            );
        }

        while new_arenas.len() > 0 {
            info!("new_arenas.len: {}", new_arenas.len());
            let mut unclean = new_arenas;
            new_arenas = HashMap::new();
            unclean.drain().into_iter().for_each(|(ti, nexts)| {
                let ts = active.get_mut(&ti).unwrap();
                let hm = ts.handler.as_mut().unwrap();

                ts.gc_arenas_full.extend(
                    nexts
                        .iter()
                        .cloned()
                        // Only add Headers
                        // nexts are still owned by Handlers
                        .filter(|ptr| *ptr as usize % ARENA_SIZE == 0)
                        .map(|header| header as *mut HeaderUnTyped),
                );

                nexts
                    .into_iter()
                    .for_each(|next| hm.root(next as *mut u8, ti.align, ti.size));

                hm.new_arenas(ti.align, &mut new_arenas);
            });
        }

        thread::sleep(Duration::from_millis(100))
    }
}
