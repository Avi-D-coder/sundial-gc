use crate::arena::*;
use crate::{gc::RootIntern, mark::*};
use log::info;
use smallvec::SmallVec;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::sync::{
    atomic::{AtomicPtr, Ordering, AtomicBool},
    Mutex,
};

use std::panic;
use std::thread::{self, ThreadId};
use std::{
    alloc::{GlobalAlloc, Layout, System},
    cmp::max,
    mem, process, ptr,
};

pub static TRIGGER_MAJOR_GC: AtomicBool = AtomicBool::new(false);

/// Used to register a thread's interest in a type.
/// Before a worker thread can allocate or read a `Gc<T>`, it must register for `T`.
pub(crate) static THREAD_TYPE_REG_BUS: AtomicPtr<Mutex<GcThreadBus>> =
    AtomicPtr::new(ptr::null_mut());

// TODO replace with a real bus.
// It was a std::sync::mpsc, but mpsc is broken: https://github.com/rust-lang/rust/issues/39364
pub(crate) struct GcThreadBus {
    bus: SmallVec<[RegMsg; 10]>,
}

impl GcThreadBus {
    /// Start the Gc thread if needed and return type registration bus.
    #[inline(always)]
    pub(crate) fn get() -> &'static Mutex<Self> {
        let bus = THREAD_TYPE_REG_BUS.load(Ordering::Relaxed);
        if bus != ptr::null_mut() {
            unsafe { &*bus }
        } else {
            let mut bus = Box::new(Mutex::new(GcThreadBus {
                bus: SmallVec::new(),
            }));

            if ptr::null_mut()
                == THREAD_TYPE_REG_BUS.compare_and_swap(
                    ptr::null_mut(),
                    bus.as_mut(),
                    Ordering::AcqRel,
                )
            {
                info!("Starting GC thread");
                thread::spawn(|| gc_loop());
                info!("Spawned GC thread");

                Box::leak(bus)
            } else {
                GcThreadBus::get()
            }
        }
    }

    pub(crate) fn register<T: Condemned>(&mut self, bus: *const Bus) {
        self.bus.push(RegMsg::Reg(
            thread::current().id(),
            GcTypeInfo::new::<T>(),
            unsafe { BusPtr::new(bus) },
        ))
    }
}

#[derive(Copy, Clone)]
pub(crate) struct BusPtr(&'static Bus);

impl BusPtr {
    pub unsafe fn new(ptr: *const Bus) -> Self {
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

#[derive(Debug)]
struct HandlerManager {
    handlers: Handlers,
    eff_types: EffTypes,
    invariant: Invariant,
    evacuate_fn: fn(*const u8, u8, u8, *const Invariant, *mut Handlers),
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

        let header = HeaderUnTyped::from(next) as usize;
        let low = header + HeaderUnTyped::low_offset(align) as usize;
        let last = max(low, size as usize + next as usize);
        let mut ptr = header + HeaderUnTyped::high_offset(align, size) as usize;

        while ptr >= last {
            let t = unsafe { &*(ptr as *const u8) };
            // TODO(mark_evac) There's redundancy in the signature of evacuate.
            evacuate_fn(t, 0, invariant.grey_feilds, invariant, handlers);
            ptr -= size as usize;
        }
    }

    fn unclean_children(
        &mut self,
        align: u16,
    ) -> SmallVec<[(&GcTypeInfo, SmallVec<[*mut u8; 4]>); 4]> {
        let HandlerManager {
            handlers,
            eff_types,
            last_nexts,
            ..
        } = self;

        if handlers.nexts != *last_nexts {
            log::trace!("last_nexts: {:?}", last_nexts);
            log::trace!("this_nexts: {:?}", handlers.nexts);
        };

        let mut unclean: SmallVec<[(&GcTypeInfo, SmallVec<[*mut u8; 4]>); 4]> =
            eff_types.iter().map(|ti| (ti, SmallVec::new())).collect();

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

        *last_nexts = handlers.nexts.clone();
        log::trace!("Cleaned handlers: {:?}", handlers);
        log::trace!("Cleaned unclean: {:?}", unclean);

        unclean.into_iter().filter(|(_, v)| !v.is_empty()).collect()
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

struct Arenas {
    /// The last seen `next`. ptr + size..high_offset is owned by gc
    /// Will not include all Arenas owned by workers.
    /// Only contains Arenas that the gc owns some of.
    worker: HashMap<*const HeaderUnTyped, *const u8>,
    /// Arena owned by GC, that have not been completely filled.
    partial: BTreeSet<*mut u8>,
    full: HashSet<*mut HeaderUnTyped>,
    condemned: HashSet<*mut u8>,
}

impl Default for Arenas {
    fn default() -> Self {
        Arenas {
            worker: HashMap::new(),
            partial: BTreeSet::new(),
            full: HashSet::new(),
            condemned: HashSet::new(),
        }
    }
}

/// This whole struct is unsafe!
struct TypeState {
    type_info: GcTypeInfo,
    direct_children: SmallVec<[GcTypeInfo; 2]>,
    handler: Option<HandlerManager>,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    arenas: Arenas,
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
            arenas,
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
        log::trace!(
            "\n  epoch: {}\n  type: {:?}\n  buses: {}\n\n",
            epoch,
            type_info,
            buses.len()
        );

        let inv = handler.as_ref().map(|h| h.invariant).clone();
        // TODO lift allocation
        let mut grey: SmallVec<[(*const u8, u8); 8]> = SmallVec::new();
        buses.iter().for_each(|(_, bus)| {
            let mut has_gc_msg = false;
            let mut bus = bus.0.lock().expect("Could not unlock bus");
            log::trace!("GC Locked bus");

            // TODO fill partial by sending them to workers.
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
                    log::trace!(
                        "GC received: {:?}",
                        Msg::End {
                            white_start: *white_start,
                            white_end: *white_end,
                            next: *next,
                            release_to_gc: *release_to_gc,
                            new_allocation: *new_allocation,
                            grey_feilds: *grey_feilds,
                        }
                    );

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
                            log::trace!("Full Arena released");
                            let header = HeaderUnTyped::from(next) as *mut _;
                            arenas.full.insert(header);
                        } else {
                            log::trace!("Non full Arena released");
                            arenas.partial.insert(next as *mut _);
                        };
                    } else {
                        debug_assert!(!full(next, type_info.align));
                        // store active worker arenas
                        arenas.worker.insert(HeaderUnTyped::from(next), next);
                    };

                    *msg = Msg::Slot;
                    ret
                }
                Msg::Gc {
                    next: next @ None, ..
                } => {
                    has_gc_msg = true;
                    *next = arenas
                        .partial
                        .pop_first()
                        .map(|p| p as *mut _)
                        .or(free.pop().map(|header| {
                            HeaderUnTyped::init(header);
                            (header as usize
                                + HeaderUnTyped::high_offset(type_info.align, type_info.size)
                                    as usize) as *mut u8
                        }));
                    log::trace!("GC sent Arena {:?}", *next);
                    None
                }
                _ => None,
            }));

            if !has_gc_msg {
                log::trace!("Adding Msg::GC to bus");
                bus.iter_mut().filter(|m| m.is_slot()).next().map(|slot| {
                    let next = arenas
                        .partial
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
            };

            log::trace!("\n\n GC sent:\n {:?}\n\n", bus);
            drop(bus);
        });

        // trace grey, updating refs
        if let Some(ref mut handler) = handler {
            // TODO bring back granular tracing using bits
            grey.iter().for_each(|(next, bits)| {
                let GcTypeInfo { align, size, .. } = *type_info;
                handler.root(*next as *mut u8, align, size)
            });

            let Arenas {
                full,
                partial,
                condemned,
                ..
            } = &mut self.arenas;

            condemned.iter().cloned().for_each(|next| {
                let header = unsafe { &*HeaderUnTyped::from(next) };
                let mut roots = header
                    .roots
                    .lock()
                    .expect("Could not unlock roots, in TypeState::step");
                roots.drain().into_iter().for_each(|(idx, root_ptr)| {
                    let GcTypeInfo { align, size, .. } = *type_info;
                    let high = header as *const _ as usize
                        + HeaderUnTyped::high_offset(align, size) as usize;
                    let gc = (high - ((idx as usize) * size as usize)) as *const u8;
                    let HandlerManager {
                        invariant,
                        handlers,
                        evacuate_fn,
                        ..
                    } = handler;
                    evacuate_fn(gc, 1, invariant.grey_feilds, invariant, handlers);

                    let root = unsafe { &mut *(root_ptr as *mut RootIntern<u8>) };
                    if root.ref_count.load(Ordering::Relaxed) == 0 {
                        unsafe { ptr::drop_in_place(root) }
                    } else {
                        let next = partial
                            .pop_first()
                            .map(|p| p as *mut _)
                            .or(free.pop().map(|header| {
                                HeaderUnTyped::init(header);
                                (header as usize
                                    + HeaderUnTyped::high_offset(type_info.align, type_info.size)
                                        as usize) as *mut u8
                            }))
                            .unwrap_or(Arena::alloc());

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
                            partial.insert(ptr as *mut _);
                        }
                    };
                });
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
        free: &mut Vec<*mut HeaderUnTyped>,
        condemned_children: &EffTypes,
    ) {
        self.phase2 = false;
        self.pending_known_transitive_grey = 0;
        self.pending_known_grey = self.pending.len();
        // It will be one upon first `step()`
        self.epoch = 0;
        self.latest_grey = 1;
        self.pending = HashMap::new();

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
            translator: self.type_info.translator_from_fn()(condemned_children).0,
            nexts,
            filled,
            free,
        };

        self.handler = Some(HandlerManager {
            last_nexts: SmallVec::from_elem(ptr::null_mut(), condemned_children.len()),
            eff_types: condemned_children.clone(),
            handlers,
            invariant: inv,
            evacuate_fn: unsafe { self.type_info.evacuate_fn() },
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
            type_info.direct_gc_types_fn()(&mut direct_children, 0);
            let direct_children = direct_children
                .into_iter()
                .map(|(child, row)| {
                    let cp = parents.entry(child).or_default();
                    cp.direct.insert(type_info, row.1);
                    child
                })
                .collect();

            let mut tti = Tti::new();
            type_info.transitive_gc_types_fn()(&mut tti as *mut Tti);
            tti.type_info.iter().for_each(|child| {
                let cp = parents.entry(*child).or_default();
                cp.transitive.insert(type_info);
            });

            TypeState {
                type_info,
                direct_children,
                handler: None,
                buses: HashMap::new(),
                arenas: Arenas::default(),
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

fn gc_loop() {
    panic::set_hook(Box::new(|panic_info| {
        let backtrace = std::backtrace::Backtrace::force_capture();
        if let Some(s) = panic_info.payload().downcast_ref::<&str>() {
            println!("panic occurred: {},\n\n{}", s, backtrace);
            log::error!("panic occurred: {},\n\n{}", s, backtrace);
        } else {
            println!("panic occurred");
            log::error!("panic occurred,\n\n{}", backtrace);
        }

        process::exit(0);
    }));

    let mut free: Vec<*mut HeaderUnTyped> = Vec::with_capacity(100);
    let mut types = TypeRelations::new();
    let mut pre_arena_count: usize = 1;

    let mut bus = ptr::null_mut();
    while bus == ptr::null_mut() {
        bus = THREAD_TYPE_REG_BUS.load(Ordering::Relaxed);
    }
    let bus = unsafe { &*bus };

    loop {
        log::trace!("GC loop");
        // Take a snapshot of the thread type reg bus
        let reg_msgs = {
            let mut bus = bus.lock().unwrap();
            let mut new = SmallVec::new();

            mem::swap(&mut bus.bus, &mut new);
            new
        };

        info!("New types registered: {}", reg_msgs.len());
        for msg in reg_msgs.into_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => {
                    info!("Thread: {:?} registered for GcTypeInfo: {:?}", id, ty);
                    types.reg(ty, id, bus)
                }
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

            arena_count += ts.arenas.full.len();
        });

        stack_cleared.drain().for_each(|ti| {
            clear_stack_old.remove(&ti).unwrap().iter().for_each(|cti| {
                let ts = active.get_mut(cti).unwrap();
                ts.waiting_on_transitive_parents -= 1;
                if ts.waiting_on_transitive_parents == 0 {
                    // Free memory
                    ts.arenas.condemned.drain().into_iter().for_each(|next| {
                        let header = HeaderUnTyped::from(next) as *mut HeaderUnTyped;

                        let roots = unsafe { &*header }
                            .roots
                            .lock()
                            .expect("Could not unlock roots while freeing Arena");
                        if roots.len() != 0 {
                            // FIXME shoud reset latest_grey and try again latter.
                            panic!("Old Arena Rooted");
                        }

                        if cti.needs_drop {
                            let GcTypeInfo { align, size, .. } = ti;

                            let low = max(
                                next as usize,
                                header as usize + HeaderUnTyped::low_offset(align) as usize,
                            );
                            let mut ptr =
                                low as usize + HeaderUnTyped::high_offset(align, size) as usize;

                            let drop_in_place = unsafe { ti.drop_in_place_fn() };

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
                             eff_types,
                             ..
                         }| {
                            debug_assert_eq!(handlers.filled.iter().flatten().count(), 0);
                            debug_assert_eq!(handlers.nexts.len(), eff_types.len());
                            handlers
                                .nexts
                                .into_iter()
                                .zip(eff_types.into_iter())
                                .for_each(|(next, ti)| {
                                    let ts = active.get_mut(&ti).unwrap();
                                    // Add arenas left in Handler to the pool;
                                    if next as usize % ARENA_SIZE == 0 {
                                        ts.arenas.full.insert(next as *mut HeaderUnTyped);
                                    } else {
                                        ts.arenas.partial.insert(next);
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

        if !gc_in_progress && (arena_count > pre_arena_count || TRIGGER_MAJOR_GC.load(Ordering::Relaxed)) {
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

                ts.set_invariant(inv, &mut free, &ts.direct_children.clone());

                // FIXME setting header.condemned causes a race with Root::from.
                // The solution is to move switch away from Mutex<HashMap>

                // Mark all the GC's arenas as condemned.
                ts.arenas.full.iter().for_each(|header| {
                    let header = unsafe { &mut **header };
                    header.condemned = true;
                });

                debug_assert_eq!(ts.arenas.condemned.len(), 0);
                unsafe {
                    ptr::swap(
                        &mut ts.arenas.full as *mut HashSet<*mut HeaderUnTyped>
                            as *mut HashSet<*mut u8>,
                        &mut ts.arenas.condemned,
                    )
                }

                let TypeState {
                    arenas:
                        Arenas {
                            ref mut condemned,
                            partial,
                            ..
                        },
                    ..
                } = ts;
                partial.iter().cloned().for_each(|next| {
                    let header = unsafe { &mut *(HeaderUnTyped::from(next) as *mut HeaderUnTyped) };
                    header.condemned = true;
                    condemned.insert(next);
                });

                ts.arenas.partial = BTreeSet::new();

                info!("Condemed {} Arenas", condemned.len());
            });

            active.iter_mut().for_each(
                |(
                    ti,
                    TypeState {
                        arenas, handler, ..
                    },
                )| {
                    if let Some(hm) = handler {
                        arenas
                            .worker
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
                if ti.gc_count != 0 {
                    let ts = active.get_mut(&ti).unwrap();
                    let hm = ts.handler.as_mut().unwrap();

                    ts.arenas.full.extend(
                        nexts
                            .iter()
                            .cloned()
                            // Only add Headers
                            // nexts are still owned by Handlers
                            .filter(|ptr| *ptr as usize % ARENA_SIZE == 0)
                            .map(|header| header as *mut HeaderUnTyped),
                    );

                    log::trace!("rooting nexts");
                    nexts
                        .into_iter()
                        .for_each(|next| hm.root(next as *mut u8, ti.align, ti.size));

                    log::trace!("new_arenas: {:?}", new_arenas);
                    hm.new_arenas(ti.align, &mut new_arenas);
                }
            });
        }

        if !gc_in_progress {
            TRIGGER_MAJOR_GC.store(false, Ordering::Relaxed);
        }
        // thread::sleep(Duration::from_millis(100))
    }
}
