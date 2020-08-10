use crate::*;
use arena::HeaderUnTyped;
use gc_logic::{free_list::FreeList, type_state::*};
use mark::GcTypeInfo;
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    mem, ptr,
    time::{Duration, Instant},
};

pub(crate) struct TypeGroups {
    pub groups: HashMap<&'static GcTypeInfo, *const TypeGroup>,
}

impl Debug for TypeGroups {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let set: HashSet<_> = self.groups.values().cloned().collect();

        let _ = f.write_str("TypeGroups: ");
        f.debug_set()
            .entries(set.iter().map(|tg| unsafe { &**tg }))
            .finish()
    }
}

impl Default for TypeGroups {
    fn default() -> Self {
        TypeGroups {
            groups: Default::default(),
        }
    }
}

impl TypeGroups {
    /// Register a new type, merging newly related `TypeGroup`s.
    pub(crate) fn register(&mut self, type_state: &'static TypeState) {
        debug_assert!(!self.groups.contains_key(&type_state.type_info));

        let relations = unsafe { &*type_state.relations.get() }.active_relations();

        let groups: HashSet<*const TypeGroup> = relations
            .iter()
            .filter_map(|ti| self.groups.get(ti).cloned())
            .collect();

        let group = Box::leak(Box::new(TypeGroup::default()));
        group.related.push(type_state);

        let gc_in_progress = groups.iter().cloned().fold(false, |gc_in_progress, tg| {
            let tg = unsafe { &mut *(tg as *mut TypeGroup) };
            group.free.merge(&mut tg.free);
            group.related.extend(tg.related.iter());

            // Registering a new type resets gc state.
            // If a collection was in progress it is restarted.
            let r = gc_in_progress || tg.gc_in_progress.is_some();

            // Free the old group
            unsafe { drop(Box::from_raw(tg as *const _ as *mut TypeGroup)) }
            r
        });

        // Add new type_state and update old type_state => TypeGroup mappings.
        group.related.iter().for_each(|ts| {
            self.groups.insert(&ts.type_info, group);
        });

        if gc_in_progress {
            group.major_gc();
        };
    }
}

pub(crate) struct TypeGroup {
    related: Vec<&'static TypeState>,
    free: FreeList,
    gc_in_progress: Option<Instant>,
    last_gc_completed: Instant,
    pre_allocated: usize,
    log_duration: Duration,
}

impl Debug for TypeGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypeGroup")
            .field(
                "related",
                &self
                    .related
                    .iter()
                    .map(|ts| ts.type_info.type_name)
                    .collect::<Vec<_>>(),
            )
            .field("gc_in_progress", &self.gc_in_progress)
            .field("last_gc_completed", &self.last_gc_completed)
            .field("pre_allocated", &self.pre_allocated)
            .finish()
    }
}

impl Default for TypeGroup {
    fn default() -> Self {
        TypeGroup {
            related: Vec::new(),
            free: FreeList::default(),
            gc_in_progress: None,
            last_gc_completed: Instant::now(),
            pre_allocated: 1,
            log_duration: Duration::from_secs(1),
        }
    }
}

impl TypeGroup {
    /// Trigger the collection of all gc owned Arenas.
    pub(crate) fn major_gc(&mut self) {
        let TypeGroup {
            related,
            ref mut free,
            ref mut gc_in_progress,
            ..
        } = self;

        related.iter().for_each(|ts| {
            // 0 is null.
            let state = unsafe { &mut *ts.state.get() };
            let arenas = unsafe { &mut *ts.arenas.get() };
            let relations = unsafe { &mut *ts.relations.get() };
            let pending = unsafe { &mut *ts.pending.get() };

            let invariant_id = ts.bump_invariant_id(pending);
            let mut collection =
                Collection::new(pending.epoch, &ts.type_info, relations, free, invariant_id);
            log::trace!("{:?}", collection);

            if let Some(Collection {
                condemned:
                    Arenas {
                        full,
                        partial,
                        worker,
                    },
                handler,
                ..
            }) = state
            {
                log::warn!("Collection already in progress");
                mem::swap(full, &mut collection.condemned.full);
                mem::swap(partial, &mut collection.condemned.partial);
                mem::swap(worker, &mut collection.condemned.worker);
                handler.drop();
            };

            log::trace!("marking: full {:?}", collection.condemned.full);
            collection
                .condemned
                .full
                .extend(arenas.full.drain().map(|(header, top)| {
                    unsafe { &mut *(header as *mut HeaderUnTyped) }.condemned = true;
                    (header, top)
                }));

            log::trace!("marking: partial {:?}", collection.condemned.partial);
            collection
                .condemned
                .partial
                .extend(arenas.partial.drain().map(|(header, (next, top))| {
                    let header = unsafe { &mut *(header as *mut HeaderUnTyped) };
                    header.condemned = true;
                    (header as _, (next, top))
                }));

            log::trace!("marking: worker {:?}", collection.condemned.worker);
            arenas.worker.iter_mut().for_each(|(h, (next, end))| {
                let header = unsafe { &mut *(*h as *mut HeaderUnTyped) };
                header.condemned = true;

                collection
                    .condemned
                    .worker
                    .entry(*h)
                    .and_modify(|(n, _)| {
                        *n = *next;
                    })
                    .or_insert((*next, *end));

                *end = *next;
            });

            *state = Some(collection);
        });

        *gc_in_progress = Some(Instant::now());
        log::warn!("Major GC started {:?}", self);
    }

    pub(crate) fn step(&mut self) {
        log::trace!("step");
        log::trace!(
            "TypeGroup {:?}",
            self.related
                .iter()
                .map(|ts| ts.type_info.type_name)
                .collect::<Vec<_>>()
        );

        let TypeGroup {
            related,
            ref mut free,
            gc_in_progress,
            last_gc_completed,
            pre_allocated,
            log_duration,
        } = self;

        if related.iter().fold(true, |done, ts| unsafe {
            let r = ts.step(free) && done;
            log::trace!("r: {}", r);
            r
        }) {
            *gc_in_progress = None;
            *last_gc_completed = Instant::now();

            log::warn!("reseting");
            related.iter().for_each(|ts| unsafe { ts.reset() });

            let debug = || {
                related.iter().cloned().for_each(|ts| {
                    let slots = unsafe { &mut *ts.slots.get() };
                    let mut ptrs: Vec<_> = slots
                        .iter()
                        .filter(|(_, live)| **live)
                        .map(|(p, _)| *p)
                        .collect();

                    if ptrs.is_empty() {
                        slots.clear();
                        slots.shrink_to_fit();
                    } else {
                        ptrs.sort();

                        let mut ranges = Vec::new();
                        let r = ptrs.into_iter().fold(
                            (ptr::null(), ptr::null()..=ptr::null()),
                            |(header, range), ptr| {
                                if *range.end() as usize + ts.type_info.size as usize
                                    == ptr as usize
                                    && header == HeaderUnTyped::from(ptr)
                                {
                                    (header, *range.start()..=ptr)
                                } else {
                                    ranges.push((header, range));
                                    (HeaderUnTyped::from(ptr), ptr..=ptr)
                                }
                            },
                        );

                        ranges.push(r);
                        log::info!("Leftover: {:?}", ranges);
                    }
                });
            };

            debug();
            log::warn!("Major GC complete {:?}", self);
        } else if let Some(start) = gc_in_progress {
            log::trace!("not done");
            *pre_allocated = free.allocated;
            let elapsed = start.elapsed();
            if elapsed > *log_duration * 2 {
                *log_duration = elapsed;
                log::warn!("Major GC running for {}s, {:?}", elapsed.as_secs(), self);
            };
        } else if self.free.allocated >= 2 * *pre_allocated
            || last_gc_completed.elapsed() > Duration::from_secs(2)
        {
            self.major_gc();
        };
        log::trace!("TypeGroup::step complete");
    }
}
