use crate::*;
use arena::HeaderUnTyped;
use gc_logic::{free_list::FreeList, type_state::*};
use mark::GcTypeInfo;
use std::{
    collections::{HashMap, HashSet},
    time::{Duration, Instant},
};

pub(crate) struct TypeGroups {
    pub groups: HashMap<&'static GcTypeInfo, *const TypeGroup>,
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
        let relations = unsafe { &*type_state.relations.get() }.active_relations();

        let groups: HashSet<*const TypeGroup> = relations
            .iter()
            .filter_map(|ti| self.groups.get(ti).cloned())
            .collect();

        let group = Box::leak(Box::new(TypeGroup::default()));
        group.related.push(type_state);

        let gc_in_progress =
            groups
                .iter()
                .cloned()
                .map(|tg| unsafe { &*tg })
                .fold(false, |gc_in_progress, tg| {
                    group.free.free.extend(tg.free.free.iter());
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
    pre_allocated: usize,
}

impl Default for TypeGroup {
    fn default() -> Self {
        TypeGroup {
            related: Vec::new(),
            free: FreeList::default(),
            gc_in_progress: None,
            pre_allocated: 1,
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
            let state = unsafe { &mut *ts.state.get() };
            let arenas = unsafe { &mut *ts.arenas.get() };
            let relations = unsafe { &mut *ts.relations.get() };

            let mut collection = Collection::new(&ts.type_info, relations, free);

            collection.condemned = state
                .as_ref()
                .map(|c| c.condemned.clone())
                .unwrap_or_default();

            collection
                .condemned
                .extend(arenas.full.drain().map(|header| {
                    unsafe { &mut *header }.condemned = true;
                    header as *mut u8
                }));
            collection
                .condemned
                .extend(arenas.partial.0.drain().map(|next| {
                    let header = unsafe { &mut *(HeaderUnTyped::from(next) as *mut HeaderUnTyped) };
                    header.condemned = true;
                    next
                }));
        });

        *gc_in_progress = Some(Instant::now());
        log::info!("Major GC started");
    }

    pub(crate) fn step(&mut self) {
        let TypeGroup {
            related,
            ref mut free,
            gc_in_progress,
            pre_allocated,
        } = self;

        if related
            .iter()
            .fold(true, |done, ts| unsafe { done && ts.step(free) })
        {
            *gc_in_progress = None;
            *pre_allocated = free.allocated;
            log::info!("Major GC complete");
        } else if let Some(start) = gc_in_progress {
            let elapsed = start.elapsed();
            if elapsed > Duration::from_secs(2) {
                log::warn!("Major Gc running for {}s", elapsed.as_secs());
            };
        };

        if self.free.allocated >= 2 * *pre_allocated {
            self.major_gc()
        };
    }
}
