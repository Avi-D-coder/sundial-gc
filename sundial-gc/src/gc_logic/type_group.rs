use crate::*;
use arena::HeaderUnTyped;
use gc_logic::{free_list::FreeList, type_state::*};
use mark::GcTypeInfo;
use std::collections::{HashMap, HashSet};

pub(crate) struct TypeGroups {
    groups: HashMap<&'static GcTypeInfo, *const TypeGroup>,
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

        groups
            .iter()
            .cloned()
            .map(|tg| unsafe { &*tg })
            .for_each(|tg| {
                group.free.free.extend(tg.free.free.iter());
                group.related.extend(tg.related.iter());
            });

        group.related.iter().for_each(|ts| {
            self.groups.insert(&ts.type_info, group);
        });

        // Free the old groups
        groups
            .iter()
            .copied()
            .for_each(|tg| unsafe { drop(Box::from_raw(tg as *mut TypeGroup)) });
    }
}

pub(crate) struct TypeGroup {
    related: Vec<&'static TypeState>,
    free: FreeList,
}

impl Default for TypeGroup {
    fn default() -> Self {
        TypeGroup {
            related: Vec::new(),
            free: FreeList::default(),
        }
    }
}

impl TypeGroup {
    /// Trigger the collection of all gc owned Arenas.
    pub(crate) fn major_gc(&mut self) {
        let TypeGroup {
            related,
            ref mut free,
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
    }

    pub(crate) fn step(&mut self) {
        let TypeGroup {
            related,
            ref mut free,
        } = self;

        related.iter().for_each(|ts| unsafe {
            ts.step(free);
        });
    }
}
