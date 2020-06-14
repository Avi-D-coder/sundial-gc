use super::arena::*;
use super::trace::*;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::{mpsc, mpsc::*, Mutex};
use std::thread::{self, ThreadId};
use std::time::Duration;

pub(crate) static mut REGISTER: Option<Sender<RegMsg>> = None;

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

/// This whole struct is unsafe!
struct TypeState {
    type_info: TypeInfo,
    /// Map Type to Bit
    buses: HashMap<ThreadId, BusPtr>,
    // Map between `Arena.next` and the invariant the section of the Msg::End..End Arena upheld.
    invariants: BTreeMap<*const u8, Invariant>,
    current_invariant: Option<Invariant>,
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
                invariants: BTreeMap::new(),
                current_invariant: None,
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
    let mut types: HashMap<GcTypeInfo, TypeInfo> = HashMap::new();
    let mut type_states: HashMap<GcTypeInfo, TypeState> = HashMap::new();

    loop {
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => {
                    types
                        .entry(ty.info)
                        .and_modify(|ti| assert_eq!(ti.drop_ptr as usize, ty.drop_ptr as usize))
                        .or_insert(ty);
                    // let buses = types.entry(ty.info)
                    // buses
                    //     .entry(id)
                    //     .and_modify(|_| {
                    //         panic!("Impossible: The same type and thread was registered twice")
                    //     })
                    //     .or_insert(bus);

                    // if known_types.insert(ty) {
                    //     direct_children.insert(ty, direct_gced(ti, gced))
                    // }
                }
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

        thread::sleep(Duration::from_millis(100))
    }
}
