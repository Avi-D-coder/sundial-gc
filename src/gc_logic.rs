use super::arena::*;
use super::trace::*;

use std::collections::{BTreeMap, HashMap};
use std::sync::{mpsc, mpsc::*, Mutex};
use std::thread::{self, ThreadId};
use std::time::Duration;

pub(crate) static mut REGISTER: Option<Sender<RegMsg>> = None;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct TypeInfo {
    info: GcTypeInfo,
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

fn start() -> Sender<RegMsg> {
    let (s, r) = channel();
    thread::spawn(move || gc_loop(r));
    s
}

fn gc_loop(r: Receiver<RegMsg>) {
    let mut types: HashMap<TypeInfo, HashMap<ThreadId, BusPtr>> = HashMap::new();
    // Key is 16 kB aligned Arena, value is `Arena.next`.
    // TODO This will not work once we are freeing regions containing multiple arenas.
    // Regions must be contiguous, since Gc.ptr may be a &'static T.
    // To fix this, we should probably differentiate between Gc<T> and &'static T.
    let mut mem: BTreeMap<usize, (*const u8, TypeInfo)> = BTreeMap::new();
    // Map between `Arena.next` and the invariant the section of the Msg::End..End Arena upheld.
    let mut arena_invariants: HashMap<TypeInfo, BTreeMap<*const u8, Invariant>> = HashMap::new();

    loop {
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty, bus) => {
                    let buses = types.entry(ty).or_insert(HashMap::new());
                    buses
                        .entry(id)
                        .and_modify(|_| {
                            panic!("Impossible: The same type and thread was registered twice")
                        })
                        .or_insert(bus);
                }
                RegMsg::Un(id, ty) => {
                    // The thread was dropped
                    let buses = types
                        .get_mut(&ty)
                        .expect("Impossible: Freed bus of type without an buses");
                    let bus = buses
                        .get(&id)
                        .expect("Impossible: Freed thread's bus that does not exist");
                    let bus = bus.0.lock().expect("Could not unlock freed bus");

                    let inv = arena_invariants.entry(ty);

                    for m in bus.iter() {
                        if let Msg::End {
                            next,
                            grey_feilds,
                            white_start,
                            white_end,
                            ..
                        } = m
                        {
                            mem.insert(Header::from(next) as usize, (*next, ty));
                            // TODO
                        }
                    }

                    buses.remove(&id);
                }
            }
        }

        thread::sleep(Duration::from_millis(100))
    }
}
