use super::arena::*;
use super::trace::*;

use lazy_static::lazy_static;
use std::collections::{BTreeMap, HashMap};
use std::sync::{mpsc, mpsc::*, Mutex};
use std::thread::{self, ThreadId};
use std::time::Duration;

pub(crate) static mut REGISTER: Option<Sender<RegMsg>> = None;

#[derive(Debug, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub(crate) struct TypeInfo {
    info: GcTypeInfo,
    drop_ptr: fn(*mut u8),
    ptr: *const Bus,
}

unsafe impl Send for TypeInfo {}

#[derive(Debug, Copy, Clone)]
pub(crate) enum RegMsg {
    Reg(ThreadId, TypeInfo),
    Un(ThreadId, TypeInfo),
}

fn start() -> Sender<RegMsg> {
    let (s, r) = channel();
    thread::spawn(move || gc_loop(r));
    s
}

fn gc_loop(r: Receiver<RegMsg>) {
    let mut types: HashMap<TypeInfo, HashMap<ThreadId, *const Bus>> = HashMap::new();

    loop {
        for msg in r.try_iter() {
            match msg {
                RegMsg::Reg(id, ty) => {
                    let buses = types.entry(ty).or_insert(HashMap::new());
                    buses
                        .entry(id)
                        .and_modify(|_| {
                            panic!("Impossible: The same type and thread was registered twice")
                        })
                        .or_insert(ty.ptr);
                }
                RegMsg::Un(id, ty) => {
                    // The thread was dropped
                    let buses = types.get_mut(&ty).expect("Impossible: Freed bus of type without an buses");
                    let bus = buses.get(&id).expect("Impossible: Freed thread's bus that does not exist");
                    let bus = unsafe { &**bus };
                    let bus = bus.lock().expect("Could not unlock freed bus");
                    // TODO retrieve mem


                    buses.remove(&id);
                },
            }
        }

        thread::sleep(Duration::from_millis(100))
    }
}
