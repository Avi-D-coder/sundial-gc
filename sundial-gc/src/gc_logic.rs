pub(crate) mod bus;
pub(crate) mod free_list;
pub(crate) mod type_group;
pub(crate) mod type_state;

use crate::mark::*;
use smallvec::SmallVec;
use std::collections::HashMap;
use std::sync::{
    atomic::{AtomicPtr, Ordering},
    Mutex,
};

use bus::{Bus, BusPtr};
use std::panic;
use std::thread::{self, ThreadId};
use std::{mem, process, ptr, time::Duration};
use type_group::{TypeGroup, TypeGroups};
use type_state::{TotalRelations, TypeState};

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
                log::info!("Starting GC thread");
                thread::spawn(|| gc_loop());
                log::info!("Spawned GC thread");

                Box::leak(bus)
            } else {
                GcThreadBus::get()
            }
        }
    }

    pub(crate) fn register<T: Trace>(&mut self, bus: *const Bus) {
        self.bus.push(RegMsg::Reg(
            thread::current().id(),
            GcTypeInfo::new::<T>(),
            unsafe { BusPtr::new(bus) },
        ))
    }
}

#[derive(Copy, Clone)]
pub(crate) enum RegMsg {
    Reg(ThreadId, GcTypeInfo, BusPtr),
    Un(ThreadId, GcTypeInfo),
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

    let mut active: HashMap<&'static GcTypeInfo, &'static TypeState> = Default::default();
    let mut type_groups: TypeGroups = Default::default();
    let mut total_relations: TotalRelations = Default::default();

    let mut bus = ptr::null_mut();
    while bus == ptr::null_mut() {
        bus = THREAD_TYPE_REG_BUS.load(Ordering::Relaxed);
    }
    let bus = unsafe { &*bus };

    loop {
        log::trace!("GC loop");
        // Take a snapshot of the thread type registration bus
        let reg_msgs = {
            let mut bus = bus.lock().unwrap();
            let mut new = SmallVec::new();

            mem::swap(&mut bus.bus, &mut new);
            new
        };

        log::trace!("New types registered: {}", reg_msgs.len());
        for msg in reg_msgs.into_iter() {
            match msg {
                RegMsg::Reg(thread_id, ti, bus) => {
                    log::info!(
                        "Thread: {:?} registered for GcTypeInfo: {:?}",
                        thread_id,
                        ti
                    );

                    if let Some(new_type_state) =
                        total_relations.register(&mut active, ti, thread_id, bus)
                    {
                        type_groups.register(new_type_state);
                        log::info!("{:?}", type_groups);
                    };
                }
                RegMsg::Un(_thread_id, _ti) => {
                    // The thread was dropped
                    // TODO Currently `Un` is never sent.
                }
            }
        }

        log::trace!("{:?}", type_groups);
        type_groups.groups.iter().for_each(|(_, tg)| {
            let tg = unsafe { &mut *(*tg as *mut TypeGroup) };
            tg.step();
            log::trace!("step");
        });

        thread::sleep(Duration::from_millis(10))
    }
}
