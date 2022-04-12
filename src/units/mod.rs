mod comp_unit;
mod global;
pub mod intrinsics;

pub use comp_unit::CompUnitStatus;
pub use global::{GlobalStatus, GLOBAL_STAT};

/// Returns true if the current thread is the
/// thread of the global compilation unit
pub fn inside_global_unit() -> bool {
    crate::GLOBAL_STAT
        .lock()
        .unwrap()
        .units
        .get(&std::thread::current().id())
        .is_none()
}

#[macro_export]
macro_rules! current_unit_status {
    () => {
        crate::GLOBAL_STAT
            .lock()
            .unwrap()
            .units
            .get(&std::thread::current().id())
            .unwrap()
    };
}

#[macro_export]
macro_rules! current_unit_st {
    () => {
        crate::GLOBAL_STAT
            .lock() // first mutex (GlobalStatus)
            .unwrap()
            .units
            .get(&std::thread::current().id())
            .unwrap()
            .lock() // second mutex (CompUnitStatus)
            .unwrap()
            .st
    };
}

#[macro_export]
macro_rules! current_unit_protos {
    () => {
        crate::GLOBAL_STAT
            .lock() // first mutex (GlobalStatus)
            .unwrap()
            .units
            .get(&std::thread::current().id())
            .unwrap()
            .lock() // second mutex (CompUnitStatus)
            .unwrap()
            .protos
    };
}

#[macro_export]
macro_rules! current_unit_ast {
    () => {
        crate::GLOBAL_STAT
            .lock() // first mutex (GlobalStatus)
            .unwrap()
            .units
            .get(&std::thread::current().id())
            .unwrap()
            .lock() // second mutex (CompUnitStatus)
            .unwrap()
            .ast
    };
}
