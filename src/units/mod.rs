mod comp_unit;
mod global;
pub mod intrinsics;

pub use comp_unit::CompUnitStatus;
pub use global::GLOBAL_STAT;

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
