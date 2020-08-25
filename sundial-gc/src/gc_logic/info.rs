use super::type_state::TypeState;
use std::sync::atomic::Ordering;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TypeCollectionInfo {
    state: &'static TypeState,
}

impl TypeCollectionInfo {
    pub fn major_cycle(&self) -> usize {
        self.state.major_cycle.load(Ordering::Acquire)
    }
}
