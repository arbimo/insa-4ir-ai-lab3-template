use std::time::Instant;

use crate::board::{Action, Board};

pub trait Engine {
    /// Selects the next action with a deadline
    /// If the engine returns `None`, it will be interpret as having lost.
    fn select(&mut self, board: &Board, deadline: Instant) -> Option<Action>;

    /// Forget all history (to be called to reset the engine in between games)
    fn clear(&mut self);
}
