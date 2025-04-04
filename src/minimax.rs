use core::f32;
use std::time::Instant;

use crate::engine::Engine;

use super::board::*;

const PAWN_VALUE: f32 = 1.;
const QUEEN_VALUE: f32 = 3.;

fn heuristic_evaluation(board: &Board) -> f32 {
    let white_pov = white_heuristic_evaluation(board);
    match board.turn {
        Color::White => white_pov,
        Color::Black => -white_pov,
    }
}

pub fn white_heuristic_evaluation(board: &Board) -> f32 {
    let cnt = |c: Cell| board.count(c) as f32;
    PAWN_VALUE * cnt(Cell::WhitePawn) + QUEEN_VALUE * cnt(Cell::WhiteQueen)
        - PAWN_VALUE * cnt(Cell::BlackPawn)
        - QUEEN_VALUE * cnt(Cell::BlackQueen)
}

pub struct MinimaxEngine {
    pub max_depth: u32,
}

impl MinimaxEngine {
    pub fn new(max_depth: u32) -> MinimaxEngine {
        MinimaxEngine { max_depth }
    }
}

impl Engine for MinimaxEngine {
    fn select(&mut self, board: &Board, _deadline: Instant) -> Option<Action> {
        let actions = board.actions();
        let mut best_value = f32::MIN;
        let mut best_action = None;
        for a in actions {
            let result = board.apply(&a);
            let value = -minimax_eval(&result, self.max_depth);
            if value > best_value {
                best_value = value;
                best_action = Some(a);
            }
        }
        best_action
    }

    fn clear(&mut self) {
        // no history to clean
    }
}

fn minimax_eval(board: &Board, remaining_depth: u32) -> f32 {
    if remaining_depth == 0 {
        return heuristic_evaluation(board);
    }

    let actions = board.actions();
    if actions.is_empty() {
        // game finished, return the evaluation
        return heuristic_evaluation(board);
    }

    let mut best_value = f32::MIN;
    for a in actions {
        let result = board.apply(&a);
        // the value for the action the opposite of the one of the resulting state
        // (which is evaluated from the point of view of the other player)
        // This variant of minimax is called `negamax`
        let value = -minimax_eval(&result, remaining_depth - 1);
        if value > best_value {
            best_value = value;
        }
    }
    best_value
}
