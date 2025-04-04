#![allow(unused)]
use std::time::{Duration, Instant};

use board::*;
use engine::Engine;
use mcts::MctsEngine;
use minimax::MinimaxEngine;
use rand::seq::IndexedRandom;

pub mod board;
pub mod engine;
pub mod mcts;
pub mod minimax;
mod uce_mcts;

/// Applies a number of randomly select moves and return the resulting board.
///
/// This function is typically used to generate original starting point for the games.
fn after_random_moves(board: &Board, n: usize) -> Board {
    let mut cur = board.clone();
    for _ in 0..n {
        let actions = &mut cur.actions();
        let action = actions.choose(&mut rand::rng()).unwrap();
        cur.apply_mut(action);
    }
    cur
}

/// Plays a single game, starting form the given board.
/// Time is limited to to the given `time_per_move` at each turn
/// (but we do not enforce it and minimax would typically ignore it).
fn play_game<'a>(
    board: &Board,
    white: &'a mut dyn Engine,
    black: &'a mut dyn Engine,
    time_per_move: Duration,
    verbose: bool,
) -> Board {
    // erase engines history
    white.clear();
    black.clear();

    let mut board = board.clone();

    while !board.is_draw() {
        if verbose {
            println!("{board}");
        }
        // select the engine
        let engine = match board.turn {
            board::Color::White => &mut *white,
            board::Color::Black => &mut *black,
        };
        let deadline = Instant::now() + time_per_move;
        if let Some(action) = engine.select(&board, deadline) {
            if verbose {
                println!("\n action: {action}\n");
            }
            board.apply_mut(&action);
        } else {
            // no possible actions, game is over
            return board;
        }
    }
    board
}

fn main() {
    let b = Board::init();

    example_game();
}

#[allow(unused)]
fn example_game() {
    // generate the initial board
    let board = Board::init();

    // play a few random move to make sure we have an fairly original starting point
    let board = after_random_moves(&board, 2);

    let mut white_engine = MinimaxEngine::new(6);
    // let mut white_engine = MctsEngine::new(1.);
    let mut black_engine = MinimaxEngine::new(6);
    let mut black_engine = uce_mcts::MinimaxEngine::new(8);

    let final_board = play_game(
        &board,
        &mut white_engine,
        &mut black_engine,
        Duration::from_millis(500),
        true,
    );

    println!("Final board: \n{final_board}");
}
