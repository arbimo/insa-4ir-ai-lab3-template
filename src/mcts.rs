use std::{
    fmt::Display,
    time::{Duration, Instant},
};

use hashbrown::HashMap;
use itertools::Itertools;
use rand::seq::IndexedRandom;

use crate::engine::Engine;

use super::board::*;

/// Function that evaluates a final board (draw, or no remaning actions for the current player).
pub fn white_score(board: &Board) -> f32 {
    debug_assert!(
        board.is_draw() || board.actions().is_empty(),
        "The board is not final"
    );
    todo!()
}

/// Performs a single rollout and returns the evaluation of the final state.
pub fn rollout(board: &Board) -> f32 {
    todo!()
}

/// Alias type to repesent a count of selections.
pub type Count = u64;

/// Node of the MCTS graph
struct Node {
    /// Board of the node
    board: Board,
    /// Numer of times this node has been selected
    count: Count,
    /// *All* valid actions available on the board, together with the number of times they have been selected (potentially 0)
    /// and the last known evaluation of the result board.
    /// The actions define the outgoing edges (the target nodes can be computed by applying the action on the board)
    out_edges: Vec<OutEdge>,
    /// Evaluation given by the initial rollout on expansion
    initial_eval: f32,
    /// Q(s): complete evaluation of the node (to be updated after each playout)
    eval: f32,
}

impl Node {
    /// Creates the node with a single evaluation from a rollout
    pub fn init(board: Board, initial_eval: f32) -> Node {
        // create one outgoing edge per valid action
        let out_edges = board
            .actions()
            .into_iter()
            .map(|a| OutEdge::new(a))
            .collect_vec();
        Node {
            board,
            count: 1,
            out_edges,
            initial_eval,
            eval: initial_eval,
        }
    }
}

/// Edge of the MCTS graph.
///
/// An `OutEdge` is attached to a node (source) and target can be computed by applying the action to the source.
struct OutEdge {
    // action of the edge
    action: Action,
    // N(s,a): number of times this edge was selected
    visits: Count,
    // Q(s,a): Last known evaluation of the board resulting from the action
    eval: f32,
}
impl OutEdge {
    /// Initializes a new edge for this actions (with a count and eval at 0)
    pub fn new(action: Action) -> OutEdge {
        OutEdge {
            action,
            visits: 0,
            eval: 0.,
        }
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "\n{}", self.board)?;
        write!(f, "Q: {}    (N: {})\n", self.eval, self.count)?;
        // display edges by decreasing number of samples
        for OutEdge {
            action,
            visits,
            eval,
        } in self.out_edges.iter().sorted_by_key(|e| u64::MAX - e.visits)
        {
            write!(f, "{visits:>8} {action}   [{eval}]\n")?;
        }
        Ok(())
    }
}

pub struct MctsEngine {
    /// Graph structure
    nodes: HashMap<Board, Node>,
    /// weight given to the exploration term in UCB1
    pub exploration_weight: f32,
}
impl MctsEngine {
    pub fn new(exploration_weight: f32) -> MctsEngine {
        MctsEngine {
            nodes: HashMap::new(),
            exploration_weight,
        }
    }
}

impl MctsEngine {
    /// Selects the best action according to UCB1, or `None` if no action is available.
    pub fn select_ucb1(&self, board: &Board) -> Option<Action> {
        debug_assert!(self.nodes.contains_key(board));
        todo!()
    }

    /// Performs a playout for this board (s) and returns the (updated) evaluation of the board (Q(s))
    fn playout(&mut self, board: &Board) -> f32 {
        if !self.nodes.contains_key(board) {
            todo!()
        } else {
            todo!()
        }
    }

    /// Updates the evaluation (Q(s)) of the board (s), after selected the action (a) for a new playout
    /// which yieled an evaluation of `action_eval` (Q(s,a))
    fn update_eval(&mut self, board: &Board, action: &Action, action_eval: f32) -> f32 {
        debug_assert!(self.nodes.contains_key(board));
        todo!()
    }
}

impl Engine for MctsEngine {
    fn select(&mut self, board: &Board, deadline: Instant) -> Option<Action> {
        todo!()
    }

    fn clear(&mut self) {
        self.nodes.clear();
    }
}

#[cfg(test)]
mod test {
    use crate::Color;

    use super::{Board, MctsEngine};

    #[test]
    fn test_mcts() {
        let board = Board::parse(
            "
              ABCDEFGH   White  (32 plies)
            1  b . b b
            2 . . . b
            3  . . . w
            4 . . . .
            5  . . . .
            6 . b w .
            7  . . w .
            8 w w w .",
            Color::White,
        );
        let mut mcts = MctsEngine::new(1.);

        println!("{board}");

        for i in 1..=4 {
            mcts.playout(&board);
            println!("After {i} playouts: \n{}", mcts.nodes[&board]);
        }
        println!("{board}");
    }
}
