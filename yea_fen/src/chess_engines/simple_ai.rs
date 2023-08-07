use std::{
    iter,
    ops::Deref,
    time::{Duration, Instant},
};

use crate::{moves::MoveType, Colored, GameState, Piece, Pos};

#[derive(Default)]
pub struct Searcher {
    search: SearchType,
    nodes_seached: usize,
}

impl Searcher {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn search(&mut self, gs: impl Deref<Target = GameState>) -> Option<Move> {
        let gs = gs.deref().clone();
        match self.search {
            SearchType::Timed(_, _) => {
                let mut best_move = None;
                for depth in 1..usize::MAX {
                    if !self.search.more_time() {
                        return best_move;
                    }
                    if let Some(new_best_move) = self.search_root_depth(gs.clone(), depth) {
                        best_move = Some(new_best_move);
                    }
                }
                best_move
            }
            SearchType::Depth(d) => self.search_root_depth(gs, d),
        }
    }

    pub fn search_root_depth(&mut self, mut gs: GameState, depth: usize) -> Option<Move> {
        let generate_all_moves: Vec<_> = generate_all_moves(&mut gs).collect();
        if generate_all_moves.len() == 1 {
            return Some(generate_all_moves[0]);
        }
        for r#move in generate_all_moves {

        }
        None
    }

    pub fn search_depth(
        &mut self,
        mut gs: GameState,
        depth_left: usize,
        beta: i32,
        mut alpha: i32,
    ) -> i32 {
        if depth_left == 0 || !self.search.more_time() {
            let moves = generate_all_moves(&mut gs);
            // int alphaBeta( int alpha, int beta, int depthleft ) {
            //     if( depthleft == 0 ) return quiesce( alpha, beta );
            //     for ( all moves)  {
            //        score = -alphaBeta( -beta, -alpha, depthleft - 1 );
            //        if( score >= beta )
            //           return beta;   //  fail hard beta-cutoff
            //        if( score > alpha )
            //           alpha = score; // alpha acts like max in MiniMax
            //     }
            //     return alpha;
            //  }
             
            for r#move in moves {
                match r#move {
                    Move::Normal(_) => todo!(),
                    Move::Promotion(_, _) => todo!(),
                }
                let score = -self.search_depth(gs.clone(), depth_left - 1, beta, alpha);
            }
        }
        todo!()
    }
}

fn generate_all_moves(gs: &mut GameState) -> impl Iterator<Item = Move> {
    let new_all_valid_moves = gs
        .new_all_valid_moves(gs.active_color);
    new_all_valid_moves.into_iter()
        .map(make_promotion).flatten()
}
fn make_promotion(r#move: MoveType<Pos, Colored<Piece>>) ->Vec<Move>{
    match r#move {
        MoveType::Capture { .. }
        | MoveType::Move { .. }
        | MoveType::EnPassant { .. }
        | MoveType::Castle { .. }
        | MoveType::Check => vec![Move::Normal(r#move)],
        MoveType::CapturePromotion { .. } | MoveType::MovePromotion { .. } => vec![
            Colored::new(r#move.color(), Piece::Queen),
            Colored::new(r#move.color(), Piece::Knight),
            Colored::new(r#move.color(), Piece::Rook),
            Colored::new(r#move.color(), Piece::Bishop),
        ]
        .iter()
        .map(move |p| Move::Promotion(r#move, *p)).collect(),
        
    }
}
#[derive(Clone, Copy)]
pub enum Move {
    Normal(MoveType<Pos, Colored<Piece>>),
    Promotion(MoveType<Pos, Colored<Piece>>, Colored<Piece>),
}

pub enum SearchType {
    Timed(Duration, Instant),
    Depth(usize),
}

impl SearchType {
    // return false if out of time
    pub fn more_time(&self) -> bool {
        if let Self::Timed(d, i) = self {
            return &i.elapsed() >= d;
        }
        true
    }
}

impl Default for SearchType {
    fn default() -> Self {
        Self::Depth(3)
    }
}
