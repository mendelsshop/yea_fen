use std::{
    iter,
    ops::Deref,
    time::{Duration, Instant},
};

use crate::{moves::MoveType, Color, Colored, GameState, Piece, Pos};

use super::get_capture_piece_value;

#[derive(Default)]
pub struct Searcher {
    search: SearchType,
    nodes_seached: usize,
    done: bool,
}

impl Searcher {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn start_search_time(
        &mut self,
        gs: impl Deref<Target = GameState>,
        time: u64,
    ) -> Option<Move> {
        self.search = SearchType::Timed(Duration::from_micros(time), Instant::now());
        self.start_search(gs.deref().clone())
    }

    pub fn start_search_depth(
        &mut self,
        gs: impl Deref<Target = GameState>,
        depth: usize,
    ) -> Option<Move> {
        self.search = SearchType::Depth(depth);
        self.start_search(gs.deref().clone())
    }

    fn start_search(&mut self, mut gs: GameState) -> Option<Move> {
        self.done = false;
        match self.search {
            SearchType::Timed(_, _) => {
                let mut best_move = None;
                for depth in 1..usize::MAX {
                    if !self.search.more_time() {
                        println!("out of time searched depth {}", depth - 1);
                        return best_move;
                    }
                    if let Some(new_best_move) = self.search_root_depth(&mut gs, depth) {
                        best_move = Some(new_best_move);
                    }
                }
                best_move
            }
            SearchType::Depth(d) => self.search_root_depth(&mut gs, d),
        }
    }

    pub fn search_root_depth(&mut self, gs: &mut GameState, depth: usize) -> Option<Move> {
        let generate_all_moves: Vec<_> = generate_all_moves(gs);
        if generate_all_moves.len() == 1 {
            return Some(generate_all_moves[0]);
        }
        let mut best_move = None;
        let mut alpha = -100000;
        let mut best_score = -99999;
        let beta = 100000;
        for r#move in generate_all_moves {
            if !self.search.more_time() {
                self.done = true;
                return None;
            }
            if match r#move {
                Move::Normal(r#move) => gs.do_move(r#move, None),
                Move::Promotion(r#move, promotion) => gs.do_move(r#move, Some(promotion)),
            } {
                let board_value = -self.search_depth(gs, depth - 1, -beta, -alpha);
                if self.done == true {
                    return None;
                }
                gs.undo_move();
                if board_value > best_score {
                    best_score = board_value;
                    best_move = Some(r#move)
                }
                alpha = alpha.max(board_value)
            }
        }
        best_move
    }

    pub fn search_depth(
        &mut self,
        gs: &mut GameState,
        depth_left: usize,
        mut alpha: i32,
        beta: i32,
    ) -> i32 {
        if depth_left == 0 {
            return quiesce(gs, alpha, beta);
        } else if !self.search.more_time() {
            self.done = true;
            return 0;
        } else {
            let moves = generate_all_moves(gs);
            let mut best_score = -9999;
            for r#move in moves {
                if match r#move {
                    Move::Normal(r#move) => gs.do_move(r#move, None),
                    Move::Promotion(r#move, promotion) => gs.do_move(r#move, Some(promotion)),
                } {
                    if self.done == true {
                        return 0;
                    }
                    let score = -self.search_depth(gs, depth_left - 1, -beta, -alpha);
                    gs.undo_move();
                    if score >= beta {
                        return score;
                    }
                    best_score = best_score.max(score);
                    alpha = alpha.max(score);
                }
            }
            best_score
        }
    }
}

fn quiesce(gs: &mut GameState, mut alpha: i32, beta: i32) -> i32 {
    let stand_pat = eval_board(&gs);
    if stand_pat >= beta {
        return beta;
    }
    if alpha < stand_pat {
        alpha = stand_pat
    }

    for r#move in generate_all_moves(gs) {
        if r#move.inner_move().is_capture() {
            if match r#move {
                Move::Normal(r#move) => gs.do_move(r#move, None),
                Move::Promotion(r#move, promotion) => gs.do_move(r#move, Some(promotion)),
            } {
                let score = -quiesce(gs, -beta, -alpha);
                gs.undo_move();
                if score >= beta {
                    return beta;
                }
                alpha = alpha.max(score)
            }
        }
    }
    return alpha;
}

// 9999 is black losose
// -9999 if white loses
// otherwise +score =
fn eval_board(gs: &GameState) -> i32 {
    match gs.get_gameresult() {
        crate::moves::GameResult::CheckMate(c) => {
            if c == Color::White {
                -9999
            } else {
                9999
            }
        }
        crate::moves::GameResult::StaleMate => 0,
        crate::moves::GameResult::Draw => 0,
        crate::moves::GameResult::InProgress => {
            let mut knights = 0;
            let mut pawns = 0;
            let mut queens = 0;
            let mut rooks = 0;
            let mut bishops = 0;
            for row in gs.get_board().board {
                for piece in row {
                    if let Some(piece) = piece {
                        match piece {
                            Colored::Black(piece) => match piece {
                                Piece::King => {}
                                Piece::Queen => queens -= 1,
                                Piece::Rook => rooks -= 1,
                                Piece::Bishop => bishops -= 1,
                                Piece::Knight => knights -= 1,
                                Piece::Pawn => pawns -= 1,
                            },
                            Colored::White(piece) => match piece {
                                Piece::King => {}
                                Piece::Queen => queens += 1,
                                Piece::Rook => rooks += 1,
                                Piece::Bishop => bishops += 1,
                                Piece::Knight => knights += 1,
                                Piece::Pawn => pawns += 1,
                            },
                        }
                    }
                }
            }
            let e =
                100 * (pawns) + 320 * (knights) + 330 * (bishops) + 500 * (rooks) + 900 * (queens);
            if gs.active_color == Color::White {
                e
            } else {
                -e
            }
        }
        crate::moves::GameResult::ThreeFoldRepetition => 0,
    }
}

pub fn generate_all_moves(gs: &mut GameState) -> Vec<Move> {
    let new_all_valid_moves = gs.new_all_valid_moves(gs.active_color);
    let mut ret = new_all_valid_moves
        .into_iter()
        .map(make_promotion)
        .flatten()
        .collect::<Vec<Move>>();
    ret.sort_by(|m1, m2| {
        let a = match m1.inner_move() {
            MoveType::Capture { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(captured_piece))
            }
            MoveType::CapturePromotion { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(captured_piece)) + 5
            }
            MoveType::EnPassant { .. } => 1,
            _ => 0,
        };
        let b = match m2.inner_move() {
            MoveType::Capture { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(captured_piece))
            }
            MoveType::CapturePromotion { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(captured_piece)) + 5
            }
            MoveType::EnPassant { .. } => 1,
            _ => 0,
        };
        b.cmp(&a)
    });
    ret
}

pub fn do_move(gs: &mut GameState, r#move: Move) -> bool {
    match r#move {
        Move::Normal(r#move) => gs.do_move(r#move, None),
        Move::Promotion(r#move, promotion) => gs.do_move(r#move, Some(promotion)),
    }
}

fn make_promotion(r#move: MoveType<Pos, Colored<Piece>>) -> Vec<Move> {
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
        .map(move |p| Move::Promotion(r#move, *p))
        .collect(),
    }
}
#[derive(Clone, Copy)]
pub enum Move {
    Normal(MoveType<Pos, Colored<Piece>>),
    Promotion(MoveType<Pos, Colored<Piece>>, Colored<Piece>),
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Normal(arg0) => write!(f, "{arg0}"),
            Self::Promotion(arg0, arg1) => write!(f, "{arg0} prom {arg1}"),
        }
    }
}

impl Move {
    pub fn inner_move(&self) -> MoveType<Pos, Colored<Piece>> {
        match self {
            Move::Normal(m) => *m,
            Move::Promotion(m, _) => *m,
        }
    }
}

pub enum SearchType {
    Timed(Duration, Instant),
    Depth(usize),
}

impl SearchType {
    // return false if out of time
    pub fn more_time(&self) -> bool {
        if let Self::Timed(d, i) = self {
            return &i.elapsed() <= d;
        }
        true
    }
}

impl Default for SearchType {
    fn default() -> Self {
        Self::Depth(3)
    }
}

