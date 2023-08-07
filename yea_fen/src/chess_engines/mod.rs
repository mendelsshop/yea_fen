#![allow(clippy::type_complexity, clippy::module_name_repetitions)]

use std::time::{SystemTime, UNIX_EPOCH};

use crate::{
    moves::{self, MoveType},
    Castling, Color, Colored, GameState, Piece, Pos,
};

pub mod eval;
pub mod minimax;
pub mod random;
pub mod random_capture;
pub mod random_maximize_capture;
pub mod simple_ai;

fn random() -> Option<usize> {
    let now = SystemTime::now();
    let time = now.duration_since(UNIX_EPOCH).ok()?;
    let time = time.as_secs() * 1000 + u64::from(time.subsec_nanos()) / 1_000_000;
    Some(time as usize)
}

fn promotion(
    r#move: &moves::MoveType<Pos, Colored<Piece>>,
    game: &GameState,
) -> Option<Colored<Piece>> {
    let rng = random()?;
    match *r#move {
        moves::MoveType::Capture { .. }
        | moves::MoveType::Move { .. }
        | moves::MoveType::EnPassant { .. }
        | moves::MoveType::Castle { .. }
        | moves::MoveType::Check => None,
        moves::MoveType::CapturePromotion { .. } | moves::MoveType::MovePromotion { .. } => {
            Some(match game.active_color {
                Color::White => match rng % 4 {
                    0 => Colored::White(Piece::Queen),
                    1 => Colored::White(Piece::Rook),
                    2 => Colored::White(Piece::Bishop),
                    3 => Colored::White(Piece::Knight),
                    _ => return None,
                },
                Color::Black => match rng % 4 {
                    0 => Colored::Black(Piece::Queen),
                    1 => Colored::Black(Piece::Rook),
                    2 => Colored::Black(Piece::Bishop),
                    3 => Colored::Black(Piece::Knight),
                    _ => return None,
                },
            })
        }
    }
}

fn pick_random<T>(items: &Vec<T>) -> Option<&T> {
    let rng = random()?.checked_rem(items.len())?;
    items.get(rng)
}

use std::ops::{AddAssign, MulAssign};

// Linear congruential generator parameters
const A: u64 = 1103515245;
const C: u64 = 12345;
const M: u64 = 1 << 31;

struct LCGRng {
    seed: u64,
}

impl LCGRng {
    fn new(seed: u64) -> Self {
        LCGRng { seed }
    }

    fn next(&mut self) -> u64 {
        self.seed = (self.seed.wrapping_mul(A) + C) % M;
        self.seed
    }
}

impl Iterator for LCGRng {
    type Item = u64;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.next())
    }
}

const fn get_capture_piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::King => 0,
        Piece::Pawn => 1,
        Piece::Queen => 9,
        Piece::Rook => 5,
        Piece::Bishop | Piece::Knight => 3,
    }
}
pub struct HashEntry {
    pub depth: i32,
    pub eval: i32,
    // pub zobrist: u64,
    pub ancient: i32,
    pub flag: Flag,
    pub best_move: Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)>,
}
pub struct Zobrist {
    pieces: [[[u64; 128]; 2]; 6],
    black_castling: [u64; 4],
    white_castling: [u64; 4],
    en_passant: [u64; 128],
    active_color: u64,
}
impl Zobrist {
    pub fn new_zobrist() -> Self {
        // set  random values
        let mut rand = LCGRng::new(0);
        let mut pieces: [[[u64; 128]; 2]; 6] = [[[0; 128]; 2]; 6];
        let mut black_castling: [u64; 4] = [0; 4];
        let mut white_castling: [u64; 4] = [0; 4];
        let mut en_passant: [u64; 128] = [0; 128];
        let active_color: u64;
        for i in 0..4 {
            black_castling[i] = rand.next();
            white_castling[i] = rand.next();
        }
        for i in 0..128 {
            en_passant[i] = rand.next();
        }
        for i in 0..6 {
            for j in 0..2 {
                for k in 0..128 {
                    pieces[i][j][k] = rand.next();
                }
            }
        }
        active_color = rand.next();
        Zobrist {
            pieces,
            black_castling,
            white_castling,
            en_passant,
            active_color,
        }
    }

    pub fn get_zobrist(&self, game: &GameState) -> u64 {
        let mut zobrist = 0;
        for i in 0..120 {
            if let Some(Some(piece)) = game.board.get_cell(Pos::from_index(i)) {
                zobrist ^= self.pieces[match Piece::from(*piece) {
                    Piece::Pawn => 0,
                    Piece::Knight => 1,
                    Piece::Bishop => 2,
                    Piece::Rook => 3,
                    Piece::Queen => 4,
                    Piece::King => 5,
                }][match Color::from(*piece) {
                    Color::White => 0,
                    Color::Black => 1,
                }][i];
            }
        }
        if let Castling::Both = game.castling_moves.black {
            zobrist ^= self.black_castling[0];
            zobrist ^= self.black_castling[1];
        }
        if let Castling::Both = game.castling_moves.white {
            zobrist ^= self.white_castling[0];
            zobrist ^= self.white_castling[1];
        }
        if game.castling_moves.black == Castling::KingSide {
            zobrist ^= self.black_castling[0];
        }
        if game.castling_moves.black == Castling::QueenSide {
            zobrist ^= self.black_castling[1];
        }
        if game.castling_moves.white == Castling::KingSide {
            zobrist ^= self.white_castling[0];
        }
        if game.castling_moves.white == Castling::QueenSide {
            zobrist ^= self.white_castling[1];
        }
        if let Some(en_passant) = game.en_passant {
            zobrist ^= self.en_passant[en_passant.get_index()]
        }
        if game.active_color == Color::Black {
            zobrist ^= self.active_color;
        }
        zobrist
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;
    #[test]
    fn test_zobrist() {
        let zobrist = Zobrist::new_zobrist();
        let mut game = GameState::new();
        let mut game2 = GameState::new();
        println!("{}", game.board);
        // in game 1 move paawn at b2 to b4
        let moves = game.new_all_valid_moves(game.active_color);
        // get the move
        let move1 = moves
            .iter()
            .find(|x| {
                x.from().0 == Pos::from_str("b2").unwrap() && x.to() == Pos::from_str("b4").unwrap()
            })
            .unwrap();
        // make the move
        game.do_move(*move1, None);
        // in game 1 move b7 to b5
        let moves = game.new_all_valid_moves(game.active_color);
        // get the move
        let move1 = moves
            .iter()
            .find(|x| {
                x.from().0 == Pos::from_str("b7").unwrap() && x.to() == Pos::from_str("b5").unwrap()
            })
            .unwrap();
        // make the move
        game.do_move(*move1, None);
        // in game 1 move paawn at c2 to c4
        let moves = game.new_all_valid_moves(game.active_color);
        // get the move
        let move1 = moves
            .iter()
            .find(|x| {
                x.from().0 == Pos::from_str("c2").unwrap() && x.to() == Pos::from_str("c4").unwrap()
            })
            .unwrap();
        // make the move
        game.do_move(*move1, None);

        let zobrist1 = zobrist.get_zobrist(&game);

        // in game 2 move paawn at c2 to c4
        let moves = game2.new_all_valid_moves(game2.active_color);
        // get the move
        let move1 = moves
            .iter()
            .find(|x| {
                x.from().0 == Pos::from_str("c2").unwrap() && x.to() == Pos::from_str("c4").unwrap()
            })
            .unwrap();
        // make the move
        game2.do_move(*move1, None);
        // in game 2 move b7 to b5
        let moves = game2.new_all_valid_moves(game2.active_color);
        // get the move
        let move1 = moves
            .iter()
            .find(|x| {
                x.from().0 == Pos::from_str("b7").unwrap() && x.to() == Pos::from_str("b5").unwrap()
            })
            .unwrap();
        // make the move
        game2.do_move(*move1, None);
        // in game 2 move paawn at b2 to b4
        let moves = game2.new_all_valid_moves(game2.active_color);
        // get the move
        let move1 = moves
            .iter()
            .find(|x| {
                x.from().0 == Pos::from_str("b2").unwrap() && x.to() == Pos::from_str("b4").unwrap()
            })
            .unwrap();
        // make the move
        game2.do_move(*move1, None);

        let zobrist2 = zobrist.get_zobrist(&game);
        println!("zobrist1: {}", zobrist1);
        assert_eq!(zobrist1, zobrist2);
    }
}

pub enum Flag {
    Exact,
    Alpha,
    Beta,
}
#[allow(dead_code)]
fn quiescence(
    game: &mut GameState,
    mut alpha: i32,
    beta: i32,
    mate: i32,
    turn_multiplier: i32,
    eval: fn(&GameState, i32) -> i32,
) -> i32 {
    let stand_pat = turn_multiplier * eval(game, mate);

    if stand_pat >= beta {
        return beta;
    }
    if alpha < stand_pat {
        alpha = stand_pat;
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let binding = game.new_all_valid_moves(game.active_color);
    let moves = binding
        .iter()
        .filter(|mv| {
            matches!(
                mv,
                MoveType::CapturePromotion { .. }
                    | MoveType::Capture { .. }
                    | MoveType::EnPassant { .. }
            )
        })
        .collect::<Vec<_>>();
    for r#move in &moves {
        if game.do_move(**r#move, Some(prom)) {
            let score = -quiescence(game, -beta, -alpha, mate - 1, -turn_multiplier, eval);
            game.undo_move();
            if score >= beta {
                return score;
            }
            if score > alpha {
                alpha = score;
            }
        }
    }

    alpha
}
