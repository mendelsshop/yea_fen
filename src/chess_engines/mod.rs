use std::time::{SystemTime, UNIX_EPOCH};

use crate::{moves, Color, Colored, GameState, Piece, Pos};

pub mod random;
pub mod random_capture;

fn random() -> Option<usize> {
    let now = SystemTime::now();
    let time = now.duration_since(UNIX_EPOCH).ok()?;
    let time = time.as_secs() * 1000 + u64::from(time.subsec_nanos()) / 1_000_000;
    Some(time as usize)
}

fn promotion(
    r#move: &moves::MoveType<Pos, Colored<Piece>>,
    game: &mut GameState,
    rng: usize,
) -> Option<Colored<Piece>> {
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
