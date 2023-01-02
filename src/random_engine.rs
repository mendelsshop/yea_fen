use std::time::{SystemTime, UNIX_EPOCH};

use crate::{Colored, GameState};

// define a random function that generates random number based on the time
// and returns the number
fn random() -> Option<u32> {
    let now = SystemTime::now();
    let time = now.duration_since(UNIX_EPOCH).ok()?;
    let time = time.as_secs() * 1000 + time.subsec_nanos() as u64 / 1_000_000;
    Some(time as u32)
}

pub fn do_random_move(game: &mut GameState) -> bool {
    let binding = game.get_all_valid_moves(game.active_color);

    let moves = Vec::from_iter(binding.iter());
    let mut rng = match random() {
        Some(x) => x,
        None => return false,
    };
    // generate a random number between 0 and the number of valid moves
    rng = rng % moves.len() as u32;
    // make the move
    let r#move = moves[rng as usize];
    // check if the move is a promotion
    let promotion = match *r#move {
        crate::moves::MoveType::Capture(_, _)
        | crate::moves::MoveType::Move(_, _)
        | crate::moves::MoveType::EnPassant(_, _, _)
        | crate::moves::MoveType::Castle(_, _)
        | crate::moves::MoveType::Check => None,
        crate::moves::MoveType::CapturePromotion(_, _)
        | crate::moves::MoveType::MovePromotion(_, _) => Some(match game.active_color {
            crate::Color::White => match rng % 4 {
                0 => Colored::White(crate::Piece::Queen),
                1 => Colored::White(crate::Piece::Rook),
                2 => Colored::White(crate::Piece::Bishop),
                3 => Colored::White(crate::Piece::Knight),
                _ => return false,
            },
            crate::Color::Black => match rng % 4 {
                0 => Colored::Black(crate::Piece::Queen),
                1 => Colored::Black(crate::Piece::Rook),
                2 => Colored::Black(crate::Piece::Bishop),
                3 => Colored::Black(crate::Piece::Knight),
                _ => return false,
            },
        }),
    };
    game.do_move(*r#move, promotion);
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::GameState;

    #[test]
    fn test_random() {
        let mut game = GameState::new();
        for _ in 0..50 {
            do_random_move(&mut game);
            println!("{}", game.board);
        }
    }
}
