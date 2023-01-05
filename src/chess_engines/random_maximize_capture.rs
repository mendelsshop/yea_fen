use std::cmp::Ordering;

use crate::{moves::MoveType, Colored, GameState, Piece, Pos};

use super::{get_capture_piece_value, pick_random, promotion};

pub fn random_maximize_capture(
    game: &GameState,
) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let mut game_clone = game.clone();
    let moves = game_clone.get_all_valid_moves(game.active_color);
    if moves.is_empty() {
        return None;
    }
    // king | move = 1
    // queen = 9
    // rook = 5
    // bishop | knight = 3
    // pawn = 1
    let moves = moves
        .iter()
        .map(|r#move| match *r#move {
            MoveType::Capture {
                piece,
                captured_piece,
                ..
            }
            | MoveType::EnPassant {
                piece,
                captured_piece,
                ..
            }
            | MoveType::CapturePromotion {
                piece,
                captured_piece,
                ..
            } => (
                (get_piece_value(Piece::from(piece)))
                    + get_capture_piece_value(Piece::from(captured_piece)),
                *r#move,
            ),
            MoveType::Move { .. } | MoveType::MovePromotion { .. } | MoveType::Castle { .. } => {
                (0, *r#move)
            }
            MoveType::Check => todo!(),
        })
        .collect::<Vec<_>>();
    let mut gt = moves
        .iter()
        .max_by(|move1, move2| move1.0.cmp(&move2.0))
        .expect("this cannot happen")
        .0;
    let moves = loop {
        let new_moves = moves
            .iter()
            .filter_map(|(rating, r#move)| if *rating == gt { Some(r#move) } else { None })
            .collect::<Vec<_>>();
        if !new_moves.is_empty() {
            break new_moves;
        }
        gt = moves
            .iter()
            .max_by(|move1, move2| {
                if move1.0 >= gt {
                    Ordering::Less
                } else {
                    move1.0.cmp(&move2.0)
                }
            })
            .expect("this cannot happen")
            .0;
    };
    let item = **pick_random(&moves)?;
    let promotion = promotion(&item, game);
    Some((item, promotion))
}

pub fn do_random_maximize_capture(game: &mut GameState) -> bool {
    random_maximize_capture(game).map_or(false, |r#move| game.do_move(r#move.0, r#move.1))
}

const fn get_piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::King => 1,
        Piece::Queen | Piece::Rook => 2,
        Piece::Bishop | Piece::Knight => 4,
        Piece::Pawn => 6,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::GameState;

    #[test]
    fn test_random_maximize_capture() {
        let mut game = GameState::new();
        for _ in 0..200 {
            do_random_maximize_capture(&mut game);
            println!("{}", game.board);
        }
    }
}
