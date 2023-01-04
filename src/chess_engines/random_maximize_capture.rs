use std::cmp::Ordering;

use crate::{moves::MoveType, GameState, Piece};

use super::{pick_random, promotion};

pub fn do_random_maximize_capture(game: &mut GameState) -> bool {
    let moves = game.get_all_valid_moves(game.active_color);
    if moves.is_empty() {
        return false;
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
    let item = match pick_random(&moves) {
        Some(x) => **x,
        None => return false,
    };
    let promotion = promotion(&item, game);
    if !game.do_move(item, promotion) {
        return false;
    }

    true
}

const fn get_piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::King => 1,
        Piece::Queen | Piece::Rook => 2,
        Piece::Bishop | Piece::Knight => 4,
        Piece::Pawn => 6,
    }
}

const fn get_capture_piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::King | Piece::Pawn => 1,
        Piece::Queen => 9,
        Piece::Rook => 5,
        Piece::Bishop | Piece::Knight => 3,
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
