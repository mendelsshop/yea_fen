use crate::{moves, moves::MoveType, Colored, GameState, Piece, Pos};

use super::{pick_random, promotion};

pub fn random_capture(
    game: &mut GameState,
) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let binding = game.new_all_valid_moves(game.active_color);
    if binding.is_empty() {
        return None;
    }
    let moves = binding
        .iter()
        .filter(|moe| {
            matches!(
                **moe,
                moves::MoveType::Capture { .. }
                    | moves::MoveType::CapturePromotion { .. }
                    | moves::MoveType::EnPassant { .. }
            )
        })
        .collect::<Vec<_>>();
    let moves = if moves.is_empty() {
        binding.iter().collect::<Vec<_>>()
    } else {
        moves
    };
    let item = pick_random(&moves)?;
    let promotion = promotion(item, game);
    Some((**item, promotion))
}
pub fn do_random_capture(game: &mut GameState) -> bool {
    random_capture(game).map_or(false, |r#move| game.do_move(r#move.0, r#move.1))
}
#[cfg(test)]
mod tests {
    use super::*;
    use crate::{chess_engines::random::do_random_move, GameState};

    #[test]
    fn test_random_capture() {
        let mut game = GameState::new();
        for _ in 0..200 {
            do_random_capture(&mut game);
            println!("{}", game.board);
            do_random_move(&mut game);
        }
        // print the result
        println!("{:?}", game.result);
    }
}
