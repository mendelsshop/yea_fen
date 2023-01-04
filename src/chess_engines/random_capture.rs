use crate::{moves, GameState};

use super::{promotion, random};

pub fn do_random_capture(game: &mut GameState) -> bool {
    let binding = game.get_all_valid_moves(game.active_color);
    if binding.is_empty() {
        return false;
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

    let mut rng = match random() {
        Some(x) => x,
        None => return false,
    };
    // generate a random number between 0 and the number of valid moves
    rng %= moves.len();
    // make the move
    let r#move = moves[rng];
    // check if the move is a promotion
    let promotion = promotion(r#move, game, rng);
    if !game.do_move(*r#move, promotion) {
        return false;
    }
    true
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
