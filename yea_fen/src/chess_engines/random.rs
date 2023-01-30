use crate::{moves::MoveType, Colored, GameState, Piece, Pos};

use super::{pick_random, promotion};

pub fn random_move(
    game: &mut GameState,
) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let binding = game.new_all_valid_moves(game.active_color);
    if binding.is_empty() {
        return None;
    }
    let moves = binding.iter().collect::<Vec<_>>();
    let item = pick_random(&moves)?;
    let promotion = promotion(item, game);
    Some((**item, promotion))
}

pub fn do_random_move(game: &mut GameState) -> bool {
    random_move(game).map_or(false, |r#move| game.do_move(r#move.0, r#move.1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::GameState;

    #[test]
    fn test_random() {
        let mut game = GameState::new();
        for _ in 0..200 {
            do_random_move(&mut game);
            println!("{}", game.board);
        }
    }
}
