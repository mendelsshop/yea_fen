use crate::GameState;

use super::{promotion, random};

pub fn do_random_move(game: &mut GameState) -> bool {
    let binding = game.get_all_valid_moves(game.active_color);
    if binding.is_empty() {
        return false;
    }

    let moves = binding.iter().collect::<Vec<_>>();
    let mut rng = match random() {
        Some(x) => x,
        None => return false,
    };
    // generate a random number between 0 and the number of valid moves
    rng %= moves.len() as u32;
    // make the move
    let r#move = moves[rng as usize];
    // check if the move is a promotion
    let promotion = promotion(r#move, game, rng);
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
        for _ in 0..200 {
            do_random_move(&mut game);
            println!("{}", game.board);
        }
    }
}
