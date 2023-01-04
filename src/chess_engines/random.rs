use crate::GameState;

use super::{pick_random, promotion};

pub fn do_random_move(game: &mut GameState) -> bool {
    let binding = game.get_all_valid_moves(game.active_color);
    if binding.is_empty() {
        return false;
    }
    let moves = binding.iter().collect::<Vec<_>>();
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
