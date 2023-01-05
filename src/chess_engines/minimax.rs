// https://www.youtube.com/wa tch?v=DpXy041BIlA
// at around 18:33 / 42:35

use crate::{chess_engines::pick_random, moves::{MoveType, GameResult}, Color, Colored, GameState, Piece, Pos};

use super::get_capture_piece_value;

pub fn minimax(
    game: &GameState,
    depth: usize,
) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let mut new_game = game.clone();
    println!("{:?}", game.active_color);
    let ret = max(&mut new_game, depth, i32::MIN, i32::MAX);
    ret.map(|num| {
        let promotion = match game.active_color {
            Color::Black => Colored::Black(Piece::Queen),
            Color::White => Colored::White(Piece::Queen),
        };
        (num.1, Some(promotion))
    })
}

pub fn do_minimax(game: &mut GameState, depth: usize) -> bool {

    minimax(game, depth).map_or(false, |r#move| game.do_move(r#move.0, r#move.1))
}

fn max(
    game: &mut GameState,
    depth: usize,
    mut alpha: i32,
    beta: i32,
) -> Option<(i32, MoveType<Pos, Colored<Piece>>)> {
    // println!("max alpha {} beta {}", alpha, beta);
    if depth == 0 {
        return Some(eval_board(game));
    }
    // println!("-");
    let moves = game.get_all_valid_moves(game.active_color);
    if game.result == GameResult::CheckMate(game.active_color) {
        println!("check mate found on {:?}", game.active_color);

        
    }
    // for r#move in moves {
    //     max(game, depth-1, color, alpha, beta );
    // }
    let mut pot_move = **pick_random(&moves.iter().collect())?;

    for r#move in &moves {
        let prom = match game.active_color {
            Color::Black => Colored::Black(Piece::Queen),
            Color::White => Colored::White(Piece::Queen),
        };
        game.do_move(*r#move, Some(prom));
        if let Some(score) = min(game, depth - 1, alpha, beta) {
            
            if score.0 >= beta {
                game.undo_move();
                return Some((beta, *r#move));
            }
            if score.0 > alpha {
                alpha = score.0;
                pot_move = *r#move;
            }
        }
        game.undo_move();
        // Some(())
    }

    Some((alpha, pot_move))
}

fn min(
    game: &mut GameState,
    depth: usize,
    alpha: i32,
    mut beta: i32,
) -> Option<(i32, MoveType<Pos, Colored<Piece>>)> {
    if depth == 0 {
        let score = eval_board(game);
        return Some((-score.0, score.1));
    }
    let moves = game.get_all_valid_moves(game.active_color);
    let mut pot_move = **pick_random(&moves.iter().collect())?;
    for r#move in &moves {
        let prom = match game.active_color {
            Color::Black => Colored::Black(Piece::Queen),
            Color::White => Colored::White(Piece::Queen),
        };
        game.do_move(*r#move, Some(prom));
        if let Some(score) = max(game, depth - 1, alpha, beta) {
            if score.0 <= alpha {
                game.undo_move();
                return Some((alpha, *r#move));
            }
            if score.0 < beta {
                beta = score.0;
                pot_move = *r#move;
            }
        }
        game.undo_move();
    }
    Some((beta, pot_move))
}


fn get_piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::Pawn => 1,
        Piece::Knight => 3,
        Piece::Bishop => 3,
        Piece::Rook => 5,
        Piece::Queen => 9,
        Piece::King => 100,
    }
}

fn eval_board(game: &GameState) -> (i32, MoveType<Pos, Colored<Piece>>) {
    // evalutes the board based on how many pieces we have and their value
    let mut ret = 0;
    for row in game.board.board {
        for piece in row.into_iter().flatten() {
            if Color::from(piece) == game.active_color {
                ret += get_piece_value(Piece::from(piece));
            }
        }
    }
    (ret, game.moves.last().unwrap().move_type)
}

#[cfg(test)]
mod tests {
    use std::time::Instant;

    use super::*;
    use crate::{chess_engines::random::do_random_move, GameState};

    #[test]
    fn min_max() {
        let mut game = GameState::new();
        for i in 0..4i32 {
            if i.is_positive() {
                let now = Instant::now();
                minimax(&mut game, 6);
                println!("{}", game.board);
                println!(
                    "took {}s color {:?}",
                    now.elapsed().as_secs(),
                    game.active_color
                );
            } else {
                do_random_move(&mut game);
                println!("{}", game.board);
                println!("random");
            }
        }
    }
}
