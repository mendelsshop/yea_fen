// https://www.youtube.com/wa tch?v=DpXy041BIlA
// at around 18:33 / 42:35

use crate::{
    chess_engines::pick_random,
    moves::{GameResult, MoveType},
    Color, Colored, GameState, Piece, Pos,
};

pub fn minimax(
    game: &mut GameState,
    depth: usize,
) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let mut new_game = game.clone();
    let color = game.active_color;
    let moves = game.new_all_valid_moves(game.active_color);
    let ret = {
        let mut alpha = i32::MIN;
        let beta = i32::MAX;
        if depth == 0 {
            None
        } else {
            let prom = match game.active_color {
                Color::Black => Colored::Black(Piece::Queen),
                Color::White => Colored::White(Piece::Queen),
            };
            if moves.len() == 1 {
                let score = moves.iter().last().unwrap();
                return Some((*score, Some(prom)));
            }

            let mut pot_move = **pick_random(&moves.iter().collect())?;
            let mut moves = moves.iter();
            loop {
                let r#move = match moves.next() {
                    None => break Some((alpha, pot_move)),
                    Some(mv) => mv,
                };

                new_game.do_move(*r#move, Some(prom));
                let score = min(&mut new_game, depth - 1, alpha, beta, color);
                if score >= beta {
                    new_game.undo_move();
                    break Some((beta, *r#move));
                }
                if score > alpha {
                    alpha = score;
                    pot_move = *r#move;
                }
                new_game.undo_move();
            }
        }
    };

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

fn max(game: &mut GameState, depth: usize, mut alpha: i32, beta: i32, color: Color) -> i32 {
    if depth == 0 {
        return eval_board(game, color);
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = game.new_all_valid_moves(game.active_color);
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = min(game, depth - 1, alpha, beta, color);
            if score >= beta {
                game.undo_move();
                return beta;
            }
            if score > alpha {
                alpha = score;
            }
        }
        game.undo_move();
    }

    alpha
}

fn min(game: &mut GameState, depth: usize, alpha: i32, mut beta: i32, color: Color) -> i32 {
    if depth == 0 {
        let score = eval_board(game, color);
        return -score;
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = game.new_all_valid_moves(game.active_color);
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = max(game, depth - 1, alpha, beta, color);
            if score <= alpha {
                game.undo_move();
                return alpha;
            }
            if score < beta {
                beta = score;
            }
        }
        game.undo_move();
    }
    beta
}

const fn get_piece_value(piece: Piece) -> i32 {
    match piece {
        Piece::Pawn => 1,
        Piece::Knight | Piece::Bishop => 3,
        Piece::Rook => 5,
        Piece::Queen => 9,
        Piece::King => 100,
    }
}

fn eval_board(game: &GameState, color: Color) -> i32 {
    // evalutes the board based on how many pieces we have and their value
    let mut ret = 0;
    match game.result {
        GameResult::CheckMate(c_color) => ret = if c_color == color { i32::MIN } else { i32::MAX },
        GameResult::StaleMate | GameResult::Draw => ret = -50,
        GameResult::InProgress => {
            for row in game.board.board {
                for piece in row.into_iter().flatten() {
                    if Color::from(piece) == game.active_color {
                        // todo: use piece list and add point for being in certain positions
                        ret += get_piece_value(Piece::from(piece));
                    }
                }
            }
        }
    }

    ret
}

#[cfg(test)]
mod tests {
    // use std::time::Instant;

    use super::*;
    use crate::{chess_engines::random::do_random_move, GameState};

    #[test]
    fn min_max() {
        let mut game = GameState::new();
        let mut i = 0;
        while game.result == GameResult::InProgress {
            if i % 2 == 0 {
                // let now = Instant::now();
                if do_minimax(&mut game, 7) {
                    // println!("{}", game.board);
                    // println!(
                    //     "minimx took {}s color {:?}",
                    //     now.elapsed().as_secs(),
                    //     game.active_color
                    // );
                } else {
                    println!("no moves {}", i);
                    break;
                };
            } else {
                if do_random_move(&mut game) {
                    // println!("{}", game.board);
                    // println!("random color {:?}", game.active_color);
                } else {
                    // println!("no moves {}", i);
                    break;
                };
            }
            i += 1;
        }
        println!("game over");
        println!("{:?}", game.result);
    }
}
