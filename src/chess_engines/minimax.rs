// https://www.youtube.com/wa tch?v=DpXy041BIlA
// at around 18:33 / 42:35

use std::{collections::HashSet, cmp};

use crate::{
    chess_engines::pick_random,
    moves::{GameResult, MoveType},
    Color, Colored, GameState, Piece, Pos,
};

use super::{get_capture_piece_value, quiescence};

pub fn minimax(
    game: &mut GameState,
    depth: usize,
) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    
    let color = game.active_color;
    let moves = game.new_all_valid_moves(game.active_color);
    let mut new_game = game.clone();
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let ret = {
        let mut alpha = i32::MIN;
        let beta = i32::MAX;
        if depth == 0 {
            None
        } else {
            if moves.len() == 1 {
                let score = moves.iter().last().unwrap();
                return match score {
                    MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } => {
                        Some((*score, Some(prom)))
                    }
                    _ => Some((*score, None)),
                };
            }

            let mut pot_move = **pick_random(&moves.iter().collect())?;
            let mut moves = moves.iter();
            // negamax:
            // let mut maxscore = i32::MIN;
            loop {
                let r#move = match moves.next() {
                    None => break Some((alpha, pot_move)),
                    Some(mv) => mv,
                };

                new_game.do_move(*r#move, Some(prom));
                
                // minimax:
                let score = min(&mut new_game, depth - 1, alpha, beta, color, i32::MAX);
                new_game.undo_move();
                if score >= beta {
                    
                    break Some((beta, *r#move));
                }
                if score > alpha {
                    alpha = score;
                    pot_move = *r#move;
                }
                // negamax:
                // let score = negamax_alpha_beta(
                //     &mut new_game,
                //     depth - 1,
                //     alpha,
                //     beta,
                //     if color == Color::White { 1 } else { -1 },
                //     i32::MAX,
                // );
                // new_game.undo_move();
                // if score > maxscore {
                //     maxscore = score;
                // }
                // if maxscore > alpha {
                //     pot_move = *r#move;
                //     alpha = maxscore;
                // }
                // if alpha >= beta {
                //     break Some((beta, *r#move));
                // }


            }
        }
    };

    ret.map(|num| match num.1 {
        MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } => (num.1, Some(prom)),
        _ => (num.1, None),
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
    color: Color,
    mate: i32,
) -> i32 {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        return quiescence(
            game,
            alpha,
            beta,
            mate,
            GameState::tapered_eval_board,
        );
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = sort_moves( game.new_all_valid_moves(game.active_color));
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = min(game, depth - 1, alpha, beta, color, mate - 1);
            game.undo_move();
            if score >= beta {
                return beta;
            }
            if score > alpha {
                alpha = score;
            }
        }
    }

    alpha
}

fn min(
    game: &mut GameState,
    depth: usize,
    alpha: i32,
    mut beta: i32,
    color: Color,
    mate: i32,
) -> i32 {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        let score = quiescence(
            game,
            alpha,
            beta,
            mate,
            GameState::tapered_eval_board,
        );
        return -score;
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = sort_moves( game.new_all_valid_moves(game.active_color));
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = max(game, depth - 1, alpha, beta, color, mate - 1);
            game.undo_move();
            if score <= alpha {
                return alpha;
            }
            if score < beta {
                beta = score;
            }
        }
    }
    beta
}


fn negamax_alpha_beta(
    game: &mut GameState,
    moves: &[MoveType<Pos, Colored<Piece>>],
    depth: usize,
    mut alpha: i32,
    mut bm: Option<usize>,
    beta: i32,
    turn_multiplier: i32,
    mate: i32,
) -> (i32, Option<usize>) {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        // println!("quiescence at depth {}", depth);
        return (turn_multiplier
            * quiescence(
                game,
                alpha,
                beta,
                mate - 1,
                GameState::tapered_eval_board
            ), bm);
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let mut maxscore = (-mate, None);
    for (idx, r#move) in moves.iter().enumerate() {
        if game.do_move(*r#move, Some(prom)) {
            if let None = bm {
                bm = Some(idx);
            }
            let new_moves = sort_moves( game.new_all_valid_moves(game.active_color));
            let score =
                -negamax_alpha_beta(game, &new_moves, depth - 1, -beta, bm,-alpha, -turn_multiplier, mate - 1).0;
            if !game.undo_move() {
                println!("undo_move failed");
            }
            if score > maxscore.0 {
                maxscore = (score, Some(idx));
            }
            if maxscore.0 > alpha {
                alpha = maxscore.0;
            }
            if alpha >= beta {
                break;
            }

        }
    }
    if maxscore.1.is_none() {
        // println!("no best move at depth {} - 2", depth);
    }
    maxscore
}

pub fn negamax(    game: &mut GameState, depth: usize) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let moves = sort_moves( game.new_all_valid_moves(game.active_color));
    let mut game = game.clone();
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let mate = i32::MAX;
    let color = match game.active_color {
        Color::Black => -1,
        Color::White => 1,
    };
    let (_, bm) = negamax_alpha_beta(&mut game, &moves, depth, -mate, None, mate, color, mate);
    let r#move = moves[bm?];
    Some(match r#move {
        MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } => (r#move, Some(prom)),
        _ => (r#move, None),
    })

}


pub fn negamax1(    game: &mut GameState, depth: usize) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let mate = i32::MAX;
    let color = match game.active_color {
        Color::Black => -1,
        Color::White => 1,
    };
    let moves = sort_moves( game.new_all_valid_moves(game.active_color));
     moves.into_iter().filter_map(|r#move| {
        if game.do_move(r#move, Some(prom)) {
            let score = negamax2(game, depth - 1, -color,i32::MIN + 1, mate , mate - 1);
            game.undo_move();
            Some((score, r#move))
        } else {
            None
        }
    }).max_by_key(|(score, _)| *score).map(|(_, r#move)| match r#move {
        MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } => (r#move, Some(prom)),
        _ => (r#move, None),
    })
    



    // let (_, bm) = negamax_alpha_beta(game, &moves, depth, -mate, None, mate, if game.active_color == Color::White {1} else {-1}, mate);
    // let r#move = moves[bm?];
    // Some(match r#move {
    //     MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } => (r#move, Some(prom)),
    //     _ => (r#move, None),
    // })

}
fn negamax2(
    game: &mut GameState,
    depth: usize,
    color: i32,
    alpha: i32,

    beta: i32,
    mate: i32,
) -> i32 {
    if depth == 0 {
        return quiescence(game, alpha, beta, mate, GameState::tapered_eval_board);
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let mut a = alpha;
    let mut best = std::i32::MIN + 1;
    let moves = game.new_all_valid_moves(game.active_color);
    // let positions = position
    //     .get_moves(&self.move_generator)
    //     .into_iter()
    //     .map(|m| position.make_move(m));

    for p in moves{
        if game.do_move(p, Some(prom)) {
            let v = -negamax2(game, depth - 1, -color, -beta, -a, mate -1);
            game.undo_move();
            best = cmp::max(v, best);
            a = cmp::max(v, a);
            if a >= beta {
                break;
            }
        }
    }

    color * best
}
fn negamax_alpha_beta1(
    game: &mut GameState,
    depth: usize,
    mut alpha: i32,
    beta: i32,
    turn_multiplier: i32,
    mate: i32,
) -> i32 {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        return turn_multiplier
            * quiescence(
                game,
                alpha,
                beta,
                mate - 1,
                GameState::simple_eval,
            );
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = sort_moves( game.new_all_valid_moves(game.active_color));
    let mut maxscore = -mate;
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score =
                -negamax_alpha_beta1(game, depth - 1, -beta, -alpha, -turn_multiplier, mate - 1);
            game.undo_move();
            if score > maxscore {
                maxscore = score; 
            }
            if maxscore > alpha {
                alpha = maxscore;
            }
            if alpha >= beta {
                break;
            }

        }
    }
    maxscore
}





fn sort_moves(
    moves: HashSet<MoveType<Pos, Colored<Piece>>>) -> Vec<MoveType<Pos, Colored<Piece>>> {
    let mut moves = moves.into_iter().collect::<Vec<_>>();
    moves.sort_by(|a, b| {
        let a = match a {
            MoveType::Capture { captured_piece, .. } => get_capture_piece_value(Piece::from(*captured_piece)),
            MoveType::CapturePromotion { captured_piece, .. } => get_capture_piece_value(Piece::from(*captured_piece)) + 5,
            MoveType::EnPassant { .. } => 1,
            _ => 0,
        };
        let b = match b {
            MoveType::Capture { captured_piece,  .. } => get_capture_piece_value(Piece::from(*captured_piece)),
            MoveType::CapturePromotion { captured_piece, .. } => get_capture_piece_value(Piece::from(*captured_piece)) + 5,
            MoveType::EnPassant { .. } => 1,
            _ => 0,
        };
        b.cmp(&a)
    });
    moves

    }

#[cfg(test)]
mod tests {
    use std::{str::FromStr, time::Instant};

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

    #[test]
    fn move_ordering() {
        let mut gs = GameState::from_str("r4rk1/pp3ppp/2nb4/2p3P1/4p3/1PP5/PR1BPq2/2QK1b2 w - - 0 21").unwrap();
        // get all valid moves
        let moves = gs.new_all_valid_moves(Color::White);
        // do a random move
        let moves = moves.into_iter().collect::<Vec<_>>();
        let m = pick_random(&moves).unwrap();
        gs.do_move(*m, None);
        let moves = gs.new_all_valid_moves(Color::Black);
        let moves = sort_moves(moves);
        println!("{}", moves.iter().map(|m| format!("{}", m)).collect::<Vec<_>>().join(", "));
    }

    #[test]
    fn longest_minimax() {
        let mut gs = GameState::new();
        let now = Instant::now();
        let mn1 = minimax(&mut gs, 1);
        println!("minimax at depth of 1 took {}ms", now.elapsed().as_millis()); 
        println!("move: {:?}", mn1);
        let now = Instant::now();
        let ne1 = negamax(&mut gs, 1);
        println!("negamax at depth of 1 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", ne1);
        let now = Instant::now();
        let mn1 = minimax(&mut gs, 4);
        println!("minimax at depth of 4 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", mn1);
        let now = Instant::now();
        let ne1 = negamax(&mut gs, 4);
        println!("negamax at depth of 4 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", ne1);

        let now = Instant::now();
        let mn1 = minimax(&mut gs, 8);
        println!("minimax at depth of 8 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", mn1);
        let now = Instant::now();
        let ne1 = negamax(&mut gs, 8);
        println!("negamax at depth of 8 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", ne1);


        let now = Instant::now();
        let mn1 = minimax(&mut gs, 12);


        println!("minimax at depth of 12 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", mn1);
        let now = Instant::now();
        let ne1 = negamax(&mut gs, 12);
        println!("negamax at depth of 12 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", ne1);

        let now = Instant::now();
        let mn1 = minimax(&mut gs, 16);
        println!("minimax at depth of 16 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", mn1);
        let now = Instant::now();
        let ne1 = negamax(&mut gs, 16);
        println!("negamax at depth of 16 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", ne1);

        let now = Instant::now();
        let mn1 = minimax(&mut gs, 20);
        println!("minimax at depth of 20 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", mn1);
        let now = Instant::now();
        let ne1 = negamax(&mut gs, 20);
        println!("negamax at depth of 20 took {}ms", now.elapsed().as_millis());
        println!("move: {:?}", ne1);
        
    }
}
