// https://www.youtube.com/wa tch?v=DpXy041BIlA
// at around 18:33 / 42:35

use std::{collections::{HashSet, HashMap}, time::{Instant, Duration}};

use crate::{
    chess_engines::pick_random,
    moves::{GameResult, MoveType},
    Color, Colored, GameState, Piece, Pos, Board,
};

use super::{get_capture_piece_value, quiescence, HashEntry, Flag, Zobrist};

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
                let turn_multiplier = if color == Color::White { 1 } else { -1 };
                let score = min(&mut new_game, depth - 1, alpha, beta, color, i32::MAX, turn_multiplier);
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
    turn_multiplier: i32
) -> i32 {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        return turn_multiplier * quiescence(game, alpha, beta, mate, turn_multiplier,GameState::tapered_eval_board, );
        // return turn_multiplier * GameState::tapered_eval_board(game, mate);
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = sort_moves(game.new_all_valid_moves(game.active_color));
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = min(game, depth - 1, alpha, beta, color, mate - 1, -turn_multiplier);
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
    turn_multiplier: i32
) -> i32 {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        // let score = turn_multiplier * GameState::tapered_eval_board(game, mate);
        let score = turn_multiplier * quiescence(game, alpha, beta, mate, turn_multiplier,GameState::tapered_eval_board, );
        return -score;
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = sort_moves(game.new_all_valid_moves(game.active_color));
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = max(game, depth - 1, alpha, beta, color, mate - 1, -turn_multiplier);
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
#[derive(Debug)]
#[allow(dead_code)]
struct Node {
    score: i32,
    move_: String,
    children: Vec<Node>,
}

pub struct Search {
    hash: HashMap<u64, HashEntry>,
    depth: usize,
    // alpha: i32,
    // beta: i32,
    node_count: usize,
    time: Duration,
    zobrist: Zobrist,
    time_or_depth: bool,
    now: Instant,
    break_search: bool,
}

impl Search {
    pub fn new_time(time: u32, nanos: u32) -> Self {
        Self {
            hash: HashMap::new(),
            depth: 1,
            // alpha: i32::MIN,
            // beta: i32::MAX,
            node_count: 0,
            // time is in milliseconds
            // duration is in seconds and nanoseconds
            // covert time to nanoseconds
        
            time: Duration::new(0, time * 1_000_000 + nanos),
            zobrist: Zobrist::new_zobrist(),
            time_or_depth: false,
            now: Instant::now(),
            break_search: false,
            
        }
    }

    pub fn new_depth(depth: usize) -> Self {
        Self {
            hash: HashMap::new(),
            depth,
            // alpha: i32::MIN,
            // beta: i32::MAX,
            time_or_depth: true,
            node_count: 0,
            time: Duration::new(0, 0),
            zobrist: Zobrist::new_zobrist(),
            now: Instant::now(),
            break_search: false,
            
        }
    }

    pub fn search(&mut self, gs: &GameState) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)>  {
        let mut best_move = self.root_search(gs);
        if self.time_or_depth {
            return best_move;
        }
        // let start = Instant::now();
        self.now = Instant::now();
        for i in 1..1000 {
            if self.now.elapsed() > self.time {
                break;
            }
            self.depth = i;
            let r#move = self.root_search(gs);
            if r#move.is_some() {
                best_move = r#move;
            }
        }
        println!("{} nodes searched", self.node_count);
        println!("{} depth searched", self.depth);
        best_move

        
    }

    fn root_search(&mut self, gs: &GameState) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
        let mut game = gs.clone();
        let moves = sort_moves(game.new_all_valid_moves(game.active_color));
        let mut best_move = None;
        let mut best_score = i32::MIN;
        let mut alpha = i32::MIN;
        let turn_multiplier = if game.active_color == Color::White { 1 } else { -1 };
        for r#move in &moves {
            if game.do_move(*r#move, None) {
                self.node_count += 1;
                let mut score = -self.negamax(&mut game, (self.depth - 1) as i32, -i32::MAX, -alpha, turn_multiplier);
                if self.break_search {
                    self.break_search = false;
                    self.depth -= 1;
                    return None;
                }
                game.undo_move();
                if game.check_repition().is_some() {
                    score = 0;
                }
                if score > best_score {
                    best_score = score;
                    best_move = Some(*r#move);
                }
                alpha = alpha.max(score);
                if score >= i32::MAX {
                    break;
                }
            }
        }
        best_move.map(|r#move| (r#move, None))
    }

    fn negamax(&mut self, game: &mut GameState, depth: i32, mut alpha: i32, beta: i32, turn_multiplier: i32) -> i32 {
        if self.now.elapsed() > self.time && !self.time_or_depth {
            self.break_search = true;
            return i32::MIN;
        }
        if let Some(score) = self.hash.get(&self.zobrist.get_zobrist(game)) {
match score.flag {
                Flag::Exact => return score.eval,
                Flag::LowerBound => alpha = alpha.max(score.eval),
                Flag::UpperBound => if score.eval <= alpha {
                    return alpha;
                }
            }
        }
        if depth == 0 || game.get_gameresult() != GameResult::InProgress {
            return turn_multiplier * GameState::tapered_eval_board(game, i32::MAX);
            // return turn_multiplier * quiescence(game, alpha, beta, i32::MAX, turn_multiplier,GameState::tapered_eval_board, );
            // return turn_multiplier * self.quiescence(depth, game, alpha, beta, turn_multiplier);
        }

        let mut best_score = i32::MIN;
        let moves = sort_moves(game.new_all_valid_moves(game.active_color));
        for r#move in &moves {
            if game.do_move(*r#move, None) {
                self.node_count += 1;
                let mut score = -self.negamax(game, depth - 1, -beta, -alpha, -turn_multiplier);
                // self.hash.insert((game.board, depth), score);
                game.undo_move();
                if game.check_repition().is_some() {
                    score = 0;
                }
                
                best_score = best_score.max(score);
                alpha = alpha.max(score);
                if alpha >= beta {
                    break;
                }
            }
        }
       self.hash.insert(self.zobrist.get_zobrist(game), HashEntry {
            depth,
            best_move: None,
            eval: best_score,
            flag: if best_score <= alpha {
                Flag::UpperBound
            } else if best_score >= beta {
                Flag::LowerBound
            } else {
                Flag::Exact
            },
            ancient: 0,
        });
        best_score
    }

    fn quiescence(&mut self, depth: i32,game: &mut GameState, mut alpha: i32, beta: i32, turn_multiplier: i32) -> i32 {
        let stand_pat = turn_multiplier * GameState::tapered_eval_board(game, i32::MAX);
        if stand_pat >= beta {
            return beta;
        }
        if stand_pat > alpha {
            alpha = stand_pat;
        }
        let moves = sort_moves(game.new_all_valid_moves(game.active_color));
        for r#move in &moves {
            if game.do_move(*r#move, None) {
                let mut score = -(self.quiescence(depth + 1, game, -beta, -alpha, -turn_multiplier));
                // self.hash.insert((game.board, depth), score);
                game.undo_move();

                if game.check_repition().is_some() {
                    score = 0;
                }
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

}

fn negamax_alpha_beta(
    game: &mut GameState,
    depth: usize,
    mut alpha: i32,
    beta: i32,
    turn_multiplier: i32,
    mate: i32,
) -> 
    i32
     {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        // println!("quiescence at depth {}", depth);
        // return 
        //     turn_multiplier * quiescence(
        //         game,
        //         alpha,
        //         beta,
        //         mate - 1,
        //         turn_multiplier,
        //         GameState::tapered_eval_board,
        //     )
        //     // bm,
        //     // Vec::new(),
        // ;

        return turn_multiplier * GameState::tapered_eval_board(game, mate - 1);
    }

    let mut value = -mate;
    let moves = sort_moves(game.new_all_valid_moves(game.active_color));
    for (_idx, r#move) in moves.iter().enumerate() {
        let mut promotions = vec![];
        promotions.push(Colored::new(game.active_color, Piece::Queen));
        if let MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } = r#move {
            promotions.push(Colored::new(game.active_color, Piece::Bishop));
            promotions.push(Colored::new(game.active_color, Piece::Knight));
            promotions.push(Colored::new(game.active_color, Piece::Rook));
        }
        let mut should_break = false;
        for prom in promotions {
            if game.do_move(*r#move, Some(prom)) {
                let mut score = negamax_alpha_beta(
                    game,
                    depth - 1,
                    -beta,
                    -alpha,
                    -turn_multiplier,
                    mate - 1,
                );
                score = if game.check_repition() == Some(()) {
                    0
                } else {
                    -1 * score
                };
                if !game.undo_move() {
                    println!("undo_move failed");
                }
                value = value.max(score);
                alpha = alpha.max(value);
                if alpha >= beta {
                    should_break = true;
                    break;
                }
            }
        }
        if should_break {
            break;
        }
    }
    alpha
}

pub fn negamax(
    game: &mut GameState,
    depth: usize,
    mut alpha: i32,
    beta: i32,
    // mut bm: Option<(MoveType<Pos, Colored<Piece>>, Colored<Piece>)>,
    turn_multiplier: i32,
    mate: i32,
) -> (Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)>, i32) {
    if depth == 0 || game.get_gameresult() != GameResult::InProgress {
        // println!("quiescence at depth {}", depth);
        // return (
        //     None,
        //     turn_multiplier * quiescence(
        //         game,
        //         alpha,
        //         beta,
        //         mate - 1,
        //         turn_multiplier,
        //         GameState::tapered_eval_board,
        //     ),
        //     // bm,
        //     // Vec::new(),
        // );

        return (
            None,
            turn_multiplier * GameState::tapered_eval_board(game, mate - 1),
        //     bm,
        //     Vec::new(),
        );
    }

    let moves = sort_moves(game.new_all_valid_moves(game.active_color));
    let mut value = i32::MIN; 
    let mut bm = match  moves.iter().next().map(|bm| (*bm, None, 0)) {
        Some(bm) => bm,
        None => return (None, value),
    };
    for r#move in moves {
        let mut should_break = false;
        let mut promotions = vec![];
        promotions.push(Colored::new(game.active_color, Piece::Queen));
        if let MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } = r#move {
            promotions.push(Colored::new(game.active_color, Piece::Bishop));
            promotions.push(Colored::new(game.active_color, Piece::Knight));
            promotions.push(Colored::new(game.active_color, Piece::Rook));
        }
        for prom in promotions {
            if game.do_move(r#move, Some(prom)) {
                let v = negamax(
                    game,
                    depth - 1,
                    -mate,
                    mate,
                    -turn_multiplier,
                    mate - 1,
                );
                    let scored = if game.check_repition() == Some(()) {
                        0
                    } else {
                        -v.1
                    };
                    value = value.max(scored);
                    alpha = alpha.max(value);
                    if alpha >= beta {
                        should_break = true;
                        bm = (r#move, Some(prom), value);
                        break;
                    }
                if !game.undo_move() {
                    println!("undo_move failed");
                }
            }
        }
        if should_break {
            break;
        }
    }

    let r#move = bm;
    match r#move.0 {
        MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } => (Some((r#move.0, r#move.1)), r#move.2),
        _ => (Some((r#move.0, r#move.1)), r#move.2),
    }
}

pub fn negamax_root(game: &GameState, depth: usize) -> Option<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)> {
    let mate = i32::MAX;
    let color = match game.active_color {
        Color::Black => -1,
        Color::White => 1,
    };
    let mut game = game.clone();
    let moves = sort_moves(game.new_all_valid_moves(game.active_color));
    let mut value = i32::MIN;
    let mut bm =moves.iter().next().map(|bm| (*bm, None))?;
    let mut alpha = i32::MIN;
    let beta = i32::MAX;

    for r#move in moves {
        let mut should_break = false;
        let mut promotions = vec![];
        promotions.push(Colored::new(game.active_color, Piece::Queen));
        if let MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } = r#move {
            promotions.push(Colored::new(game.active_color, Piece::Bishop));
            promotions.push(Colored::new(game.active_color, Piece::Knight));
            promotions.push(Colored::new(game.active_color, Piece::Rook));
        }
        for prom in promotions {
            if game.do_move(r#move, Some(prom)) {
                let mut score = -1 *negamax_alpha_beta(&mut game, depth, -beta, -alpha, -color, mate - 1);
                if game.check_repition() == Some(()) {
                    score = 0;
                }
                if !game.undo_move() {
                    println!("undo_move failed");
                }
                if score > value {
                    value = score;
                    bm = (r#move, Some(prom));
                }
                alpha = alpha.max(value);
                if alpha >= beta {
                    should_break = true;
                    break;
                }


            }
        }
        if should_break {
            break;
        }
    }

    Some(bm)
        // .map(|(r#move, prom, _)| (r#move, prom))
}

fn sort_moves(moves: HashSet<MoveType<Pos, Colored<Piece>>>) -> Vec<MoveType<Pos, Colored<Piece>>> {
    let mut moves = moves.into_iter().collect::<Vec<_>>();
    moves.sort_by(|a, b| {
        let a = match a {
            MoveType::Capture { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(*captured_piece))
            }
            MoveType::CapturePromotion { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(*captured_piece)) + 5
            }
            MoveType::EnPassant { .. } => 1,
            _ => 0,
        };
        let b = match b {
            MoveType::Capture { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(*captured_piece))
            }
            MoveType::CapturePromotion { captured_piece, .. } => {
                get_capture_piece_value(Piece::from(*captured_piece)) + 5
            }
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
        let mut gs =
            GameState::from_str("r4rk1/pp3ppp/2nb4/2p3P1/4p3/1PP5/PR1BPq2/2QK1b2 w - - 0 21")
                .unwrap();
        // get all valid moves
        let moves = gs.new_all_valid_moves(Color::White);
        // do a random move
        let moves = moves.into_iter().collect::<Vec<_>>();
        let m = pick_random(&moves).unwrap();
        gs.do_move(*m, None);
        let moves = gs.new_all_valid_moves(Color::Black);
        let moves = sort_moves(moves);
        println!(
            "{}",
            moves
                .iter()
                .map(|m| format!("{}", m))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    #[test]
    fn longest_minimax() {
        let mut gs = GameState::new();
        let now = Instant::now();
        let mut search = Search::new_time(100, 0);
        let m = search.search(&mut gs);
        println!("longest minimax took {}ms depth {}", now.elapsed().as_millis(), search.depth);
        // let mn1 = minimax(&mut gs, 1);
        // println!("minimax at depth of 1 took {}ms", now.elapsed().as_millis());
        // println!("move: {:?}", mn1);
        // let now = Instant::now();
        // // let ne1 = negamax_root(&mut gs, 1);
        // let ne1 = Search::new(0, 100).search(&mut gs);
        // println!("negamax at depth of 1 took {}ms", now.elapsed().as_millis());
        // println!("move: {:?}", ne1);
        // let now = Instant::now();
        // let mn1 = minimax(&mut gs, 4);
        // println!("minimax at depth of 4 took {}ms", now.elapsed().as_millis());
        // println!("move: {:?}", mn1);
        // let now = Instant::now();
        // // let ne1 = negamax_root(&mut gs, 4);
        // let ne1 = Search::new(2,5 ).search(&mut gs);
        // println!("negamax at depth of 4 took {}ms", now.elapsed().as_millis());
        // println!("move: {:?}", ne1);

        // // let now = Instant::now();
        // // let mn1 = minimax(&mut gs, 8);
        // // println!("minimax at depth of 8 took {}ms", now.elapsed().as_millis());
        // // println!("move: {:?}", mn1);
        // let now = Instant::now();
        // // let ne1 = negamax_root(&mut gs, 8);
        // let ne1 = Search::new(4, 5).search(&mut gs);
        // println!("negamax at depth of 8 took {}ms", now.elapsed().as_millis());
        // println!("move: {:?}", ne1);

        // // let now = Instant::now();
        // // let mn1 = minimax(&mut gs, 12);

        // // println!(
        // //     "minimax at depth of 12 took {}ms",
        // //     now.elapsed().as_millis()
        // // );
        // // println!("move: {:?}", mn1);
        // let now = Instant::now();
        // // let ne1 = negamax_root(&mut gs, 12);
        // let ne1 = Search::new(24, 5).search(&mut gs);
        // println!(
        //     "negamax at depth of 12 took {}ms",
        //     now.elapsed().as_millis()
        // );
        // println!("move: {:?}", ne1);

        // let now = Instant::now();
        // let mn1 = minimax(&mut gs, 16);
        // println!(
        //     "minimax at depth of 16 took {}ms",
        //     now.elapsed().as_millis()
        // );
        // println!("move: {:?}", mn1);
        // let now = Instant::now();
        // let ne1 = negamax_root(&mut gs, 16);
        // println!(
        //     "negamax at depth of 16 took {}ms",
        //     now.elapsed().as_millis()
        // );
        // println!("move: {:?}", ne1);

        // let now = Instant::now();
        // let mn1 = minimax(&mut gs, 20);
        // println!(
        //     "minimax at depth of 20 took {}ms",
        //     now.elapsed().as_millis()
        // );
        // println!("move: {:?}", mn1);
        // let now = Instant::now();
        // let ne1 = negamax_root(&mut gs, 20);
        // println!(
        //     "negamax at depth of 20 took {}ms",
        //     now.elapsed().as_millis()
        // );
        // println!("move: {:?}", ne1);
    }
}
