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
            loop {
                let r#move = match moves.next() {
                    None => break Some((alpha, pot_move)),
                    Some(mv) => mv,
                };

                new_game.do_move(*r#move, Some(prom));
                let score = min(&mut new_game, depth - 1, alpha, beta, color, i32::MAX);
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
    if depth == 0 {
        return tapered_eval_board(game, color, mate);
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = game.new_all_valid_moves(game.active_color);
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = min(game, depth - 1, alpha, beta, color, mate - 1);
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

fn min(
    game: &mut GameState,
    depth: usize,
    alpha: i32,
    mut beta: i32,
    color: Color,
    mate: i32,
) -> i32 {
    if depth == 0 {
        let score = tapered_eval_board(game, color, mate);
        return -score;
    }
    let prom = match game.active_color {
        Color::Black => Colored::Black(Piece::Queen),
        Color::White => Colored::White(Piece::Queen),
    };
    let moves = game.new_all_valid_moves(game.active_color);
    for r#move in &moves {
        if game.do_move(*r#move, Some(prom)) {
            let score = max(game, depth - 1, alpha, beta, color, mate - 1);
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
    // the values are high so as when we do piece postion bonus ultimatly what pieces you have should have the most effect on the eval
    match piece {
        Piece::Pawn => 100,
        Piece::Knight | Piece::Bishop => 300,
        Piece::Rook => 500,
        Piece::Queen => 900,
        Piece::King => 10000,
    }
}

fn eval_board(game: &GameState, color: Color, mate: i32) -> i32 {
    // evalutes the board based on how many pieces we have and their value
    let mut ret = 0;
    match game.result {
        GameResult::CheckMate(c_color) => ret = if c_color == color { -mate } else { mate },
        // GameResult::CheckMate(c_color) => ret = 0,
        GameResult::StaleMate | GameResult::Draw | GameResult::InProgress => {
            // first check if we have piece repetition

            let mut pieces = vec![];
            for (rowidx, row) in game.board.board.iter().enumerate() {
                for (col, piece) in row
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, piece)| piece.map(|piece| (idx, piece)))
                {
                    // todo: use piece list and add point for being in certain positions
                    pieces.push(((rowidx, col), piece));
                }
            }
            // todo if pieces count is less than a certain number use end game piece tables
            let eg = pieces.len() < 13;
            // pawn count
            ret += pieces
                .iter()
                .filter(|piece| {
                    Color::from(piece.1) == color && Piece::from(piece.1) == Piece::Pawn
                })
                .count() as i32
                * 2;
            // ret -= pieces.iter().filter(|piece| Color::from(piece.1) != color && Piece::from(piece.1) == Piece::Pawn).count() as i32 *2;
            for piece in pieces {
                if Color::from(piece.1) == game.active_color {
                    // todo: use piece list and add point for being in certain positions
                    ret += get_piece_value(Piece::from(piece.1))
                        + eval_piece_pos(
                            piece.1.into(),
                            (piece.0 .0, piece.0 .1),
                            Color::from(piece.1),
                            eg,
                        );
                } else {
                    ret -= get_piece_value(Piece::from(piece.1))
                        + eval_piece_pos(
                            piece.1.into(),
                            (piece.0 .0, piece.0 .1),
                            Color::from(piece.1),
                            eg,
                        );
                }
            }

            if let GameResult::StaleMate | GameResult::Draw = game.result {
                // if we have a stalemate or draw we to see if we have an advantage
                // if we have an advantage we can still win so we should return around 0
                // if we have a big disadvantage we should return a big number so we do pick that move
                // if we have a small disadvantage we should return a small number close to 0 so if we have a better move we will pick that

                if ret.is_negative() {
                    // we have a disadvantage
                    if ret.abs() > 1000 {
                        // we have a big disadvantage
                        ret = ret.abs() / 2;
                    } else {
                        // we have a small disadvantage
                        ret = ret.abs() / 4;
                    }
                } else {
                    ret = 0;
                }
            }
        }
    }

    ret
}

fn tapered_eval_board(game: &GameState, color: Color, mate: i32) -> i32 {
    // evalutes the board based on how many pieces we have and their value
    let mut ret = 0;
    match game.result {
        GameResult::CheckMate(c_color) => ret = if c_color == color { -mate } else { mate },
        // GameResult::CheckMate(c_color) => ret = 0,
        GameResult::StaleMate | GameResult::Draw | GameResult::InProgress => {
            // first check if we have piece repetition

            let mut pieces = vec![];
            for (rowidx, row) in game.board.board.iter().enumerate() {
                for (col, piece) in row
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, piece)| piece.map(|piece| (idx, piece)))
                {
                    // todo: use piece list and add point for being in certain positions
                    pieces.push(((rowidx, col), piece));
                }
            }
            // todo if pieces count is less than a certain number use end game piece tables
            // let eg = pieces.len() < 13;
            let mut eg = 0;
            let mut mg = 0;
            let pawn_phase = 0;
            let knight_phase = 1;
            let bishop_phase = 1;
            let rook_phase = 2;
            let queen_phase = 4;

            let total_phase = pawn_phase * 16
                + knight_phase * 4
                + bishop_phase * 4
                + rook_phase * 4
                + queen_phase * 2;

            let mut phase = total_phase;

            // let mut pawn_count = 0;
            // let mut knight_count = 0;
            // let mut bishop_count = 0;
            // let mut rook_count = 0;
            // let mut queen_count = 0;
            // pawn count
            ret += pieces
                .iter()
                .filter(|piece| {
                    Color::from(piece.1) == color && Piece::from(piece.1) == Piece::Pawn
                })
                .count() as i32
                * 2;
            // ret -= pieces.iter().filter(|piece| Color::from(piece.1) != color && Piece::from(piece.1) == Piece::Pawn).count() as i32 *2;
            for piece in pieces {
                match Piece::from(piece.1) {
                    Piece::Pawn => {
                        // pawn_count += 1;
                        phase -= pawn_phase;
                    }
                    Piece::Knight => {
                        // knight_count += 1;
                        phase -= knight_phase;
                    }
                    Piece::Bishop => {
                        // bishop_count += 1;
                        phase -= bishop_phase;
                    }
                    Piece::Rook => {
                        // rook_count += 1;
                        phase -= rook_phase;
                    }
                    Piece::Queen => {
                        // queen_count += 1;
                        phase -= queen_phase;
                    }
                    Piece::King => {}
                }
                if Color::from(piece.1) == game.active_color {
                    // todo: use piece list and add point for being in certain positions
                    eg += get_piece_value(Piece::from(piece.1))
                        + eval_piece_pos(
                            piece.1.into(),
                            (piece.0 .0, piece.0 .1),
                            Color::from(piece.1),
                            true,
                        );
                    mg += get_piece_value(Piece::from(piece.1))
                        + eval_piece_pos(
                            piece.1.into(),
                            (piece.0 .0, piece.0 .1),
                            Color::from(piece.1),
                            false,
                        );
                } else {
                    eg -= get_piece_value(Piece::from(piece.1))
                        + eval_piece_pos(
                            piece.1.into(),
                            (piece.0 .0, piece.0 .1),
                            Color::from(piece.1),
                            true,
                        );
                    mg -= get_piece_value(Piece::from(piece.1));
                }
            }
            // if
            match game.get_castling_moves().get(color) {
                crate::Castling::None => mg -= 5,
                crate::Castling::KingSide | crate::Castling::QueenSide => mg += 25,
                crate::Castling::Both => mg += 55,
            }
            match game.get_castling_moves().get(color.opposite()) {
                crate::Castling::None => mg += 5,
                crate::Castling::KingSide | crate::Castling::QueenSide => mg -= 25,
                crate::Castling::Both => mg -= 55,
            }

            phase = (phase * 256 + (total_phase / 2)) / total_phase;
            ret += ((mg * (256 - phase)) + eg * phase) / 256;

            if let GameResult::StaleMate | GameResult::Draw = game.result {
                // if we have a stalemate or draw we to see if we have an advantage
                // if we have an advantage we can still win so we should return around 0
                // if we have a big disadvantage we should return a big number so we do pick that move
                // if we have a small disadvantage we should return a small number close to 0 so if we have a better move we will pick that

                if ret.is_negative() {
                    // we have a disadvantage
                    if ret.abs() > 1000 {
                        // we have a big disadvantage
                        ret = ret.abs() / 2;
                    } else {
                        // we have a small disadvantage
                        ret = ret.abs() / 4;
                    }
                } else {
                    ret = 0;
                }
            }
        }
    }

    ret
}
const MG_PAWN: [[i32; 8]; 8] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [50, 50, 50, 50, 50, 50, 50, 50],
    [10, 10, 20, 30, 30, 20, 10, 10],
    [5, 5, 10, 25, 25, 10, 5, 5],
    [0, 0, 0, 20, 20, 0, 0, 0],
    [5, -5, -10, 0, 0, -10, -5, 5],
    [5, 10, 10, -20, -20, 10, 10, 5],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const EG_PAWN: [[i32; 8]; 8] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [20, 20, 20, 20, 20, 20, 20, 20],
    [25, 30, 25, 25, 25, 30, 25, 25],
    [30, 30, 30, 30, 30, 30, 30, 30],
    [40, 40, 40, 40, 40, 40, 40, 40],
    [50, 50, 50, 45, 45, 50, 50, 50],
    [60, 60, 60, 55, 55, 60, 60, 60],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const MG_KNIGHT: [[i32; 8]; 8] = [
    [-50, -40, -30, -30, -30, -30, -40, -50],
    [-40, -20, 0, 0, 0, 0, -20, -40],
    [-30, 0, 10, 15, 15, 10, 0, -30],
    [-30, 5, 15, 20, 20, 15, 5, -30],
    [-30, 0, 15, 20, 20, 15, 0, -30],
    [-30, 5, 10, 15, 15, 10, 5, -30],
    [-40, -20, 0, 5, 5, 0, -20, -40],
    [-50, -40, -30, -30, -30, -30, -40, -50],
];

const MG_BISHOP: [[i32; 8]; 8] = [
    [-20, -10, -10, -10, -10, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 10, 10, 5, 0, -10],
    [-10, 5, 5, 10, 10, 5, 5, -10],
    [-10, 0, 10, 10, 10, 10, 0, -10],
    [-10, 10, 10, 10, 10, 10, 10, -10],
    [-10, 5, 0, 0, 0, 0, 5, -10],
    [-20, -10, -10, -10, -10, -10, -10, -20],
];

const MG_ROOK: [[i32; 8]; 8] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [5, 10, 10, 10, 10, 10, 10, 5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [-5, 0, 0, 0, 0, 0, 0, -5],
    [0, 0, 0, 5, 5, 0, 0, 0],
];

// make queen mid game table
const MG_QUEEN: [[i32; 8]; 8] = [
    [-20, -10, -10, -5, -5, -10, -10, -20],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 5, 5, 5, 0, -10],
    [-5, 0, 5, 5, 5, 5, 0, -5],
    [0, 0, 5, 5, 5, 5, 0, -5],
    [-10, 5, 5, 5, 5, 5, 0, -10],
    [-10, 0, 5, 0, 0, 0, 0, -10],
    [-20, -10, -10, -5, -5, -10, -10, -20],
];

// king table
const MG_KING: [[i32; 8]; 8] = [
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-30, -40, -40, -50, -50, -40, -40, -30],
    [-20, -30, -30, -40, -40, -30, -30, -20],
    [-10, -20, -20, -20, -20, -20, -20, -10],
    [20, 20, 0, 0, 0, 0, 20, 20],
    [20, 30, 10, 0, 0, 10, 30, 20],
];

const EG_KING: [[i32; 8]; 8] = [
    [-10, -10, -10, -10, -10, -10, -10, -10],
    [-10, 0, 0, 0, 0, 0, 0, -10],
    [-10, 0, 5, 10, 10, 5, 0, -10],
    [-10, 5, 5, 10, 10, 5, 5, -10],
    [-10, 0, 10, 10, 10, 10, 0, -10],
    [-10, 10, 10, 10, 10, 10, 10, -10],
    [-10, 5, 0, 0, 0, 0, 5, -10],
    [-20, -10, -10, -10, -10, -10, -10, -20],
];

macro_rules! piece_table {
    ($pos:ident, $color:ident, $table:ident) => {
        if $color == Color::White {
            $table[$pos.0][$pos.1]
        } else {
            $table[7 - $pos.0][$pos.1]
        }
    };
}

fn eval_piece_pos(piece: Piece, pos: (usize, usize), color: Color, end_game: bool) -> i32 {
    match piece {
        Piece::Pawn => {
            if end_game {
                piece_table!(pos, color, EG_PAWN)
            } else {
                piece_table!(pos, color, MG_PAWN)
            }
        }
        Piece::Knight => {
            piece_table!(pos, color, MG_KNIGHT)
        }
        Piece::Bishop => {
            piece_table!(pos, color, MG_BISHOP)
        }
        Piece::Rook => {
            piece_table!(pos, color, MG_ROOK)
        }
        Piece::Queen => {
            piece_table!(pos, color, MG_QUEEN)
        }
        Piece::King => {
            // piece_table!(pos, color, MG_KING)
            if end_game {
                piece_table!(pos, color, EG_KING)
            } else {
                piece_table!(pos, color, MG_KING)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    // use std::time::Instant;

    use std::str::FromStr;

    use super::*;
    use crate::{chess_engines::random::do_random_move, Board, GameState};

    #[test]
    fn test_eval() {
        let mut state = GameState::from_str(
            "r1bqkbnr/pppp1Qp1/2n4p/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4",
        )
        .unwrap();
        let mut state1 =
            GameState::from_str("r4k1b/ppp5/6K1/3B4/8/4P3/PPpP2P1/RNB5 w - - 0 23").unwrap();
        let mut state2 = GameState::from_str("6kr/1p4p1/8/3n3p/rP6/3b4/1K6/8 b - - 1 40").unwrap();

        println!("s1b {}", eval_board(&state, Color::Black, i32::MAX));
        println!(
            "ts1b {}",
            tapered_eval_board(&state, Color::Black, i32::MAX)
        );
        state.active_color = Color::White;
        println!("s1w {}", eval_board(&state, Color::White, i32::MAX));
        // use taperd eval
        println!(
            "ts1w {}",
            tapered_eval_board(&state, Color::White, i32::MAX)
        );

        println!("s2w {}", eval_board(&state1, Color::White, i32::MAX));
        println!(
            "ts2w {}",
            tapered_eval_board(&state1, Color::White, i32::MAX)
        );
        state1.active_color = Color::Black;
        println!("s2b {}", eval_board(&state1, Color::Black, i32::MAX));
        println!(
            "ts2b {}",
            tapered_eval_board(&state1, Color::Black, i32::MAX)
        );
        println!("s3b {}", eval_board(&state2, Color::Black, i32::MAX));
        println!(
            "ts3b {}",
            tapered_eval_board(&state2, Color::Black, i32::MAX)
        );
        state2.active_color = Color::White;
        println!("s3w {}", eval_board(&state2, Color::White, i32::MAX));
        println!(
            "ts3w {}",
            tapered_eval_board(&state2, Color::White, i32::MAX)
        );
    }
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
    fn tapered() {
        let board = Board::from_str("rnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap();

        let pawn_phase = 0;
        let knight_phase = 1;
        let bishop_phase = 1;
        let rook_phase = 2;
        let queen_phase = 4;

        let total_phase = pawn_phase * 16
            + knight_phase * 4
            + bishop_phase * 4
            + rook_phase * 4
            + queen_phase * 2;

        let mut phase = total_phase;

        let mut pawn_count = 0;
        let mut knight_count = 0;
        let mut bishop_count = 0;
        let mut rook_count = 0;
        let mut queen_count = 0;

        for row in board.board {
            for piece in row {
                match piece {
                    Some(piece) => match Piece::from(piece) {
                        Piece::Pawn => {
                            pawn_count += 1;
                        }
                        Piece::Knight => {
                            knight_count += 1;
                        }
                        Piece::Bishop => {
                            bishop_count += 1;
                        }
                        Piece::Rook => {
                            rook_count += 1;
                        }
                        Piece::Queen => {
                            queen_count += 1;
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }
        println!("pawn {}", pawn_count);
        println!("knight {}", knight_count);
        println!("bishop {}", bishop_count);
        println!("rook {}", rook_count);
        println!("queen {}", queen_count);

        phase -= pawn_phase * pawn_count;
        phase -= knight_phase * knight_count;
        phase -= bishop_phase * bishop_count;
        phase -= rook_phase * rook_count;
        phase -= queen_phase * queen_count;
        println!("{}", phase);
        phase = (phase * 256 + (total_phase / 2)) / total_phase;
        println!("{}", phase);
        println!("{}", total_phase);
    }
}
