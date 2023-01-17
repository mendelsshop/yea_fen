use crate::{moves::GameResult, Color, GameState, Piece};

impl GameState {
    pub fn simple_eval(&self,  mate: i32) -> i32 {
        // if we have a checkmate return the mate value
        match self.result {
            GameResult::CheckMate(c_color) => {
                if c_color == Color::White {
                    return -mate;
                } else {
                    return mate;
                }
            }
            GameResult::StaleMate | GameResult::Draw => return 0,
            GameResult::InProgress | GameResult::ThreeFoldRepetition => {
                let mut ret = 0;
                for row in self.board.board.iter() {
                    for piece in row.iter() {
                        if let Some(piece) = piece {
                            if Color::from(*piece) == Color::White {
                                ret += get_piece_value_mg(Piece::from(*piece));
                            } else {
                                ret -= get_piece_value_mg(Piece::from(*piece));
                            }
                            // ret += get_piece_value(Piece::from(*piece));
                        }
                    }
                }
                ret
            }
        }
    }

    pub fn simple_tapered_eval(&self, mate: i32) -> i32 {
        // if we have a checkmate return the mate value
        match self.result {
            GameResult::CheckMate(c_color) => {
                if c_color == Color::White {
                    return -mate;
                } else {
                    return mate;
                }
            }
            GameResult::StaleMate | GameResult::Draw => return 0,
            GameResult::InProgress | GameResult::ThreeFoldRepetition => {
                let mut eg = 0;
                let mut mg = 0;
                let total_phase = 24;
                let mut phase = 24;
                for row in self.board.board {
                    for piece in row {
                        if let Some(piece) = piece {
                            match Piece::from(piece) {
                                Piece::Pawn => {
                                    // pawn_count += 1;
                                    phase -= 0;
                                }
                                Piece::Knight => {
                                    // knight_count += 1;
                                    phase -= 1;
                                }
                                Piece::Bishop => {
                                    // bishop_count += 1;
                                    phase -= 1;
                                }
                                Piece::Rook => {
                                    // rook_count += 1;
                                    phase -= 2;
                                }
                                Piece::Queen => {
                                    // queen_count += 1;
                                    phase -= 4;
                                }
                                Piece::King => {}
                            }
                            if Color::from(piece) == Color::White {
                                // todo: use piece list and add point for being in certain positions
                                eg += get_piece_value_eg(Piece::from(piece));
                                mg += get_piece_value_mg(Piece::from(piece));
                            } else {
                                eg -= get_piece_value_eg(Piece::from(piece));
                                mg -= get_piece_value_mg(Piece::from(piece));
                            }
                        }
                    }
                }
                phase = (phase * 256 + (total_phase / 2)) / total_phase;
                ((mg * (256 - phase)) + eg * phase) / 256
            }
        }
    }
    pub fn eval_board(&self, mate: i32) -> i32 {
        // evalutes the board based on how many pieces we have and their value
        let mut ret = 0;
        match self.result {
            GameResult::CheckMate(c_color) => ret = if c_color == Color::White { -mate } else { mate },
            GameResult::StaleMate | GameResult::Draw | GameResult::ThreeFoldRepetition => ret = 0,
            GameResult::InProgress => {
                // first check if we have piece repetition

                let mut pieces = vec![];
                for (rowidx, row) in self.board.board.iter().enumerate() {
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
                for piece in pieces {
                    if Color::from(piece.1) == Color::White {
                        // todo: use piece list and add point for being in certain positions
                        ret += get_piece_value_mg(Piece::from(piece.1))
                            + eval_piece_pos(
                                piece.1.into(),
                                (piece.0 .0, piece.0 .1),
                                Color::from(piece.1),
                                eg,
                            );
                    } else {
                        ret -= get_piece_value_mg(Piece::from(piece.1))
                            + eval_piece_pos(
                                piece.1.into(),
                                (piece.0 .0, piece.0 .1),
                                Color::from(piece.1),
                                eg,
                            );
                    }
                }
                // match self.get_castling_moves().get(Color::White) {
                //     crate::Castling::None => ret -= 5,
                //     crate::Castling::KingSide | crate::Castling::QueenSide => ret += 25,
                //     crate::Castling::Both => ret += 55,
                // }
                // match self.get_castling_moves().get(Color::Black) {
                //     crate::Castling::None => ret += 5,
                //     crate::Castling::KingSide | crate::Castling::QueenSide => ret -= 25,
                //     crate::Castling::Both => ret -= 55,
                // }
            }
        }

        ret
    }

    pub fn tapered_eval_board(&self, mate: i32) -> i32 {
        // evalutes the board based on how many pieces we have and their value
        let mut ret = 0;
        match self.result {
            GameResult::CheckMate(c_color) => ret = if c_color == Color::White { -mate } else { mate },
            GameResult::StaleMate | GameResult::Draw | GameResult::ThreeFoldRepetition => ret = 0,
            GameResult::InProgress => {
                let mut pieces = vec![];
                for (rowidx, row) in self.board.board.iter().enumerate() {
                    for (col, piece) in row
                        .iter()
                        .enumerate()
                        .filter_map(|(idx, piece)| piece.map(|piece| (idx, piece)))
                    {
                        pieces.push(((rowidx, col), piece));
                    }
                }
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
                for piece in pieces {
                    match Piece::from(piece.1) {
                        Piece::Pawn => {
                            phase -= pawn_phase;
                        }
                        Piece::Knight => {
                            phase -= knight_phase;
                        }
                        Piece::Bishop => {
                            phase -= bishop_phase;
                        }
                        Piece::Rook => {
                            phase -= rook_phase;
                        }
                        Piece::Queen => {
                            phase -= queen_phase;
                        }
                        Piece::King => {}
                    }
                    if Color::from(piece.1) == Color::White {
                        // todo: use piece list and add point for being in certain positions
                        eg += get_piece_value_eg(Piece::from(piece.1))
                            + eval_piece_pos(
                                piece.1.into(),
                                (piece.0 .0, piece.0 .1),
                                Color::from(piece.1),
                                true,
                            );
                        mg += get_piece_value_mg(Piece::from(piece.1))
                            + eval_piece_pos(
                                piece.1.into(),
                                (piece.0 .0, piece.0 .1),
                                Color::from(piece.1),
                                false,
                            );
                    } else {
                        eg -= get_piece_value_eg(Piece::from(piece.1))
                            + eval_piece_pos(
                                piece.1.into(),
                                (piece.0 .0, piece.0 .1),
                                Color::from(piece.1),
                                true,
                            );
                        mg -= get_piece_value_mg(Piece::from(piece.1))
                            + eval_piece_pos(
                                piece.1.into(),
                                (piece.0 .0, piece.0 .1),
                                Color::from(piece.1),
                                false,
                            );
                    }
                }
                // match self.get_castling_moves().get(Color::White) {
                //     crate::Castling::None => mg -= 5,
                //     crate::Castling::KingSide | crate::Castling::QueenSide => mg += 25,
                //     crate::Castling::Both => mg += 55,
                // }
                // match self.get_castling_moves().get(Color::Black) {
                //     crate::Castling::None => mg += 5,
                //     crate::Castling::KingSide | crate::Castling::QueenSide => mg -= 25,
                //     crate::Castling::Both => mg -= 55,
                // }

                phase = (phase * 256 + (total_phase / 2)) / total_phase;
                ret += ((mg * (256 - phase)) + eg * phase) / 256;
            }
        }

        ret
    }
}

const fn get_piece_value_mg(piece: Piece) -> i32 {
    // the values are high so as when we do piece postion bonus ultimatly what pieces you have should have the most effect on the eval
    match piece {
        Piece::Pawn => 100,
        Piece::Knight | Piece::Bishop => 300,
        Piece::Rook => 500,
        Piece::Queen => 900,
        Piece::King => 10000,
    }
}

const fn get_piece_value_eg(piece: Piece) -> i32 {
    // the values are high so as when we do piece postion bonus ultimatly what pieces you have should have the most effect on the eval
    match piece {
        Piece::Pawn => 100,
        Piece::Knight | Piece::Bishop => 300,
        Piece::Rook => 500,
        Piece::Queen => 900,
        Piece::King => 10000,
    }
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

    #[test]
    fn test_eval() {
        let mut state = GameState::from_str(
            "r1bqkbnr/pppp1Qp1/2n4p/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 4",
        )
        .unwrap();
        let mut state1 =
            GameState::from_str("r4k1b/ppp5/6K1/3B4/8/4P3/PPpP2P1/RNB5 w - - 0 23").unwrap();

        let mut state2 = GameState::from_str("6kr/1p4p1/8/3n3p/rP6/3b4/1K6/8 b - - 1 40").unwrap();
        println!("{}", state.board);
        println!("s1b {}", -state.eval_board( i32::MAX));
        println!("ts1b {}", -state.tapered_eval_board( i32::MAX));
        state.active_color = Color::White;
        println!("s1w {}", state.eval_board( i32::MAX));
        // use taperd eval
        println!("ts1w {}", state.tapered_eval_board( i32::MAX));

        println!("{}", state1.board);
        println!("s2w {}", state1.eval_board( i32::MAX));
        println!("ts2w {}", state1.tapered_eval_board( i32::MAX));
        state1.active_color = Color::Black;
        println!("s2b {}", -state1.eval_board( i32::MAX));
        println!("ts2b {}", -state1.tapered_eval_board( i32::MAX));

        println!("{}", state2.board);
        println!("s3b {}", -state2.eval_board( i32::MAX));
        println!("ts3b {}", -state2.tapered_eval_board( i32::MAX));
        state2.active_color = Color::White;
        println!("s3w {}", state2.eval_board( i32::MAX));
        println!("ts3w {}", state2.tapered_eval_board( i32::MAX));
    }
}
