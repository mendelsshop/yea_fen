use crate::{moves::GameResult, Color, GameState, Piece};
use std::fs::OpenOptions;
use std::io::Write;

impl GameState {
    pub fn simple_eval(&self, mate: i32) -> i32 {
        // if we have a checkmate return the mate value
        match self.result {
            GameResult::CheckMate(c_color) => {
                if c_color == Color::White {
                    return mate;
                }
                -mate
            }
            GameResult::StaleMate | GameResult::Draw => 0,
            GameResult::InProgress | GameResult::ThreeFoldRepetition => {
                if self.check_repition().is_some() {
                    return 0;
                }
                let mut ret = 0;
                for row in &self.board.board {
                    for piece in row.iter().flatten() {
                        if Color::from(*piece) == Color::White {
                            ret += get_piece_value_mg(Piece::from(*piece));
                        } else {
                            ret -= get_piece_value_mg(Piece::from(*piece));
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
                    return mate;
                }
                -mate
            }
            GameResult::StaleMate | GameResult::Draw => 0,
            GameResult::InProgress | GameResult::ThreeFoldRepetition => {
                // first check if we have piece repetition
                if self.check_repition().is_some() {
                    return 0;
                }
                let mut eg = 0;
                let mut mg = 0;
                let total_phase = 24;
                let mut phase = 24;
                for row in self.board.board {
                    for piece in row.into_iter().flatten() {
                        match Piece::from(piece) {
                            Piece::Knight | Piece::Bishop => {
                                phase -= 1;
                            }
                            Piece::Rook => {
                                phase -= 2;
                            }
                            Piece::Queen => {
                                phase -= 4;
                            }
                            Piece::King | Piece::Pawn => {}
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
                phase = (phase * 256 + (total_phase / 2)) / total_phase;
                ((mg * (256 - phase)) + eg * phase) / 256
            }
        }
    }
    pub fn eval_board(&self, mate: i32) -> i32 {
        // evalutes the board based on how many pieces we have and their value
        let mut ret = 0;
        match self.result {
            GameResult::CheckMate(c_color) => {
                ret = if c_color == Color::White { mate } else { -mate }
            }
            GameResult::StaleMate | GameResult::Draw | GameResult::ThreeFoldRepetition => ret = 0,
            GameResult::InProgress => {
                // first check if we have piece repetition
                if self.check_repition().is_some() {
                    return 0;
                }
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

    pub(crate) fn check_repition(&self) -> Option<()> {
        if self.moves.len() >= 4 {
            let last_4_moves = self
                .moves
                .iter()
                .rev()
                .take(4)
                .map(|m| m.move_type)
                .collect::<Vec<_>>();
            // if move 0 == move 2 and move 1 == move 3 then we have piece repetition
            if last_4_moves[0].to() == last_4_moves[2].from().0
                && last_4_moves[1].to() == last_4_moves[3].from().0
            {
                return Some(());
            }
        }
        None
    }

    pub fn tapered_eval_board(&self, mate: i32) -> i32 {
        // evalutes the board based on how many pieces we have and their value
        let mut ret = 0;
        match self.result {
            GameResult::CheckMate(c_color) => {
                ret = if c_color == Color::White { mate } else { -mate }
            }
            GameResult::StaleMate | GameResult::Draw | GameResult::ThreeFoldRepetition => {
                ret = 0;
                // println!("{:?}", self.result);
            }
            GameResult::InProgress => {
                // first check if we have piece repetition
                if self.check_repition().is_some() {
                    return 0;
                }
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
        if ret == 0 {
            // println!("draw");
        }
        ret
    }
}

const fn get_piece_value_mg(piece: Piece) -> i32 {
    // the values are high so as when we do piece postion bonus ultimatly what pieces you have should have the most effect on the eval
    match piece {
        Piece::Pawn => 1000,
        Piece::Knight | Piece::Bishop => 3000,
        Piece::Rook => 5000,
        Piece::Queen => 9000,
        Piece::King => 100_000,
    }
}

const fn get_piece_value_eg(piece: Piece) -> i32 {
    // the values are high so as when we do piece postion bonus ultimatly what pieces you have should have the most effect on the eval
    match piece {
        Piece::Pawn => 1000,
        Piece::Knight | Piece::Bishop => 3000,
        Piece::Rook => 5000,
        Piece::Queen => 9000,
        Piece::King => 100_000,
    }
}

// taken from https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function
const MG_PAWN_TABLE: [[i32; 8]; 8] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [98, 134, 61, 95, 68, 126, 34, -11],
    [-6, 7, 26, 31, 65, 56, 25, -20],
    [-14, 13, 6, 21, 23, 12, 17, -23],
    [-27, -2, -5, 12, 17, 6, 10, -25],
    [-26, -4, -4, -10, 3, 3, 33, -12],
    [-35, -1, -20, -23, -15, 24, 38, -22],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const EG_PAWN_TABLE: [[i32; 8]; 8] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [178, 173, 158, 134, 147, 132, 165, 187],
    [94, 100, 85, 67, 56, 53, 82, 84],
    [32, 24, 13, 5, -2, 4, 17, 17],
    [13, 9, -3, -7, -7, -8, 3, -1],
    [4, 7, -6, 1, 0, -5, -1, -8],
    [13, 8, 8, 10, 13, 0, 2, -7],
    [0, 0, 0, 0, 0, 0, 0, 0],
];

const MG_KNIGHT_TABLE: [[i32; 8]; 8] = [
    [-167, -89, -34, -49, 61, -97, -15, -107],
    [-73, -41, 72, 36, 23, 62, 7, -17],
    [-47, 60, 37, 65, 84, 129, 73, 44],
    [-9, 17, 19, 53, 37, 69, 18, 22],
    [-13, 4, 16, 13, 28, 19, 21, -8],
    [-23, -9, 12, 10, 19, 17, 25, -16],
    [-29, -53, -12, -3, -1, 18, -14, -19],
    [-105, -21, -58, -33, -17, -28, -19, -23],
];

const EG_KNIGHT_TABLE: [[i32; 8]; 8] = [
    [-58, -38, -13, -28, -31, -27, -63, -99],
    [-25, -8, -25, -2, -9, -25, -24, -52],
    [-24, -20, 10, 9, -1, -9, -19, -41],
    [-17, 3, 22, 22, 22, 11, 8, -18],
    [-18, -6, 16, 25, 16, 17, 4, -18],
    [-23, -3, -1, 15, 10, -3, -20, -22],
    [-42, -20, -10, -5, -2, -20, -23, -44],
    [-29, -51, -23, -15, -22, -18, -50, -64],
];

const MG_BISHOP_TABLE: [[i32; 8]; 8] = [
    [-29, 4, -82, -37, -25, -42, 7, -8],
    [-26, 16, -18, -13, 30, 59, 18, -47],
    [-16, 37, 43, 40, 35, 50, 37, -2],
    [-4, 5, 19, 50, 37, 37, 7, -2],
    [-6, 13, 13, 26, 34, 12, 10, 4],
    [0, 15, 15, 15, 14, 27, 18, 10],
    [4, 15, 16, 0, 7, 21, 33, 1],
    [-33, -3, -14, -21, -13, -12, -39, -21],
];

const EG_BISHOP_TABLE: [[i32; 8]; 8] = [
    [-14, -21, -11, -8, -7, -9, -17, -24],
    [-8, -4, 7, -12, -3, -13, -4, -14],
    [2, -8, 0, -1, -2, 6, 0, 4],
    [-3, 9, 12, 9, 14, 10, 3, 2],
    [-6, 3, 13, 19, 7, 10, -3, -9],
    [-12, -3, 8, 10, 13, 3, -7, -15],
    [-14, -18, -7, -1, 4, -9, -15, -27],
    [-23, -9, -23, -5, -9, -16, -5, -17],
];

const MG_ROOK_TABLE: [[i32; 8]; 8] = [
    [32, 42, 32, 51, 63, 9, 31, 43],
    [27, 32, 58, 62, 80, 67, 26, 44],
    [-5, 19, 26, 36, 17, 45, 61, 16],
    [-24, -11, 7, 26, 24, 35, -8, -20],
    [-36, -26, -12, -1, 9, -7, 6, -23],
    [-45, -25, -16, -17, 3, 0, -5, -33],
    [-44, -16, -20, -9, -1, 11, -6, -71],
    [-19, -13, 1, 17, 16, 7, -37, -26],
];

const EG_ROOK_TABLE: [[i32; 8]; 8] = [
    [13, 10, 18, 15, 12, 12, 8, 5],
    [11, 13, 13, 11, -3, 3, 8, 3],
    [7, 7, 7, 5, 4, -3, -5, -3],
    [4, 3, 13, 1, 2, 1, -1, 2],
    [3, 5, 8, 4, -5, -6, -8, -11],
    [-4, 0, -5, -1, -7, -12, -8, -16],
    [-6, -6, 0, 2, -9, -9, -11, -3],
    [-9, 2, 3, -1, -5, -13, 4, -20],
];

const MG_QUEEN_TABLE: [[i32; 8]; 8] = [
    [-28, 0, 29, 12, 59, 44, 43, 45],
    [-24, -39, -5, 1, -16, 57, 28, 54],
    [-13, -17, 7, 8, 29, 56, 47, 57],
    [-27, -27, -16, -16, -1, 17, -2, 1],
    [-9, -26, -9, -10, -2, -4, 3, -3],
    [-14, 2, -11, -2, -5, 2, 14, 5],
    [-35, -8, 11, 2, 8, 15, -3, 1],
    [-1, -18, -9, 10, -15, -25, -31, -50],
];

const EG_QUEEN_TABLE: [[i32; 8]; 8] = [
    [-9, 22, 22, 27, 27, 19, 10, 20],
    [-17, 20, 32, 41, 58, 25, 30, 0],
    [-20, 6, 9, 49, 47, 35, 19, 9],
    [3, 22, 24, 45, 57, 40, 57, 36],
    [-18, 28, 19, 47, 31, 34, 39, 23],
    [-16, -27, 15, 6, 9, 17, 10, 5],
    [-22, -23, -30, -16, -16, -23, -36, -32],
    [-33, -28, -22, -43, -5, -32, -20, -41],
];

const MG_KING_TABLE: [[i32; 8]; 8] = [
    [-65, 23, 16, -15, -56, -34, 2, 13],
    [29, -1, -20, -7, -8, -4, -38, -29],
    [-9, 24, 2, -16, -20, 6, 22, -22],
    [-17, -20, -12, -27, -30, -25, -14, -36],
    [-49, -1, -27, -39, -46, -44, -33, -51],
    [-14, -14, -22, -46, -44, -30, -15, -27],
    [1, 7, -8, -64, -43, -16, 9, 8],
    [-15, 36, 12, -54, 8, -28, 24, 14],
];

const EG_KING_TABLE: [[i32; 8]; 8] = [
    [-74, -35, -18, -18, -11, 15, 4, -17],
    [-12, 17, 14, 17, 17, 38, 23, 11],
    [10, 17, 23, 15, 20, 45, 44, 13],
    [-8, 22, 24, 27, 26, 33, 26, 3],
    [-18, -4, 21, 24, 27, 23, 9, -11],
    [-19, -3, 11, 21, 23, 16, 7, -9],
    [-27, -11, 4, 13, 14, 4, -5, -17],
    [-53, -34, -21, -11, -28, -14, -24, -43],
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
                piece_table!(pos, color, EG_PAWN_TABLE)
            } else {
                piece_table!(pos, color, MG_PAWN_TABLE)
            }
        }
        Piece::Knight => {
            if end_game {
                piece_table!(pos, color, EG_KNIGHT_TABLE)
            } else {
                piece_table!(pos, color, MG_KNIGHT_TABLE)
            }
        }
        Piece::Bishop => {
            if end_game {
                piece_table!(pos, color, EG_BISHOP_TABLE)
            } else {
                piece_table!(pos, color, MG_BISHOP_TABLE)
            }
        }
        Piece::Rook => {
            if end_game {
                piece_table!(pos, color, EG_ROOK_TABLE)
            } else {
                piece_table!(pos, color, MG_ROOK_TABLE)
            }
        }
        Piece::Queen => {
            if end_game {
                piece_table!(pos, color, EG_QUEEN_TABLE)
            } else {
                piece_table!(pos, color, MG_QUEEN_TABLE)
            }
        }
        Piece::King => {
            if end_game {
                piece_table!(pos, color, EG_KING_TABLE)
            } else {
                piece_table!(pos, color, MG_KING_TABLE)
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
        println!("s1b {}", -state.eval_board(i32::MAX));
        println!("ts1b {}", -state.tapered_eval_board(i32::MAX));
        state.active_color = Color::White;
        println!("s1w {}", state.eval_board(i32::MAX));
        // use taperd eval
        println!("ts1w {}", state.tapered_eval_board(i32::MAX));

        println!("{}", state1.board);
        println!("s2w {}", state1.eval_board(i32::MAX));
        println!("ts2w {}", state1.tapered_eval_board(i32::MAX));
        state1.active_color = Color::Black;
        println!("s2b {}", -state1.eval_board(i32::MAX));
        println!("ts2b {}", -state1.tapered_eval_board(i32::MAX));

        println!("{}", state2.board);
        println!("s3b {}", -state2.eval_board(i32::MAX));
        println!("ts3b {}", -state2.tapered_eval_board(i32::MAX));
        state2.active_color = Color::White;
        println!("s3w {}", state2.eval_board(i32::MAX));
        println!("ts3w {}", state2.tapered_eval_board(i32::MAX));

        let mut state3 =
            GameState::from_str("rnbqkb1r/pppppppp/5n2/8/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 2 2 ")
                .unwrap();
        println!("{}", state3.board);
        println!("s4b {}", -state3.eval_board(i32::MAX));
        println!("ts4b {}", -state3.tapered_eval_board(i32::MAX));
        state3.active_color = Color::White;
        println!("s4w {}", state3.eval_board(i32::MAX));
        println!("ts4w {}", state3.tapered_eval_board(i32::MAX));
    }
}
