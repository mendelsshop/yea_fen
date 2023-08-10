#![allow(long_running_const_eval)]

use std::{
    fmt::{Debug, Display},
    ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Shr},
    str::FromStr,
};
// mod attack_consts;
mod moves;
mod random;
// include!(concat!(env!("OUT_DIR"), "/hello.rs"));
pub mod attack_consts;
pub use attack_consts::*;
// these impl macros are taken from:
// https://github.com/analog-hors/magic-bitboards-demo/blob/main/types/src/bitboard.rs

macro_rules! impl_math_ops {
    ($($trait:ident,$fn:ident;)*) => {$(
        impl $trait for BitBoard {
            type Output = Self;

            fn $fn(self, other: Self) -> Self::Output {
                Self{board: $trait::$fn(self.board, other.board)}
            }
        }
    )*};
}
impl_math_ops! {
    BitAnd, bitand;
    BitOr, bitor;
    BitXor, bitxor;
    Shr, shr;
}

macro_rules! impl_math_assign_ops {
    ($($trait:ident,$fn:ident;)*) => {$(
        impl $trait for BitBoard {
            fn $fn(&mut self, other: Self) {
                $trait::$fn(&mut self.board, other.board)
            }
        }
    )*};
}
impl_math_assign_ops! {
    BitAndAssign, bitand_assign;
    BitOrAssign, bitor_assign;
    BitXorAssign, bitxor_assign;
}

#[derive(Default, Clone, Copy, PartialEq)]
pub struct BitBoard {
    board: u64,
}

#[macro_export(local_inner_macros)]
macro_rules! pop_bit {
    ($bb:expr, $index:expr) => {
        $bb.board &= !(1 << $index);
    };
}

impl BitBoard {
    /// Note: this will create a [BitBoard] with the literal value passed in,
    /// if you want to create a new [BitBoard] with some index initalized
    /// consider using [BitBoard::new_with_index]
    pub const fn new(board: u64) -> Self {
        Self { board }
    }

    /// creates a new [BitBoard] with bit at index on
    pub const fn new_with_index(index: u64) -> Self {
        Self { board: 1 << index }
    }

    // needed as a `Default` implentation for const contexts
    pub const fn new_empty() -> Self {
        Self { board: 0 }
    }

    fn remove_index(&mut self, index: usize) {
        // set board to and of itself and the opposite of a number with a bit at index
        // example board = 1001, index = 3
        // 1 << 3 -> 1000, !1000 -> 0111
        // 1001 & 0111 -> 0001

        // so because the not inverts the "index" it makes it that when we or the board we only effect the index
        self.board &= !(1 << index);
    }

    fn set_index(&mut self, index: usize) {
        self.board |= 1 << index;
    }

    pub const fn get_index(&self, index: usize) -> u64 {
        self.board & (1 << index)
    }

    /// returns true if there is a one in the binary repr
    /// at the index given
    const fn exists(&self, index: usize) -> bool {
        self.get_index(index) != 0
    }

    const fn count_bits(&self) -> u32 {
        self.board.count_ones()
    }

    // #[const_eval_limit]
    // TODO: return option/result of u32

    const fn least_significant_first_bit_index(&self) -> i64 {
        if self.board != 0 {
            self.board.trailing_zeros() as i64
        } else {
            -1
        }
    }
}

impl Debug for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.board.to_ne_bytes();
        let mut count = 9;
        write!(
            f,
            "{}\n{}\t +--------\n\t  abcdefgh",
            self.board,
            bytes
                .map(|byte| {
                    count -= 1;
                    format!("\t{count}|{:08b}\n", byte.reverse_bits())
                })
                .join("")
        )
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq)]
pub struct Board {
    /// white pieces
    white_pawns: BitBoard,
    white_bishops: BitBoard,
    white_knights: BitBoard,
    white_rooks: BitBoard,
    white_queens: BitBoard,
    white_kings: BitBoard,

    // black pieces
    black_pawns: BitBoard,
    black_bishops: BitBoard,
    black_knights: BitBoard,
    black_rooks: BitBoard,
    black_queens: BitBoard,
    black_kings: BitBoard,
}

impl Board {
    /// generates a the board for the start of a chess game
    pub fn new_board() -> Self {
        Self {
            white_pawns: BitBoard::new(71776119061217280),
            white_bishops: BitBoard::new(2594073385365405696),
            white_knights: BitBoard::new(4755801206503243776),
            white_rooks: BitBoard::new(9295429630892703744),
            white_queens: BitBoard::new(576460752303423488),
            white_kings: BitBoard::new(1152921504606846976),
            black_pawns: BitBoard::new(65280),
            black_bishops: BitBoard::new(36),
            black_knights: BitBoard::new(66),
            black_rooks: BitBoard::new(129),
            black_queens: BitBoard::new(8),
            black_kings: BitBoard::new(16),
        }
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for rank_index in 0..8 {
            write!(
                f,
                "\n--+---+---+---+---+---+---+---+---+\n{} |",
                8 - rank_index
            )?;
            for file_index in 0..8 {
                let square_index = rank_index * 8 + file_index;
                if self.black_bishops.exists(square_index) {
                    write!(f, " b |")?;
                } else if self.black_kings.exists(square_index) {
                    write!(f, " k |")?;
                } else if self.black_rooks.exists(square_index) {
                    write!(f, " r |")?;
                } else if self.black_pawns.exists(square_index) {
                    write!(f, " p |")?;
                } else if self.black_knights.exists(square_index) {
                    write!(f, " n |")?;
                } else if self.black_queens.exists(square_index) {
                    write!(f, " q |")?;
                } else if self.white_bishops.exists(square_index) {
                    write!(f, " B |")?;
                } else if self.white_kings.exists(square_index) {
                    write!(f, " K |")?;
                } else if self.white_rooks.exists(square_index) {
                    write!(f, " R |")?;
                } else if self.white_pawns.exists(square_index) {
                    write!(f, " P |")?;
                } else if self.white_knights.exists(square_index) {
                    write!(f, " N |")?;
                } else if self.white_queens.exists(square_index) {
                    write!(f, " Q |")?;
                } else {
                    write!(f, "   |")?
                }
            }
        }
        writeln!(
            f,
            "\n--+---+---+---+---+---+---+---+---+\n  | a | b | c | d | e | f | g | h |"
        )
    }
}

impl Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.board)
    }
}

impl FromStr for Board {
    type Err = BoardParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut new_board = Board::default();
        let mut rank = 0;
        let ranks = s.split('/');
        for rank_string in ranks {
            if rank > 7 {
                return Err(BoardParseError::TooManyRanks);
            }
            let mut file = 0;
            for cell in rank_string.chars() {
                if file > 7 {
                    return Err(BoardParseError::TooManyFiles);
                }
                let square_index = rank * 8 + file;
                match cell {
                    'r' => new_board.black_rooks.set_index(square_index),
                    'p' => new_board.black_pawns.set_index(square_index),
                    'n' => new_board.black_knights.set_index(square_index),
                    'q' => new_board.black_queens.set_index(square_index),
                    'b' => new_board.black_bishops.set_index(square_index),
                    'k' => new_board.black_kings.set_index(square_index),
                    number if ('1'..='8').contains(&number) => {
                        // we subtract on from whatever number we parse b/c we anyway update the file var at the end of each iteration
                        file += number.to_digit(10).expect(&format!("[error] parsing what is assusmed to be digit failed digit: {number} [this should not happen]")) as usize- 1;
                    }
                    'R' => new_board.white_rooks.set_index(square_index),
                    'P' => new_board.white_pawns.set_index(square_index),
                    'N' => new_board.white_knights.set_index(square_index),
                    'Q' => new_board.white_queens.set_index(square_index),
                    'B' => new_board.white_bishops.set_index(square_index),
                    'K' => new_board.white_kings.set_index(square_index),
                    invalid => return Err(BoardParseError::InvalidCharacter(invalid)),
                }
                file += 1;
            }
            if file < 8 {
                return Err(BoardParseError::TooFewFiles);
            }
            rank += 1;
        }
        if rank < 8 {
            return Err(BoardParseError::TooFewRanks);
        }
        Ok(new_board)
    }
}

#[derive(Debug)]
pub enum BoardParseError {
    TooManyRanks,
    InvalidCharacter(char),
    TooManyFiles,
    TooFewRanks,
    TooFewFiles,
}

#[derive(Default, Debug, PartialEq)]
pub struct GameState {
    board: Board,
}

impl GameState {
    /// generates the needed state for a new game
    /// alternativly use the [FromStr::from_str] method to start from a FEN
    pub fn new_game() -> Self {
        Self {
            board: Board::new_board(),
        }
    }
}

#[derive(Debug)]
pub enum FenParseError {
    BoardParseError(BoardParseError),
    NoSpace,
}

impl FromStr for GameState {
    type Err = FenParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (board_string, _rest) = s.split_once(' ').ok_or(FenParseError::NoSpace)?;
        let board = Board::from_str(board_string).map_err(FenParseError::BoardParseError)?;
        Ok(GameState { board })
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::GameState;
    // parse_* tests are for testing the fen parser (from_str)
    #[test]
    fn parse_start() {
        let start =
            GameState::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 ")
                .unwrap();
        assert_eq!(start, GameState::new_game());
        println!("{}", start)
    }

    #[test]
    fn parse_copmplex() {
        println!(
            "{}",
            GameState::from_str(
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            )
            .unwrap()
        )
    }
}

pub enum Color {
    White = 0,
    Black = 1,
}
