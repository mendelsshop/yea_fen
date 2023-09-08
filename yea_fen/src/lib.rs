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
    black: BitBoard,
    white: BitBoard,
    all: BitBoard,
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
            black: BitBoard::new(65535),
            white: BitBoard::new(18446462598732840960),
            all: BitBoard::new(18446462598732906495),
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
        new_board.white |= new_board.white_bishops
            | new_board.white_kings
            | new_board.white_queens
            | new_board.white_pawns
            | new_board.white_rooks
            | new_board.white_knights;
        new_board.black |= new_board.black_bishops
            | new_board.black_kings
            | new_board.black_queens
            | new_board.black_pawns
            | new_board.black_rooks
            | new_board.black_knights;
        new_board.all |= new_board.black | new_board.white;

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

#[derive(Default, PartialEq)]
pub struct GameState {
    board: Board,
    side_to_move: Color,
    en_pessant: Option<usize>,
    castling_rights: CastlingRights,
}
fn index_to_position(index: usize) -> &'static str {
    SQUARE_COORDINATES[index]
 }
impl Debug for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}\ncolor: {:?}\nen pessent: {}\ncastling rights: {}",
            self.board, self.side_to_move, self.en_pessant.map(index_to_position).unwrap_or("-"), self.castling_rights
        )
    }
}

impl GameState {
    /// generates the needed state for a new game
    /// alternativly use the [FromStr::from_str] method to start from a FEN
    pub fn new_game() -> Self {
        Self {
            board: Board::new_board(),
            castling_rights: CastlingRights::build_all(),
            ..Default::default()
        }
    }
}

#[derive(Default, Debug, PartialEq, Clone, Copy)]
pub struct CastlingRights {
    inner: u8,
}

impl Display for CastlingRights {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let get_right = |index, string| if self.inner as i32 & index != 0{ string } else { "-" }; 
        write!(f, "{}{}{}{}", get_right(wk, "K" ), get_right(wq, "Q" ), get_right(bk, "k"), get_right(bq, "q"))
    }
}

impl CastlingRights {
    pub fn build_king_side_black(mut self) -> Self {
        self.inner |= 4;
        self
    }
    pub fn build_queen_side_black(mut self) -> Self {
        self.inner |= 8;
        self
    }
    pub fn build_king_side_white(mut self) -> Self {
        self.inner |= 1;
        self
    }
    pub fn build_queen_side_white(mut self) -> Self {
        self.inner |= 2;
        self
    }

    pub fn build_all() -> Self {
        Self::default()
            .build_king_side_black()
            .build_king_side_white()
            .build_queen_side_black()
            .build_queen_side_white()
    }

    pub fn king_side_black(&mut self) {
        *self = self.build_king_side_black();
    }
    pub fn queen_side_black(&mut self) {
        *self = self.build_queen_side_black()
    }
    pub fn king_side_white(&mut self) {
        *self = self.build_king_side_white()
    }
    pub fn queen_side_white(&mut self) {
        *self = self.build_queen_side_white()
    }
}

#[derive(Debug)]
pub enum CastlingParseError {
    NonASCCIStr,
    InvalidCharacter(char),
    CastlingRightStringToLong,
}

impl FromStr for CastlingRights {
    type Err = CastlingParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.is_ascii() {
            return Err(CastlingParseError::NonASCCIStr);
        }
        if s.len() > 4 {
            return Err(CastlingParseError::CastlingRightStringToLong);
        }
        let mut ret = CastlingRights::default();
        for c in s.chars() {
            match c {
                'q' => ret.queen_side_black(),
                'k' => ret.king_side_black(),
                'Q' => ret.queen_side_white(),
                'K' => ret.king_side_white(),
                invalid => return Err(CastlingParseError::InvalidCharacter(invalid)),
            }
        }
        return Ok(ret);
    }
}

#[derive(Debug)]
pub enum FenParseError {
    BoardParseError(BoardParseError),
    CastlingParseError(CastlingParseError),
    EnPessantParseError(PositionParseError),
    NoSpace,
    InvalidColorSpecifier,
}

impl FromStr for GameState {
    type Err = FenParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // fen structure board color castling-rights en-pessant
        let (board_string, rest) = s.split_once(' ').ok_or(FenParseError::NoSpace)?;
        let board = Board::from_str(board_string).map_err(FenParseError::BoardParseError)?;
        let (color_string, rest) = rest.split_once(' ').ok_or(FenParseError::NoSpace)?;
        let side_to_move =
            Color::from_str(color_string).map_err(|_| FenParseError::InvalidColorSpecifier)?;
        let (castling_rights_string, rest) = rest.split_once(' ').ok_or(FenParseError::NoSpace)?;
        let castling_rights = if castling_rights_string == "-" {
            CastlingRights::default()
        } else {
            CastlingRights::from_str(castling_rights_string)
                .map_err(FenParseError::CastlingParseError)?
        };
        let (en_pessant_string, rest) = rest.split_once(' ').ok_or(FenParseError::NoSpace)?;
        let en_pessant = if en_pessant_string == "-" {
            None
        } else {
            // parsing en pessant string seems to be broken
            Some(pos_to_index(en_pessant_string).map_err(FenParseError::EnPessantParseError)?)
        };
        Ok(GameState {
            board,
            side_to_move,
            en_pessant,
            castling_rights,
        })
    }
}

#[derive(Debug)]
pub enum PositionParseError {
    PositionStringToLong,
    InvalidRankChar(char),
    InvalidFileChar(char),
}

macro_rules! c_enum {
    ($($name:ident = $number:literal),*) => {$(
        #[allow(non_upper_case_globals)]
        #[allow(unused)]
        pub (crate) const $name: i32 = $number;
    )*};
    (@nonpublic $init:literal$(+$step:literal)*, $first:ident) => {
        #[allow(non_upper_case_globals)]
        #[allow(unused)]
        pub (crate) const $first: i32 = $init$(+$step)*+1;
    };
    (@nonpublic $init:literal$(+$step:literal)*, $first:ident, $($name:ident),*) => {
        #[allow(non_upper_case_globals)]
        #[allow(unused)]
        pub (crate) const $first: i32 = $init$(+$step)*;
        c_enum!{@nonpublic $init$(+$step)*+1, $($name),*}
    };
    ($($name:ident),*) => {
        c_enum!{@nonpublic 0, $($name),*}
    };
    (@struct $structname:ident $($name:ident = $number:literal),*) => {
    struct $structname { $(
        $name: i32
    )*}
    const $structname: $structname =  $structname {
        $($name: $number,)*
    };
    };

    (@enum $enumname:ident $($name:ident = $number:literal),*) => {enum $enumname { $(
        $name = $number,
    )*}};
}

c_enum!(P, N, B, R, Q, K, p, n, b, r, q, k);
c_enum!(
    a8, b8, c8, d8, e8, f8, g8, h8, a7, b7, c7, d7, e7, f7, g7, h7, a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5, a4, b4, c4, d4, e4, f4, g4, h4, a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2, a1, b1, c1, d1, e1, f1, g1, h1, no_sq
);
c_enum!(white, black, both);
c_enum! { wk = 1, wq = 2, bk = 4, bq = 8 }
c_enum! { rook, bishop }
pub const SQUARE_COORDINATES: [&str; 64] = [
    "a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8", "a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
    "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6", "a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
    "a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4", "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
    "a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2", "a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1",
];

// ASCII pieces
const ASCII_PIECES: [char; 12] = ['P', 'N', 'B', 'R', 'Q', 'K', 'p', 'n', 'b', 'r', 'q', 'k'];

// unicode pieces
const UNICODE_PIECES: [char; 12] = [
    '♙', '♘', '♗', '♖', '♕', '♔', '\u{265f}', '♞', '♝', '♜', '♛', '♚',
];

macro_rules! c_init_array {
    ($arr:expr; $([$index:expr] = $value:expr),*) => {
        {
            $($arr[$index as usize] = $value;)*
            $arr
        }
    }
}

const CHAR_PIECES: [i32; 115] = {
    let mut out = [-1; 115];
    out['P' as usize] = P;
    out['N' as usize] = N;
    out['B' as usize] = B;
    out['R' as usize] = R;
    out['Q' as usize] = Q;
    out['K' as usize] = K;
    out['p' as usize] = p;
    out['n' as usize] = n;
    out['b' as usize] = b;
    out['r' as usize] = r;
    out['q' as usize] = q;
    out['k' as usize] = k;
    out
};

pub const fn pos_to_index(pos_str: &str) -> Result<usize, PositionParseError> {
    if !(pos_str.len() == 2) {
        return Err(PositionParseError::PositionStringToLong);
    }
    let individual_parts = pos_str.as_bytes();

    let rank = match individual_parts[0] as char {
        'a'..='h' => individual_parts[0] as usize - 97,
        'A'..='H' => individual_parts[0] as usize - 65,
        invalid_rank => return Err(PositionParseError::InvalidRankChar(invalid_rank)),
    };
    let file = match individual_parts[1] as char {
        // TODO: it might be 8 - ...
        '1'..='8' => individual_parts[1] as usize - 31,
        invalid_file => return Err(PositionParseError::InvalidFileChar(invalid_file)),
    };
    return Ok(rank * 8 + file);
}

#[derive(Default, Debug, PartialEq)]
pub enum Color {
    #[default]
    White = 0,
    Black = 1,
}

impl FromStr for Color {
    // TODO: return string of invalid color instead of nothing upon invalid color specification
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "w" => Ok(Self::White),
            "b" => Ok(Self::Black),
            _ => Err(()),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{BitBoard, GameState, B};
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
            "{:?}",
            GameState::from_str(
                "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1"
            )
            .unwrap()
        )
    }

    #[test]
    fn main() {
        let mut bb = BitBoard::default();
        for i in 48..64 {
            bb.set_index(i)
        }
        println!("{:?}", bb)
    }
}
