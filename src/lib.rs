#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
#![deny(clippy::use_self, rust_2018_idioms)]
#![allow(clippy::must_use_candidate)]
/// stuff that has to do with making chess moves
pub mod moves;
use std::{error::Error, fmt, str::FromStr};

use moves::Game;
macro_rules! impl_default {
    ($type:ty) => {
        impl Default for $type {
            fn default() -> Self {
                Self::new()
            }
        }
    };
}

impl IntoIterator for Board {
    type Item = Row;

    type IntoIter = std::array::IntoIter<Row, 8>;

    fn into_iter(self) -> Self::IntoIter {
        self.board.into_iter()
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A chess piece (does not hold the color of the piece)
pub enum Piece {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}

impl fmt::Display for Piece {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::King => write!(f, "k"),
            Self::Queen => write!(f, "q"),
            Self::Rook => write!(f, "r"),
            Self::Bishop => write!(f, "b"),
            Self::Knight => write!(f, "n"),
            Self::Pawn => write!(f, "p"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// represents a colored thing usally a piece
pub enum Colored<A> {
    Black(A),
    White(A),
}

// implent the From trait to convert Colored<A> to Color
impl<A> From<Colored<A>> for Color {
    fn from(colored: Colored<A>) -> Self {
        match colored {
            Colored::Black(_) => Self::Black,
            Colored::White(_) => Self::White,
        }
    }
}
impl fmt::Display for Colored<Piece> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Black(piece) => match piece {
                // we use the unicode escape sequence for the pawn
                // because some terminals display it as a an emoji
                // thus ruining the board
                Piece::Pawn => write!(f, "\u{265f}"),
                Piece::Knight => write!(f, "♞"),
                Piece::Bishop => write!(f, "♝"),
                Piece::Rook => write!(f, "♜"),
                Piece::Queen => write!(f, "♛"),
                Piece::King => write!(f, "♚"),
            },
            Self::White(piece) => match piece {
                Piece::Pawn => write!(f, "♙"),
                Piece::Knight => write!(f, "♘"),
                Piece::Bishop => write!(f, "♗"),
                Piece::Rook => write!(f, "♖"),
                Piece::Queen => write!(f, "♕"),
                Piece::King => write!(f, "♔"),
            },
        }
    }
}

type Row = [Option<Colored<Piece>>; 8];

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// A chess board
pub struct Board {
    board: [Row; 8],
}

impl Board {
    /// Prints the board from the vantage point of the given color
    /// uses the unicode representation of each chess piece
    pub fn format_with_color(&self, color: Color) -> String {
        let mut ret = String::new();

        let (mut row_num, min_max, board) = if Color::White == color {
            (8u8, 0u8, self.board)
        } else {
            // we need to reverse the letters
            ret.push_str("   | h | g | f | e | d | c | b | a |\n");
            let mut board = self.board;
            board.reverse();
            for row in &mut board.iter_mut() {
                row.reverse();
            }
            (1u8, 9u8, board)
        };
        ret.push_str(&format!(
            " --+---+---+---+---+---+---+---+---+\n {}",
            row_num
        ));
        for row in board {
            if Color::White == color {
                row_num -= 1;
            } else {
                row_num += 1;
            }
            for cell in row {
                ret.push_str(&format!(
                    " | {}",
                    cell.map_or_else(|| String::from(" "), |c| format!("{}", c))
                ));
            }
            ret.push_str(&format!(
                " |\n --+---+---+---+---+---+---+---+---+\n {}",
                if row_num == min_max {
                    String::from(" ")
                } else {
                    format!("{}", row_num)
                }
            ));
        }
        if Color::White == color {
            ret.push_str(" | a | b | c | d | e | f | g | h |\n");
        }
        ret
    }

    /// Creates a new chess board with the deafault chess starting positions
    pub const fn new() -> Self {
        // another way to generate the board at the beginning
        // is to use the from_str method with `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR`
        Self {
            board: [
                [
                    Some(Colored::Black(Piece::Rook)),
                    Some(Colored::Black(Piece::Knight)),
                    Some(Colored::Black(Piece::Bishop)),
                    Some(Colored::Black(Piece::Queen)),
                    Some(Colored::Black(Piece::King)),
                    Some(Colored::Black(Piece::Bishop)),
                    Some(Colored::Black(Piece::Knight)),
                    Some(Colored::Black(Piece::Rook)),
                ],
                [Some(Colored::Black(Piece::Pawn)); 8],
                [None; 8],
                [None; 8],
                [None; 8],
                [None; 8],
                [Some(Colored::White(Piece::Pawn)); 8],
                [
                    Some(Colored::White(Piece::Rook)),
                    Some(Colored::White(Piece::Knight)),
                    Some(Colored::White(Piece::Bishop)),
                    Some(Colored::White(Piece::Queen)),
                    Some(Colored::White(Piece::King)),
                    Some(Colored::White(Piece::Bishop)),
                    Some(Colored::White(Piece::Knight)),
                    Some(Colored::White(Piece::Rook)),
                ],
            ],
        }
    }
}
impl_default!(Board);
impl FromStr for Board {
    type Err = Box<dyn Error>;

    /// Parses a board from the board part of a FEN string
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let board = s.split('/').collect::<Vec<&str>>();
        if board.len() != 8 {
            return Err("Invalid board length")?;
        }
        Ok(Self {
            board: board
                .iter()
                .map(|row| parse_fen_row(row))
                .collect::<Result<Vec<Row>, _>>()?
                .try_into()
                .map_err(|_| "number of cloumns to less than or greater than 8")?,
        })
    }
}

impl fmt::Display for Board {
    /// formats the board in the standard chess format from white's perspective
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_with_color(Color::White))
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// The Colors of chess used to determine the current player among other things
pub enum Color {
    Black,
    White,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Castling Options
pub enum Castling {
    None,
    KingSide,
    QueenSide,
    Both,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// The state of a game
/// icludes the board, current player, number of moves taken, and other FEN related metadata
pub struct GameState {
    /// part of the fen string that holds the state of the board
    board: Board,
    /// the color of the current player (black or white)
    active_color: Color,
    /// starts at 1
    /// increments after both white and black go
    full_move_clock: usize,
    /// number of moves since the last pawn move or piece kill
    /// increments after either white or black go
    half_move_clock: usize,
    /// castling moves available
    castling_moves: CastlingOptions,
    /// En_passant moves available, vec of row and column of En_passant(s)
    en_passant: Option<Pos>,
}

impl From<Game> for GameState {
    fn from(value: Game) -> Self {
        Self {
            board: value.board,
            active_color: value.active_color,
            full_move_clock: value.full_move_clock,
            half_move_clock: value.half_move_clock,
            castling_moves: value.castling_moves,
            en_passant: value.en_passant,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
/// The castling option for both player
pub struct CastlingOptions {
    black: Castling,
    white: Castling,
}

impl CastlingOptions {
    /// the default castling options (`Castling::Both` for both players)
    pub const fn new() -> Self {
        Self {
            black: Castling::Both,
            white: Castling::Both,
        }
    }
}
impl_default!(CastlingOptions);

impl GameState {
    /// returns the `GameState` for a new game, with Pieces in the start positions
    pub const fn new() -> Self {
        Self {
            board: Board::new(),
            active_color: Color::White,
            full_move_clock: 1,
            half_move_clock: 0,
            castling_moves: CastlingOptions::new(),
            en_passant: None,
        }
    }

    /// Returns a refernce to the current board
    pub const fn get_board(&self) -> &Board {
        &self.board
    }

    /// Returns the current player's color
    pub const fn get_active_color(&self) -> Color {
        self.active_color
    }

    /// Returns the number of moves taken
    pub const fn get_full_move_clock(&self) -> usize {
        self.full_move_clock
    }

    /// Returns the number of moves since the last pawn move or piece kill
    pub const fn get_half_move_clock(&self) -> usize {
        self.half_move_clock
    }

    /// Returns the castling options for both players
    pub const fn get_castling_moves(&self) -> &CastlingOptions {
        &self.castling_moves
    }

    /// Returns the en passant options
    pub const fn get_en_passant(&self) -> Option<Pos> {
        self.en_passant
    }
}
impl_default!(GameState);
impl FromStr for GameState {
    type Err = Box<dyn Error>;

    /// Parses a FEN string into a `GameState`
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (board, config) = s
            .split_once(' ')
            .map_or_else(|| Err(""), |(board, config)| Ok((board, config)))?;
        let board = Board::from_str(board)?;
        let mut config = config.split(' ');
        let active_color = match config.next() {
            Some("w") => Color::White,
            Some("b") => Color::Black,
            _ => return Err("invalid color to go")?,
        };
        let mut castling_moves = CastlingOptions {
            black: Castling::None,
            white: Castling::None,
        };
        match config.next() {
            Some("-") => {}

            Some(castlings) => {
                for castling in castlings.chars() {
                    match castling {
                        'K' => {
                            castling_moves.white = if castling_moves.white == Castling::QueenSide
                                || castling_moves.white == Castling::Both
                            {
                                Castling::Both
                            } else {
                                Castling::KingSide
                            }
                        }
                        'Q' => {
                            castling_moves.white = if castling_moves.white == Castling::KingSide
                                || castling_moves.white == Castling::Both
                            {
                                Castling::Both
                            } else {
                                Castling::QueenSide
                            }
                        }
                        'k' => {
                            castling_moves.black = if castling_moves.black == Castling::QueenSide
                                || castling_moves.black == Castling::Both
                            {
                                Castling::Both
                            } else {
                                Castling::KingSide
                            }
                        }
                        'q' => {
                            castling_moves.black = if castling_moves.black == Castling::KingSide
                                || castling_moves.black == Castling::Both
                            {
                                Castling::Both
                            } else {
                                Castling::QueenSide
                            }
                        }
                        _ => return Err("invalid castling move")?,
                    }
                }
            }

            None => return Err("invalid castling moves")?,
        };
        let en_passant = match config.next() {
            Some("-") => None,
            Some(pos) => Pos::from_str(pos).ok(),
            None => return Err("invalid en passant")?,
        };
        let half_move_clock = match config.next() {
            Some(num) => num.parse()?,
            None => return Err("invalid half move clock")?,
        };
        let full_move_clock = match config.next() {
            Some(num) => num.parse()?,
            None => return Err("invalid full move clock")?,
        };

        Ok(Self {
            board,
            active_color,
            full_move_clock,
            half_move_clock,
            castling_moves,
            en_passant,
        })
    }
}

impl fmt::Display for GameState {
    /// returns the game state as a fen encoded string
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (idx, row) in self.board.into_iter().enumerate() {
            let mut blank_count = 0;
            for (idx, piece) in row.iter().enumerate() {
                match piece {
                    Some(p) => {
                        if blank_count > 0 {
                            write!(f, "{}", blank_count)?;
                        }

                        blank_count = 0;
                        match p {
                            Colored::White(p) => write!(f, "{}", p.to_string().to_uppercase())?,
                            Colored::Black(p) => write!(f, "{}", p)?,
                        }
                    }
                    None => {
                        blank_count += 1;
                        if idx > 6 {
                            write!(f, "{}", blank_count)?;
                        }
                    }
                }
            }
            if idx != 7 {
                write!(f, "/")?;
            }
        }

        match self.active_color {
            Color::Black => write!(f, " b")?,
            Color::White => write!(f, " w")?,
        }
        match self.castling_moves {
            CastlingOptions {
                black: Castling::None,
                white: Castling::None,
            } => {
                write!(f, " -")?;
            }
            CastlingOptions {
                black: Castling::Both,
                white: Castling::Both,
            } => {
                write!(f, " KQkq")?;
            }
            CastlingOptions {
                black: Castling::Both,
                white: Castling::KingSide,
            } => {
                write!(f, " Kkq")?;
            }
            CastlingOptions {
                black: Castling::Both,
                white: Castling::QueenSide,
            } => {
                write!(f, " Qkq")?;
            }
            CastlingOptions {
                black: Castling::KingSide,
                white: Castling::Both,
            } => {
                write!(f, " KQk")?;
            }
            CastlingOptions {
                black: Castling::KingSide,
                white: Castling::KingSide,
            } => {
                write!(f, " Kk")?;
            }
            CastlingOptions {
                black: Castling::KingSide,
                white: Castling::QueenSide,
            } => {
                write!(f, " QK")?;
            }
            CastlingOptions {
                black: Castling::QueenSide,
                white: Castling::Both,
            } => {
                write!(f, " KQq")?;
            }
            CastlingOptions {
                black: Castling::QueenSide,
                white: Castling::KingSide,
            } => {
                write!(f, " Kq")?;
            }
            CastlingOptions {
                black: Castling::QueenSide,
                white: Castling::QueenSide,
            } => {
                write!(f, " Qq")?;
            }
            CastlingOptions {
                black: Castling::None,
                white: Castling::Both,
            } => {
                write!(f, " KQ")?;
            }
            CastlingOptions {
                black: Castling::None,
                white: Castling::KingSide,
            } => {
                write!(f, " K")?;
            }
            CastlingOptions {
                black: Castling::None,
                white: Castling::QueenSide,
            } => {
                write!(f, " Q")?;
            }
            CastlingOptions {
                black: Castling::Both,
                white: Castling::None,
            } => {
                write!(f, " kq")?;
            }
            CastlingOptions {
                black: Castling::KingSide,
                white: Castling::None,
            } => {
                write!(f, " k")?;
            }
            CastlingOptions {
                black: Castling::QueenSide,
                white: Castling::None,
            } => {
                write!(f, " q")?;
            }
        }

        match self.en_passant {
            Some(pos) => write!(f, " {}", pos)?,
            None => write!(f, " -")?,
        }

        write!(f, " {}", self.half_move_clock)?;
        write!(f, " {}", self.full_move_clock)?;

        Ok(())
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// A position on the board
pub struct Pos {
    row: u8,
    column: u8,
}

impl Pos {
    /// creates a new position from a row and column
    /// # Panics
    /// Panics if row or column is not between 1 and 8
    pub fn new(row: u8, column: u8) -> Self {
        assert!(
            !(!(1..=8).contains(&row) || !(1..=8).contains(&column)),
            "invalid row or column"
        );
        Self { row, column }
    }

    /// returns the row of the position
    pub const fn get_row(&self) -> u8 {
        self.row
    }

    /// returns the column of the position
    pub const fn get_column(&self) -> u8 {
        self.column
    }

    /// returns the column as a char ie 0 is a, 1 is b, etc
    pub const fn get_column_char(self) -> char {
        (self.column + 96) as char
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.get_column_char(), self.row)
    }
}

impl FromStr for Pos {
    type Err = Box<dyn Error>;
    /// creates a new position from a string ie "a1" becomes Pos { row: 1, column: 1 }
    /// Y can be a-h, and X can 1-8
    #[allow(clippy::cast_possible_truncation)]
    // when wa cast u32 to u8, we know that the u32 is less than or equal to 8
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ret = Self { row: 0, column: 0 };
        if s.chars().count() != 2 {
            return Err("invalid length for position string")?;
        }
        let mut chars = s.chars();
        let column = chars.next().unwrap();
        let row = chars.next().unwrap();
        // we need to get the column number from the letter
        // a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8
        // so we subtract 96 from the ascii value of the letter
        // this is because the ascii value of a is 97
        ret.column = (column as u32 - 96) as u8;

        ret.row = row.to_string().parse()?;
        Ok(ret)
    }
}

fn parse_fen_row(row: &str) -> Result<Row, Box<dyn Error>> {
    let mut piece_count = 0;
    let cells = row.chars();
    let mut ret = [None; 8];
    let mut error = String::new();
    if cells.clone().count() > 8 {
        return Err("row is too long")?;
    }
    cells.for_each(|char| {
        if piece_count > 8 {
            return;
        }
        match char {
            'P' => ret[piece_count] = Some(Colored::White(Piece::Pawn)),
            'p' => ret[piece_count] = Some(Colored::Black(Piece::Pawn)),
            'N' => ret[piece_count] = Some(Colored::White(Piece::Knight)),
            'n' => ret[piece_count] = Some(Colored::Black(Piece::Knight)),
            'B' => ret[piece_count] = Some(Colored::White(Piece::Bishop)),
            'b' => ret[piece_count] = Some(Colored::Black(Piece::Bishop)),
            'R' => ret[piece_count] = Some(Colored::White(Piece::Rook)),
            'r' => ret[piece_count] = Some(Colored::Black(Piece::Rook)),
            'Q' => ret[piece_count] = Some(Colored::White(Piece::Queen)),
            'q' => ret[piece_count] = Some(Colored::Black(Piece::Queen)),
            'K' => ret[piece_count] = Some(Colored::White(Piece::King)),
            'k' => ret[piece_count] = Some(Colored::Black(Piece::King)),
            num if num.is_ascii_digit() => {
                match num.to_digit(10) {
                    Some(digit) => {
                        if (1..=8).contains(&digit) {
                            piece_count += digit as usize;
                        } else {
                            error = format!("{} is not a valid fen board character", num);
                        }
                    }
                    None => error = format!("{} is not a valid fen board character", num),
                }
                return;
            }
            other => error = format!("{} is not a valid fen board character", other),
        }
        piece_count += 1;
    });
    if !error.is_empty() {
        return Err(error)?;
    }

    Ok(ret)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_start() {
        let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_ok());

        let gamestate = gamestate.unwrap();
        println!("{}", gamestate.board);
        assert_eq!(GameState::new(), gamestate);
        println!("{}", GameState::new().board.format_with_color(Color::Black));
    }

    #[test]
    fn invalid_board_length() {
        let fen = "8/8/8/8 ";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_err());
        assert_eq!(gamestate.unwrap_err().to_string(), "Invalid board length");
    }

    #[test]
    fn invalid_board_character() {
        let fen = "cnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_err());
        assert!(gamestate
            .unwrap_err()
            .to_string()
            .contains("is not a valid fen board character"));
    }

    #[test]
    fn parse_mid_game() {
        let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_ok());
        let gamestate = gamestate.unwrap();
        // println!("{}", gamestate.board);
        println!("{}", gamestate.board.format_with_color(Color::Black));
        println!("{}", gamestate.board.format_with_color(Color::White));
    }
    #[test]
    fn parse_with_en_pessant() {
        let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_ok());
        let gamestate = gamestate.unwrap();
        println!("{:?}", gamestate);
        println!("{}", gamestate.board.format_with_color(Color::Black));
        println!("{}", gamestate.board.format_with_color(Color::White));
    }

    #[test]
    fn test_pos() {
        let a1 = Pos::from_str("a1").unwrap();
        assert_eq!(a1, Pos::new(1, 1));
        assert_eq!(a1.to_string(), "a1");
    }

    #[test]
    fn to_fen() {
        let gamestate = GameState::new();
        println!("{}", gamestate);
        assert_eq!(
            gamestate.to_string(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        );

        let in_game =
            GameState::from_str("rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2")
                .unwrap();
        println!("{}", in_game);
        assert_eq!(
            in_game.to_string(),
            "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
        );
    }
}
