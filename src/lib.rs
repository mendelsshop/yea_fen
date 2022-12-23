#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
#![deny(clippy::use_self, rust_2018_idioms)]

use std::{error::Error, fmt, str::FromStr};

pub trait PieceMove {}
#[derive(Copy, Clone, Debug)]
pub enum Piece {
    King,
    Queen,
    Rook,
    Bishop,
    Knight,
    Pawn,
}
#[derive(Copy, Clone, Debug)]
pub enum Colored<A> {
    Black(A),
    White(A),
    None,
}

impl fmt::Display for Colored<Piece> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Black(piece) => match piece {
                Piece::Pawn => write!(f, "♙"),
                Piece::Knight => write!(f, "♘"),
                Piece::Bishop => write!(f, "♗"),
                Piece::Rook => write!(f, "♖"),
                Piece::Queen => write!(f, "♕"),
                Piece::King => write!(f, "♔"),
            },
            Self::White(piece) => {
                match piece {
                    // we use the unicode escape sequence for the pawn
                    // because some terminals display it as a an emoji
                    // thus ruining the board
                    Piece::Pawn => write!(f, "\u{265f}"),
                    Piece::Knight => write!(f, "♞"),
                    Piece::Bishop => write!(f, "♝"),
                    Piece::Rook => write!(f, "♜"),
                    Piece::Queen => write!(f, "♛"),
                    Piece::King => write!(f, "♚"),
                }
            }
            Self::None => {
                write!(f, " ")
            }
        }
    }
}

type Row = [Colored<Piece>; 8];

#[derive(Copy, Clone, Debug)]
pub struct Board {
    board: [Row; 8],
}

impl Board {
    pub fn format_with_color(&self, color: Color) -> String {
        let mut ret = String::new();

        let (mut row_num, min_max, board) = if let Color::White = color {
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
            if let Color::White = color {
                row_num -= 1;
            } else {
                row_num += 1;
            }
            for cell in row {
                ret.push_str(&format!(" | {}", cell));
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
        if let Color::White = color {
            ret.push_str(" | a | b | c | d | e | f | g | h |\n");
        }
        ret
    }
}

impl FromStr for Board {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let board = s.split('/').collect::<Vec<&str>>();
        if board.len() != 8 {
            return Err("Invalid board length")?;
        }
        let board_rows = board.iter().map(|row| parse_fen_row(row));
        if let Some(error) = board_rows.clone().find(Result::is_err) {
            return Err(error.expect_err("could not retrive error message"))?;
        }
        Ok(Self {
            board: board_rows
                .filter_map(Result::ok)
                .collect::<Vec<Row>>()
                .try_into()
                .map_err(|_| "number of cloumns to less than or greatee than 8")?,
        })
    }
}

impl fmt::Display for Board {
    /// formats the board in the standard chess format from white's perspective
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_with_color(Color::White))
    }
}
#[derive(Copy, Clone, Debug)]
pub enum Color {
    Black,
    White,
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Castling {
    None,
    KingSide,
    QueenSide,
    Both,
}
#[derive(Clone, Debug)]
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
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct CastlingOptions {
    black: Castling,
    white: Castling,
}

impl FromStr for GameState {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (board, config) = s
            .split_once(' ')
            .map_or_else(|| Err(""), |(board, config)| Ok((board, config)))?;
        let board = Board::from_str(board)?;
        println!("{}", config);
        let config: [&str; 5] = config
            .split(' ')
            .collect::<Vec<&str>>()
            .try_into()
            .map_err(|_| "invalid config")?;
        let active_color = match config[0] {
            "w" => Color::White,
            "b" => Color::Black,
            _ => return Err("invalid color to go")?,
        };
        let mut castling_moves = CastlingOptions {
            black: Castling::None,
            white: Castling::None,
        };
        match config[1] {
            "-" => {}
            castlings => {
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
        };
        let en_passant = match config[2] {
            "-" => None,
            pos => Pos::from_str(pos).ok(),
        };
        let half_move_clock = config[3].parse()?;
        let full_move_clock = config[4].parse()?;

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
#[derive(Copy, Clone, Debug)]
pub struct Pos {
    x: u8,
    y: u8,
}

impl FromStr for Pos {
    type Err = Box<dyn Error>;
    /// Y can be a-h, and X can 1-8
    #[allow(clippy::cast_possible_truncation)]
    // when wa cast u32 to u8, we know that the u32 is less than or equal to 8
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ret = Self { x: 0, y: 0 };
        if s.chars().count() != 2 {
            return Err("invalid length for position string")?;
        }
        for char in s.chars() {
            match char {
                char if char.is_ascii_alphabetic() => {
                    if ('a'..='h').contains(&char) {
                        ret.x = turn_char_to_u8(char)?;
                    } else {
                        return Err(format!("invalid postion string {}", char))?;
                    }
                }
                char if char.is_ascii_digit() => {
                    let char = char
                        .to_digit(10)
                        .map_or_else(|| Err("couldnt parse digit")?, Ok::<u32, Self::Err>)?;
                    if (1..=8).contains(&char) {
                        ret.y = char as u8;
                    } else {
                        return Err(format!("invalid postion string {}", char))?;
                    }
                }
                char => {
                    return Err(format!("invalid postion string {}", char))?;
                }
            }
        }
        Ok(ret)
    }
}

fn turn_char_to_u8(charr: char) -> Result<u8, Box<dyn Error>> {
    match charr {
        'a' => Ok(1),
        'b' => Ok(2),
        'c' => Ok(3),
        'd' => Ok(4),
        'e' => Ok(5),
        'f' => Ok(6),
        'g' => Ok(7),
        'h' => Ok(8),
        other => Err(format!("invalid char {}", other))?,
    }
}

fn parse_fen_row(row: &str) -> Result<Row, Box<dyn Error>> {
    let mut piece_count = 0;
    let cells = row.chars();
    let mut ret = [Colored::None; 8];
    let mut error = String::new();
    if cells.clone().count() > 8 {
        return Err("row is too long")?;
    }
    cells.for_each(|char| {
        if piece_count > 8 {
            return;
        }
        match char {
            'P' => ret[piece_count] = Colored::White(Piece::Pawn),
            'p' => ret[piece_count] = Colored::Black(Piece::Pawn),
            'N' => ret[piece_count] = Colored::White(Piece::Knight),
            'n' => ret[piece_count] = Colored::Black(Piece::Knight),
            'B' => ret[piece_count] = Colored::White(Piece::Bishop),
            'b' => ret[piece_count] = Colored::Black(Piece::Bishop),
            'R' => ret[piece_count] = Colored::White(Piece::Rook),
            'r' => ret[piece_count] = Colored::Black(Piece::Rook),
            'Q' => ret[piece_count] = Colored::White(Piece::Queen),
            'q' => ret[piece_count] = Colored::Black(Piece::Queen),
            'K' => ret[piece_count] = Colored::White(Piece::King),
            'k' => ret[piece_count] = Colored::Black(Piece::King),
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
        println!("{}", gamestate.board.format_with_color(Color::Black));
    }

    #[test]
    fn invalid_board_length() {
        let fen = "8/8/8/8 ";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_err());
        assert_eq!(gamestate.unwrap_err().to_string(), "Invalid board length")
    }

    #[test]
    fn invalid_board_character() {
        let fen = "cnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let gamestate = GameState::from_str(fen);
        assert!(gamestate.is_err());
        assert!(gamestate
            .unwrap_err()
            .to_string()
            .contains("is not a valid fen board character"))
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
}
