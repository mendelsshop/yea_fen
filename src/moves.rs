use std::{
    collections::{HashMap, HashSet},
    fmt,
    ops::Range,
};

use crate::{Board, CastlingOptions, Color, Colored, GameState, Piece, Pos};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Represents a move which can be a capture of an enemy piece or a move to an empty cell
/// Note that the generic POS will always be of type `Pos`
pub enum MoveType<POS, PIECE> {
    Capture((Pos, PIECE), (POS, PIECE)),
    Move((Pos, PIECE), POS),
    CapturePromotion((Pos, PIECE), (POS, PIECE)),
    MovePromotion((Pos, PIECE), POS),
}

impl fmt::Display for MoveType<Pos, Colored<Piece>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Capture((pos_o, piece_m), (pos_n, piece))
            | Self::CapturePromotion((pos_o, piece_m), (pos_n, piece)) => write!(
                f,
                "Capture {} at {} from {} at {}",
                piece, pos_o, piece_m, pos_n
            ),
            Self::Move((pos_o, piece_m), pos_n) | Self::MovePromotion((pos_o, piece_m), pos_n) => {
                write!(f, "Move to {} from {} at {}", pos_n, piece_m, pos_o)
            }
        }
    }
}

macro_rules! check_insert {
    ($itt:ident, $pos:ident, $piece_color:ident, $ret:ident, $pos_o:expr) => {
        match $itt.board.get_cell($pos) {
            None | Some(None) => {
                match $pos_o {
                    // if the piece is a pawn and its at the end of the board, then it can be promoted
                    // for a white pawn, the row is 8, for a black pawn, the row is 1
                    (Pos { row: 7, .. }, Colored::White(Piece::Pawn)) => {
                        $ret.insert(MoveType::MovePromotion($pos_o, $pos));
                    }
                    (Pos { row: 2, .. }, Colored::Black(Piece::Pawn)) => {
                        $ret.insert(MoveType::MovePromotion($pos_o, $pos));
                    }
                    _ => {
                        $ret.insert(MoveType::Move($pos_o, $pos));
                    }
                }
                // $ret.insert(MoveType::Move($pos_o, $pos));
            }
            Some(Some(piece_c)) => {
                if !(Color::from(*piece_c) == $piece_color) {
                    // $ret.insert(MoveType::Capture($pos_o, ($pos, *piece_c)));
                    match $pos_o {
                        // if the piece is a pawn and its at the end of the board, then it can be promoted
                        // for a white pawn, the row is 8, for a black pawn, the row is 1
                        (Pos { row: 7, .. }, Colored::White(Piece::Pawn)) => {
                            $ret.insert(MoveType::CapturePromotion($pos_o, ($pos, *piece_c)));
                        }
                        (Pos { row: 2, .. }, Colored::Black(Piece::Pawn)) => {
                            $ret.insert(MoveType::CapturePromotion($pos_o, ($pos, *piece_c)));
                        }
                        _ => {
                            $ret.insert(MoveType::Capture($pos_o, ($pos, *piece_c)));
                        }
                    }
                }

                break;
            }
        }
    };
}
impl Pos {
    #[allow(clippy::cast_sign_loss)]
    #[allow(clippy::cast_possible_wrap)]
    pub(crate) fn add_row_and_column(self, row: i8, column: i8) -> Option<Self> {
        let row = self.row as i8 + row;
        let column = self.column as i8 + column;
        if !(1..=8).contains(&row) || !(1..=8).contains(&column) {
            None
        } else {
            Some(Self {
                row: row as u8,
                column: column as u8,
            })
        }
    }
}
pub struct Game {
    /// part of the fen string that holds the state of the board
    pub(crate) board: Board,
    /// the color of the current player (black or white)
    pub(crate) active_color: Color,
    /// starts at 1
    /// increments after both white and black go
    pub(crate) full_move_clock: usize,
    /// number of moves since the last pawn move or piece kill
    /// increments after either white or black go
    pub(crate) half_move_clock: usize,
    /// castling moves available
    pub(crate) castling_moves: CastlingOptions,
    /// En_passant moves available, vec of row and column of En_passant(s)
    pub(crate) en_passant: Option<Pos>,

    pub(crate) moves: Vec<(MoveType<Pos, Colored<Piece>>, Option<Colored<Piece>>)>,
    pub(crate) result: GameResult,
}

pub enum GameResult {
    CheckMate(Color),
    StaleMate,
    Draw,
    InProgress,
}

impl From<GameState> for Game {
    fn from(value: GameState) -> Self {
        // TODO: check if the game is over using get_valid_moves
        Self {
            board: value.board,
            active_color: value.active_color,
            full_move_clock: value.full_move_clock,
            half_move_clock: value.half_move_clock,
            castling_moves: value.castling_moves,
            en_passant: value.en_passant,
            moves: vec![],
            result: GameResult::InProgress,
        }
    }
}
impl Game {
    /// This will return the possible moves for a given piece (which is specified by its position)
    /// it will no return moves that are blocked by other pieces
    /// but it does not account for checks so it can return illegal moves
    pub fn get_piece_moves(&self, pos: Pos) -> HashSet<MoveType<Pos, Colored<Piece>>> {
        match self.board.get_cell(pos) {
            None | Some(None) => HashSet::new(),
            Some(Some(piece)) => {
                let color = Color::from(*piece);
                let blank_piece = match piece {
                    Colored::Black(piece) | Colored::White(piece) => piece,
                };
                let mut ret = HashSet::new();
                match blank_piece {
                    Piece::King => {
                        self.horiz_and_vert(pos, color, *piece, 1, &mut ret);
                        self.diag(pos, color, *piece, 1, &mut ret);
                    }
                    Piece::Queen => {
                        self.horiz_and_vert(pos, color, *piece, 7, &mut ret);
                        self.diag(pos, color, *piece, 7, &mut ret);
                    }
                    Piece::Rook => {
                        self.horiz_and_vert(pos, color, *piece, 7, &mut ret);
                    }
                    Piece::Bishop => self.diag(pos, color, *piece, 7, &mut ret),
                    Piece::Knight => {
                        for x in -2..3i8 {
                            if x == 0 {
                                continue;
                            }
                            if let Some(pos_n) = pos.add_row_and_column(x, 3 - x.abs()) {
                                match self.board.get_cell(pos_n) {
                                    None | Some(None) => {
                                        ret.insert(MoveType::Move((pos, *piece), pos_n));
                                    }
                                    Some(Some(piece_c)) => {
                                        if !(Color::from(*piece) == color) {
                                            ret.insert(MoveType::Capture(
                                                (pos, *piece),
                                                (pos_n, *piece_c),
                                            ));
                                        }
                                    }
                                }
                            }
                            if let Some(pos_n) = pos.add_row_and_column(x, -3 + x.abs()) {
                                match self.board.get_cell(pos_n) {
                                    None | Some(None) => {
                                        ret.insert(MoveType::Move((pos, *piece), pos_n));
                                    }
                                    Some(Some(piece_c)) => {
                                        if !(Color::from(*piece) == color) {
                                            ret.insert(MoveType::Capture(
                                                (pos, *piece),
                                                (pos_n, *piece_c),
                                            ));
                                        }
                                    }
                                }
                            }
                        }
                    }
                    Piece::Pawn => {
                        // we need to check color and row if black and 7 then 2
                        // if white and 2 use 2 otherwise 1
                        // check if diagonal foward left & right is a capture of enemy if so add to ret
                        // we need to check that the vertical moves are not captures
                        let mut possible = HashSet::new();
                        match (color, pos.row) {
                            (Color::White, 2) => {
                                self.vert_half(pos, color, *piece, 2, &mut possible);
                            }
                            (Color::Black, 7) => {
                                self.vert_half(pos, color, *piece, -2, &mut possible);
                            }
                            (Color::Black, _) => {
                                self.vert_half(pos, color, *piece, -1, &mut possible);
                            }
                            (Color::White, _) => {
                                self.vert_half(pos, color, *piece, 1, &mut possible);
                            }
                        }
                        ret.extend(possible.iter().filter(|thing| match **thing {
                            MoveType::Capture(..) | MoveType::CapturePromotion(..) => false,
                            MoveType::Move(..) | MoveType::MovePromotion(..) => true,
                        }));
                        let mut possible = HashSet::new();
                        match color {
                            Color::Black => {
                                self.diag_left(pos, color, *piece, -1, &mut possible);
                                self.diag_right(pos, color, *piece, -1, &mut possible);
                            }
                            Color::White => {
                                self.diag_left(pos, color, *piece, 1, &mut possible);
                                self.diag_right(pos, color, *piece, 1, &mut possible);
                            }
                        }
                        // validate the the possible values are captures and not just plain moves
                        ret.extend(possible.iter().filter(|thing| match **thing {
                            MoveType::Capture(..) | MoveType::CapturePromotion(..) => true,
                            MoveType::Move(..) | MoveType::MovePromotion(..) => false,
                        }));
                    }
                }
                ret
            }
        }
    }
    // if the length is negative we need to reverse so that it goes 0 -1 -2 .. n and not n n-1 .. 0
    fn hoirz_half(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(0, i) {
                check_insert!(self, pos_n, piece_color, ret, (pos, piece));
            }
        }
    }

    fn vert_half(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(i, 0) {
                check_insert!(self, pos_n, piece_color, ret, (pos, piece));
            }
        }
    }

    fn diag_right(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(i, i) {
                check_insert!(self, pos_n, piece_color, ret, (pos, piece));
            }
        }
    }

    fn diag_left(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(i, -i) {
                check_insert!(self, pos_n, piece_color, ret, (pos, piece));
            }
        }
    }

    fn horiz_and_vert(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
    ) {
        // iterate over lower and upper each twice doing pos.add_row_column(x, 0) in the first iteration pos.add_row_column(0, x) in the second one
        // at eac check we need to check if a piece is there and see what color it is if its the same color as the current piece. then we dont add the pos to the hashset
        // otherwise we do in bothe cases where the is already a piece there we break from the loop
        self.hoirz_half(pos, piece_color, piece, length, ret);
        self.vert_half(pos, piece_color, piece, length, ret);
        self.hoirz_half(pos, piece_color, piece, -length, ret);
        self.vert_half(pos, piece_color, piece, -length, ret);
    }

    fn diag(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
    ) {
        // samme thing as horiz_and_vert but using pos.add_row_and_column(x, x) and pos.add_row_and_column(x, -x)
        self.diag_left(pos, piece_color, piece, length, ret);
        self.diag_right(pos, piece_color, piece, length, ret);
        self.diag_left(pos, piece_color, piece, -length, ret);
        self.diag_right(pos, piece_color, piece, -length, ret);
    }

    #[allow(clippy::cast_possible_truncation)]
    // we will never truncate a value becuse the board does not go past 8
    /// gets all posible moves for a given color/player
    /// the moves can be invalid in a way such that the king can still be in check
    pub fn get_all_moves(
        &self,
        player: Color,
    ) -> HashMap<(Pos, Colored<Piece>), HashSet<MoveType<Pos, Colored<Piece>>>> {
        // gets all moves for a given color by iterating through the board and finding pieces of the color and generating the moves for that piece using self.get_piece_moves()
        self.board.into_iter()
            .rev()
            .enumerate()
            .flat_map(|(row_idx, row)| {
                row.iter()
                    .enumerate()
                    .filter_map(|(column_idx, cell)| {
                        // println!("{:?} {:?}", cell, (row_idx + 1, column_idx + 1));
                        let cell = (*cell)?;
                        if Color::from(cell) == player {
                            let pos = Pos::new(row_idx as u8 + 1, column_idx as u8 + 1);
                            let moves = self.get_piece_moves(pos);
                            if moves.is_empty() {
                                None
                            } else {
                                Some(((pos, cell), moves))
                            }
                        } else {
                            None
                        }
                    })
                    .collect::<HashMap<(Pos, Colored<Piece>), HashSet<MoveType<Pos, Colored<Piece>>>>>()
            })
            .collect::<HashMap<(Pos, Colored<Piece>), HashSet<MoveType<Pos, Colored<Piece>>>>>()
        // HashMap::new()
    }

    pub fn get_all_valid_moves(
        &mut self,
        player: Color,
    ) -> HashMap<(Pos, Colored<Piece>), HashSet<MoveType<Pos, Colored<Piece>>>> {
        let mut ret = self.get_all_moves(player);
        ret = ret
            .iter()
            .filter_map(
                |((pos, piece), moves)| -> Option<(
                    (Pos, Colored<Piece>),
                    HashSet<MoveType<Pos, Colored<Piece>>>,
                )> {
                    // iterate over moves
                    // need a way to do a move
                    // do move
                    // generate enemy moves
                    // find any enemy moves where its a capture and the cell its capturing is allay king
                    let moves = moves
                        .iter()
                        .filter(|pos| {
                            // check if its a pawn promotion
                            let piece = match piece {
                                Colored::White(piece) | Colored::Black(piece) => piece,
                            };
                            if piece == &Piece::Pawn {
                                if let MoveType::MovePromotion(_, _)
                                | MoveType::CapturePromotion(_, _) = pos
                                {
                                    self.do_move(**pos, Some(Colored::White(Piece::Queen)));
                                } else {
                                    self.do_move(**pos, None);
                                }
                            } else {
                                self.do_move(**pos, None);
                            }

                            let openent = match player {
                                Color::White => Color::Black,
                                Color::Black => Color::White,
                            };
                            let enemy_moves = self.get_all_moves(openent);
                            self.undo_move();
                            !enemy_moves.iter().any(|(_, moves)| {
                                moves.iter().any(|move_type| {
                                    match move_type {
                                        MoveType::Capture((_, _), (_, piece))
                                        | MoveType::CapturePromotion((_, _), (_, piece)) => {
                                            if Color::from(*piece) == player {
                                                let piece = match piece {
                                                    Colored::White(piece)
                                                    | Colored::Black(piece) => piece,
                                                };
                                                piece == &Piece::King
                                                // piece == &Colored::White(Piece::King) || piece == &Colored::Black(Piece::King)
                                            } else {
                                                false
                                            }
                                        }
                                        MoveType::Move(..) | MoveType::MovePromotion(..) => {
                                            // piece == &Colored::White(Piece::King) || piece == &Colored::Black(Piece::King)
                                            false
                                        }
                                    }
                                })
                            })
                        })
                        .copied()
                        .collect::<HashSet<MoveType<Pos, Colored<Piece>>>>();
                    // todo!()
                    if moves.is_empty() {
                        None
                    } else {
                        Some(((*pos, *piece), moves))
                    }
                    // self.do_move(start, end)
                },
            )
            .collect();
        if ret.is_empty() {
            // check if king is in check
            // if king is in check then the game is over
            // if king is not in check then the game is a draw
        }

        // one way to figure out if a move is legal
        // is to play a move and see if any of the oppenets moves is to capture the king
        // by iterastion over ret doing the move and then generating all oppenets moves using self.get_all_moves
        //
        ret
    }

    pub fn do_move(
        &mut self,
        r#move: MoveType<Pos, Colored<Piece>>,
        promotion: Option<Colored<Piece>>,
    ) {
        match r#move {
            MoveType::Capture((pos, piece), (pos_c, _)) | MoveType::Move((pos, piece), pos_c) => {
                self.board.insert(None, pos);
                self.board.insert(Some(piece), pos_c);
            }
            MoveType::CapturePromotion((pos, _), (pos_c, _))
            | MoveType::MovePromotion((pos, _), pos_c) => {
                if let Some(promotion) = promotion {
                    match promotion {
                        Colored::White(piece) | Colored::Black(piece) => {
                            if piece != Piece::Queen
                                && piece != Piece::Rook
                                && piece != Piece::Bishop
                                && piece != Piece::Knight
                            {
                                return;
                            }
                        }
                    }
                    self.board.insert(None, pos);
                    self.board.insert(Some(promotion), pos_c);
                }
            }
        }
        self.moves.push((r#move, promotion));
    }

    pub fn undo_move(&mut self) {
        if let Some(r#move) = self.moves.pop() {
            match r#move {
                (
                    MoveType::Capture((pos, piece), (pos_c, piece_c))
                    | MoveType::CapturePromotion((pos, piece), (pos_c, piece_c)),
                    _,
                ) => {
                    self.board.insert(Some(piece), pos);
                    self.board.insert(Some(piece_c), pos_c);
                }
                (
                    MoveType::Move((pos, piece), blank_pos)
                    | MoveType::MovePromotion((pos, piece), blank_pos),
                    _,
                ) => {
                    self.board.insert(Some(piece), pos);
                    self.board.insert(None, blank_pos);
                }
            }
        }
    }
}

impl Board {
    fn insert(&mut self, piece: Option<Colored<Piece>>, location: Pos) {
        // TODO: use checked indexing
        self.board[7 - (location.row as usize - 1)][location.column as usize - 1] = piece;
    }
}

fn to_range(length: i8) -> Vec<i8> {
    if length.is_negative() {
        (length..0).rev().collect::<Vec<i8>>()
    } else {
        (0..=length).collect::<Vec<i8>>()
    }
}
impl Piece {
    /// unlike `Board::get_moves` this method does not consider the board so it will return moves that are blocked
    pub fn get_moves(&self, pos: Pos) -> HashSet<Pos> {
        match self {
            Self::King => {
                let mut ret = HashSet::new();
                // the king can move one square in any direction
                diag(pos, -1..2, &mut ret);
                horiz_and_vert(pos, -1..2, &mut ret);
                ret
            }
            // queen can go diagonally, vertically and horizontally
            // around the entire board
            Self::Queen => {
                let mut ret = HashSet::new();
                diag(pos, -7..8, &mut ret);
                horiz_and_vert(pos, -7..8, &mut ret);
                ret
            }
            Self::Rook => {
                let mut ret = HashSet::new();

                horiz_and_vert(pos, -7..8, &mut ret);
                ret
            }
            Self::Bishop => {
                let mut ret = HashSet::new();
                diag(pos, -7..8, &mut ret);
                ret
            }
            Self::Knight => {
                let mut ret = HashSet::new();
                for x in -2..3i8 {
                    if x == 0 {
                        continue;
                    }
                    if let Some(pos) = pos.add_row_and_column(x, 3 - x.abs()) {
                        ret.insert(pos);
                    }
                    if let Some(pos) = pos.add_row_and_column(x, -3 + x.abs()) {
                        ret.insert(pos);
                    }
                }
                ret
            }
            Self::Pawn => {
                let mut ret = HashSet::new();
                if let Some(pos) = pos.add_row_and_column(1, 0) {
                    ret.insert(pos);
                }
                if let Some(pos) = pos.add_row_and_column(1, 1) {
                    ret.insert(pos);
                }
                if let Some(pos) = pos.add_row_and_column(1, -1) {
                    ret.insert(pos);
                }
                ret
            }
        }
    }
}

fn horiz_and_vert(pos: Pos, upper_lower: Range<i8>, ret: &mut HashSet<Pos>) {
    for x in upper_lower {
        if x == 0 {
            continue;
        }
        if let Some(pos) = pos.add_row_and_column(x, 0) {
            ret.insert(pos);
        }
        if let Some(pos) = pos.add_row_and_column(0, x) {
            ret.insert(pos);
        }
    }
}

fn diag(pos: Pos, upper_lower: Range<i8>, ret: &mut HashSet<Pos>) {
    for x in upper_lower {
        if x == 0 {
            continue;
        }
        if let Some(pos) = pos.add_row_and_column(x, x) {
            ret.insert(pos);
        }
        if let Some(pos) = pos.add_row_and_column(x, -x) {
            ret.insert(pos);
        }
    }
}

impl Colored<Piece> {
    /// an extension of `Piece::get_moves` that adds the extra starting move for a pawn
    pub fn get_moves(&self, pos: Pos) -> HashSet<Pos> {
        match self {
            // we need to check if its a pawn on its first move
            // if it is, it can move two squares forward
            Self::Black(piece) => {
                let mut ret = piece.get_moves(pos);
                if pos.row == 7 {
                    if let Some(pos) = pos.add_row_and_column(-2, 0) {
                        ret.insert(pos);
                    }
                }
                piece.get_moves(pos)
            }
            Self::White(piece) => {
                let mut ret = piece.get_moves(pos);
                if pos.row == 2 {
                    if let Some(pos) = pos.add_row_and_column(2, 0) {
                        ret.insert(pos);
                    }
                }
                ret
            }
        }
    }
}

impl Board {
    /// retrives the piece at the given position in the board
    pub fn get_cell(&self, cell: Pos) -> Option<&Option<Colored<Piece>>> {
        self.board
            .get(8u8.checked_sub(cell.row)? as usize)
            .and_then(|row| row.get(cell.column as usize - 1))
    }
}

#[cfg(test)]
mod move_tests {
    use std::{collections::HashSet, str::FromStr, time::Instant};

    use crate::{moves::Game, Color, Colored, GameState, Piece, Pos};

    #[test]
    fn test_possible_moves() {
        // create the board
        let gamestate = GameState::new();
        // get white kingS position (e1)
        let king_pos = gamestate
            .board
            .get_cell(Pos::from_str("e1").unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(king_pos, Colored::White(Piece::King));
        let king_moves = king_pos.get_moves(Pos::from_str("e1").unwrap());
        assert_eq!(king_moves.len(), 5);
        assert_eq!(
            king_moves,
            HashSet::from_iter(vec![
                Pos::from_str("e2").unwrap(),
                Pos::from_str("d2").unwrap(),
                Pos::from_str("d1").unwrap(),
                Pos::from_str("f1").unwrap(),
                Pos::from_str("f2").unwrap()
            ])
        );
        let queen_pos = gamestate
            .board
            .get_cell(Pos::from_str("d1").unwrap())
            .unwrap()
            .unwrap();
        assert_eq!(queen_pos, Colored::White(Piece::Queen));
        let queen_moves = queen_pos.get_moves(Pos::from_str("d1").unwrap());
        assert_eq!(queen_moves.len(), 21);
        assert_eq!(
            queen_moves,
            HashSet::from_iter(vec![
                Pos::from_str("d2").unwrap(),
                Pos::from_str("d3").unwrap(),
                Pos::from_str("d4").unwrap(),
                Pos::from_str("d5").unwrap(),
                Pos::from_str("d6").unwrap(),
                Pos::from_str("d7").unwrap(),
                Pos::from_str("d8").unwrap(),
                Pos::from_str("e2").unwrap(),
                Pos::from_str("f3").unwrap(),
                Pos::from_str("g4").unwrap(),
                Pos::from_str("h5").unwrap(),
                Pos::from_str("c2").unwrap(),
                Pos::from_str("b3").unwrap(),
                Pos::from_str("a4").unwrap(),
                Pos::from_str("e1").unwrap(),
                Pos::from_str("c1").unwrap(),
                Pos::from_str("b1").unwrap(),
                Pos::from_str("a1").unwrap(),
                Pos::from_str("g1").unwrap(),
                Pos::from_str("h1").unwrap(),
                Pos::from_str("f1").unwrap()
            ])
        );
        let knight_pos = Pos::from_str("b1").unwrap();
        let knight_moves = gamestate
            .board
            .get_cell(knight_pos)
            .unwrap()
            .unwrap()
            .get_moves(knight_pos);
        assert_eq!(knight_moves.len(), 3);
        assert_eq!(
            knight_moves,
            HashSet::from_iter(vec![
                Pos::from_str("c3").unwrap(),
                Pos::from_str("a3").unwrap(),
                Pos::from_str("d2").unwrap()
            ])
        );
        let pawn_pos = Pos::from_str("a2").unwrap();
        let pawn_moves = gamestate
            .board
            .get_cell(pawn_pos)
            .unwrap()
            .unwrap()
            .get_moves(pawn_pos);
        assert_eq!(pawn_moves.len(), 3);
        assert_eq!(
            pawn_moves,
            HashSet::from_iter(vec![
                Pos::from_str("a3").unwrap(),
                Pos::from_str("a4").unwrap(),
                Pos::from_str("b3").unwrap()
            ])
        );
    }

    #[test]
    fn test_semi_legal_moves() {
        let game_state = Game::from(GameState::new());
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("e1").unwrap());
        assert_eq!(pos_moves.len(), 0);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("e2").unwrap());
        assert_eq!(pos_moves.len(), 2);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("b1").unwrap());
        assert_eq!(pos_moves.len(), 2);

        let game_state = Game::from(
            GameState::from_str("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
                .unwrap(),
        );
        println!("{}", game_state.board);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("e1").unwrap());
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("f1").unwrap());
        assert_eq!(pos_moves.len(), 5);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("d1").unwrap());
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("h1").unwrap());
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("e4").unwrap());
        assert_eq!(pos_moves.len(), 1);

        // test pawn capture
        let game_state = Game::from(
            GameState::from_str("rnbqkbnr/ppp1pppp/8/3p4/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
                .unwrap(),
        );
        println!("{}", game_state.board);
        let pos_moves = game_state
            // .board
            .get_piece_moves(Pos::from_str("e4").unwrap());
        assert_eq!(pos_moves.len(), 2);
    }

    #[test]
    fn generate_all_moves() {
        let game_state = Game::from(GameState::new());
        println!(
            "{}",
            game_state
                // .board
                .get_all_moves(Color::White)
                .iter()
                .map(|((pos, piece), moves)| {
                    let moves = moves
                        .iter()
                        .map(|pos| format!("{} ", pos.to_string()))
                        .collect::<String>();
                    format!("\n{} at {} with moves {}", pos, piece, moves)
                })
                .collect::<String>()
        );
    }

    #[test]
    fn board_insert() {
        let mut game_state = Game::from(GameState::new());
        println!("{}", game_state.board);
        let moves = game_state
            // .board
            .get_all_moves(Color::White);

        let i = moves[&(Pos::new(2, 2), Colored::White(Piece::Pawn))].clone();
        let mut i = i.iter();
        let i = i.nth(0).unwrap();

        game_state.do_move(*i, None);
        println!("{}", game_state.board);
        let mut game_state = Game::from(
            GameState::from_str("2kr3r/ppp2p1p/1b4nQ/1P3b2/P7/2q2NBB/3N1P1P/R3K2R w KQ - 0 23")
                .unwrap(),
        );
        let now = Instant::now();
        let valid_moves = game_state
            // .board
            .get_all_valid_moves(Color::White);

        println!("{}ms", now.elapsed().as_millis());
        println!("{}", game_state.board);
        println!("{}", valid_moves.len());
        println!(
            "{}",
            valid_moves
                .iter()
                .map(|((pos, piece), moves)| {
                    let moves = moves
                        .iter()
                        .map(|pos| format!("{} ", pos.to_string()))
                        .collect::<String>();
                    format!("\n{} at {} with moves {}", pos, piece, moves)
                })
                .collect::<String>()
        );
        // check that the horse at d2 cannot move anywhere and thus is not in the hashmap
        assert_eq!(
            valid_moves.contains_key(&(Pos::new(3, 2), Colored::White(Piece::Knight))),
            false
        );
    }

    #[test]
    fn test_promotion() {
        let mut game_state = Game::from(GameState::from_str("8/8/8/8/8/8/7p/8 b - - 0 1").unwrap());
        let moves = game_state.get_all_moves(Color::Black);
        println!("{}", game_state.board);
        let i = moves[&(Pos::new(2, 8), Colored::Black(Piece::Pawn))].clone();
        let mut i = i.iter();
        let i = i.nth(0).unwrap();
        game_state.do_move(*i, Some(Colored::Black(Piece::Queen)));
        println!("{}", game_state.board);

        assert_eq!(
            game_state.board.get_cell(Pos::new(1, 8)),
            Some(&Some(Colored::Black(Piece::Queen)))
        );
    }
}
