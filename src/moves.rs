use std::{collections::HashSet, ops::Range};

use crate::{Board, Color, Colored, Piece, Pos};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Represents a move which can be a capture of an enemy piece or a move to an empty cell
/// Note that the generic POS will always be of type `Pos`
pub enum MoveType<POS> {
    Capture(POS),
    Move(POS),
}

macro_rules! check_insert {
    ($itt:ident, $pos:ident, $piece_color:ident, $ret:ident) => {
        match $itt.get_cell($pos) {
            None | Some(None) => {
                $ret.insert(MoveType::Move($pos));
            }
            Some(Some(piece)) => {
                if !(Color::from(*piece) == $piece_color) {
                    $ret.insert(MoveType::Capture($pos));
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
impl Board {
    /// This will return the possible moves for a given piece (which is specified by its position)
    /// it will no return moves that are blocked by other pieces
    /// but it does not account for checks so it can return illegal moves
    pub fn get_moves(&self, pos: Pos) -> HashSet<MoveType<Pos>> {
        match self.get_cell(pos) {
            None | Some(None) => HashSet::new(),
            Some(Some(piece)) => {
                let color = Color::from(*piece);
                let piece = match piece {
                    Colored::Black(piece) | Colored::White(piece) => piece,
                };
                let mut ret = HashSet::new();
                match piece {
                    Piece::King => {
                        self.horiz_and_vert(pos, color, 1, &mut ret);
                        self.diag(pos, color, 1, &mut ret);
                    }
                    Piece::Queen => {
                        self.horiz_and_vert(pos, color, 7, &mut ret);
                        self.diag(pos, color, 7, &mut ret);
                    }
                    Piece::Rook => {
                        self.horiz_and_vert(pos, color, 7, &mut ret);
                    }
                    Piece::Bishop => self.diag(pos, color, 7, &mut ret),
                    Piece::Knight => {
                        for x in -2..3i8 {
                            if x == 0 {
                                continue;
                            }
                            if let Some(pos) = pos.add_row_and_column(x, 3 - x.abs()) {
                                match self.get_cell(pos) {
                                    None | Some(None) => {
                                        ret.insert(MoveType::Move(pos));
                                    }
                                    Some(Some(piece)) => {
                                        if !(Color::from(*piece) == color) {
                                            ret.insert(MoveType::Capture(pos));
                                        }
                                    }
                                }
                            }
                            if let Some(pos) = pos.add_row_and_column(x, -3 + x.abs()) {
                                match self.get_cell(pos) {
                                    None | Some(None) => {
                                        ret.insert(MoveType::Move(pos));
                                    }
                                    Some(Some(piece)) => {
                                        if !(Color::from(*piece) == color) {
                                            ret.insert(MoveType::Capture(pos));
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
                        match (color, pos.row) {
                            (Color::White, 2) => self.vert_half(pos, color, 2, &mut ret),
                            (Color::Black, 7) => self.vert_half(pos, color, -2, &mut ret),
                            (Color::Black, _) => self.vert_half(pos, color, -1, &mut ret),
                            (Color::White, _) => self.vert_half(pos, color, 1, &mut ret),
                        }
                        let mut possible = HashSet::new();
                        match color {
                            Color::Black => {
                                self.diag_left(pos, color, -1, &mut possible);
                                self.diag_right(pos, color, -1, &mut possible);
                            }
                            Color::White => {
                                self.diag_left(pos, color, 1, &mut possible);
                                self.diag_right(pos, color, 1, &mut possible);
                            }
                        }
                        // validate the the possible values are captures and not just plain moves

                        ret.extend(possible.iter().filter(|thing| match **thing {
                            MoveType::Capture(_) => true,
                            MoveType::Move(_) => false,
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
        length: i8,
        ret: &mut HashSet<MoveType<Pos>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos) = pos.add_row_and_column(0, i) {
                check_insert!(self, pos, piece_color, ret);
            }
        }
    }

    fn vert_half(
        &self,
        pos: Pos,
        piece_color: Color,
        length: i8,
        ret: &mut HashSet<MoveType<Pos>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos) = pos.add_row_and_column(i, 0) {
                check_insert!(self, pos, piece_color, ret);
            }
        }
    }

    fn diag_right(
        &self,
        pos: Pos,
        piece_color: Color,
        length: i8,
        ret: &mut HashSet<MoveType<Pos>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos) = pos.add_row_and_column(i, i) {
                check_insert!(self, pos, piece_color, ret);
            }
        }
    }

    fn diag_left(
        &self,
        pos: Pos,
        piece_color: Color,
        length: i8,
        ret: &mut HashSet<MoveType<Pos>>,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos) = pos.add_row_and_column(i, -i) {
                check_insert!(self, pos, piece_color, ret);
            }
        }
    }

    fn horiz_and_vert(
        &self,
        pos: Pos,
        piece_color: Color,
        length: i8,
        ret: &mut HashSet<MoveType<Pos>>,
    ) {
        // iterate over lower and upper each twice doing pos.add_row_column(x, 0) in the first iteration pos.add_row_column(0, x) in the second one
        // at eac check we need to check if a piece is there and see what color it is if its the same color as the current piece. then we dont add the pos to the hashset
        // otherwise we do in bothe cases where the is already a piece there we break from the loop
        self.hoirz_half(pos, piece_color, length, ret);
        self.vert_half(pos, piece_color, length, ret);
        self.hoirz_half(pos, piece_color, -length, ret);
        self.vert_half(pos, piece_color, -length, ret);
    }

    fn diag(&self, pos: Pos, piece_color: Color, length: i8, ret: &mut HashSet<MoveType<Pos>>) {
        // samme thing as horiz_and_vert but using pos.add_row_and_column(x, x) and pos.add_row_and_column(x, -x)
        self.diag_left(pos, piece_color, length, ret);
        self.diag_right(pos, piece_color, length, ret);
        self.diag_left(pos, piece_color, -length, ret);
        self.diag_right(pos, piece_color, -length, ret);
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
    use std::{collections::HashSet, str::FromStr};

    use crate::{Colored, GameState, Piece, Pos};

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
        let game_state = GameState::new();
        let pos_moves = game_state.board.get_moves(Pos::from_str("e1").unwrap());
        assert_eq!(pos_moves.len(), 0);
        let pos_moves = game_state.board.get_moves(Pos::from_str("e2").unwrap());
        assert_eq!(pos_moves.len(), 2);
        let pos_moves = game_state.board.get_moves(Pos::from_str("b1").unwrap());
        assert_eq!(pos_moves.len(), 2);

        let game_state =
            GameState::from_str("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
                .unwrap();
        println!("{}", game_state.board);
        let pos_moves = game_state.board.get_moves(Pos::from_str("e1").unwrap());
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state.board.get_moves(Pos::from_str("f1").unwrap());
        assert_eq!(pos_moves.len(), 5);
        let pos_moves = game_state.board.get_moves(Pos::from_str("d1").unwrap());
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state.board.get_moves(Pos::from_str("h1").unwrap());
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state.board.get_moves(Pos::from_str("e4").unwrap());
        assert_eq!(pos_moves.len(), 1);

        // test pawn capture
        let game_state =
            GameState::from_str("rnbqkbnr/ppp1pppp/8/3p4/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
                .unwrap();
        println!("{}", game_state.board);
        let pos_moves = game_state.board.get_moves(Pos::from_str("e4").unwrap());
        assert_eq!(pos_moves.len(), 2);
    }
}
