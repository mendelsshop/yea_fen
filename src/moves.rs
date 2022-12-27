use std::{collections::HashSet, ops::Range};

use crate::{Board, Colored, Piece, Pos};

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

impl Piece {
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
    pub fn get_cell(&self, cell: Pos) -> Option<&Option<Colored<Piece>>> {
        self.board
            .get(8u8.checked_sub(cell.row)? as usize)
            .and_then(|row| row.get(cell.column as usize - 1))
    }

    pub fn get_moves(&self, cell: Pos) -> HashSet<Pos> {
        let possible_moves = if let Some(Some(piece)) = self.get_cell(cell) {
            piece.get_moves(cell)
        } else {
            HashSet::new()
        };
        // if we have a horse we need to check if the destinations cells are covered by other pieces of the same color,
        // if they are , we need to remove them from the possible moves
        // we also need to check if knight is pinned by preventing a check,
        // if so we cannot move anywhere
        // we need to check if the knight king is in check, if so we need to check if the knight can move to a cell that will prevent the check
        // if not, we cannot move the knight
        if self.get_cell(cell) == Some(&Some(Colored::Black(Piece::Knight))) {
            possible_moves
        }
        // for all other pieces the conditions are the same plus we need to check if the at any point from the current position to the destination there is a friendly piece in the way
        // if there is, we need to remove the destination from the possible moves
        // we probably need to have special cases for the king
        else {
            HashSet::new()
        }
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
}
