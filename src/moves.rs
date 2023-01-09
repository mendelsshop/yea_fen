use std::{collections::HashSet, fmt, ops::Range};

use crate::{Board, Castling, CastlingOptions, Color, Colored, GameState, Piece, Pos};

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
/// Represents a move which can be a capture of an enemy piece or a move to an empty cell
/// Note that the generic POS will always be of type `Pos`
pub enum MoveType<POS, PIECE> {
    Capture {
        origin: POS,
        piece: PIECE,
        new: POS,
        captured_piece: PIECE,
    },
    Move {
        origin: POS,
        piece: PIECE,
        new: POS,
    },
    CapturePromotion {
        origin: POS,
        piece: PIECE,
        new: POS,
        captured_piece: PIECE,
    },
    MovePromotion {
        origin: POS,
        piece: PIECE,
        new: POS,
    },
    EnPassant {
        origin: POS,
        piece: PIECE,
        new: POS,
        captured_piece: PIECE,
        captured_pos: POS,
    },
    Castle {
        king_origin: POS,
        king: PIECE,
        king_new: POS,
        rook_origin: POS,
        rook: PIECE,
        rook_new: POS,
    },
    /// when the piece doiing the moves is capturing the enemy king
    Check,
}

macro_rules! return_false {
    ($x:expr) => {
        if !$x {
            return false;
        }
    };
}

impl MoveType<Pos, Colored<Piece>> {
    pub fn from(&self, piece: (Pos, Option<Colored<Piece>>)) -> Option<Pos> {
        // check if the piece is the piece that is doing the move
        match self {
            Self::Castle {
                king_origin,
                king,
                king_new,
                rook_origin,
                rook,
                rook_new,
            } => {
                if piece == (*king_origin, Some(*king)) {
                    Some(*king_new)
                } else if piece == (*rook_origin, Some(*rook)) {
                    Some(*rook_new)
                } else {
                    None
                }
            }
            Self::Capture { origin, .. }
            | Self::Move { origin, .. }
            | Self::CapturePromotion { origin, .. }
            | Self::MovePromotion { origin, .. }
            | Self::EnPassant { origin, .. } => {
                if piece.0 == *origin {
                    Some(piece.0)
                } else {
                    None
                }
            }
            Self::Check => None,
        }
    }

    pub fn color(&self) -> Color {
        match *self {
            Self::Capture { piece, .. }
            | Self::Move { piece, .. }
            | Self::CapturePromotion { piece, .. }
            | Self::MovePromotion { piece, .. }
            | Self::EnPassant { piece, .. }
            | Self::Castle { king: piece, .. } => Color::from(piece),
            Self::Check => todo!(),
        }
    }

    pub fn to(&self, piece: Option<Colored<Piece>>) -> Pos {
        // if its a castle, then check if its a king or a rook and return the new position of the king or the rook
        match self {
            Self::Castle {
                king,
                king_new,
                rook,
                rook_new,
                ..
            } => {
                if piece == Some(*king) {
                    *king_new
                } else if piece == Some(*rook) {
                    *rook_new
                } else {
                    *king_new
                }
            }
            Self::Capture { new, .. }
            | Self::Move { new, .. }
            | Self::CapturePromotion { new, .. }
            | Self::MovePromotion { new, .. }
            | Self::EnPassant { new, .. } => *new,
            Self::Check => unreachable!(),
        }
    }

    pub const fn is_special(&self) -> bool {
        matches!(
            self,
            Self::Castle { .. }
                | Self::EnPassant { .. }
                | Self::MovePromotion { .. }
                | Self::CapturePromotion { .. }
        )
    }

    pub const fn is_promotion(&self) -> bool {
        matches!(
            self,
            Self::MovePromotion { .. } | Self::CapturePromotion { .. }
        )
    }
}
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Move {
    pub(crate) move_type: MoveType<Pos, Colored<Piece>>,
    pub(crate) en_passant: Option<Pos>,
    pub(crate) castling: CastlingOptions,
    pub(crate) half_move_clock: usize,
    pub(crate) full_move_clock: usize,
}

impl Move {
    pub const fn get_type(&self) -> &MoveType<Pos, Colored<Piece>> {
        &self.move_type
    }
}

impl fmt::Display for MoveType<Pos, Colored<Piece>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::CapturePromotion {
                origin,
                piece,
                new,
                captured_piece,
            }
            | Self::Capture {
                origin,
                piece,
                new,
                captured_piece,
            } => write!(
                f,
                "Capture of {} at {} from {} at {}",
                captured_piece, new, piece, origin
            ),

            Self::Move { origin, new, piece } | Self::MovePromotion { origin, piece, new } => {
                write!(f, "Move from {} with {} to {}", origin, piece, new)
            }
            Self::EnPassant {
                origin: pos_o,
                piece: piece_m,
                new: pos_n,
                captured_piece: piece,
                captured_pos: pos_e,
            } => write!(
                f,
                "En_passant {} at {} from {} at {} to {}",
                piece, pos_o, piece_m, pos_n, pos_e
            ),
            Self::Castle {
                king_origin: pos_k,
                king,
                king_new: new_k,
                rook_origin: pos_c,
                rook: castle,
                rook_new: new_c,
            } => write!(
                f,
                "castle at {}:{} {}:{} from {} and {}",
                new_k, king, new_c, castle, pos_k, pos_c
            ),
            Self::Check => todo!(),
        }
    }
}

macro_rules! check_insert {
    ($itt:ident, $new:ident, $piece_color:ident, $ret:ident, $origin:expr, $checks_n_pins:ident, $dir:expr) => {
        if $checks_n_pins {
            // we first check if the piece a king
            if Piece::King == Piece::from($origin.1) {
                // we run self(itt).pins_and_checks() to get the pins and checks
                let (.., in_check) = $itt.pins_and_checks($new, $piece_color);
                if in_check {
                    break;
                }

            }
            // if the piece is pinned and cannot move in the direction of the pin, then it cannot move
            // if so we break the loop
            // first we need to check if the piece is pinned
            if let Some(pin) = $itt.pins.iter().find(|pin| pin.0 == $origin.0 && pin.2 == $origin.1) {
                // if the piece is pinned, then we need to check if the piece can move in the direction of the pin
                // if it can, then we can insert the move, otherwise we break the loop
                // we need to match on the direction of the pin and the piece being pinned
                match (Piece::from(pin.2), pin.1) {
                    // if the pin direction is one of a knight, then the piece cannot move in that direction
                    (_, (-2 | 2, -1 | 1) | (-1 | 1, -2 | 2)) => {
                        break;
                    }
                    // pawns can potentially move in diagonal directions, or up or down
                    (Piece::Pawn, (-1 | 1, 0 | -1 | 1)) => {
                        // we have to check the piece color to see if the pawn is moving in the right direction
                        match $piece_color {
                            Color::White => {
                                // if the piece is white, then it can only move up
                                if pin.1.0 != -1 {
                                    break;
                                }
                            }
                            Color::Black => {
                                // if the piece is black, then it can only move down
                                if pin.1.0 != 1 {
                                    break;
                                }
                            }
                        }
                    }
                    // if the pin direction is one of a bishop, then the piece can only move in that direction if it is a bishop or a queen
                    (_, (-1 | 1, -1 | 1)) => {
                        if !matches!(Piece::from($origin.1), Piece::Bishop | Piece::Queen){
                            break;
                        } else if $dir.contains(&pin.1) && matches!(Piece::from($origin.1), Piece::Bishop | Piece::Queen) {
                            break;
                        }
                    }
                    // if the pin direction is one of a rook, then the piece can only move in that direction if it is a rook or a queen
                    (_, (-1 | 1, 0) | (0, -1 | 1)) => {
                        if !matches!(Piece::from($origin.1), Piece::Rook | Piece::Queen) {
                            break;
                        } else if $dir.contains(&pin.1) && matches!(Piece::from($origin.1), Piece::Rook | Piece::Queen) {
                            break;
                        }
                    }
                    (Piece::King | Piece::Knight, _) => {}
                    _ => {}
                }
            }

        }
        match $itt.board.get_cell($new) {
            None | Some(None) => {
                match $origin {
                    // if the piece is a pawn and its at the end of the board, then it can be promoted
                    // for a white pawn, the row is 8, for a black pawn, the row is 1
                    (Pos { row: 7, .. }, Colored::White(Piece::Pawn)) | (Pos { row: 2, .. }, Colored::Black(Piece::Pawn)) => {
                        $ret.insert(MoveType::MovePromotion{new: $new , origin: $origin.0, piece: $origin.1});
                    }
                    _ => {
                        $ret.insert(MoveType::Move{new: $new , origin: $origin.0, piece: $origin.1});
                    }
                }

            }
            Some(Some(piece_c)) => {
                if !(Color::from(*piece_c) == $piece_color) {
                    match $origin {
                        // if the piece is a pawn and its at the end of the board, then it can be promoted
                        // for a white pawn, the row is 8, for a black pawn, the row is 1
                        (Pos { row: 7, .. }, Colored::White(Piece::Pawn)) | (Pos { row: 2, .. }, Colored::Black(Piece::Pawn)) => {
                            $ret.insert(MoveType::CapturePromotion{new: $new, origin: $origin.0, piece: $origin.1, captured_piece: *piece_c});
                        }
                        _ => {
                            $ret.insert(MoveType::Capture{new: $new, origin: $origin.0, piece: $origin.1, captured_piece: *piece_c});
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

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum GameResult {
    CheckMate(Color),
    StaleMate,
    Draw,
    InProgress,
}

impl GameState {
    /// the `castle_king_side` and `castle_queen_side` methods do not actually do a castle they just check if a castle is available and if it is and it to ret
    /// we can also assume that the pieces are in there starting positions so we don'y have to around the board looking for them
    fn castle_king_side(
        &self,
        player: Color,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
        checks_n_pins: bool,
    ) {
        let ((king_pos, rook_pos), (new_k, new_r), (king, rook)) = match player {
            Color::Black => (
                (Pos { row: 8, column: 5 }, Pos { row: 8, column: 8 }),
                (Pos { row: 8, column: 7 }, Pos { row: 8, column: 6 }),
                (Colored::Black(Piece::King), Colored::Black(Piece::Rook)),
            ),
            Color::White => (
                (Pos { row: 1, column: 5 }, Pos { row: 1, column: 8 }),
                (Pos { row: 1, column: 7 }, Pos { row: 1, column: 6 }),
                (Colored::White(Piece::King), Colored::White(Piece::Rook)),
            ),
        };
        for i in to_range(2) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = king_pos.add_row_and_column(0, i) {
                if let Some(Some(_)) = self.board.get_cell(pos_n) {
                    break;
                } else if checks_n_pins && self.pins_and_checks(pos_n, player).2 {
                    break;
                }

                if i == 2 {
                    ret.insert(MoveType::Castle {
                        king,
                        rook,
                        king_origin: king_pos,
                        king_new: new_k,
                        rook_origin: rook_pos,
                        rook_new: new_r,
                    });
                }
            }
        }
    }
    fn castle_queen_side(
        &self,
        player: Color,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
        checks_n_pins: bool,
    ) {
        let ((king_pos, rook_pos), (new_k, new_r), (king, rook)) = match player {
            Color::Black => (
                (Pos { row: 8, column: 5 }, Pos { row: 8, column: 1 }),
                (Pos { row: 8, column: 3 }, Pos { row: 8, column: 4 }),
                (Colored::Black(Piece::King), Colored::Black(Piece::Rook)),
            ),
            Color::White => (
                (Pos { row: 1, column: 5 }, Pos { row: 1, column: 1 }),
                (Pos { row: 1, column: 3 }, Pos { row: 1, column: 4 }),
                (Colored::White(Piece::King), Colored::White(Piece::Rook)),
            ),
        };
        for i in to_range(-3) {
            if i == 0 {
                continue;
            }

            if let Some(pos_n) = king_pos.add_row_and_column(0, i) {
                if let Some(Some(_)) = self.board.get_cell(pos_n) {
                    break;
                }
                if checks_n_pins && self.pins_and_checks(pos_n, player).2 {
                    break;
                }
                if i == -3 {
                    ret.insert(MoveType::Castle {
                        king,
                        rook,
                        king_origin: king_pos,
                        king_new: new_k,
                        rook_origin: rook_pos,
                        rook_new: new_r,
                    });
                }
            }
        }
    }
    /// This will return the possible moves for a given piece (which is specified by its position)
    /// it will no return moves that are blocked by other pieces
    /// but it does not account for checks so it can return illegal moves
    pub fn get_piece_moves(
        &mut self,
        pos: Pos,
        checks_n_pins: bool,
    ) -> HashSet<MoveType<Pos, Colored<Piece>>> {
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
                        self.horiz_and_vert(pos, color, *piece, 1, &mut ret, checks_n_pins);
                        self.diag(pos, color, *piece, 1, &mut ret, checks_n_pins);

                        // to figure out if we can castle
                        // first we see if the king is in check by by generating all the enemy moves and seeing if theres is a possiblity of the king being captured
                        // we first need to get the castling rights of the given piece's color
                        // then we need to use self.horiz_half to determine if all the spaces between thr king & rook are empty
                        // and we need to check that the 2 spaces closest to the king are also not in check
                        // if they are not in check then we can add the castle to ret
                        // the check validation we will do in get_all_valid_moves
                        let casling = self.castling_moves.get(color);
                        match casling {
                            Castling::None => {}
                            Castling::KingSide => {
                                self.castle_king_side(color, &mut ret, checks_n_pins);
                            }
                            Castling::QueenSide => {
                                self.castle_queen_side(color, &mut ret, checks_n_pins);
                            }
                            Castling::Both => {
                                self.castle_king_side(color, &mut ret, checks_n_pins);
                                self.castle_queen_side(color, &mut ret, checks_n_pins);
                            }
                        }
                    }
                    Piece::Queen => {
                        self.horiz_and_vert(pos, color, *piece, 7, &mut ret, checks_n_pins);
                        self.diag(pos, color, *piece, 7, &mut ret, checks_n_pins);
                    }
                    Piece::Rook => {
                        self.horiz_and_vert(pos, color, *piece, 7, &mut ret, checks_n_pins);
                    }
                    Piece::Bishop => self.diag(pos, color, *piece, 7, &mut ret, checks_n_pins),
                    Piece::Knight => {
                        for x in -2..3i8 {
                            if checks_n_pins
                                && self.pins.iter().any(|pin| pin.0 == pos && pin.2 == *piece)
                            {
                                break;
                            }
                            if x == 0 {
                                continue;
                            }
                            if let Some(pos_n) = pos.add_row_and_column(x, 3 - x.abs()) {
                                match self.board.get_cell(pos_n) {
                                    None | Some(None) => {
                                        ret.insert(MoveType::Move {
                                            origin: pos,
                                            piece: *piece,
                                            new: pos_n,
                                        });
                                    }
                                    Some(Some(piece_c)) => {
                                        if !(Color::from(*piece_c) == color) {
                                            ret.insert(MoveType::Capture {
                                                origin: pos,
                                                piece: *piece,
                                                new: pos_n,
                                                captured_piece: *piece_c,
                                            });
                                        }
                                    }
                                }
                            }
                            if let Some(pos_n) = pos.add_row_and_column(x, -3 + x.abs()) {
                                match self.board.get_cell(pos_n) {
                                    None | Some(None) => {
                                        ret.insert(MoveType::Move {
                                            origin: pos,
                                            piece: *piece,
                                            new: pos_n,
                                        });
                                    }
                                    Some(Some(piece_c)) => {
                                        if !(Color::from(*piece_c) == color) {
                                            ret.insert(MoveType::Capture {
                                                origin: pos,
                                                piece: *piece,
                                                new: pos_n,
                                                captured_piece: *piece_c,
                                            });
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
                                self.vert_half(pos, color, *piece, 2, &mut possible, checks_n_pins);
                            }
                            (Color::Black, 7) => {
                                self.vert_half(
                                    pos,
                                    color,
                                    *piece,
                                    -2,
                                    &mut possible,
                                    checks_n_pins,
                                );
                            }
                            (Color::Black, _) => {
                                self.vert_half(
                                    pos,
                                    color,
                                    *piece,
                                    -1,
                                    &mut possible,
                                    checks_n_pins,
                                );
                            }
                            (Color::White, _) => {
                                self.vert_half(pos, color, *piece, 1, &mut possible, checks_n_pins);
                            }
                        }
                        ret.extend(possible.iter().filter(|thing| match **thing {
                            MoveType::Capture { .. }
                            | MoveType::CapturePromotion { .. }
                            | MoveType::EnPassant { .. }
                            | MoveType::Castle { .. } => false,
                            MoveType::Move { .. } | MoveType::MovePromotion { .. } => true,
                            MoveType::Check => todo!(),
                        }));
                        let mut possible = HashSet::new();
                        match color {
                            Color::Black => {
                                self.diag_left(
                                    pos,
                                    color,
                                    *piece,
                                    -1,
                                    &mut possible,
                                    checks_n_pins,
                                );
                                self.diag_right(
                                    pos,
                                    color,
                                    *piece,
                                    -1,
                                    &mut possible,
                                    checks_n_pins,
                                );
                            }
                            Color::White => {
                                self.diag_left(pos, color, *piece, 1, &mut possible, checks_n_pins);
                                self.diag_right(
                                    pos,
                                    color,
                                    *piece,
                                    1,
                                    &mut possible,
                                    checks_n_pins,
                                );
                            }
                        }
                        // validate the the possible values are captures and not just plain moves
                        ret.extend(possible.iter().filter(|thing| match **thing {
                            MoveType::Capture { .. }
                            | MoveType::CapturePromotion { .. }
                            | MoveType::EnPassant { .. } => true,
                            MoveType::Move { .. }
                            | MoveType::MovePromotion { .. }
                            | MoveType::Castle { .. } => false,
                            MoveType::Check => todo!(),
                        }));
                        // check if last move was from an enemy pawn and if it was 2 spaces away
                        let new_enpessant = self.moves.last().and_then(|last_move| {
                            if pos.row
                                != match color {
                                    Color::White => 5,
                                    Color::Black => 4,
                                }
                            {
                                return None;
                            }
                            // check if the last move was a pawn move
                            if let MoveType::Move {
                                origin,
                                piece: piece_n,
                                new,
                            } = last_move.move_type
                            {
                                if (color == Color::from(piece_n)
                                    && (piece_n == Colored::White(Piece::Pawn)
                                        || piece_n == Colored::Black(Piece::Pawn)))
                                    || (origin.row.max(new.row) - origin.row.min(new.row) != 2)
                                {
                                    return None;
                                }
                                // check if the last move was to the left or right of the current position
                                // but first check if column is 1 or 8
                                let new_column: u8 = if (pos.column == 1 && new.column != 2)
                                    || (pos.column == 8 && new.column != 7)
                                {
                                    return None;
                                } else if new.column == pos.column - 1 {
                                    pos.column - 1
                                } else if new.column == pos.column + 1 {
                                    pos.column + 1
                                } else {
                                    return None;
                                };
                                // check if checks_n_pins is true
                                //  this is currently not working
                                if checks_n_pins {
                                    // get the king position
                                    let king_pos = self.find_king(color).unwrap();

                                    // if the kings row is pos.row

                                    if king_pos.row == pos.row {
                                        // find any enemy pieces that can attack horizontally and are on the same row

                                        // to do this we iterate over either fro 0..pos.column or pos.column..8 depending on the relative position of the king ie if the king is to the left of the pawn we iterate form pos.column..8 otherwise we iterate from 0..pos.column we should always start from th piecs's column
                                        // if at any point we find a piece we check if it is an enemy piece and if it can attack horizontally if it can then we return None otherwise we break
                                        // we should skip the pawn's column
                                        for i in if king_pos.column < pos.column {
                                            to_range_both(pos.column.max(new.column) as i8 + 1, 8)
                                        } else {
                                            to_range_both(pos.column.min(new.column) as i8 - 1, 1)
                                        } {
                                            if !(1..=8).contains(&i) {
                                                // we don't want to check outside the board causr Pos::new will panic
                                                break;
                                            }
                                            if let Some(Some(piece_c)) =
                                                self.board.get_cell(Pos::new(pos.row, i as u8))
                                            {
                                                if Color::from(*piece_c) != color {
                                                    if let Piece::Rook | Piece::Queen =
                                                        Piece::from(*piece_c)
                                                    {
                                                        return None;
                                                    }
                                                }
                                                break;
                                            }
                                        }
                                    }
                                }

                                let new_pos = match color {
                                    Color::White => Pos::new(pos.row + 1, new_column),
                                    Color::Black => Pos::new(pos.row - 1, new_column),
                                };

                                ret.insert(MoveType::EnPassant {
                                    origin: pos,
                                    piece: *piece,
                                    captured_pos: new,
                                    captured_piece: piece_n,
                                    new: new_pos,
                                });
                                return Some(new_pos);
                            }
                            None
                        });
                        // todo: the en_passant might not be valid so we need to set the en_passant in get_all_valid_moves
                        if let Some(new_enpessant) = new_enpessant {
                            self.en_passant = Some(new_enpessant);
                        }
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
        checks_n_pins: bool,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(0, i) {
                check_insert!(
                    self,
                    pos_n,
                    piece_color,
                    ret,
                    (pos, piece),
                    checks_n_pins,
                    [(0, 1), (0, -1)]
                );
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
        checks_n_pins: bool,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(i, 0) {
                check_insert!(
                    self,
                    pos_n,
                    piece_color,
                    ret,
                    (pos, piece),
                    checks_n_pins,
                    [(1, 0), (-1, 0)]
                );
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
        checks_n_pins: bool,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(i, i) {
                check_insert!(
                    self,
                    pos_n,
                    piece_color,
                    ret,
                    (pos, piece),
                    checks_n_pins,
                    [(-1, 1), (1, -1)]
                );
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
        checks_n_pins: bool,
    ) {
        for i in to_range(length) {
            if i == 0 {
                continue;
            }
            if let Some(pos_n) = pos.add_row_and_column(i, -i) {
                check_insert!(
                    self,
                    pos_n,
                    piece_color,
                    ret,
                    (pos, piece),
                    checks_n_pins,
                    [(1, 1), (-1, -1)]
                );
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
        checks_n_pins: bool,
    ) {
        // iterate over lower and upper each twice doing pos.add_row_column(x, 0) in the first iteration pos.add_row_column(0, x) in the second one
        // at eac check we need to check if a piece is there and see what color it is if its the same color as the current piece. then we dont add the pos to the hashset
        // otherwise we do in bothe cases where the is already a piece there we break from the loop
        self.hoirz_half(pos, piece_color, piece, length, ret, checks_n_pins);
        self.vert_half(pos, piece_color, piece, length, ret, checks_n_pins);
        self.hoirz_half(pos, piece_color, piece, -length, ret, checks_n_pins);
        self.vert_half(pos, piece_color, piece, -length, ret, checks_n_pins);
    }

    fn diag(
        &self,
        pos: Pos,
        piece_color: Color,
        piece: Colored<Piece>,
        length: i8,
        ret: &mut HashSet<MoveType<Pos, Colored<Piece>>>,
        checks_n_pins: bool,
    ) {
        // samme thing as horiz_and_vert but using pos.add_row_and_column(x, x) and pos.add_row_and_column(x, -x)
        self.diag_left(pos, piece_color, piece, length, ret, checks_n_pins);
        self.diag_right(pos, piece_color, piece, length, ret, checks_n_pins);
        self.diag_left(pos, piece_color, piece, -length, ret, checks_n_pins);
        self.diag_right(pos, piece_color, piece, -length, ret, checks_n_pins);
    }

    #[allow(clippy::cast_possible_truncation)]
    // we will never truncate a value becuse the board does not go past 8
    /// gets all posible moves for a given color/player
    /// the moves can be invalid in a way such that the king can still be in check
    pub fn get_all_moves_pins(
        &mut self,
        player: Color,
        checks_n_pins: bool,
    ) -> HashSet<MoveType<Pos, Colored<Piece>>> {
        // gets all moves for a given color by iterating through the board and finding pieces of the color and generating the moves for that piece using self.get_piece_moves()
        self.board
            .into_iter()
            .rev()
            .enumerate()
            .flat_map(|(row_idx, row)| {
                row.iter()
                    .enumerate()
                    .filter_map(|(column_idx, cell)| {
                        let cell = (*cell)?;
                        if Color::from(cell) == player {
                            let pos = Pos::new(row_idx as u8 + 1, column_idx as u8 + 1);
                            let moves = self.get_piece_moves(pos, checks_n_pins);
                            if moves.is_empty() {
                                None
                            } else {
                                Some(moves)
                            }
                        } else {
                            None
                        }
                    })
                    .flatten()
                    .collect::<HashSet<MoveType<Pos, Colored<Piece>>>>()
            })
            .collect::<HashSet<MoveType<Pos, Colored<Piece>>>>()
    }

    // we will never truncate a value becuse the board does not go past 8
    /// gets all posible moves for a given color/player
    /// the moves can be invalid in a way such that the king can still be in check
    pub fn get_all_moves(&mut self, player: Color) -> HashSet<MoveType<Pos, Colored<Piece>>> {
        // gets all moves for a given color by iterating through the board and finding pieces of the color and generating the moves for that piece using self.get_piece_moves()
        self.get_all_moves_pins(player, false)
    }

    pub fn get_all_valid_moves(&mut self, player: Color) -> HashSet<MoveType<Pos, Colored<Piece>>> {
        if self.result != GameResult::InProgress {
            return HashSet::new();
        }
        let mut ret = self.get_all_moves(player);
        ret.retain(|pos| {
            // iterate over moves
            // need a way to do a move
            // do move
            // generate enemy moves
            // find any enemy moves where its a capture and the cell its capturing is allay king
            let enemy_moves = self.get_all_moves(match player {
                Color::White => Color::Black,
                Color::Black => Color::White,
            });
            if let MoveType::Castle {
                king,
                king_origin: pos_k,
                rook_origin: pos_r,
                ..
            } = pos
            {
                // check if the king is in check
                // then check if the king pos (+ if kingside, - if queenside) 1 is in check
                // then check if the king pos (+ if kingside, - if queenside) 2 is in check
                // if any of these are true then the move is invalid
                let mut checks = vec![];
                match pos_r {
                    // queenside
                    Pos {
                        row: 8 | 1,
                        column: 1,
                    } => {
                        checks.push(in_check(&enemy_moves, player));
                        // move the king 1 to the left
                        let move_ = MoveType::Move {
                            origin: *pos_k,
                            piece: *king,
                            new: Pos {
                                row: pos_k.row,
                                column: pos_k.column - 1,
                            },
                        };
                        self.do_move(move_, None);
                        let enemy_moves = self.get_all_moves(match player {
                            Color::White => Color::Black,
                            Color::Black => Color::White,
                        });
                        checks.push(in_check(&enemy_moves, player));
                        // move the king 1 to the left
                        let move_ = MoveType::Move {
                            origin: *pos_k,
                            piece: *king,
                            new: Pos {
                                row: pos_k.row,
                                column: pos_k.column - 2,
                            },
                        };
                        self.do_move(move_, None);
                        let enemy_moves = self.get_all_moves(match player {
                            Color::White => Color::Black,
                            Color::Black => Color::White,
                        });
                        checks.push(in_check(&enemy_moves, player));
                        self.undo_move();
                        self.undo_move();
                        return !checks.contains(&true);
                    }
                    // kingside
                    Pos {
                        row: 8 | 1,
                        column: 8,
                    } => {
                        checks.push(in_check(&enemy_moves, player));
                        // move the king 1 to the right
                        let move_ = MoveType::Move {
                            origin: *pos_k,
                            piece: *king,
                            new: Pos {
                                row: pos_k.row,
                                column: pos_k.column + 1,
                            },
                        };
                        self.do_move(move_, None);
                        let enemy_moves = self.get_all_moves(match player {
                            Color::White => Color::Black,
                            Color::Black => Color::White,
                        });
                        checks.push(in_check(&enemy_moves, player));
                        let move_ = MoveType::Move {
                            origin: *pos_k,
                            piece: *king,
                            new: Pos {
                                row: pos_k.row,
                                column: pos_k.column + 2,
                            },
                        };
                        self.do_move(move_, None);
                        let enemy_moves = self.get_all_moves(match player {
                            Color::White => Color::Black,
                            Color::Black => Color::White,
                        });
                        checks.push(in_check(&enemy_moves, player));
                        self.undo_move();
                        self.undo_move();
                        return !checks.contains(&true);
                    }
                    _ => return false,
                }
            }

            match player {
                Color::White => self.do_move(*pos, Some(Colored::White(Piece::Queen))),
                Color::Black => self.do_move(*pos, Some(Colored::Black(Piece::Queen))),
            };

            let openent = match player {
                Color::White => Color::Black,
                Color::Black => Color::White,
            };
            let enemy_moves = self.get_all_moves(openent);
            self.undo_move();
            !in_check(&enemy_moves, player)
        });
        // it might not be the best place to check for checkmate and stalemate
        if ret.is_empty() {
            let enemy_moves = self.get_all_moves(match player {
                Color::White => Color::Black,
                Color::Black => Color::White,
            });
            // check if king is in check
            if in_check(&enemy_moves, player) {
                // checkmate
                self.result = GameResult::CheckMate(player);
            } else {
                // stalemate
                self.result = GameResult::StaleMate;
            }
            // if king is in check then the game is over
            // if king is not in check then the game is a draw
        }
        // we need to check if there is only kings left on the board
        // if there is only kings left on the board then the game is a draw
        let count = self
            .board
            .board
            .iter()
            .flat_map(|x| x.to_vec())
            .filter(Option::is_some)
            .count();
        if count == 2 {
            self.result = GameResult::Draw;
        }
        // one way to figure out if a move is legal
        // is to play a move and see if any of the oppenets moves is to capture the king
        // by iterastion over ret doing the move and then generating all oppenets moves using self.get_all_moves
        //
        ret
    }

    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    pub fn new_all_valid_moves(&mut self, player: Color) -> HashSet<MoveType<Pos, Colored<Piece>>> {
        // find king pos
        // really the king should always be on the board
        let king_pos = match self.find_king(player) {
            Some(pos) => pos,
            None => return HashSet::new(),
        };

        let (pins, checks, in_check) = self.pins_and_checks(king_pos, player);
        self.pins = pins;
        self.checks = checks.clone();
        self.in_check = in_check;
        let ret = if in_check {
            if checks.len() == 1 {
                let mut valid_poses = vec![];
                // get all moves for player and check if any of the moves block the check
                let check = checks[0];
                let moves = self.get_all_moves_pins(player, true);
                if check.2
                    == Colored::new(
                        match player {
                            Color::White => Color::Black,
                            Color::Black => Color::White,
                        },
                        Piece::Knight,
                    )
                {
                    valid_poses.push(check.0);
                    // if the piece checking the king is a knight then the only move that can be made is to capture the knight
                } else {
                    for i in 1..8 {
                        // we have to do bounds checking
                        let row = i32::from(king_pos.row) + check.1 .0 * i;
                        let col = i32::from(king_pos.column) + check.1 .1 * i;
                        if (1..=8).contains(&row) && (1..=8).contains(&col) {
                            let pos = Pos::new(row as u8, col as u8);
                            valid_poses.push(pos);
                            if pos == check.0 {
                                break;
                            }
                        }
                    }
                }
                moves
                    .into_iter()
                    .filter(|r#move| {
                        // if the move is not from a king or nott a piace in valid_poses then it is not a valid move
                        match *r#move {
                            MoveType::Move { piece, .. }
                            | MoveType::Capture { piece, .. }
                            | MoveType::EnPassant { piece, .. }
                            | MoveType::MovePromotion { piece, .. }
                            | MoveType::CapturePromotion { piece, .. } => {
                                piece.is(&Piece::King)
                                    || valid_poses.contains(&r#move.to(Some(piece)))
                            }
                            MoveType::Castle { .. } => false,
                            MoveType::Check => todo!(),
                        }
                    })
                    .collect()
            } else {
                // todo make get_piece_moves have special cases for pinned and checking pieces
                self.get_piece_moves(king_pos, false)
            }
        } else {
            // todo make get_all_moves have special cases for pinned and checking pieces
            self.get_all_moves_pins(player, true)
        };
        if ret.is_empty() {
            // check if king is in check
            if in_check {
                // checkmate
                self.result = GameResult::CheckMate(player);
            } else {
                // stalemate
                self.result = GameResult::StaleMate;
            }

            // if king is in check then the game is over
            // if king is not in check then the game is a draw
        }
        let count = self
            .board
            .board
            .iter()
            .flat_map(|x| x.to_vec())
            .filter(Option::is_some)
            .count();
        if count == 2 {
            self.result = GameResult::Draw;
        }
        ret
    }

    // note this doesn not check for en passant pins that happens in get_piece_moves with check_for_pins = true
    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
    fn pins_and_checks(
        &self,
        king_pos: Pos,
        player: Color,
    ) -> (
        Vec<(Pos, (i32, i32), Colored<Piece>)>,
        Vec<(Pos, (i32, i32), Colored<Piece>)>,
        bool,
    ) {
        let mut pins = vec![];
        let mut checks = vec![];
        let mut in_check: bool = false;
        let directions: [(i32, i32); 8] = [
            (-1, 0),
            (0, -1),
            (1, 0),
            (0, 1),
            (-1, -1),
            (-1, 1),
            (1, -1),
            (1, 1),
        ];
        for (j, direction) in directions.iter().enumerate() {
            // let direction = directions[j];
            let mut possuble_pins = None;
            for i in 1..8 {
                let row = i32::from(king_pos.row) + direction.0 * i;
                let col = i32::from(king_pos.column) + direction.1 * i;
                if (1..=8).contains(&row) && (1..=8).contains(&col) {
                    let piece_pos = Pos::new(row as u8, col as u8);
                    if let Some(Some(piece)) = self.board.get_cell(piece_pos) {
                        let piece_type = Piece::from(*piece);
                        let piece_color = Color::from(*piece);
                        if piece_color == player {
                            // this is for a gohst king
                            if piece_type == Piece::King {
                                continue;
                            }
                            if possuble_pins.is_none() {
                                possuble_pins = Some((piece_pos, *direction, *piece));
                            } else {
                                break;
                            }
                        } else {
                            // enemy piece

                            if
                            // rook case
                            ((0..=3).contains(&j) && piece_type == Piece::Rook) ||
                            // bishop case
                            ((4..=7).contains(&j) && piece_type == Piece::Bishop) ||
                            // pawn case
                            (i==1 && piece_type == Piece::Pawn &&
                                ((piece_color == Color::Black && (6..=7).contains(&j))  ||
                                 (piece_color == Color::White && (4..=5).contains(&j)))
                            ) ||
                            // queen case
                            (piece_type == Piece::Queen) ||
                            // king case
                            (i == 1 && piece_type == Piece::King)
                            {
                                possuble_pins.map_or_else(
                                    || {
                                        in_check = true;
                                        checks.push((piece_pos, *direction, *piece));
                                        // break;
                                    },
                                    |pin| {
                                        pins.push(pin);
                                        // break;
                                    },
                                );
                            }
                            break;
                        }
                    }
                } else {
                    break;
                }
            }
        }

        let night_moves: [(i32, i32); 8] = [
            (-2, -1),
            (-2, 1),
            (-1, -2),
            (-1, 2),
            (1, -2),
            (1, 2),
            (2, -1),
            (2, 1),
        ];
        for m in night_moves {
            let row = i32::from(king_pos.row) + m.0;
            let col = i32::from(king_pos.column) + m.1;
            if (1..=8).contains(&row) && (1..=8).contains(&col) {
                let piece_pos = Pos::new(row as u8, col as u8);
                if let Some(Some(piece)) = self.board.get_cell(piece_pos) {
                    if &Colored::new(
                        match player {
                            Color::Black => Color::White,
                            Color::White => Color::Black,
                        },
                        Piece::Knight,
                    ) == piece
                    {
                        in_check = true;
                        checks.push((piece_pos, m, *piece));
                    }
                }
            }
        }
        (pins, checks, in_check)
    }

    #[allow(clippy::cast_possible_truncation)]
    fn find_king(&self, player: Color) -> Option<Pos> {
        self.board
            .board
            .iter()
            .rev()
            .enumerate()
            .flat_map(|(row, x)| {
                x.iter().enumerate().filter_map(move |(column, y)| {
                    y.map_or(None, |c| {
                        if c == Colored::new(player, Piece::King) {
                            Some(Pos::new(row as u8 + 1, column as u8 + 1))
                        } else {
                            None
                        }
                    })
                })
            })
            .next()
    }

    pub fn do_move(
        &mut self,
        r#move: MoveType<Pos, Colored<Piece>>,
        promotion: Option<Colored<Piece>>,
    ) -> bool {
        let castling_bak = self.castling_moves;
        match r#move {
            MoveType::Move { origin, piece, new } => {
                return_false!(self.board.insert(None, origin));
                return_false!(self.board.insert(Some(piece), new));
                self.set_castling(origin, piece);
            }
            MoveType::Capture {
                origin,
                piece,
                new,
                captured_piece,
            } => {
                return_false!(self.board.insert(None, origin));
                return_false!(self.board.insert(Some(piece), new));
                // here we have to account for both the piece that was captured and the piece that did the capturing

                self.set_castling(origin, piece);
                self.set_castling(new, captured_piece);
            }
            MoveType::CapturePromotion {
                origin,
                new,
                captured_piece,
                ..
            } => {
                if let Some(promotion) = promotion {
                    match promotion {
                        Colored::White(piece) | Colored::Black(piece) => {
                            if piece != Piece::Queen
                                && piece != Piece::Rook
                                && piece != Piece::Bishop
                                && piece != Piece::Knight
                            {
                                return false;
                            }
                        }
                    }
                    self.board.insert(None, origin);
                    self.board.insert(Some(promotion), new);
                    self.set_castling(new, captured_piece);
                }
            }
            MoveType::MovePromotion { origin, new, .. } => {
                if let Some(promotion) = promotion {
                    match promotion {
                        Colored::White(piece) | Colored::Black(piece) => {
                            if piece != Piece::Queen
                                && piece != Piece::Rook
                                && piece != Piece::Bishop
                                && piece != Piece::Knight
                            {
                                return false;
                            }
                        }
                    }
                    return_false!(self.board.insert(None, origin));
                    return_false!(self.board.insert(Some(promotion), new));
                }
            }
            MoveType::EnPassant {
                origin,
                new,
                piece,
                captured_pos,
                ..
            } => {
                return_false!(self.board.insert(None, origin));
                return_false!(self.board.insert(None, captured_pos));
                return_false!(self.board.insert(Some(piece), new));
            }
            MoveType::Castle {
                king_origin,
                king,
                king_new,
                rook_origin,
                rook,
                rook_new,
            } => {
                return_false!(self.board.insert(None, rook_origin));
                return_false!(self.board.insert(None, king_origin));
                return_false!(self.board.insert(Some(king), king_new));
                return_false!(self.board.insert(Some(rook), rook_new));
                match Color::from(king) {
                    Color::Black => self.castling_moves.black = Castling::None,
                    Color::White => self.castling_moves.white = Castling::None,
                }
            }
            MoveType::Check => todo!(),
        };
        self.moves.push(Move {
            move_type: r#move,
            en_passant: self.en_passant,
            castling: castling_bak,
            full_move_clock: self.full_move_clock,
            half_move_clock: self.half_move_clock,
        });
        self.en_passant = None;
        match r#move {
            MoveType::Move { .. } | MoveType::MovePromotion { .. } | MoveType::Check => {
                self.half_move_clock += 1;
            }
            MoveType::Capture { .. }
            | MoveType::CapturePromotion { .. }
            | MoveType::EnPassant { .. }
            | MoveType::Castle { .. } => self.half_move_clock = 0,
        }
        if self.active_color == Color::Black {
            self.full_move_clock += 1;
        }
        self.active_color = match self.active_color {
            Color::White => Color::Black,
            Color::Black => Color::White,
        };
        true
    }

    fn set_castling(&mut self, pos: Pos, piece: Colored<Piece>) {
        match piece {
            Colored::Black(Piece::Rook) => {
                if pos == Pos::new(8, 1) {
                    self.castling_moves
                        .remove(Color::Black, Castling::QueenSide);
                } else if pos == Pos::new(8, 8) {
                    self.castling_moves.remove(Color::Black, Castling::KingSide);
                }
            }
            Colored::White(Piece::Rook) => {
                if pos == Pos::new(1, 1) {
                    self.castling_moves
                        .remove(Color::White, Castling::QueenSide);
                } else if pos == Pos::new(1, 8) {
                    self.castling_moves.remove(Color::White, Castling::KingSide);
                }
            }
            Colored::Black(Piece::King) => self.castling_moves.remove(Color::Black, Castling::Both),
            Colored::White(Piece::King) => self.castling_moves.remove(Color::White, Castling::Both),
            _ => {}
        }
    }

    pub fn undo_move(&mut self) -> bool {
        if let Some(r#move) = self.moves.pop() {
            match r#move.move_type {
                MoveType::Capture {
                    origin,
                    piece,
                    captured_piece,
                    new,
                }
                | MoveType::CapturePromotion {
                    origin,
                    piece,
                    new,
                    captured_piece,
                } => {
                    return_false!(self.board.insert(Some(piece), origin));
                    return_false!(self.board.insert(Some(captured_piece), new));
                }
                MoveType::Move { origin, piece, new }
                | MoveType::MovePromotion { origin, piece, new } => {
                    return_false!(self.board.insert(Some(piece), origin));
                    return_false!(self.board.insert(None, new));
                }
                MoveType::EnPassant {
                    origin,
                    new,
                    piece,
                    captured_piece,
                    captured_pos,
                } => {
                    return_false!(self.board.insert(Some(piece), origin));
                    return_false!(self.board.insert(Some(captured_piece), captured_pos));
                    return_false!(self.board.insert(None, new));
                }
                MoveType::Castle {
                    king_origin,
                    king,
                    king_new,
                    rook_origin,
                    rook,
                    rook_new,
                } => {
                    return_false!(self.board.insert(Some(rook), rook_origin));
                    return_false!(self.board.insert(Some(king), king_origin));
                    return_false!(self.board.insert(None, king_new));
                    return_false!(self.board.insert(None, rook_new));
                }
                MoveType::Check => {}
            }
            self.en_passant = r#move.en_passant;
            self.castling_moves = r#move.castling;
            self.full_move_clock = r#move.full_move_clock;
            self.half_move_clock = r#move.half_move_clock;
            self.active_color = match self.active_color {
                Color::White => Color::Black,
                Color::Black => Color::White,
            };
            true
        } else {
            false
        }
    }
}

fn in_check(enemy_moves: &HashSet<MoveType<Pos, Colored<Piece>>>, player: Color) -> bool {
    enemy_moves.iter().any(|move_type| match move_type {
        MoveType::Capture { captured_piece, .. }
        | MoveType::CapturePromotion { captured_piece, .. } => {
            if Color::from(*captured_piece) == player {
                let piece = match captured_piece {
                    Colored::White(piece) | Colored::Black(piece) => piece,
                };
                piece == &Piece::King
            } else {
                false
            }
        }
        MoveType::Move { .. }
        | MoveType::MovePromotion { .. }
        | MoveType::EnPassant { .. }
        | MoveType::Castle { .. } => false,
        MoveType::Check => todo!(),
    })
}

impl Board {
    fn insert(&mut self, piece: Option<Colored<Piece>>, location: Pos) -> bool {
        self.board
            .get_mut(7 - (location.row as usize - 1))
            .and_then(|row| row.get_mut(location.column as usize - 1))
            .map(|square| *square = piece)
            .is_some()
    }
}

fn to_range(length: i8) -> Vec<i8> {
    if length.is_negative() {
        (length..0).rev().collect::<Vec<i8>>()
    } else {
        (0..=length).collect::<Vec<i8>>()
    }
}

fn to_range_both(start: i8, end: i8) -> Vec<i8> {
    if start > end {
        (start..=end).rev().collect::<Vec<i8>>()
    } else {
        (start..=end).collect::<Vec<i8>>()
    }
}

// fn to_range(length: i8) -> Vec<i8> {
//     to_range_both(length,0 )
// }

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

    use crate::{moves::MoveType, Color, Colored, GameState, Piece, Pos};

    #[test]
    fn benchmark_move_generation() {
        let mut gamestate = GameState::new();
        // move the white pawn at e2 to e4
        let r#move = MoveType::Move {
            origin: Pos::from_str("e2").unwrap(),
            new: Pos::from_str("e4").unwrap(),
            piece: Colored::White(Piece::Pawn),
        };
        gamestate.do_move(r#move, None);
        // move the black pawn at e7 to e5
        let r#move = MoveType::Move {
            origin: Pos::from_str("e7").unwrap(),
            new: Pos::from_str("e5").unwrap(),
            piece: Colored::Black(Piece::Pawn),
        };
        gamestate.do_move(r#move, None);
        println!("{}", gamestate.board);
        // move the white queen out from d1 to h5
        let r#move = MoveType::Move {
            origin: Pos::from_str("d1").unwrap(),
            new: Pos::from_str("h5").unwrap(),
            piece: Colored::White(Piece::Queen),
        };
        gamestate.do_move(r#move, None);
        println!("{}", gamestate.board);
        // move the black queen out from d8 to h4
        let r#move = MoveType::Move {
            origin: Pos::from_str("d8").unwrap(),
            new: Pos::from_str("h4").unwrap(),
            piece: Colored::Black(Piece::Queen),
        };
        let start = Instant::now();
        gamestate.do_move(r#move, None);
        let end = start.elapsed();
        println!("do {}ms", (end * 4000).as_micros());
        let start = Instant::now();
        gamestate.undo_move();
        let end = start.elapsed();
        println!("undo {}ms", (end * 4000).as_micros());
        println!("{}", gamestate.board);
        // move the black paw
        let start = Instant::now();
        let slmoves = gamestate.get_all_moves(Color::Black);
        let end = start.elapsed();
        println!(
            "{} psudo-legal moves generated in {:?}ms",
            slmoves.len(),
            end.as_micros()
        );
        // println!("{:?}", slmoves);
        let start = Instant::now();
        let moves = gamestate.get_all_valid_moves(Color::Black);
        assert_eq!(moves, gamestate.new_all_valid_moves(Color::Black));
        let end = start.elapsed();
        println!(
            "{} legal moves generated in {:?}ms",
            moves.len(),
            end.as_micros()
        );

        let start = Instant::now();
        let newmoves = gamestate.new_all_valid_moves(Color::Black);
        let end = start.elapsed();
        println!(
            "{} new legal moves generated in {:?}ms",
            moves.len(),
            end.as_micros()
        );
        println!("{:?}", newmoves);
        newmoves.difference(&moves).for_each(|x| println!("{}", x));
    }

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
        let mut game_state = GameState::new();
        let pos_moves = game_state.get_piece_moves(Pos::from_str("e1").unwrap(), false);
        assert_eq!(pos_moves.len(), 0);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("e2").unwrap(), false);
        assert_eq!(pos_moves.len(), 2);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("b1").unwrap(), false);
        assert_eq!(pos_moves.len(), 2);

        let mut game_state =
            GameState::from_str("rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
                .unwrap();
        println!("{}", game_state.board);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("e1").unwrap(), false);
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("f1").unwrap(), false);
        assert_eq!(pos_moves.len(), 5);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("d1").unwrap(), false);
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("h1").unwrap(), false);
        assert_eq!(pos_moves.len(), 1);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("e4").unwrap(), false);
        assert_eq!(pos_moves.len(), 1);

        // test pawn capture
        let mut game_state =
            GameState::from_str("rnbqkbnr/ppp1pppp/8/3p4/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2")
                .unwrap();
        println!("{}", game_state.board);
        let pos_moves = game_state.get_piece_moves(Pos::from_str("e4").unwrap(), false);
        assert_eq!(pos_moves.len(), 2);
    }

    #[test]
    fn generate_all_moves() {
        let mut game_state = GameState::new();
        println!(
            "{}",
            game_state
                .get_all_moves(Color::White)
                .iter()
                .map(|moves| { format!("\n{} ", moves) })
                .collect::<String>()
        );
    }

    #[test]
    fn board_insert() {
        let mut game_state = GameState::new();
        println!("{}", game_state.board);
        let moves = game_state.get_all_moves(Color::White);

        let i = moves.iter().next().unwrap();

        game_state.do_move(*i, None);
        println!("{}", game_state.board);
        let mut game_state =
            GameState::from_str("2kr3r/ppp2p1p/1b4nQ/1P3b2/P7/2q2NBB/3N1P1P/R3K2R w KQ - 0 23")
                .unwrap();
        let now = Instant::now();
        println!("{}", game_state.board);
        let valid_moves = game_state.get_all_valid_moves(Color::Black);
        println!("{}ms", now.elapsed().as_millis());
        game_state
            .new_all_valid_moves(Color::Black)
            .difference(&valid_moves)
            .for_each(|x| {
                println!("dif {}", x);
            });
        println!("{}ms", now.elapsed().as_millis());
        game_state
            .new_all_valid_moves(Color::Black)
            .difference(&valid_moves)
            .for_each(|x| {
                println!("dif {:?}", x);
            });
        assert_eq!(valid_moves, game_state.new_all_valid_moves(Color::Black));

        println!("{}ms", now.elapsed().as_millis());
        println!("{}", game_state.board);
        println!("{}", valid_moves.len());
        println!(
            "{}",
            valid_moves
                .iter()
                .map(|moves| { format!("\n{} ", moves) })
                .collect::<String>()
        );

        // check that there are no moves for black horse at c6
        assert_eq!(
            valid_moves.iter().any(|move_| match *move_ {
                MoveType::Capture { origin, piece, .. } | MoveType::Move { origin, piece, .. } =>
                    match piece {
                        Colored::Black(Piece::Knight) => origin == Pos::new(6, 3),
                        _ => false,
                    },
                MoveType::CapturePromotion { .. } => false,
                MoveType::MovePromotion { .. } => false,
                MoveType::EnPassant { .. } => false,
                MoveType::Castle { .. } => false,
                MoveType::Check => false,
            }),
            false
        )
    }

    #[test]
    fn test_promotion() {
        let mut game_state = GameState::from_str("8/8/8/8/8/8/7p/8 b - - 0 1").unwrap();
        let moves = game_state.get_all_moves(Color::Black);
        println!("{}", game_state.board);
        let i = moves
            .iter()
            .find(|move_| match *move_ {
                MoveType::MovePromotion { .. } => true,
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, Some(Colored::Black(Piece::Queen)));
        println!("{}", game_state.board);

        assert_eq!(
            game_state.board.get_cell(Pos::new(1, 8)),
            Some(&Some(Colored::Black(Piece::Queen)))
        );
    }

    #[test]
    fn simulate_en_passant() {
        let mut game_state = GameState::new();
        println!("{}", game_state.board);
        let moves = game_state.get_all_moves(Color::White);
        // move pawn at e2 to e4
        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Move { origin, new, .. } => {
                    (new == Pos::new(4, 5)) && (origin == Pos::new(2, 5))
                }
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);

        println!("{}", game_state.board);

        // move pawn at d7 to e5

        let moves = game_state.get_all_moves(Color::Black);
        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Move { origin, new, .. } => {
                    (new == Pos::new(5, 4)) && (origin == Pos::new(7, 4))
                }
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);
        println!("{}", game_state.board);

        // move pawn at e4 to e5
        let moves = game_state.get_all_moves(Color::White);
        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                // MoveType::Move(_, pos) => pos == Pos::new(5, 5),
                MoveType::Move { new, .. } => new == Pos::new(5, 5),

                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);
        println!("{}", game_state.board);

        // move pawn at f7 to f5
        let moves = game_state.get_all_moves(Color::Black);

        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Move { origin, new, .. } => {
                    (new == Pos::new(5, 6)) && (origin == Pos::new(7, 6))
                }
                _ => false,
            })
            .unwrap();

        game_state.do_move(*i, None);
        println!("{}", game_state.board);

        // move pawn at e5 to f6
        let moves = game_state.get_all_moves(Color::White);

        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::EnPassant { new, .. } => new == Pos::new(6, 6),
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);
        println!("{}", game_state.board);

        // move from d5 to d4
        let moves = game_state.get_all_moves(Color::Black);

        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Move { new, .. } => new == Pos::new(4, 4),
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);
        println!("{}", game_state.board);

        // move from c2 to c4
        let moves = game_state.get_all_moves(Color::White);

        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Move { origin, new, .. } => {
                    (new == Pos::new(4, 3)) && (origin == Pos::new(2, 3))
                }
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);
        println!("{}", game_state.board);

        // move from d4 to c3 (en passant)
        let moves = game_state.get_all_moves(Color::Black);
        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::EnPassant { new, .. } => new == Pos::new(3, 3),
                _ => false,
            })
            .unwrap();
        game_state.do_move(*i, None);
        println!("{}", game_state.board);
    }

    #[test]
    fn valid_moves() {
        let mut game =
            GameState::from_str("r2qk2r/pp2ppbp/2n2np1/1B1p4/3pP3/5N2/PPP2PPP/RNBQ1RK1 w kq - 0 2")
                .unwrap();
        println!("{}", game.board);
        let moves = game.get_all_valid_moves(Color::Black);
        assert_eq!(moves, game.new_all_valid_moves(Color::Black));
        println!(
            "{:?}",
            moves
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
        );
        // check that there are no moves for the knight at c6
        assert_eq!(
            moves.iter().any(|movetype| match *movetype {
                MoveType::Move { origin, .. } => origin == Pos::new(6, 3),
                _ => false,
            }),
            false
        );
    }

    #[test]
    fn castles() {
        let mut game =
            GameState::from_str("rnbqkbnr/ppp2ppp/8/3pp3/2B1P3/7N/PPPP1PPP/RNBQK2R w KQkq - 0 4")
                .unwrap();
        let moves = game.get_piece_moves(Pos { row: 1, column: 5 }, false);

        println!("{}", game.board);
        // do the castle
        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Castle { .. } => true,
                _ => false,
            })
            .unwrap();
        game.do_move(*i, None);
        println!("{}", game.board);
        let mut game = GameState::from_str(
            "rnbqk2r/pppp1ppp/3b3n/4p3/4P3/3P1P2/PPP1B1PP/RNBQK1NR b KQkq - 0 4",
        )
        .unwrap();
        let moves = game.get_piece_moves(Pos { row: 8, column: 5 }, false);

        println!("{}", game.board);
        // do the castle
        let i = moves
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Castle { .. } => true,
                _ => false,
            })
            .unwrap();
        game.do_move(*i, None);
        println!("{}", game.board);
        let mut game = GameState::from_str(
            "r3kbnr/ppp1pppp/2nq4/3p1b2/3P1B2/2NQ4/PPP1PPPP/R3KBNR w KQkq - 6 5",
        )
        .unwrap();
        let moves_white = game.get_piece_moves(Pos { row: 1, column: 5 }, false);
        let moves_black = game.get_piece_moves(Pos { row: 8, column: 5 }, false);
        println!("{}", game.board);
        // do the castle
        let i = moves_white
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Castle { .. } => true,
                _ => false,
            })
            .unwrap();
        game.do_move(*i, None);
        println!("{}", game.board);
        let i = moves_black
            .iter()
            .find(|movetype| match **movetype {
                MoveType::Castle { .. } => true,
                _ => false,
            })
            .unwrap();
        game.do_move(*i, None);
        println!("{}", game.board);
    }

    #[test]
    fn valid_castles() {
        let mut game =
            GameState::from_str("rnbqkbnr/ppp2ppp/8/3pp3/2B1P3/7N/PPPP1PPP/RNBQK2R w KQkq - 0 4")
                .unwrap();
        let moves = game.get_all_valid_moves(Color::White);
        assert_eq!(moves, game.new_all_valid_moves(Color::White));
        println!("{}", game.board);
        println!(
            "{:?}",
            moves
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
        );
        // check that there are no moves for the knight at c6
        assert_eq!(
            moves.iter().any(|movetype| match *movetype {
                MoveType::Castle { .. } => true,
                _ => false,
            }),
            true
        );
    }

    #[test]
    fn test_knight_capturing() {
        let mut game =
            GameState::from_str("rnbqkbnr/ppp1pppp/8/3p4/8/2N5/PPPPPPPP/R1BQKBNR w KQkq - 2 2")
                .unwrap();
        let moves = game.get_piece_moves(Pos { row: 3, column: 3 }, false);
        println!("{}", game.board);
        println!(
            "{:?}",
            moves
                .iter()
                .map(std::string::ToString::to_string)
                .collect::<Vec<_>>()
        );
    }
}
