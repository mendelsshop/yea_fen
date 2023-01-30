#![warn(clippy::pedantic, clippy::nursery, clippy::cargo)]
#![deny(clippy::use_self, rust_2018_idioms)]
#![allow(clippy::must_use_candidate)]

use eframe::{
    egui::{self, Frame, Separator, TextEdit, Ui},
    emath::Align,
    epaint::{Color32, Vec2},
};
use egui_extras::RetainedImage;
use std::{
    collections::HashMap,
    collections::HashSet,
    str::FromStr,
    sync::mpsc::{Receiver, Sender},
    time::Duration,
};
use wigets::ImageButton;
use yea_fen::chess_engines::minimax::{negamax};
pub mod wigets;
use yea_fen::{
    chess_engines::{
        random::random_move, random_capture::random_capture,
        random_maximize_capture::random_maximize_capture,
    },
    moves::GameResult,
    moves::MoveType,
    Color, Colored, GameState, Piece, Pos,
};
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PromotedPiece {
    Queen,
    Rook,
    Bishop,
    Knight,
}

impl From<PromotedPiece> for Piece {
    fn from(piece: PromotedPiece) -> Self {
        match piece {
            PromotedPiece::Queen => Self::Queen,
            PromotedPiece::Rook => Self::Rook,
            PromotedPiece::Bishop => Self::Bishop,
            PromotedPiece::Knight => Self::Knight,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Player {
    Human,
    Computer(Computer),
}
impl Player {
    pub fn is_computer(&self) -> bool {
        match self {
            Self::Human => false,
            Self::Computer(_) => true,
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Computer {
    Random,
    RandomCapture,
    RandomMaximizeCapture,
    Minimax(usize),
}
pub struct ChessApp {
    gamestate: GameState,
    pieces: HashMap<Piece, RetainedImage>,
    blank: RetainedImage,
    mode: AppType,
    input_fen: String,
    fen: String,
    color: Color,
    current_piece: Option<(Pos, Colored<Piece>)>,
    moves: HashSet<MoveType<Pos, Colored<Piece>>>,
    move_blank: RetainedImage,
    special_move: RetainedImage,
    move_todo: Option<MoveType<Pos, Colored<Piece>>>,
    piece_promotion: PromotedPiece,
    promotion: bool,
    black: Player,
    white: Player,
    done_cfg: bool,
    not_blank: RetainedImage,
    aisender: Sender<(Computer, Color, GameState)>,
    sent: bool,
    recv: Receiver<(Color, HashSet<MoveType<Pos, Colored<Piece>>>, GameState)>,
    theme: bool,
    white_color: Color32,
    black_color: Color32,
    black_square: Color32,
    white_square: Color32,
    last_black: Color32,
    last_white: Color32,
    flip: bool,
    black_depth: usize,
    white_depth: usize,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AppType {
    Display,
    Game,
}

macro_rules! load_image {
    ($name:expr) => {
        RetainedImage::from_image_bytes("a.png", include_bytes!($name)).unwrap()
    };
}

impl ChessApp {
    pub fn new(
        _cc: &eframe::CreationContext<'_>,
        aisender: Sender<(Computer, Color, GameState)>,
        recv: Receiver<(Color, HashSet<MoveType<Pos, Colored<Piece>>>, GameState)>,
    ) -> Self {
        // we have to load the images here from the resources folder and then store them in a hashmap using load_image! macro
        // each image is named by the piece color_name.png
        let mut images = HashMap::new();
        images.insert(Piece::Pawn, load_image!("../resources/pawn.png"));
        images.insert(Piece::Rook, load_image!("../resources/rook.png"));
        images.insert(Piece::Knight, load_image!("../resources/knight.png"));
        images.insert(Piece::Bishop, load_image!("../resources/bishop.png"));
        images.insert(Piece::Queen, load_image!("../resources/queen.png"));
        images.insert(Piece::King, load_image!("../resources/king.png"));
        let mut gamestate = GameState::new();
        Self {
            pieces: images,
            blank: load_image!("../resources/blank.png"),
            move_blank: load_image!("../resources/move.png"),
            special_move: load_image!("../resources/special_move.png"),
            not_blank: load_image!("../resources/not_blank.png"),
            mode: AppType::Game,
            input_fen: String::new(),
            fen: gamestate.clone().to_string(),
            color: gamestate.get_active_color(),
            current_piece: None,
            moves: gamestate.get_all_valid_moves(gamestate.get_active_color()),
            move_todo: None,
            gamestate,
            piece_promotion: PromotedPiece::Queen,
            promotion: false,
            black: Player::Human,
            white: Player::Human,
            done_cfg: false,
            aisender,
            sent: false,
            recv,
            theme: false,
            white_color: Color32::WHITE,
            black_color: Color32::BLACK,
            black_square: Color32::BROWN,
            white_square: Color32::GOLD,
            last_black: Color32::from_rgb(25, 25, 25),
            last_white: Color32::from_rgb(200, 200, 200),
            flip: true,
            white_depth: 3,
            black_depth: 3,
        }
    }

    fn new_game(&mut self, ui: &mut egui::Ui, end: bool) {
        // change black and white to using a selectable value
        self.gamestate = GameState::new();
        self.color = self.gamestate.get_active_color();
        self.moves = self.gamestate.get_all_valid_moves(self.color);
        self.current_piece = None;
        self.move_todo = None;
        self.fen = self.gamestate.clone().to_string();
        self.promotion = false;
        if end {
            return;
        }
        self.mode_selector(ui);
    }

    fn mode_selector(&mut self, ui: &mut egui::Ui) {
        egui::ComboBox::from_id_source("black")
            .selected_text("Black: mode")
            .show_ui(ui, |ui| {
                ui.selectable_value(&mut self.black, Player::Human, "Human");
                ui.separator();
                ui.selectable_value(
                    &mut self.black,
                    Player::Computer(Computer::Random),
                    "Computer random",
                );
                ui.separator();
                ui.selectable_value(
                    &mut self.black,
                    Player::Computer(Computer::RandomCapture),
                    "Computer random capture",
                );
                ui.separator();
                ui.selectable_value(
                    &mut self.black,
                    Player::Computer(Computer::RandomMaximizeCapture),
                    "Computer random maximize capture",
                );
                ui.separator();
                ui.selectable_value(
                    &mut self.black,
                    Player::Computer(Computer::Minimax(self.black_depth)),
                    format!("Computer minimax {}", self.black_depth),
                );
                if let Player::Computer(Computer::Minimax(_)) = self.black {
                    ui.add(
                        egui::Slider::new(&mut self.black_depth, 1..=50)
                            .show_value(false)
                            .step_by(1.),
                    );
                }
            });
        egui::ComboBox::from_id_source("white")
            .selected_text("White: mode")
            .show_ui(ui, |ui| {
                ui.selectable_value(&mut self.white, Player::Human, "Human");
                ui.separator();
                ui.selectable_value(
                    &mut self.white,
                    Player::Computer(Computer::Random),
                    "Computer random",
                );
                ui.separator();
                ui.selectable_value(
                    &mut self.white,
                    Player::Computer(Computer::RandomCapture),
                    "Computer random capture",
                );
                ui.separator();
                ui.selectable_value(
                    &mut self.white,
                    Player::Computer(Computer::RandomMaximizeCapture),
                    "Computer random maximize capture",
                );
                ui.separator();
                ui.selectable_value(
                    &mut self.white,
                    Player::Computer(Computer::Minimax(self.white_depth)),
                    format!("Computer minimax {}", self.white_depth),
                );
                if let Player::Computer(Computer::Minimax(_)) = self.white {
                    ui.add(
                        egui::Slider::new(&mut self.white_depth, 1..=50)
                            .show_value(false)
                            .step_by(0.5),
                    );
                }
            });
    }

    fn new_game_button(&mut self, ui: &mut egui::Ui, frame: &mut eframe::Frame) {
        self.mode_selector(ui);

        if ui.button("New Game").clicked() {
            self.new_game(ui, true);
            println!("new game");
        } else if ui.button("quit app").clicked() {
            frame.close()
        }
    }
}

impl eframe::App for ChessApp {
    fn update(&mut self, ctx: &egui::Context, frame: &mut eframe::Frame) {
        if self.sent {
            if let Ok((color, moves, gamestate)) =
                self.recv.recv_timeout(Duration::from_millis(100))
            {
                self.color = color;
                self.moves = moves;
                self.gamestate = gamestate;
                self.sent = false;
                self.mode = AppType::Game;
                println!("recieved");
            } else {
            }
        }
        // check if the game just started
        if !self.done_cfg {
            // create a new window
            egui::Window::new("New Game")
                .open(&mut true)
                .show(ctx, |ui| {
                    self.new_game(ui, false);
                    if ui.button("start").clicked() {
                        self.done_cfg = true;
                    }
                });
        }

        ctx.request_repaint();
        // check if black ai is in minimax if it is then set to its depth to self.black_depth
        if let Player::Computer(Computer::Minimax(_)) = self.black {
            self.black = Player::Computer(Computer::Minimax(self.black_depth));
        }
        // check if white ai is in minimax if it is then set to its depth to self.white_depth
        if let Player::Computer(Computer::Minimax(_)) = self.white {
            self.white = Player::Computer(Computer::Minimax(self.white_depth));
        }
        let moves = if let Some((pos, piece)) = self.current_piece {
            self.moves
                .iter()
                .filter(|m| m.is_from((pos, piece)))
                .copied()
                .collect::<HashSet<_>>()
        } else {
            HashSet::new()
        };
        if self.theme {
            egui::Window::new("themeing ")
                .open(&mut true)
                .show(ctx, |ui| {
                    // ui.horizontal_top(|ui| {
                    ui.allocate_ui_with_layout(
                        Vec2 { x: 225.0, y: 100.0 },
                        egui::Layout::left_to_right(Align::TOP),
                        |ui| {
                            ui.horizontal_wrapped(|ui| {
                                ui.vertical(|ui| {
                                    ui.horizontal_wrapped(|ui| {
                                        ui.vertical(|ui| {
                                            ui.label(" white piece ");
                                            ui.color_edit_button_srgba(&mut self.white_color);
                                        });
                                        ui.separator();
                                        ui.vertical(|ui| {
                                            ui.label(" black piece ");
                                            ui.color_edit_button_srgba(&mut self.black_color);
                                        });
                                    });
                                    ui.add(Separator::default().horizontal());
                                    ui.horizontal_wrapped(|ui| {
                                        ui.vertical(|ui| {
                                            ui.label("white square");
                                            ui.color_edit_button_srgba(&mut self.white_square);
                                        });
                                        ui.separator();
                                        ui.vertical(|ui| {
                                            ui.label("black square");
                                            ui.color_edit_button_srgba(&mut self.black_square);
                                        });
                                    });
                                    ui.add(Separator::default().horizontal());
                                    ui.horizontal_wrapped(|ui| {
                                        ui.vertical(|ui| {
                                            ui.label("   last white   ");
                                            ui.color_edit_button_srgba(&mut self.last_white);
                                        });
                                        ui.separator();
                                        ui.vertical(|ui| {
                                            ui.label("   last black   ");
                                            ui.color_edit_button_srgba(&mut self.last_black);
                                        });
                                    });
                                    ui.add(Separator::default().horizontal());
                                    if ui.button("reset").clicked() {
                                        self.black_color = Color32::BLACK;
                                        self.white_color = Color32::WHITE;
                                        self.black_square = Color32::BROWN;
                                        self.white_square = Color32::GOLD;
                                        self.last_black = Color32::from_rgb(25, 25, 25);
                                        self.last_white = Color32::from_rgb(200, 200, 200)

                                    }
                                    ui.add(Separator::default().horizontal());
                                });
                                ui.vertical(|ui| {
                                    ui.separator();
                                    self.mode_selector(ui);
                                    ui.separator();
                                    ui.checkbox(&mut self.flip, "flip board");
                                });
                            });
                        },
                    );
                    if ui.button("done").clicked() {
                        self.theme = false;
                    }
                });
        }
        if let Some(MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. }) =
            self.move_todo
        {
            self.promotion = true;
        }
        match self.gamestate.get_gameresult() {
            yea_fen::moves::GameResult::CheckMate(color) => {
                let winner = match color {
                    Color::White => "Black",
                    Color::Black => "White",
                };
                egui::Window::new("result").open(&mut true).show(ctx, |ui| {
                    ui.label(format!("{winner} won"));
                    self.new_game_button(ui, frame);
                });
            }
            yea_fen::moves::GameResult::StaleMate | yea_fen::moves::GameResult::Draw | GameResult::ThreeFoldRepetition => {
                egui::Window::new("result").open(&mut true).show(ctx, |ui| {
                    ui.label("Draw");
                    self.new_game_button(ui, frame);
                });
            }
            yea_fen::moves::GameResult::InProgress => {}
        }

        if self.promotion {
            let mut promotion = self.promotion;
            egui::Window::new("piece promotion")
                .open(&mut self.promotion)
                .show(ctx, |ui| {
                    ui.horizontal(|ui| {
                        ui.selectable_value(
                            &mut self.piece_promotion,
                            PromotedPiece::Queen,
                            "Queen",
                        );
                        ui.selectable_value(&mut self.piece_promotion, PromotedPiece::Rook, "Rook");
                        ui.selectable_value(
                            &mut self.piece_promotion,
                            PromotedPiece::Bishop,
                            "Bishop",
                        );
                        ui.selectable_value(
                            &mut self.piece_promotion,
                            PromotedPiece::Knight,
                            "Knight",
                        );
                    });
                    if ui.button("ok").clicked() {
                        promotion = false;
                    }
                });
            self.promotion = promotion;
        }

        if let Some(from) = self.move_todo {
            if let MoveType::MovePromotion { .. } | MoveType::CapturePromotion { .. } = from {
                let piece = Piece::from(self.piece_promotion);
                // set the pieces color to the color of the piece that is being promoted
                let piece = match self.current_piece {
                    Some((_, c_piece)) => match c_piece {
                        Colored::White(_) => Colored::White(piece),
                        Colored::Black(_) => Colored::Black(piece),
                    },
                    None => panic!("no piece"),
                };
                if !self.promotion {
                    self.gamestate.do_move(from, Some(piece));
                    println!("human move {from} {piece}");
                    println!("human move{:?}", self.gamestate.get_castling_moves());
                    self.move_todo = None;
                    self.moves = self
                        .gamestate
                        .new_all_valid_moves(self.gamestate.get_active_color());
                    self.color = self.gamestate.get_active_color();
                }
            } else {
                println!("human move {from}");
                println!("human move{:?}", self.gamestate.get_castling_moves());
                self.gamestate.do_move(from, None);
                self.move_todo = None;
                self.moves = self
                    .gamestate
                    .new_all_valid_moves(self.gamestate.get_active_color());
                self.color = self.gamestate.get_active_color();
            }
        }
        if self.color == Color::Black
            && self.gamestate.get_gameresult() == GameResult::InProgress
            && self.done_cfg
            && self.mode == AppType::Game
        {
            match self.black {
                Player::Human => {}
                Player::Computer(computer) => {
                    println!("send command");
                    self.aisender
                        .send((computer, self.color, self.gamestate.clone()))
                        .unwrap();
                    self.sent = true;
                    self.mode = AppType::Display;
                }
            }
        } else if self.color == Color::White
            && self.gamestate.get_gameresult() == GameResult::InProgress
            && self.done_cfg
            && self.mode == AppType::Game
        {
            match self.white {
                Player::Human => {}
                Player::Computer(computer) => {
                    println!("send command");
                    self.aisender
                        .send((computer, self.color, self.gamestate.clone()))
                        .unwrap();
                    self.sent = true;
                    self.mode = AppType::Display;
                }
            }
        }
        let mut t = |b, pos: Pos, ui: &mut Ui, bcolor| {
            match b {
                Some(piece) => {
                    let piece_no = Piece::from(piece);
                    let color = Color::from(piece);
                    let texture = self.pieces.get(&piece_no).unwrap();
                    let was_last_move = self
                        .gamestate
                        .get_past_moves()
                        .last()
                        .map_or(false, |m| m.get_type().to() == pos);

                    let tint = if was_last_move {
                        match self
                            .gamestate
                            .get_past_moves()
                            .last()
                            .unwrap()
                            .get_type()
                            .color()
                        {
                            Color::Black => Color32::from_rgb(25, 25, 25),
                            Color::White => Color32::from_rgb(200, 200, 200),
                        }
                    } else if color == Color::Black {
                        self.black_color
                    } else {
                        self.white_color
                    };
                    match self.mode {
                        AppType::Display => {
                            ui.add_enabled(
                                false,
                                ImageButton::new(
                                    texture.texture_id(ctx),
                                    texture.size_vec2(),
                                    bcolor,
                                )
                                .tint(tint),
                            );
                        }
                        AppType::Game => {
                            let move_type = moves.iter().find(|m| match self.current_piece {
                                Some(_) => m.to() == pos,
                                None => false,
                            });
                            // if move_type is some then use self.captured_pieces
                            // else use self.pieces
                            if let Some(move_type) = move_type {
                                // let texture = self.captured_pieces.get(&piece).unwrap();
                                let size = texture.size_vec2();
                                if ui
                                    .add(
                                        ImageButton::new(texture.texture_id(ctx), size, bcolor)
                                            .tint(Color32::DARK_RED),
                                    )
                                    .clicked()
                                {
                                    self.move_todo = Some(*move_type);
                                }
                            } else {
                                let size = texture.size_vec2();
                                if ui
                                    .add(
                                        ImageButton::new(texture.texture_id(ctx), size, bcolor)
                                            .tint(tint),
                                    )
                                    .clicked()
                                {
                                    if self.current_piece == Some((pos, piece)) {
                                        self.current_piece = None;
                                    } else {
                                        self.current_piece = Some((pos, piece));
                                    }
                                }
                            }
                        }
                    }
                }
                None => {
                    let was_last_move = self
                        .gamestate
                        .get_past_moves()
                        .last()
                        .map_or(false, |m| m.get_type().from().0 == pos);
                    let tint = if was_last_move {
                        match self
                            .gamestate
                            .get_past_moves()
                            .last()
                            .unwrap()
                            .get_type()
                            .color()
                        {
                            Color::Black => Color32::BLACK,
                            Color::White => Color32::WHITE,
                        }
                    } else {
                        Color32::TEMPORARY_COLOR
                    };
                    match self.mode {
                        AppType::Display => {
                            ui.add_enabled(
                                false,
                                ImageButton::new(
                                    if !(was_last_move) {
                                        self.blank.texture_id(ctx)
                                    } else {
                                        self.not_blank.texture_id(ctx)
                                    },
                                    self.blank.size_vec2(),
                                    bcolor,
                                )
                                .tint(tint),
                            );
                        }
                        AppType::Game => {
                            // check if there is a move from this position using the .to() method
                            // if there is, then use the move_blank texture
                            // if there is a move from this position, and it is a special move, use the special_move texture
                            let move_type = moves.iter().find(|m| match self.current_piece {
                                Some(_) => m.to() == pos,
                                None => false,
                            });
                            if let Some(m) = move_type {
                                if m.is_special() {
                                    if ui
                                        .add(ImageButton::new(
                                            self.special_move.texture_id(ctx),
                                            self.special_move.size_vec2(),
                                            bcolor,
                                        ))
                                        .clicked()
                                    {
                                        self.move_todo = Some(*m);
                                    }
                                } else if ui
                                    .add(ImageButton::new(
                                        self.move_blank.texture_id(ctx),
                                        self.move_blank.size_vec2(),
                                        bcolor,
                                    ))
                                    .clicked()
                                {
                                    self.move_todo = Some(*m);
                                }
                            } else if ui
                                .add(
                                    ImageButton::new(
                                        if !(was_last_move) {
                                            self.blank.texture_id(ctx)
                                        } else {
                                            self.not_blank.texture_id(ctx)
                                        },
                                        self.blank.size_vec2(),
                                        bcolor,
                                    )
                                    .tint(tint),
                                )
                                .clicked()
                            {
                            }
                        }
                    }
                }
            }
        };

        let frame = Frame::none()
            .fill(egui::Color32::TRANSPARENT)
            .outer_margin(egui::vec2(10.0, 10.0));
        match (self.color, self.flip) {
            (Color::Black, true) => {
                egui::SidePanel::left("chess")
                    // .frame(frame)
                    .show(ctx, |ui| {
                        for (idx, i) in self.gamestate.get_board().into_iter().rev().enumerate() {
                            ui.horizontal(|ui| {
                                ui.label(format!("\n\n{} ", (idx + 1)));
                                for (idxx, p) in i.iter().rev().enumerate() {
                                    let pos = Pos::new(idx as u8 + 1, 8 - (idxx as u8));
                                    // if row is is odd and column is odd or column is even and row is even black else white
                                    let color = if (pos.get_row() % 2 == 1
                                        && pos.get_column() % 2 == 1)
                                        || (pos.get_row() % 2 == 0 && pos.get_column() % 2 == 0)
                                    {
                                        self.black_square
                                    } else {
                                        self.white_square
                                    };

                                    // t(*p, Pos::new(idx as u8 + 1, 8 - (idxx as u8)), ui, color);
                                    if idx == 7 {
                                        ui.vertical(|ui| {
                                            t(
                                                *p,
                                                Pos::new(idx as u8 + 1, 8 - (idxx as u8)),
                                                ui,
                                                color,
                                            );
                                            ui.label(format!(
                                                "\t \t{}",
                                                (8 - idxx as u8 + 96) as char
                                            ));
                                        });
                                    } else {
                                        t(*p, Pos::new(idx as u8 + 1, 8 - (idxx as u8)), ui, color);
                                    }
                                }
                            });
                        }
                    });
            }
            _ => {
                egui::SidePanel::left("chess")
                    // .frame(frame)
                    .show(ctx, |ui| {
                        for (idx, i) in self.gamestate.get_board().into_iter().enumerate() {
                            ui.horizontal(|ui| {
                                ui.label(format!("\n\n{} ", 8 - (idx)));
                                for (idxx, p) in i.iter().enumerate() {
                                    let pos = Pos::new(idx as u8 + 1, idxx as u8 + 1);
                                    let color = if (pos.get_row() % 2 == 0
                                        && pos.get_column() % 2 == 1)
                                        || (pos.get_row() % 2 == 1 && pos.get_column() % 2 == 0)
                                    {
                                        self.black_square
                                    } else {
                                        self.white_square
                                    };
                                    if idx == 7 {
                                        ui.vertical(|ui| {
                                            t(
                                                *p,
                                                Pos::new(8 - (idx as u8), idxx as u8 + 1),
                                                ui,
                                                color,
                                            );
                                            ui.label(format!("\t \t{}", (idxx as u8 + 97) as char));
                                        });
                                    } else {
                                        t(*p, Pos::new(8 - (idx as u8), idxx as u8 + 1), ui, color);
                                    }
                                }
                            });
                        }
                    });
            }
        }

        egui::SidePanel::left("ctrl")
            .frame(frame)
            .resizable(false)
            .show(ctx, |ui| {
                // game/display toggle
                egui::ComboBox::from_id_source("mode")
                    .selected_text("mode")
                    .show_ui(ui, |ui| {
                        ui.selectable_value(&mut self.mode, AppType::Display, "display");
                        ui.selectable_value(&mut self.mode, AppType::Game, "game");
                    });
                // fen text input with button to load
                ui.add(
                    TextEdit::singleline(&mut self.input_fen)
                        .hint_text("fen")
                        .code_editor(),
                );
                let resp = ui.button("go");
                if resp.clicked() {
                    self.gamestate = GameState::from_str(&self.input_fen)
                        .map_or(Err("could not retrive gamestate from fen"), |ok| {
                            self.color = ok.get_active_color();
                            self.input_fen.clear();
                            Ok(ok)
                        })
                        .unwrap_or(self.gamestate.clone());

                    self.fen = self.gamestate.to_string();
                }

                if self.mode == AppType::Display {
                    egui::ComboBox::from_id_source("color")
                        .selected_text("color")
                        .show_ui(ui, |ui| {
                            ui.selectable_value(&mut self.color, Color::Black, "black");
                            ui.selectable_value(&mut self.color, Color::White, "white");
                        });
                }

                // current fen string label
                ui.label("current FEN string:");
                if ui
                    .button(&self.gamestate.to_string())
                    .on_hover_text("Click to copy")
                    .clicked()
                {
                    ui.output().copied_text = self.gamestate.to_string();
                }
                if ui.button("new game").clicked() {
                    self.done_cfg = false;
                }
                if ui.button("theme/config").clicked() {
                    self.theme = true
                }
            });
    }
}

pub fn threads(
    rx: Receiver<(Computer, Color, GameState)>,
    send: Sender<(Color, HashSet<MoveType<Pos, Colored<Piece>>>, GameState)>,
) {
    std::thread::spawn(move || {
        println!("thread");
        loop {
            if let Ok(rc) = rx.recv() {
                println!("received message");
                let color = rc.1;
                let ai = rc.0;
                let mut gamestate = rc.2;
                println!("getting move");
                let r#move = match ai {
                    Computer::Random => random_move(&mut gamestate),
                    Computer::RandomCapture => random_capture(&mut gamestate),
                    Computer::RandomMaximizeCapture => random_maximize_capture(&mut gamestate),
                    Computer::Minimax(depth) => {
                        println!("color {color:?} depth {}", depth);
                        negamax(&mut gamestate, depth)
                    }
                };
                println!("doing move");
                {
                    if let Some(r#move) = r#move {
                        if gamestate.do_move(r#move.0, r#move.1) {
                            println!("computer moved {}", r#move.0);
                            let send_c = match color {
                                Color::Black => Color::White,
                                Color::White => Color::Black,
                            };
                            let color = gamestate.get_active_color();
                            let moves = gamestate.get_all_valid_moves(color);
                            println!("done");
                            send.send((send_c, moves, gamestate)).unwrap();
                        } else {
                            for r#move in gamestate.get_past_moves() {
                                println!("{}", r#move.get_type());
                            }
                            println!("could not move"); //
                        }
                    } else {
                        for r#move in gamestate.get_past_moves() {
                            println!("{}", r#move.get_type());
                        }
                        println!("could not move");
                    }
                }
            }
        }
    });
}
