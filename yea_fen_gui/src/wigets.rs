// This is a slightly modified version of the original ImageButton widget from egui

use eframe::{
    egui::{Image, Response, Sense, Ui, Widget, WidgetInfo, WidgetType},
    epaint::{Color32, Rect, Rounding, TextureId, Vec2},
};
#[must_use = "You should put this widget in an ui with `ui.add(widget);`"]
#[derive(Clone, Debug)]
pub struct ImageButton {
    image: Image,
    sense: Sense,
    frame: bool,
    selected: bool,
    bg: Color32,
}
impl ImageButton {
    pub fn new(texture_id: impl Into<TextureId>, size: impl Into<Vec2>, bg: Color32) -> Self {
        Self {
            image: Image::new(texture_id, size),
            sense: Sense::click(),
            frame: true,
            selected: false,
            bg,
        }
    }

    /// Select UV range. Default is (0,0) in top-left, (1,1) bottom right.
    pub fn uv(mut self, uv: impl Into<Rect>) -> Self {
        self.image = self.image.uv(uv);
        self
    }

    /// Multiply image color with this. Default is WHITE (no tint).
    pub fn tint(mut self, tint: impl Into<Color32>) -> Self {
        self.image = self.image.tint(tint);
        self
    }

    /// If `true`, mark this button as "selected".
    pub fn selected(mut self, selected: bool) -> Self {
        self.selected = selected;
        self
    }

    /// Turn off the frame
    pub fn frame(mut self, frame: bool) -> Self {
        self.frame = frame;
        self
    }

    /// By default, buttons senses clicks.
    /// Change this to a drag-button with `Sense::drag()`.
    pub fn sense(mut self, sense: Sense) -> Self {
        self.sense = sense;
        self
    }
}

impl Widget for ImageButton {
    fn ui(self, ui: &mut Ui) -> Response {
        let Self {
            image,
            sense,
            frame,
            selected,
            bg,
        } = self;

        let padding = if frame {
            // so we can see that it is a button:
            Vec2::splat(ui.spacing().button_padding.x)
        } else {
            Vec2::ZERO
        };
        let padded_size = image.size() + 2.0 * padding;
        let (rect, response) = ui.allocate_exact_size(padded_size, sense);
        response.widget_info(|| WidgetInfo::new(WidgetType::ImageButton));

        if ui.is_rect_visible(rect) {
            let (expansion, rounding, _, stroke) = if selected {
                let selection = ui.visuals().selection;
                (
                    Vec2::ZERO,
                    Rounding::none(),
                    selection.bg_fill,
                    selection.stroke,
                )
            } else if frame {
                let visuals = ui.style().interact(&response);
                let expansion = Vec2::splat(visuals.expansion);
                (
                    expansion,
                    visuals.rounding,
                    visuals.bg_fill,
                    visuals.bg_stroke,
                )
            } else {
                Default::default()
            };

            // Draw frame background (for transparent images):
            ui.painter()
                .rect_filled(rect.expand2(expansion), rounding, bg);

            let image_rect = ui
                .layout()
                .align_size_within_rect(image.size(), rect.shrink2(padding));
            // let image_rect = image_rect.expand2(expansion); // can make it blurry, so let's not
            image.paint_at(ui, image_rect);

            // Draw frame outline:
            ui.painter()
                .rect_stroke(rect.expand2(expansion), rounding, stroke);
        }

        response
    }
}
