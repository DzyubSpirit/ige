use std::sync::{Arc, RwLock};
use std::rc::Rc;
use std::sync::mpsc::{Receiver, Sender};
use std::cell::Cell;
use std::char;
use std::str::FromStr;

use std::f64::consts::PI;

use cgmath::{Point2, Vector2, Matrix2, EuclideanSpace};

use gtk;
use gtk::prelude::*;
use gtk::DrawingArea;

use cairo::{Context, FontFace};
use cairo::enums::{FontWeight, FontSlant};

use gdk::enums::key::Key;
use gdk::ModifierType;

use messages::Event::*;

/// The primitive for drawing
/// Coordinates are in the range [0.0,1.0], and are transformed by DisplayData
pub enum Shape {
    Line { p1: Point2<f64>, p2: Point2<f64> },
    Dot { p: Point2<f64>, r: f64 },
    Text { lower_left: Point2<f64>, text: String }
}

/// A struct representing what should currently be drawn on the screen
/// Essentially like SVG, but with some metadata as well, like what to send back
/// on a selection
pub struct DisplayInput {
    pub shapes: Vec<Shape>,
    /// These are like callbacks, they tell where the nodes are and what should
    /// be sent back when a specific node is selected
    pub selectors: Vec<(Point2<f64>, u32)>
}

impl Shape {
    /// Draw the shape to a cairo context
    /// Change this method in order to draw more shapes
    fn draw(&self, display_data: DisplayData, cr: &Context) {
        match self {
            &Shape::Line { p1, p2 } => {
                let new_p1 = display_data.location_scaling * p1.to_vec() + display_data.translation;
                let new_p2 = display_data.location_scaling * p2.to_vec() + display_data.translation;
                cr.move_to(new_p1.x, new_p1.y);
                cr.line_to(new_p2.x, new_p2.y);
                cr.stroke()
            }
            &Shape::Dot { p, r } => {
                let new_p = display_data.location_scaling * p.to_vec() + display_data.translation;
                // println!("Displaying node at position {:?}", new_p);
                cr.arc(new_p.x, new_p.y, display_data.size_scaling * r, 0.0, 2.0 * PI);
                cr.fill();
            }
            &Shape::Text { lower_left, ref text } => {
                let new_p = display_data.location_scaling * lower_left.to_vec() + display_data.translation;
                cr.move_to(new_p.x, new_p.y);
                cr.show_text(text);
            }
        }
    }
}

/// Describes the current focused window
/// TODO: Can this be replaced by just a matrix?
#[derive(Copy, Clone)]
struct DisplayData {
    translation: Vector2<f64>,
    size_scaling: f64,
    location_scaling: Matrix2<f64>
}

impl DisplayData {
    fn translate(self, v: Vector2<f64>) -> Self {
        DisplayData {
            translation: self.translation + v,
            size_scaling: self.size_scaling,
            location_scaling: self.location_scaling
        }
    }
    fn zoom(self, s: f64) -> Self {
        DisplayData {
            translation: self.translation,
            size_scaling: self.size_scaling * s,
            location_scaling: self.location_scaling * s
        }
    }
    fn rotate(self, d: f64) -> Self {
        DisplayData {
            translation: self.translation,
            size_scaling: self.size_scaling,
            location_scaling: self.location_scaling * Matrix2::new(d.cos(), -d.sin(), d.sin(), d.cos())
        }
    }
}

/// May add more fields later -- selection interface?
#[derive(Copy, Clone)]
struct EditorState {
    mode: Mode,
    selector: Option<Rc<Selector>>
}

/// TODO: remove this, and instead have window focus commands come from the backend
#[derive(Copy, Clone)]
#[allow(dead_code)]
enum Mode {
    /// Send keypresses to backend
    Regular,
    Selector
}

/// Contains the current state of the window
/// Has all the data necessary to draw the window
/// TODO: Add statusline
struct WindowState {
    display_data: Rc<Cell<DisplayData>>,
    editor_state: Rc<Cell<EditorState>>,
    window: Rc<gtk::Window>,
    drawing_area: Rc<DrawingArea>
}

/// Runs the main window. Takes in an arc of a mutex of a graph to be rendered as well
/// as a channel where a message is sent whenever a the graph gets updated and the
/// graph should be re-rendered
pub fn main_window(display_input: Arc<RwLock<DisplayInput>>, rx: Receiver<()>, tx: Sender<Event>) -> () {
    if gtk::init().is_err() {
        panic!("Failed to initialize gtk");
    }

    // let drawing_area
    let window_state = Rc::new(WindowState {
        display_data: Rc::new(Cell::new(DisplayData {
            translation: Vector2::new(200., 200.),
            size_scaling: 500.,
            location_scaling: Matrix2::new(100., 0., 0., 100.)
        })),
        window: Rc::new(gtk::Window::new(gtk::WindowType::Toplevel)),
        drawing_area: Rc::new(DrawingArea::new()),
        editor_state: Rc::new(Cell::new(EditorState {
            mode: Mode::Viewer
        }))
    });

    {
        let window_state_draw = window_state.clone();
        window_state.drawing_area.connect_draw(move |_, cr| {
            display(display_input.clone(), window_state_draw.clone(), cr);
            Inhibit(false)
        });
    }
    {
        let event_sender = tx.clone();
        let editor_state = window_state.editor_state.clone();
        let display_data = window_state.display_data.clone();
        let window_key = window_state.window.clone();

        // We should put some of this logic in haskell, except for the command string
        // Maybe we want to have a separate REPL instead of the bottom line, in a terminal?
        // In that case, no command stuff
        window_state.window.connect_key_press_event(move |_, key| {
            let keyval = key.get_keyval();
            let keystate = key.get_state();

            // println!("key pressed: {} / {:?}", keyval, keystate);
            match editor_state.get().mode {
                Mode::Regular => {
                    event_sender.send(KeyPress { key: keyval, modifier: keystate }).unwrap();
                }
                Mode::Selector => {
                    // roughly
                    let mut selector = match something {
                        None => { new selector }
                        Some(v) => v
                    };
                    match selector.add_key(keyval) {
                        Finished(node_id) => {
                            event_sender.send(Selection { node_id: node_id });
                            mode = Mode::Regular;
                            queue_draw;
                        }
                        Continue => {
                            queue_draw;
                        }
                    }
                }
            }

            Inhibit(false)
        });
    }

    window_state.window.connect_delete_event(|_, _| {
        gtk::main_quit();
        Inhibit(false)
    });

    {
        let drawing_area = window_state.drawing_area.clone();
        // 50ms is essentially the refresh rate
        // We should make this level triggered instead of edge triggered so that we
        // don't do unnecessary redraws, but that can wait
        gtk::timeout_add(50, move || {
            match rx.try_recv() {
                Ok(_) => {
                    drawing_area.queue_draw();
                }
                Err(_) => {}
            }
            gtk::Continue(true)
        });
    }

    window_state.window.add(&*window_state.drawing_area);
    window_state.window.show_all();
    gtk::main()
}


/// `display` takes in a pointer to the DisplayInput, which contains the vector drawing
/// of the screen, and rasterizes it to the cairo context cr
fn display(display_input: Arc<RwLock<DisplayInput>>,
           window_state: Rc<WindowState>,
           cr: &Context) {
    let display_input_read = display_input.read().unwrap();
    let ref shapes = display_input_read.shapes;
    let ref selectors = display_input_read.selectors;
    cr.set_source_rgb(0.1, 0.1, 0.1);
    cr.paint();
    cr.set_source_rgb(0.9, 0.9, 0.9);
    let disp_rect = window_state.drawing_area.get_allocation();
    let face = FontFace::toy_create("monospace", FontSlant::Normal, FontWeight::Normal);
    cr.set_font_face(face);
    cr.set_font_size(15.);
    for s in shapes.iter() {
        s.draw(window_state.display_data.get(), cr);
    }
    match window_state.editor_state.get().mode{
        // special case logic for drawing the selection labels and the command string
        Mode::Selector => {
            let display_data = window_state.display_data.get();
            for &(p, i) in selectors.iter() {
                let new_p = display_data.location_scaling * p.to_vec() + display_data.translation;
                let text = format!("{}", i);
                let extents = cr.text_extents(text.as_str());
                cr.set_source_rgb(0.9, 0.9, 0.1);
                cr.rectangle(new_p.x - 2., new_p.y - extents.height - 2., extents.width + 4., extents.height + 4.);
                cr.fill();
                cr.move_to(new_p.x, new_p.y);
                cr.set_source_rgb(0.2, 0.2, 0.2);
                cr.show_text(format!("{}", i).as_str());
            }
        }
        _ => {}
    }
}
