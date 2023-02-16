(** The main paint application *)

;; open Gctx
;; open Widget

(******************************************)
(**    SHAPES, MODES, and PROGRAM STATE   *)
(******************************************)

(** A location in the paint_canvas widget *)
type point = position  (* from Gctx *)

(** The shapes that are visible in the paint canvas -- these make up the
    picture that the user has drawn, as well as any other "visible" elements
    that must show up in the canvas area (e.g. a "selection rectangle"). At
    the start of the homework, the only available shape is a line.  *)
(* TODO: You will modify this definition in Tasks 3, 4, 5 and maybe 6. *)
type shape = 
  | Line of {color: color; thickness: int; p1: point; p2: point}
  | Points of {color: Gctx.color; thickness: int; points: point list }
  | Ellipse of {color: Gctx.color; thickness: int; 
  center: point; rx: int; ry: int}

(** These are the possible interaction modes that the paint program might be
    in. Some interactions require two modes. For example, the GUI might
    recognize the first mouse click as starting a line and a second mouse
    click as finishing the line.

    To start out, there are only two modes:

      - LineStartMode means the paint program is waiting for the user to make
        the first click to start a line.

      - LineEndMode means that the paint program is waiting for the user's
        second click. The point associated with this mode stores the location
        of the user's first mouse click.  *)
(* TODO: You will need to modify this type in Tasks 3 and 4, and maybe 6. *)
type mode = 
  | LineStartMode
  | LineEndMode of point
  | PointMode
  | EllipseMode of point

(** The state of the paint program. *)
type state = {
  (** The sequence of all shapes drawn by the user, in order from
      least recent (the head) to most recent (the tail). *)
  shapes : shape Deque.deque;

  (** The input mode the Paint program is in. *)
  mutable mode : mode;

  (** The currently selected pen color. *)
  mutable color : color;

  (* TODO: You will need to add new state for Tasks 2, 5, and *)
  (* possibly 6 *) 
  mutable preview : shape option;

  mutable thickness : int;
}

(** Initial values of the program state. *)
let paint : state = {
  shapes = Deque.create ();
  mode = LineStartMode;
  color = black;
  preview = None;
  thickness = 1;
  (* TODO: You will need to add new state for Tasks 2, 5, and maybe 6 *)
}



(** This function creates a graphics context with the appropriate
    pen color. *)
(* TODO: Your will need to modify this function in Task 5 *)
let with_params (g: gctx) (c: color) (t: int): gctx =
  let gc = with_thickness (with_color g c) t in 
  gc
  


(*********************************)
(**    MAIN CANVAS REPAINTING    *)
(*********************************)

(** The paint_canvas repaint function.

    This function iterates through all the drawn shapes (in order of least
    recent to most recent so that they are layered on top of one another
    correctly) and uses the Gctx.draw_xyz functions to display them on the
    canvas.  *)

(* TODO: You will need to modify this repaint function in Tasks 2, 3,
   4, and possibly 5 or 6. For example, if the user is performing some
   operation that provides "preview" (see Task 2) the repaint function
   must also show the preview. *)
let repaint (g: gctx) : unit =
  let draw_shape (s: shape) : unit =
    begin match s with
      | Line l -> draw_line (with_params g l.color l.thickness) l.p1 l.p2
      (* adds points and ellipses as shapes to be repainted *)
      | Points ps -> draw_points (with_params g ps.color ps.thickness) ps.points
      | Ellipse e -> draw_ellipse (with_params g e.color e.thickness) 
      e.center e.rx e.ry
    end in
  Deque.iterate draw_shape paint.shapes

(** Create the actual paint_canvas widget and its associated
    notifier_controller . *)
let ((paint_canvas : widget), (paint_canvas_controller : notifier_controller)) =
  canvas (600, 350) repaint


(************************************)
(**  PAINT CANVAS EVENT HANDLER     *)
(************************************)

(** The paint_action function processes all events that occur
    in the canvas region. *)
(* TODO: Tasks 2, 3, 4, 5, and 6 involve changes to paint_action. *)
let paint_action (gc:gctx) (event:event) : unit =
  let p  = event_pos event gc in  (* mouse position *)
  begin match (event_type event) with
    | MouseDown ->
       (* This case occurs when the mouse has been clicked in the
          canvas, but before the button has been released. How we
          process the event depends on the current mode of the paint
          canvas.  *)
      (begin match paint.mode with 
          | LineStartMode ->
            (* The paint_canvas was waiting for the first click of a line,
              so change it to LineEndMode, recording the starting point of
              the line. *)
            paint.mode <- LineEndMode p
          | LineEndMode p1 -> ()
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
          | PointMode -> 
          (* puts points into the preview and draws where it was clicked*)
          paint.preview <- Some (Points {color = paint.color; thickness = 1; 
          points = [p]});
          draw_point gc p
          (* sets the mode to store the initial click *)
          | EllipseMode p1 -> paint.mode <- EllipseMode p
       end)
    | MouseDrag ->
      (* In this case, the mouse has been clicked, and it's being dragged
         with the button down. Initially there is nothing to do, but you'll
         need to update this part for Task 2, 3, 4 and maybe 6. *)
      (begin match paint.mode with 
          | LineStartMode ->
            ()
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
              paint.preview <- Some (Line {color = paint.color; 
              thickness = paint.thickness; p1 = p1; p2 = p});
              draw_line (with_params gc paint.color paint.thickness) p1 p
          | PointMode ->
          (* draws the list of points created by dragging and stores 
          them in preview *)
              begin match paint.preview with
                | Some (Points ps) -> 
                draw_points (with_params gc ps.color ps.thickness) ps.points;
                paint.preview <- Some (Points {color = paint.color; 
                thickness = 1;
                points = ps.points @ [p]});
                | _ -> ()
              end
            | EllipseMode (p1x, p1y) -> 
            (* previews the ellipse and stores it*)
            begin match p with
            |(px, py) ->
              paint.preview <- Some (Ellipse {color = paint.color; 
              thickness = paint.thickness; center = (abs ((px + p1x) / 2), 
              abs ((py + p1y) / 2)); 
              rx = abs ((px - p1x) / 2); ry = abs ((py - p1y) / 2)});
              begin match paint.preview with
                | Some (Ellipse e) -> 
                draw_ellipse (with_params gc e.color e.thickness) 
                e.center e.rx e.ry
                | _ -> ()
              end 
            end
            end)
    | MouseUp ->
      (* In this case there was a mouse button release event. TODO: Tasks 2, 
         3, 4, and possibly 6 need to do something different here. *)
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
      (begin match paint.mode with 
          | LineStartMode -> ()
          | LineEndMode p1 ->
            (* The paint_canvas was waiting for the second click of a line,
              so create the line and add it to the deque of shapes. Go back
              to waiting for the first click. *)
            Deque.insert_tail
              (Line {color = paint.color; thickness = paint.thickness; 
              p1= p1; p2=p}) paint.shapes;
            paint.mode <- LineStartMode
          | PointMode -> 
          (* pulls points from preview and puts them in deque*)
          let points_list =
            begin match paint.preview with
            | Some (Points ps) -> ps.points
            | _ -> []
          end in
            paint.preview <- None;
            Deque.insert_tail (Points {color = paint.color; thickness = 1;
            points = points_list}) paint.shapes
          | EllipseMode (p1x, p1y) -> 
          (* inseerts ellipse from preview to deque *)
            begin match paint.preview with
              | Some (Ellipse e) -> Deque.insert_tail
              (Ellipse e) paint.shapes
              | _ -> ()
            end
          end)
    | _ -> ()
    (* This catches the MouseMove event (where the user moved the mouse over
       the canvas without pushing any buttons) and the KeyPress event (where
       the user typed a key when the mouse was over the canvas). *)
  end

(** Add the paint_action function as a listener to the paint_canvas *)
;; paint_canvas_controller.add_event_listener paint_action


(**************************************)
(** TOOLBARS AND PAINT PROGRAM LAYOUT *)
(**************************************)

(** This part of the program creates the other widgets for the paint
    program -- the buttons, color selectors, etc., and lays them out
    in the top - level window. *)
(* TODO: Tasks 1, 4, 5, and 6 involve adding new buttons or changing
   the layout of the Paint GUI. Initially the layout is ugly because
   we use only the hpair widget demonstrated in Lecture. Task 1 is to
   make improvements to make the layout more appealing. You may choose
   to arrange the buttons and other GUI elements of the paint program
   however you like (so long as it is easily apparent how to use the
   interface).  The sample screenshot of our solution shows one
   possible design.  Also, feel free to improve the visual components
   of the GUI; for example, our solution puts borders around the
   buttons and uses a custom "color button" that changes its
   appearance based on whether or not the color is currently
   selected. *)

(** Create the Undo button *)
let (w_undo, lc_undo, nc_undo) = button "Undo"
(** Create the line button *)
let (w_line, lc_line, nc_line) = button "Line"
(** Create the point button *)
let (w_point, lc_point, nc_point) = button "Point"
(** Create the ellipse button *)
let (w_ellipse, lc_ellipse, nc_ellipse) = button "Ellipse"

(** This function runs when the Undo button is clicked.
    It simply removes the last shape from the shapes deque. *)

let undo () : unit =
  if Deque.is_empty paint.shapes then () else
    ignore (Deque.remove_tail paint.shapes)
;; nc_undo.add_event_listener (mouseclick_listener undo)

(** This function runs when the line button is clicked.
    It sets the mode to linestartmode *)
let line () : unit =
  paint.mode <- LineStartMode
;; nc_line.add_event_listener (mouseclick_listener line)

(** This function runs when the point button is clicked.
    It sets the mode to pointmode *)
let point () : unit =
  paint.mode <- PointMode
;; nc_point.add_event_listener (mouseclick_listener point)

(** This function runs when the ellipse button is clicked.
    It sets the mode to ellipsemode with an arbitrary initial value *)
let ellipse () : unit =
  paint.mode <- EllipseMode (0,0)
;; nc_ellipse.add_event_listener (mouseclick_listener ellipse)

(** This function creates the thickness checkbox. It updates the thickness
field *)
let thickness (g: gctx) : widget =
  let (w_checkbox, c_checkbox) = checkbox false "Thick" in
  c_checkbox.add_change_listener (fun b -> if b then paint.thickness <- 7 
  else paint.thickness <- 1);
  w_checkbox

(** This function creates the slider for red. It updates the color
to have r = x *)
let redslider (g: gctx) : widget * int value_controller =
  let (wr_slider, cr_slider) =  slider 0 "Red" red 255 in
  cr_slider.add_change_listener (fun (x: int) -> 
  paint.color <- { paint.color with r = x });
  (wr_slider, cr_slider)

(** This function creates the slider for green. It updates the color
to have g = x *)
let greenslider (g: gctx) : widget =
  let (wg_slider, cg_slider) = slider 0 "Green" green 255 in
  cg_slider.add_change_listener (fun x -> 
  paint.color <- { paint.color with g = x });
  wg_slider


(** This function creates the slider for blue. It updates the color
to have b = x *)
let blueslider (g: gctx) : widget =
  let (wb_slider, cb_slider) =  slider 0 "Blue" blue 255 in
  cb_slider.add_change_listener (fun (x: int) -> 
  paint.color <- { paint.color with b = x });
  wb_slider
 
(** A spacer widget *)
let spacer : widget = space (10,10)

let (wr,cr) = redslider top_level
(** The mode toolbar, initially containing just the Undo button.
    TODO: you will need to modify this widget to add more buttons 
    to the toolbar in Tasks 5, and possibly 6. *)
let mode_toolbar : widget = Widget.hlist [border w_undo; spacer; 
border w_line; spacer; border w_point; spacer; border w_ellipse; 
spacer; border (thickness top_level); spacer; border 
(Widget.vlist [wr; spacer; greenslider top_level; 
spacer; blueslider top_level])]

(* The color selection toolbar. *)
(* This toolbar contains an indicator for the currently selected color
   and some buttons for changing it. Both the indicator and the buttons
   are small square widgets built from this higher-order function. *)
(** Create a widget that displays itself as colored square with the given
    width and color specified by the [get_color] function. *)
let colored_square (width:int) (get_color:unit -> color)
  : widget * notifier_controller =
  let repaint_square (gc:gctx) =
    let c = get_color () in
    fill_rect (with_color gc c) (0, 0) (width-1, width-1) in
  canvas (width,width) repaint_square

(** The color_indicator repaints itself with the currently selected
    color of the paint application. *)
let color_indicator =
  let indicator,_ = colored_square 24 (fun () -> paint.color) in
  let lab, _ = label "Current Color" in
  border (hpair lab indicator)

(** color_buttons repaint themselves with whatever color they were created
    with. They are also installed with a mouseclick listener
    that changes the selected color of the paint app to their color. *)
let color_button (c: color) : widget =
  let w,nc = colored_square 10 (fun () -> c) in
  nc.add_event_listener (mouseclick_listener (fun () ->
      paint.color <- c;
      cr.change_value (c.r)
       ));
  w

(** The color selection toolbar. Contains the color indicator and
    buttons for several different colors. *)
(* TODO: Task 1 - This code contains a great deal of boilerplate.  You
     should come up with a better, more elegant, more concise solution... *)  
   let color_toolbar : widget =
   Widget.hlist [color_indicator; spacer; color_button black; spacer; 
   color_button white; spacer; color_button red; spacer; color_button green;
    spacer; color_button blue; spacer; color_button yellow; spacer; 
    color_button cyan; spacer; color_button magenta]

(** The top-level paint program widget: a combination of the
    mode_toolbar, the color_toolbar and the paint_canvas widgets. *)

let paint_widget =
   Widget.vlist [mode_toolbar; spacer; color_toolbar; paint_canvas]


(**************************************)
(**      Start the application        *)
(**************************************)

(** Run the event loop to process user events. *)
;; Eventloop.run paint_widget
