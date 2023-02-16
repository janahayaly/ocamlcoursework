(** A library of widgets for building GUIs. *)

(********************)
(** The widget type *)
(********************)

(** A widget is an object that provides three services:
    - it can repaint itself (given an appropriate graphics context)
    - it can handle events
    - it knows its dimensions  *)
type widget = {
  repaint: Gctx.gctx -> unit;
  handle: Gctx.gctx -> Gctx.event -> unit;
  size: unit -> Gctx.dimension
}

(************************)
(**   Layout Widgets    *)
(************************)

(** A simple widget that just occupies space *)
let space (p: Gctx.dimension) : widget = {
  repaint = (fun _ -> ());
  handle = (fun _ _ -> ());
  size = (fun _ -> p);
}


(** A widget that adds a one-pixel border to an existing widget *)
let border (w: widget) : widget = {
  repaint = (fun (g: Gctx.gctx) ->
    let (width, height) = w.size () in
    let x = width + 3 in    (* not "+ 4" because we start at 0! *)
    let y = height + 3 in
    Gctx.draw_line g (0,0) (x,0);
    Gctx.draw_line g (0,0) (0, y);
    Gctx.draw_line g (x,0) (x, y);
    Gctx.draw_line g (0, y) (x, y);
    let g = Gctx.translate g (2,2) in
    w.repaint g);

  handle = (fun (g: Gctx.gctx) (e: Gctx.event) ->
    w.handle (Gctx.translate g (2,2)) e);

  size = (fun () ->
    let (width, height) = w.size () in
    width + 4, height + 4);
}

(* A helper function that determines whether a given event is within a
   region of a widget whose upper-left hand corner is (0,0) with width
   w and height h.  *)
let event_within (g: Gctx.gctx) (e: Gctx.event)
    ((w, h): Gctx.dimension) : bool =
  let (mouse_x, mouse_y) = Gctx.event_pos e g in
  mouse_x >= 0 && mouse_x < w && mouse_y >= 0 && mouse_y < h

(** The hpair widget lays out two widgets horizontally, aligned at
   their top edges. *)
let hpair (w1:widget) (w2:widget) : widget = {
  repaint = (fun (g: Gctx.gctx) ->
      let (x1, _) = w1.size () in
      w1.repaint g;
      w2.repaint (Gctx.translate g (x1,0)));
  handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
      if event_within g e (w1.size ())
      then w1.handle g e
      else
        let g2 = (Gctx.translate g (fst (w1.size ()), 0)) in
        if event_within g2 e (w2.size ())
        then w2.handle g2 e
        else ());
   size = (fun () ->
      let (x1, y1) = w1.size () in
      let (x2, y2) = w2.size () in
      (x1 + x2, max y1 y2));
}

(** The vpair widget lays out two widgets vertically, aligned at their
    left edges. *)
let vpair (w1: widget) (w2: widget) : widget = {
    repaint = (fun (g: Gctx.gctx) ->
      let (_, y1) = w1.size () in
      w1.repaint g;
      w2.repaint (Gctx.translate g (0, y1)));
    handle = (fun (g:Gctx.gctx) (e:Gctx.event) ->
      if event_within g e (w1.size ())
      then w1.handle g e
      else
        let g2 = (Gctx.translate g (0, snd (w1.size ()))) in
        if event_within g2 e (w2.size ())
        then w2.handle g2 e
        else ());
      size = (fun () ->
      let (x1, y1) = w1.size () in
      let (x2, y2) = w2.size () in
      (max x1 x2, y1 + y2));
}

(* TIP: the OCaml List module provides a function fold_right
   (List.fold_right) that behaves like the "fold" function we've seen
   on previous homeworks except that it takes its arguments in a
   different order.

   Also, if you look at the List interface, you will see that there is
   a fold_left function. You may want to think about what this does,
   and how it's different from the fold you're used to.  *)

(* Iterates through a list of widgets and cntinuously pairs them to create a
composite function using a pair function *)
let list_layout (pair: widget -> widget -> widget)
         (ws: widget list) : widget =
  List.fold_right pair ws (space(0,0))

(* continuously pairs widgets horizontally using list layout*)
let hlist (ws: widget list) : widget = list_layout hpair ws

(* continuously pairs widgets vertically using list layout*)
let vlist (ws: widget list) : widget = list_layout vpair ws


(*****************************)
(**       Label Widgets      *)
(*****************************)

(* Throughout the paint program, we will find the need to associate some value
   with a widget, and also to provide a way to update that value. The simplest
   example of this is a label widget, where the value we're dealing with is a
   string (which is displayed by the label).

   Because both the widget and the label_controller share the same, mutable
   value, the constructor must create both together.  *)

(** A record of functions that allows us to read and write the string
    associated with a label. *)
type label_controller = {
  get_label : unit -> string;
  set_label : string -> unit
}

(** Construct a label widget and its controller. *)
let label (s: string) : widget * label_controller =
  (* construction of the label widget *)
  let r = {contents = s} in {
    repaint = (fun (g: Gctx.gctx) ->
        Gctx.draw_string g (0,0) r.contents);
    handle = (fun _ _ -> ());
    size = (fun () -> Gctx.text_size r.contents)
  },
  (* construction of the label controller *)
  {
    get_label = (fun () -> r.contents);
    set_label = (fun (s: string) -> r.contents <- s);
  }


(*****************************************)
(**    Event Listeners and Notifiers     *)
(*****************************************)

(** An event listener processes events as they "flow" through the widget
    hierarchy.

    The file notifierdemo.ml in the GUI demo project gives a longer
    explanation of what notifiers and event_listeners are. *)

type event_listener = Gctx.gctx -> Gctx.event -> unit

(* Below we define two special forms of event_listeners. *)

(** Performs an action upon receiving a mouse click. *)
let mouseclick_listener (action: unit -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    if Gctx.event_type e = Gctx.MouseDown then action ()

(** Performs an action upon receiving a key press. *)
let key_listener (action: char -> unit) : event_listener =
  fun (g: Gctx.gctx) (e: Gctx.event) ->
    begin match Gctx.event_type e with
      | Gctx.KeyPress key -> action key
      | _ -> ()
    end

(** A notifier_controller is associated with a notifier widget.  It
   allows the program to add event listeners to the notifier. *)
type notifier_controller = {
  add_event_listener: event_listener -> unit
}

(** A notifier widget is a widget "wrapper" that doesn't take up any
   extra screen space -- it extends an existing widget with the
   ability to react to events. It maintains a list of of
   event_listeners that eavesdrop on the events propagated through the
   notifier widget.

   When an event comes in to the notifier, it is passed to each
   event_listener in turn, and then passed to the child widget. *)
let notifier (w: widget) : widget * notifier_controller =
  let listeners = {contents = []} in {
    repaint = w.repaint;
    handle =
      (fun (g: Gctx.gctx) (e: Gctx.event) ->
         List.iter (fun h -> h g e) listeners.contents;
         w.handle g e);
    size = w.size
  },{
    add_event_listener =
      fun (newl: event_listener) ->
        listeners.contents <- newl :: listeners.contents
  }


(*****************************************)
(**               Button                 *)
(*****************************************)

(** A button has a string, which can be controlled by the
   corresponding label_controller, and notifier, which can be
   controlled by the notifier_controller to add listeners (e.g., a
   mouseclick_listener) that will perform an action when the button is
   pressed. *)
let button (s: string)
         : widget * label_controller * notifier_controller =
  let (w, lc) = label s in
  let (w', nc) = notifier w in
  (w', lc, nc)


(*****************************************)
(**               Canvas                 *)
(*****************************************)

(** A bare_canvas widget just provides a region of the screen where
   low-level painting operations can be carried out directly. *)
let bare_canvas (dim: Gctx.dimension) (f : Gctx.gctx -> unit)
         : widget = {
  repaint = f;
  handle = (fun _ _ -> ());
  size = (fun _ -> dim)
}

(** A canvas is a bordered widget with a notifier_controller. New
   event listeners can be added using the notifier_controller. The
   interior of the canvas will be redrawn by calling a user-specified
   function, provided as a parameter of the canvas widget
   constructor. *)
let canvas (dim: Gctx.dimension) (f : Gctx.gctx -> unit)
         : widget * notifier_controller =
  let w = bare_canvas dim f in
  notifier (border w)


(*****************************************)
(**              Checkbox                *)
(*****************************************)


(** A checkbox is a controller for a boolean value associated with a widget.
   Other widgets might store other data -- a slider might store an integer, for
   example, and the label_controller we saw above is specialized to strings.

   Here we introduce a general-purpose value_controller, which stores a generic
   value. This controller can read (via get_value) and write the value (via
   change_value). It also allows change listeners to be registered by the
   application. All of the added listeners are run whenever this value is
   changed.

   We will use this value_controller as part of the checkbox implementation, and
   you are free to use it (if needed) for whatever widget you create in
   Task 6. *)
type 'a value_controller = {
  add_change_listener : ('a -> unit) -> unit;
  get_value           : unit -> 'a;
  change_value        : 'a -> unit
}

(** TODO: The first part of task 5 requires you to implement the following
    generic function. This function takes a value of type 'a and returns a
    value controller for it. Carefully consider what state needs to be
    associated with any value controller. *)
let make_controller (v: 'a) : 'a value_controller =
  let controller = {contents = v} in  
  let liststuff = {contents = []} in 
  (* defining three functions in the type *)
    { add_change_listener = (fun (f) ->
     liststuff.contents <- f :: liststuff.contents);
    get_value = (fun () -> controller.contents);
    change_value = (fun (x: 'a) -> controller.contents <- x;
    List.iter (fun x -> x controller.contents) liststuff.contents)}


(** TODO: Don't forget to use the helper function you defined above
   when implementing the checkbox function!

   If your checkbox implementation does not work, do _not_ comment it
   out, because your code will not compile upon submission. Instead,
   you can replace the function body with

      failwith "Checkbox: unimplemented"

    before submitting your code. *)
let checkbox (init: bool) (s: string) : widget * bool value_controller =
  let c = make_controller (init) in
  let (label, _) = label s in
  let w =
  (let text = {contents = s} in
    (* repaints the widget based on whether it's been clicked or not *)
   { repaint = (fun (g) -> Gctx.draw_rect g (0,0) (20,20); 
   (if c.get_value() then 
      (Gctx.draw_line g (0,0) (20,20); 
      Gctx.draw_line g (20,0) (0,20)) 
    else ());
      label.repaint (Gctx.translate g (20,0)));
    (* sets the size of widget to include the box and label *)
      size = (fun () -> 
      let (x,y) = Gctx.text_size text.contents in
      (20 + x, 20));
    (* handles a click by changing the boolean value *)
      handle = (fun (g)(e: Gctx.event) -> 
      let (x,y) = Gctx.text_size text.contents in
      if event_within g e (20 + x, 20) && Gctx.event_type e = MouseDown then  
      c.change_value(not (c.get_value())))}) in
      (w, c)


(*****************************************)
(**          Additional widgets          *)
(*****************************************)

(* A slider widget function. Creates a generic slider with an int value 
controller. It takes in an initial int value, a string to label the slider, 
a color that the slider should be, and a scale for the slider. *)

let slider (init: int) (s: string) (col: Gctx.color) (scale: int)
: widget * int value_controller =
  let c = make_controller (init) in
  let w = 
  (* creates a widget that changes the int value *)
    (let value = { contents = init } in
    { repaint = (fun (g) -> let gc = Gctx.with_color g col in
                Gctx.draw_rect gc (0,0) (scale,10);
                Gctx.fill_rect gc (0,0) (value.contents,10));
      size = (fun () -> (scale,10));
      handle = (fun (g) (e: Gctx.event) -> if event_within g e (scale, 10) && 
      (Gctx.event_type e = MouseDrag || Gctx.event_type e = MouseDown) 
      then let (x, y) = Gctx.event_pos e g in
      value.contents <- x;
      c.change_value x)
    }) in
    let (label, _) = label s in
    let updatedw = hlist [w; space(10,10); label] in
    (* returns a tuple of the widget and controller *)
    (updatedw, c)