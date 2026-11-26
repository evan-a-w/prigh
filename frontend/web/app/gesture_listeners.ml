open! Core
open Js_of_ocaml

(* Touch events, not pointer events: a touchmove's preventDefault stops the
   page scrolling once a drag is ours, where a pointer's would come too late
   (the browser takes the touch for a pan and cancels the pointer). *)

let get o name = Js.Unsafe.get o (Js.string name)
let meth o name args = Js.Unsafe.meth_call o name args
let float_of o name : float = Js.float_of_number (get o name)

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (meth el "closest" [| Js.Unsafe.inject (Js.string selector) |])
;;

let query selector : Dom_html.element Js.t option =
  Js.Opt.to_option (Dom_html.document##querySelector (Js.string selector))
;;

let query_all selector : Dom_html.element Js.t list =
  Dom.list_of_nodeList (Dom_html.document##querySelectorAll (Js.string selector))
;;

let has (el : Dom_html.element Js.t) cls = Js.to_bool (el##.classList##contains (Js.string cls))

let set_style (el : Dom_html.element Js.t) name value =
  ignore
    (meth
       el##.style
       "setProperty"
       [| Js.Unsafe.inject (Js.string name); Js.Unsafe.inject (Js.string value) |]
     : Js.Unsafe.any)
;;

let clear_style (el : Dom_html.element Js.t) name =
  ignore
    (meth el##.style "removeProperty" [| Js.Unsafe.inject (Js.string name) |]
     : Js.Unsafe.any)
;;

let now () = Js.to_float (new%js Js.date_now)##getTime

let reduced_motion () =
  Js.to_bool
    (Dom_html.window##matchMedia (Js.string "(prefers-reduced-motion: reduce)"))##.matches
;;

let narrow () = Option.is_some (query ".app.narrow")
let drawer_open () = Option.is_some (query ".app.narrow.sidebar-open")

(* Something over the page that owns it: the drawer's edge is not for it. *)
let covered () =
  Option.is_some
    (query
       ".modal-backdrop, dialog[open], .chrome-menu, .chip-popover, \
        .context-menu, .account-menu, .filter-menu, .review-popover")
;;

let sheets =
  ".chrome-menu, .chip-popover, .context-menu, .account-menu, .filter-menu, \
   .review-popover"
;;

(* The edge a finger has to start in for the drawer, or back. *)
let edge = 24.

(* How far a finger goes before the drag is taken to be one way or the
   other. *)
let slop = 8.

type kind =
  | Drawer_open of Dom_html.element Js.t (* the sidebar *)
  | Drawer_close of Dom_html.element Js.t
  | Back of Dom_html.element Js.t (* the modal *)
  | Sheet of Dom_html.element Js.t
  | Row of Dom_html.element Js.t (* the .session *)
  | Tile of Dom_html.element Js.t (* the .pending-image *)

let horizontal = function
  | Drawer_open _ | Drawer_close _ | Back _ | Row _ -> true
  | Sheet _ | Tile _ -> false
;;

type drag =
  { kind : kind
  ; x0 : float
  ; y0 : float
  ; t0 : float
  ; mutable live : bool option (* [None]: not yet one way or the other *)
  ; mutable dx : float
  ; mutable dy : float
  }

let drag : drag option ref = ref None

(* The click a drag's touch would make, or the tap that put a row back. *)
let swallow_click = ref false

(* {1 A session's row: its actions behind it} *)

let actions_width = 148.

let row_actions (row : Dom_html.element Js.t) =
  Js.Opt.to_option (row##querySelector (Js.string ".session-swipe"))
;;

let place_row (row : Dom_html.element Js.t) x =
  set_style row "--swipe-x" (sprintf "%.0fpx" x);
  Option.iter (row_actions row) ~f:(fun a ->
    set_style a "width" (sprintf "%.0fpx" (Float.max 0. (-.x))))
;;

let unswipe_row (row : Dom_html.element Js.t) =
  row##.classList##remove (Js.string "swiped");
  row##.classList##remove (Js.string "swiping");
  clear_style row "--swipe-x";
  Option.iter (row_actions row) ~f:(fun a -> clear_style a "width");
  Option.iter (row_actions row) ~f:(fun a -> a##setAttribute (Js.string "aria-hidden") (Js.string "true"))
;;

let swiped_rows () = query_all ".session.swiped"

(* {1 What a finger starting here may drag} *)

let candidate (target : Dom_html.element Js.t) ~x =
  match closest target sheets with
  | Some sheet ->
    (* Only when what is under the finger is scrolled to its top: a list
       scrolled down scrolls back up as it would otherwise. *)
    let rec at_top (el : Dom_html.element Js.t) =
      Float.(float_of el "scrollTop" <= 0.)
      && (phys_equal el sheet
          ||
          match Js.Opt.to_option (get el "parentElement" : Dom_html.element Js.t Js.opt) with
          | Some p -> at_top p
          | None -> true)
    in
    if at_top target then Some (Sheet sheet) else None
  | None ->
    (match closest target ".pending-image" with
     | Some tile -> Some (Tile tile)
     | None ->
       (match closest target ".modal" with
        | Some modal ->
          if Float.(x < edge) && Option.is_some (Js.Opt.to_option (modal##querySelector (Js.string ".modal-back")))
          then Some (Back modal)
          else None
        | None ->
          (match query ".app.narrow .sidebar" with
           | None -> None
           | Some sidebar ->
             if drawer_open ()
             then (
               match closest target ".sidebar" with
               | Some _ ->
                 (match closest target ".session" with
                  | Some row when Option.is_some (row_actions row) -> Some (Row row)
                  | _ -> Some (Drawer_close sidebar))
               | None ->
                 if Option.is_some (closest target ".scrim")
                 then Some (Drawer_close sidebar)
                 else None)
             else if Float.(x < edge) && not (covered ())
             then Some (Drawer_open sidebar)
             else None)))
;;

(* {1 Following the finger} *)

let drawer_width (sidebar : Dom_html.element Js.t) =
  Float.max 1. (Float.of_int sidebar##.offsetWidth)
;;

let follow ~schedule:_ d =
  let dx = d.dx
  and dy = d.dy in
  match d.kind with
  | Drawer_open sidebar ->
    let w = drawer_width sidebar in
    let x = Float.min 0. (-.w +. Float.max 0. dx) in
    set_style sidebar "transform" (sprintf "translateX(%.0fpx)" x);
    set_style sidebar "transition" "none"
  | Drawer_close sidebar ->
    let x = Float.min 0. dx in
    set_style sidebar "transform" (sprintf "translateX(%.0fpx)" x);
    set_style sidebar "transition" "none";
    Option.iter (query ".app.narrow .scrim") ~f:(fun scrim ->
      set_style
        scrim
        "opacity"
        (sprintf "%.3f" (1. +. (x /. drawer_width sidebar))))
  | Back modal ->
    set_style modal "transform" (sprintf "translateX(%.0fpx)" (Float.max 0. dx));
    set_style modal "transition" "none"
  | Sheet sheet ->
    set_style sheet "transform" (sprintf "translateY(%.0fpx)" (Float.max 0. dy));
    set_style sheet "transition" "none"
  | Row row ->
    let start = if has row "swiped" then -.actions_width else 0. in
    let x = Float.min 0. (start +. dx) in
    (* Past its actions it resists. *)
    let x = if Float.(x < -.actions_width) then -.actions_width +. ((x +. actions_width) /. 3.) else x in
    row##.classList##add (Js.string "swiping");
    Option.iter (row_actions row) ~f:(fun a -> a##removeAttribute (Js.string "aria-hidden"));
    place_row row x
  | Tile tile ->
    let y = Float.min 0. dy in
    set_style tile "transform" (sprintf "translateY(%.0fpx)" y);
    set_style tile "opacity" (sprintf "%.3f" (Float.max 0.2 (1. +. (y /. 80.))));
    set_style tile "transition" "none"
;;

(* Back where CSS puts it: with its transition, from where the finger left
   it (no style is read between, so the browser animates from there). *)
let settle (el : Dom_html.element Js.t) props =
  List.iter props ~f:(clear_style el);
  clear_style el "transition"
;;

let speed d ~dist = Float.abs dist /. Float.max 1. (now () -. d.t0)

let release ~schedule d =
  let far ~dist ~min:least ~fast = Float.(abs dist > least || (abs dist > 24. && speed d ~dist > fast)) in
  match d.kind with
  | Drawer_open sidebar ->
    let opens = Float.(d.dx > 0.) && far ~dist:d.dx ~min:(drawer_width sidebar /. 3.) ~fast:0.4 in
    settle sidebar [ "transform" ];
    if opens then schedule Prigh_web.App.Action.Toggle_sidebar
  | Drawer_close sidebar ->
    let closes = Float.(d.dx < 0.) && far ~dist:d.dx ~min:(drawer_width sidebar /. 3.) ~fast:0.4 in
    settle sidebar [ "transform" ];
    Option.iter (query ".app.narrow .scrim") ~f:(fun s -> clear_style s "opacity");
    if closes then schedule Prigh_web.App.Action.Toggle_sidebar
  | Back modal ->
    let back = Float.(d.dx > 0.) && far ~dist:d.dx ~min:90. ~fast:0.4 in
    settle modal [ "transform" ];
    if back
    then
      Option.iter
        (Js.Opt.to_option (modal##querySelector (Js.string ".modal-back")))
        ~f:(fun b -> b##click)
  | Sheet sheet ->
    let closes = Float.(d.dy > 0.) && far ~dist:d.dy ~min:90. ~fast:0.5 in
    if closes
    then (
      let close () =
        schedule
          (if has sheet "chrome-menu"
           then Prigh_web.App.Action.Toggle_status_tray
           else Prigh_web.App.Action.Close_dialog)
      in
      if reduced_motion ()
      then close ()
      else (
        (* Down out of sight, then gone. *)
        set_style sheet "transition" "transform 0.16s ease-in";
        set_style sheet "transform" "translateY(100%)";
        ignore
          (Dom_html.setTimeout close 150. : Dom_html.timeout_id_safe)))
    else settle sheet [ "transform" ]
  | Row row ->
    row##.classList##remove (Js.string "swiping");
    let start = if has row "swiped" then -.actions_width else 0. in
    let x = start +. d.dx in
    let opens = Float.(x < -.actions_width /. 2.) || (Float.(d.dx < -24.) && Float.(speed d ~dist:d.dx > 0.4)) in
    let opens = opens && not (Float.(d.dx > 24.) && Float.(speed d ~dist:d.dx > 0.4)) in
    if opens
    then (
      List.iter (swiped_rows ()) ~f:(fun r -> if not (phys_equal r row) then unswipe_row r);
      row##.classList##add (Js.string "swiped");
      place_row row (-.actions_width))
    else unswipe_row row
  | Tile tile ->
    if Float.(d.dy < 0.) && far ~dist:d.dy ~min:56. ~fast:0.4
    then
      Option.iter
        (Js.Opt.to_option (tile##querySelector (Js.string ".remove-image")))
        ~f:(fun b ->
          settle tile [ "transform"; "opacity" ];
          b##click)
    else settle tile [ "transform"; "opacity" ]
;;

let touch ev =
  let touches = get ev "touches" in
  if Float.(float_of touches "length" <> 1.) then None else Some (get touches "0")
;;

let listen event handler ~passive =
  let opts : < capture : bool Js.t Js.prop ; passive : bool Js.t Js.prop > Js.t =
    Js.Unsafe.obj
      [| "capture", Js.Unsafe.inject Js._true
       ; "passive", Js.Unsafe.inject (Js.bool passive)
      |]
  in
  ignore
    (meth
       Dom_html.document
       "addEventListener"
       [| Js.Unsafe.inject (Js.string event)
        ; Js.Unsafe.inject (Js.wrap_callback handler)
        ; Js.Unsafe.inject opts
       |]
     : Js.Unsafe.any)
;;

let install ~schedule =
  listen "touchstart" ~passive:true (fun ev ->
    (* A drag's touch that made no click leaves nothing to swallow. *)
    swallow_click := false;
    drag := None;
    match touch ev with
    | None -> ()
    | Some t when narrow () ->
      let target : Dom_html.element Js.t = get ev "target" in
      let x = float_of t "clientX"
      and y = float_of t "clientY" in
      (* A tap anywhere but on a swiped row's actions puts it back; a tap
         on the row itself only does that. *)
      List.iter (swiped_rows ()) ~f:(fun row ->
        let on_actions =
          Option.is_some (closest target ".session-swipe")
          && Option.exists (closest target ".session") ~f:(phys_equal row)
        in
        if not on_actions
        then (
          if Option.exists (closest target ".session") ~f:(phys_equal row)
          then swallow_click := true;
          unswipe_row row));
      Option.iter (candidate target ~x) ~f:(fun kind ->
        drag := Some { kind; x0 = x; y0 = y; t0 = now (); live = None; dx = 0.; dy = 0. })
    | Some _ -> ());
  listen "touchmove" ~passive:false (fun ev ->
    match !drag, touch ev with
    | Some d, Some t ->
      d.dx <- float_of t "clientX" -. d.x0;
      d.dy <- float_of t "clientY" -. d.y0;
      (match d.live with
       | None ->
         if Float.(Float.abs d.dx > slop || Float.abs d.dy > slop)
         then (
           let sideways = Float.(Float.abs d.dx > Float.abs d.dy) in
           let ours =
             Bool.equal sideways (horizontal d.kind)
             &&
             match d.kind with
             | Drawer_open _ | Back _ -> Float.(d.dx > 0.)
             | Drawer_close _ -> Float.(d.dx < 0.)
             | Sheet _ -> Float.(d.dy > 0.)
             | Tile _ -> Float.(d.dy < 0.)
             | Row row -> Float.(d.dx < 0.) || has row "swiped"
           in
           d.live <- Some ours;
           if not ours then drag := None)
       | Some _ -> ());
      (match d.live with
       | Some true ->
         Dom.preventDefault ev;
         follow ~schedule d
       | Some false | None -> ())
    | Some _, None ->
      (* A second finger: not a drag any more. *)
      Option.iter !drag ~f:(fun d -> if Option.is_some d.live then release ~schedule { d with dx = 0.; dy = 0. });
      drag := None
    | None, _ -> ());
  let finish ~cancel ev =
    match !drag with
    | Some ({ live = Some true; _ } as d) ->
      drag := None;
      (* Its own clicks (back, remove) first: then the touch's is swallowed. *)
      if cancel then release ~schedule { d with dx = 0.; dy = 0. } else release ~schedule d;
      swallow_click := true;
      ignore ev
    | Some _ | None -> drag := None
  in
  listen "touchend" ~passive:true (finish ~cancel:false);
  listen "touchcancel" ~passive:true (finish ~cancel:true);
  (* The click that ends a drag (or the tap that put a row back) does
     nothing else. *)
  listen "click" ~passive:false (fun ev ->
    if !swallow_click
    then (
      swallow_click := false;
      Dom.preventDefault ev;
      Dom_html.stopPropagation ev))
;;
