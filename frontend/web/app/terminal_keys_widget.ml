open! Core
open Js_of_ocaml
module Keys = Prigh_web.Terminal_keys

type t =
  { element : Dom_html.element Js.t
  ; mutable keys : Keys.t
  ; mutable modifiers : (Keys.Modifier.t * Dom_html.buttonElement Js.t) list
  ; press : string -> unit
  ; app_cursor : unit -> bool
  ; refocus : unit -> unit
  }

let element t = t.element
let now () = Js.to_float (new%js Js.date_now)##getTime

(* Held: the first repeat after [repeat_after_ms], then every [repeat_ms]. *)
let repeat_after_ms = 400.
let repeat_ms = 60.

(* A tap's pressed look lasts long enough to be seen. *)
let pressed_ms = 90.

let on (target : #Dom_html.eventTarget Js.t) name f =
  ignore
    (Dom_html.addEventListener
       target
       (Dom_html.Event.make name)
       (Dom.handler (fun ev ->
          f ev;
          Js._true))
       Js._false
     : Dom_html.event_listener_id)
;;

let show_latches t =
  List.iter t.modifiers ~f:(fun (modifier, button) ->
    let latch = Keys.latch t.keys modifier in
    let set cls on =
      if on
      then button##.classList##add (Js.string cls)
      else button##.classList##remove (Js.string cls)
    in
    set "latched" (Keys.Latch.equal latch Once);
    set "locked" (Keys.Latch.equal latch Locked);
    button##setAttribute
      (Js.string "aria-pressed")
      (Js.string (Bool.to_string (not (Keys.Latch.equal latch Off)))))
;;

let fire t key =
  let keys, bytes =
    Keys.press t.keys key ~app_cursor:(t.app_cursor ()) ~now:(now ())
  in
  t.keys <- keys;
  show_latches t;
  if not (String.is_empty bytes) then t.press bytes;
  t.refocus ()
;;

let transform t data =
  let keys, data = Keys.type_ t.keys data in
  if not (phys_equal keys t.keys)
  then (
    t.keys <- keys;
    show_latches t);
  data
;;

let button t key =
  let button = Dom_html.createButton Dom_html.document in
  button##.className
  := Js.string
       (match (key : Keys.Key.t) with
        | Modifier _ -> "terminal-key modifier"
        | _ -> "terminal-key");
  button##setAttribute (Js.string "type") (Js.string "button");
  button##setAttribute (Js.string "tabindex") (Js.string "-1");
  button##setAttribute
    (Js.string "aria-label")
    (Js.string (Keys.Key.name key));
  button##.textContent := Js.some (Js.string (Keys.Key.label key));
  let timer = ref None in
  let held = ref false in
  let fired = ref false in
  let stop () =
    Option.iter !timer ~f:Dom_html.clearTimeout;
    timer := None
  in
  let rec repeat delay =
    timer
    := Some
         (Dom_html.setTimeout
            (fun () ->
               fired := true;
               fire t key;
               repeat repeat_ms)
            delay)
  in
  let release ~act =
    if !held
    then (
      held := false;
      stop ();
      if act && not !fired then fire t key;
      ignore
        (Dom_html.setTimeout
           (fun () ->
              if not !held then button##.classList##remove (Js.string "pressed"))
           pressed_ms
         : Dom_html.timeout_id_safe))
  in
  (* Not the focus: the shell keeps it, and the phone its keyboard. *)
  on button "pointerdown" (fun ev ->
    Dom.preventDefault ev;
    held := true;
    fired := false;
    button##.classList##add (Js.string "pressed");
    stop ();
    if Keys.Key.repeats key then repeat repeat_after_ms);
  on button "mousedown" Dom.preventDefault;
  on button "pointerup" (fun _ -> release ~act:true);
  (* A swipe that scrolls the bar, or a finger slid off the key. *)
  on button "pointercancel" (fun _ -> release ~act:false);
  on button "pointerleave" (fun _ -> release ~act:false);
  on button "contextmenu" Dom.preventDefault;
  (* A screen reader's activation: a click without a pointer. *)
  on button "click" (fun ev ->
    if (Js.Unsafe.coerce ev)##.detail = 0 then fire t key);
  button
;;

let create ~press ~app_cursor ~refocus =
  let element = Dom_html.createDiv Dom_html.document in
  element##.className := Js.string "terminal-keys";
  element##setAttribute (Js.string "role") (Js.string "toolbar");
  element##setAttribute (Js.string "aria-label") (Js.string "Terminal keys");
  let t =
    { element = (element :> Dom_html.element Js.t)
    ; keys = Keys.empty
    ; modifiers = []
    ; press
    ; app_cursor
    ; refocus
    }
  in
  List.iter Keys.Key.all ~f:(fun key ->
    let b = button t key in
    (match key with
     | Modifier m -> t.modifiers <- t.modifiers @ [ m, b ]
     | _ -> ());
    Dom.appendChild element b);
  show_latches t;
  t
;;
