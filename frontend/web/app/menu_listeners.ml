open! Core
open Js_of_ocaml

let margin = 8.

let set (el : Dom_html.element Js.t) name value =
  el##.style##setProperty
    (Js.string name)
    (Js.string (sprintf "%.0fpx" value))
    Js.undefined
  |> ignore
;;

let place (anchor : Prigh_web.Context_menu.Anchor.t) =
  Browser.after_render (fun () ->
    Option.iter (Dom_html.getElementById_opt "dialog") ~f:(fun menu ->
      let x, y =
        match anchor with
        | Point { x; y } -> Float.of_int x, Float.of_int y
        | Element id ->
          (match Dom_html.getElementById_opt id with
           | Some el ->
             let r = el##getBoundingClientRect in
             Js.to_float r##.left, Js.to_float r##.bottom +. 2.
           | None -> margin, margin)
      in
      let width = Float.of_int menu##.offsetWidth in
      let height = Float.of_int menu##.offsetHeight in
      let window_width = Float.of_int Dom_html.window##.innerWidth in
      let window_height = Float.of_int Dom_html.window##.innerHeight in
      let left =
        Float.max margin (Float.min x (window_width -. width -. margin))
      in
      (* No room below: above the anchor instead, in the window (a message
         taller than it ends below it). *)
      let top =
        if Float.(y +. height +. margin > window_height)
        then
          Float.max
            margin
            (Float.min (y -. height) (window_height -. height -. margin))
        else y
      in
      set menu "left" left;
      set menu "top" top;
      menu##.classList##add (Js.string "placed");
      menu##focus))
;;

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let listen ?(capture = false) target event handler =
  ignore
    (Dom_html.addEventListener
       target
       event
       (Dom.handler handler)
       (Js.bool capture)
     : Dom_html.event_listener_id)
;;

let long_press_field = "prighLongPress"
let selectable_attribute = "data-selectable"

(* What a long press opens the menu of: the rows and messages with one. *)
let rows =
  sprintf
    "[data-session], [data-worktree], .review-file-row, [%s][tabindex]"
    Prigh_web.Chat_view.message_attribute
;;

let selection () = Js.Unsafe.meth_call Dom_html.window "getSelection" [||]

let clear_selection () =
  ignore
    (Js.Unsafe.meth_call (selection ()) "removeAllRanges" [||] : Js.Unsafe.any)
;;

let query_all selector =
  Dom.list_of_nodeList
    (Dom_html.document##querySelectorAll (Js.string selector))
;;

(* Its words: a user's bubble, a reply's text (not its tool calls' lines or
   its time), from the first to the last. *)
let select_message n =
  Browser.after_render (fun () ->
    Option.iter
      (Dom_html.getElementById_opt (Prigh_web.Chat_view.message_id n))
      ~f:(fun el ->
        el##setAttribute (Js.string selectable_attribute) (Js.string "");
        let range = Js.Unsafe.meth_call Dom_html.document "createRange" [||] in
        (match
           Dom.list_of_nodeList
             (el##querySelectorAll (Js.string ".bubble, .markdown"))
         with
         | [] ->
           ignore
             (Js.Unsafe.meth_call
                range
                "selectNodeContents"
                [| Js.Unsafe.inject el |]
              : Js.Unsafe.any)
         | first :: _ as texts ->
           ignore
             (Js.Unsafe.meth_call
                range
                "setStart"
                [| Js.Unsafe.inject first; Js.Unsafe.inject 0 |]
              : Js.Unsafe.any);
           let last = List.last_exn texts in
           ignore
             (Js.Unsafe.meth_call
                range
                "setEnd"
                [| Js.Unsafe.inject last
                 ; Js.Unsafe.inject last##.childNodes##.length
                |]
              : Js.Unsafe.any));
        let selection = selection () in
        ignore
          (Js.Unsafe.meth_call selection "removeAllRanges" [||] : Js.Unsafe.any);
        ignore
          (Js.Unsafe.meth_call selection "addRange" [| Js.Unsafe.inject range |]
           : Js.Unsafe.any)))
;;

(* A short buzz, where the phone has one. *)
let vibrate () =
  let navigator = Js.Unsafe.get Dom_html.window "navigator" in
  if Js.Optdef.test (Js.Unsafe.get navigator "vibrate")
  then
    ignore
      (Js.Unsafe.meth_call navigator "vibrate" [| Js.Unsafe.inject 10 |]
       : Js.Unsafe.any)
;;

(* How long a press is held, and how far the finger may wander meanwhile. *)
let hold_ms = 480.
let pressing_ms = 150.
let wander = 10.

type press =
  { row : Dom_html.element Js.t
  ; pointer : int
  ; x : float
  ; y : float
  ; timers : Dom_html.timeout_id_safe list
  }

let install () =
  let doc = Dom_html.document in
  let press = ref None in
  (* The menu opened from this press: the click it ends with is swallowed. *)
  let pressed = ref false in
  let cancel () =
    Option.iter !press ~f:(fun p ->
      List.iter p.timers ~f:Dom_html.clearTimeout;
      p.row##.classList##remove (Js.string "pressing"));
    press := None
  in
  (* The menu, from our timer (iOS fires no [contextmenu] for a touch) or
     from the browser's own long press (Android's, which first selects the
     word under the finger, and shows its copy bar unless the event is
     cancelled). Either way, once. *)
  let fire ~x ~y =
    Option.iter !press ~f:(fun p ->
      cancel ();
      pressed := true;
      clear_selection ();
      List.iter
        (query_all (sprintf "[%s]" selectable_attribute))
        ~f:(fun el -> el##removeAttribute (Js.string selectable_attribute));
      vibrate ();
      let event =
        Js.Unsafe.new_obj
          Js.Unsafe.global##._MouseEvent
          [| Js.Unsafe.inject (Js.string "contextmenu")
           ; Js.Unsafe.obj
               [| "bubbles", Js.Unsafe.inject Js._true
                ; "cancelable", Js.Unsafe.inject Js._true
                ; "clientX", Js.Unsafe.inject (Js.number_of_float x)
                ; "clientY", Js.Unsafe.inject (Js.number_of_float y)
               |]
          |]
      in
      Js.Unsafe.set event long_press_field Js._true;
      ignore (Js.Unsafe.meth_call p.row "dispatchEvent" [| event |] : bool Js.t))
  in
  listen
    doc
    Dom_html.Event.pointerdown
    (fun (ev : Dom_html.pointerEvent Js.t) ->
       cancel ();
       pressed := false;
       let touch =
         String.equal (Js.to_string (Js.Unsafe.coerce ev)##.pointerType) "touch"
       in
       (match Js.Opt.to_option ev##.target with
        | Some target when touch ->
          (match closest target rows with
           | Some row
             when Option.is_none
                    (closest target (sprintf "[%s]" selectable_attribute)) ->
             let x = Js.to_float ev##.clientX
             and y = Js.to_float ev##.clientY in
             let pointer = (Js.Unsafe.coerce ev)##.pointerId in
             press
             := Some
                  { row
                  ; pointer
                  ; x
                  ; y
                  ; timers =
                      [ Dom_html.setTimeout
                          (fun () ->
                             row##.classList##add (Js.string "pressing"))
                          pressing_ms
                      ; Dom_html.setTimeout (fun () -> fire ~x ~y) hold_ms
                      ]
                  }
           | Some _ | None -> ())
        | Some _ | None -> ());
       Js._true);
  listen
    doc
    Dom_html.Event.pointermove
    (fun (ev : Dom_html.pointerEvent Js.t) ->
       Option.iter !press ~f:(fun p ->
         if
           (Js.Unsafe.coerce ev)##.pointerId = p.pointer
           && Float.(
                abs (Js.to_float ev##.clientX -. p.x) > wander
                || abs (Js.to_float ev##.clientY -. p.y) > wander)
         then cancel ());
       Js._true);
  List.iter
    [ Dom_html.Event.pointerup; Dom_html.Event.pointercancel ]
    ~f:(fun e ->
      listen doc e (fun _ ->
        cancel ();
        Js._true));
  (* The browser's own long press, while a press is held still on a row:
     ours, ahead of every other listener (the message's would leave a
     selection's menu to the browser). One with no press of its own (the end
     of a dragged selection, a mouse's right-click) goes on as it is. *)
  listen
    ~capture:true
    Dom_html.window
    (Dom.Event.make "contextmenu")
    (fun (ev : Dom_html.mouseEvent Js.t) ->
       if
         Js.Optdef.test (Js.Unsafe.get ev long_press_field)
         || Option.is_none !press
       then Js._true
       else (
         Dom.preventDefault ev;
         Dom_html.stopPropagation ev;
         fire ~x:(Js.to_float ev##.clientX) ~y:(Js.to_float ev##.clientY);
         Js._false));
  (* The press opened the menu: the click it ends with must not also open the
     session. *)
  listen ~capture:true doc Dom_html.Event.click (fun ev ->
    if !pressed
    then (
      pressed := false;
      Dom_html.stopPropagation ev;
      Dom.preventDefault ev;
      Js._false)
    else Js._true)
;;
