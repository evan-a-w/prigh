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
      (* No room below: above the anchor instead. *)
      let top =
        if Float.(y +. height +. margin > window_height)
        then Float.max margin (y -. height)
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

let install () =
  let doc = Dom_html.document in
  let timer = ref None in
  let start = ref (0., 0.) in
  let pressed = ref false in
  let cancel () =
    Option.iter !timer ~f:Dom_html.clearTimeout;
    timer := None
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
       (match
          Option.bind
            (Js.Opt.to_option ev##.target)
            ~f:(fun el -> closest el "[data-session], [data-worktree]")
        with
        | Some row when touch ->
          let x = Js.to_float ev##.clientX
          and y = Js.to_float ev##.clientY in
          start := x, y;
          timer
          := Some
               (Dom_html.setTimeout
                  (fun () ->
                     timer := None;
                     pressed := true;
                     let event =
                       Js.Unsafe.new_obj
                         Js.Unsafe.global##._MouseEvent
                         [| Js.Unsafe.inject (Js.string "contextmenu")
                          ; Js.Unsafe.obj
                              [| "bubbles", Js.Unsafe.inject Js._true
                               ; "cancelable", Js.Unsafe.inject Js._true
                               ; ( "clientX"
                                 , Js.Unsafe.inject (Js.number_of_float x) )
                               ; ( "clientY"
                                 , Js.Unsafe.inject (Js.number_of_float y) )
                              |]
                         |]
                     in
                     ignore
                       (Js.Unsafe.meth_call row "dispatchEvent" [| event |]
                        : bool Js.t))
                  550.)
        | Some _ | None -> ());
       Js._true);
  listen
    doc
    Dom_html.Event.pointermove
    (fun (ev : Dom_html.pointerEvent Js.t) ->
       let x0, y0 = !start in
       if
         Float.(
           abs (Js.to_float ev##.clientX -. x0) > 10.
           || abs (Js.to_float ev##.clientY -. y0) > 10.)
       then cancel ();
       Js._true);
  List.iter
    [ Dom_html.Event.pointerup; Dom_html.Event.pointercancel ]
    ~f:(fun e ->
      listen doc e (fun _ ->
        cancel ();
        Js._true));
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
