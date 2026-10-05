open! Core
open Js_of_ocaml
module Browser = Prigh_ui_web_app.Browser
module Action = Prigh_web.App.Action

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let target (ev : #Dom_html.event Js.t) = Js.Opt.to_option ev##.target

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

let listen ?(capture = false) event handler =
  ignore
    (Dom_html.addEventListener
       Dom_html.document
       event
       (Dom.handler (fun ev ->
          handler ev;
          Js._true))
       (Js.bool capture)
     : Dom_html.event_listener_id)
;;

(* ---- width *)

let width_key = "prigh-web.agents-width"

let set_width px =
  let style = (Js.Unsafe.coerce Dom_html.document##.documentElement)##.style in
  ignore
    (Js.Unsafe.meth_call
       style
       "setProperty"
       [| Js.Unsafe.inject (Js.string "--agents-width")
        ; Js.Unsafe.inject (Js.string (sprintf "%dpx" px))
       |]
     : Js.Unsafe.any)
;;

let clamp px =
  let max = Dom_html.window##.innerWidth * 6 / 10 in
  Int.max 300 (Int.min px max)
;;

let install_resize () =
  Option.iter
    (Option.bind (Browser.get_item width_key) ~f:Int.of_string_opt)
    ~f:(fun px -> set_width (clamp px));
  let dragging = ref None in
  listen Dom_html.Event.pointerdown (fun (ev : Dom_html.pointerEvent Js.t) ->
    match
      Option.bind (target ev) ~f:(fun el -> closest el ".agents-resize")
    with
    | None -> ()
    | Some _ ->
      Dom.preventDefault ev;
      dragging := Some 0;
      Dom_html.document##.body##.classList##add (Js.string "resizing"));
  listen Dom_html.Event.pointermove (fun (ev : Dom_html.pointerEvent Js.t) ->
    match !dragging with
    | None -> ()
    | Some _ ->
      let px =
        clamp
          (Dom_html.window##.innerWidth
           - Float.iround_nearest_exn (Js.to_float ev##.clientX))
      in
      dragging := Some px;
      set_width px);
  listen Dom_html.Event.pointerup (fun (_ : Dom_html.pointerEvent Js.t) ->
    match !dragging with
    | None -> ()
    | Some px ->
      dragging := None;
      Dom_html.document##.body##.classList##remove (Js.string "resizing");
      if px > 0 then Browser.set_item width_key (Int.to_string px))
;;

(* ---- following *)

let body () = Dom_html.getElementById_opt "agents-body"

let at_bottom (el : Dom_html.element Js.t) =
  Float.(
    of_int el##.scrollHeight
    -. Js.to_float el##.scrollTop
    -. of_int el##.clientHeight
    < 60.)
;;

(* A transcript or a job's output starts at its end and follows it, unless
   scrolled up; the list stays where it is. *)
let install_follow () =
  let stick = ref true in
  let shown = ref None in
  listen ~capture:true (Dom_html.Event.make "scroll") (fun ev ->
    match target ev, body () with
    | Some el, Some body
      when phys_equal (el :> Dom.node Js.t) (body :> Dom.node Js.t) ->
      stick := at_bottom body
    | _ -> ());
  let observer =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun records _ ->
         if not (Terminal_widget.only_inside records)
         then
           Option.iter (body ()) ~f:(fun body ->
             let now_shown =
               Js.Opt.to_option
                 (body##querySelector (Js.string ".agents-detail[data-shown]"))
               |> Option.bind ~f:(fun el -> attribute el "data-shown")
             in
             if not (Option.equal String.equal now_shown !shown)
             then (
               shown := now_shown;
               stick := true);
             if Option.is_some now_shown && !stick
             then
               body##.scrollTop := Js.float (Float.of_int body##.scrollHeight))))
  in
  let options = MutationObserver.empty_mutation_observer_init () in
  options##.childList := true;
  options##.subtree := true;
  options##.characterData := true;
  observer##observe (Dom_html.document :> Dom.node Js.t) options
;;

let install ~schedule =
  listen Dom_html.Event.click (fun (ev : Dom_html.mouseEvent Js.t) ->
    match Option.bind (target ev) ~f:(fun el -> closest el "[data-agent]") with
    | None -> ()
    | Some el ->
      Option.iter (attribute el "data-agent") ~f:(fun id ->
        schedule (Action.Open_subagents (Some id))));
  install_resize ();
  install_follow ()
;;

(* ---- revealing a card *)

let quote s =
  "\""
  ^ String.concat_map s ~f:(function
    | ('"' | '\\') as c -> "\\" ^ String.of_char c
    | c -> String.of_char c)
  ^ "\""
;;

let reveal path =
  Browser.after_render (fun () ->
    let find (el : Dom_html.element Js.t) call =
      Js.Opt.to_option
        (el##querySelector (Js.string (sprintf "[data-call=%s]" (quote call))))
    in
    let rec walk el = function
      | [] -> el
      | call :: rest ->
        (match find el call with
         | Some card -> walk card rest
         | None -> el)
    in
    Option.iter (Dom_html.getElementById_opt "chat") ~f:(fun chat ->
      let card = walk chat path in
      if not (phys_equal card chat)
      then (
        let rec open_folds (el : Dom_html.element Js.t) =
          if not (phys_equal el chat)
          then (
            if String.equal (Js.to_string el##.tagName) "DETAILS"
            then el##setAttribute (Js.string "open") (Js.string "");
            Js.Opt.iter el##.parentNode (fun parent ->
              Js.Opt.iter (Dom_html.CoerceTo.element parent) open_folds))
        in
        open_folds card;
        ignore
          (Js.Unsafe.meth_call
             card
             "scrollIntoView"
             [| Js.Unsafe.inject
                  (Js.Unsafe.obj
                     [| "block", Js.Unsafe.inject (Js.string "center") |])
             |]
           : Js.Unsafe.any);
        card##.classList##add (Js.string "flash");
        ignore
          (Dom_html.setTimeout
             (fun () -> card##.classList##remove (Js.string "flash"))
             1600.
           : Dom_html.timeout_id_safe))))
;;
