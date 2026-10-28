open! Core
open Js_of_ocaml

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let set_label (button : Dom_html.element Js.t) ~copied =
  if copied
  then button##.classList##add (Js.string "copied")
  else button##.classList##remove (Js.string "copied");
  button##.textContent
  := Js.some (Js.string (if copied then "Copied" else "Copy"))
;;

let copy (ev : Dom_html.mouseEvent Js.t) =
  let open Option.Let_syntax in
  ignore
    (let%bind target = Js.Opt.to_option ev##.target in
     let%bind button = closest target "button.copy" in
     let%bind block = closest button ".copyable" in
     let%map text =
       Js.Opt.to_option (block##querySelector (Js.string ".copy-text"))
       |> Option.bind ~f:(fun el -> Js.Opt.to_option el##.textContent)
     in
     let clipboard = Js.Unsafe.get Dom_html.window##.navigator "clipboard" in
     ignore
       (Js.Unsafe.meth_call clipboard "writeText" [| Js.Unsafe.inject text |]
        : Js.Unsafe.any);
     set_label button ~copied:true;
     ignore
       (Dom_html.setTimeout (fun () -> set_label button ~copied:false) 1500.
        : Dom_html.timeout_id_safe)
     : unit option)
;;

let close_image (ev : Dom_html.keyboardEvent Js.t) =
  if String.equal (Js.Optdef.case ev##.key (fun () -> "") Js.to_string) "Escape"
  then
    Js.Opt.iter
      (Dom_html.document##querySelector (Js.string "details.image[open]"))
      (fun details ->
         details##removeAttribute (Js.string "open");
         Dom_html.stopPropagation ev)
;;

let toggle_fold ~schedule (ev : Dom_html.mouseEvent Js.t) =
  Option.iter
    (Option.bind
       (Js.Opt.to_option ev##.target)
       ~f:(fun el -> closest el (sprintf "[%s]" Prigh_web.Fold.attribute)))
    ~f:(fun el ->
      Js.Opt.iter
        (el##getAttribute (Js.string Prigh_web.Fold.attribute))
        (fun key ->
           schedule (Prigh_web.App.Action.Toggle_fold (Js.to_string key))))
;;

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

(* The clicked element with [attribute], and its value. *)
let clicked (ev : Dom_html.mouseEvent Js.t) name =
  Option.bind (Js.Opt.to_option ev##.target) ~f:(fun el ->
    Option.bind (closest el (sprintf "[%s]" name)) ~f:(fun el ->
      attribute el name |> Option.map ~f:(fun value -> el, value)))
;;

let open_menu ~schedule n anchor =
  schedule (Prigh_web.App.Action.Open_menu { target = Message n; anchor })
;;

(* Whether the selection has text in [el]: its menu is the browser's then,
   to copy it. *)
let selects (el : Dom_html.element Js.t) =
  let selection = Js.Unsafe.meth_call Dom_html.window "getSelection" [||] in
  (not (Js.to_bool (Js.Unsafe.get selection "isCollapsed")))
  && Js.to_bool
       (Js.Unsafe.meth_call
          selection
          "containsNode"
          [| Js.Unsafe.inject el; Js.Unsafe.inject Js._true |])
;;

(* A right-click (or a long press, {!Menu_listeners}) on a message with a
   menu: the menu at the pointer, or, from the keyboard, under it. A
   link's or an image's menu, or one with text selected in the message, is
   the browser's. *)
let context_menu ~schedule (ev : Dom_html.mouseEvent Js.t) =
  let long_press =
    Js.Optdef.test (Js.Unsafe.get ev Menu_listeners.long_press_field)
  in
  match Js.Opt.to_option ev##.target with
  | None -> ()
  | Some target ->
    (match
       closest
         target
         (sprintf "[%s][tabindex]" Prigh_web.Chat_view.message_attribute)
     with
     | Some msg
       when Option.is_none (closest target "a[href], img")
            && (long_press || not (selects msg)) ->
       Option.iter
         (Option.bind
            (attribute msg Prigh_web.Chat_view.message_attribute)
            ~f:Int.of_string_opt)
         ~f:(fun n ->
           Dom.preventDefault ev;
           let x = Float.to_int (Js.to_float ev##.clientX)
           and y = Float.to_int (Js.to_float ev##.clientY) in
           open_menu
             ~schedule
             n
             (if x = 0 && y = 0
              then Element (Prigh_web.Chat_view.message_id n)
              else Point { x; y }))
     | Some _ | None -> ())
;;

(* A trim after a message's divider's Show, Hide and Undo. *)
let hidden_action ~schedule (ev : Dom_html.mouseEvent Js.t) =
  Option.iter
    (clicked ev Prigh_web.Chat_view.hidden_action_attribute)
    ~f:(fun (el, what) ->
      Option.iter
        (attribute el Prigh_web.Chat_view.hidden_attribute)
        ~f:(fun entry ->
          match what with
          | "show" ->
            schedule (Prigh_web.App.Action.Show_hidden { entry; show = true })
          | "hide" ->
            schedule (Prigh_web.App.Action.Show_hidden { entry; show = false })
          | "undo" -> schedule (Prigh_web.App.Action.Restore_range entry)
          | _ -> ()))
;;

let install ~schedule =
  let listen event handler =
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         event
         (Dom.handler (fun ev ->
            handler ev;
            Js._true))
         Js._true
       : Dom_html.event_listener_id)
  in
  listen Dom_html.Event.click copy;
  listen Dom_html.Event.click (toggle_fold ~schedule);
  listen (Dom.Event.make "contextmenu") (context_menu ~schedule);
  listen Dom_html.Event.click (hidden_action ~schedule);
  listen Dom_html.Event.keydown close_image
;;
