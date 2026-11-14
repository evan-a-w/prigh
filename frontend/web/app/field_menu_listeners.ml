open! Core
open Js_of_ocaml
module Field_menu = Prigh_web.Field_menu

(* What the menu was opened on: a text field, or xterm.js's element with the
   handle bin/terminal.js puts on it. *)
module Target = struct
  type t =
    | Field of Dom_html.element Js.t
    | Terminal of Dom_html.element Js.t

  let element = function
    | Field el | Terminal el -> el
  ;;
end

let target : Target.t option ref = ref None
let text_types = [ "text"; "search"; "url"; "tel"; "email"; "password" ]

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let terminal_handle el = Js.Unsafe.get el (Js.string "prighTerminal")

let text_field (el : Dom_html.element Js.t) =
  let type_ () =
    String.lowercase (Js.to_string (Js.Unsafe.get el (Js.string "type")))
  in
  match String.lowercase (Js.to_string el##.tagName) with
  | "textarea" -> true
  | "input" -> List.mem text_types (type_ ()) ~equal:String.equal
  | _ -> false
;;

let find (el : Dom_html.element Js.t) : Target.t option =
  match closest el ".xterm" with
  | Some term when Js.Optdef.test (terminal_handle term) -> Some (Terminal term)
  | Some _ -> None
  | None -> Option.some_if (text_field el) (Target.Field el)
;;

let int_field el name =
  Js.Opt.case
    (Js.Unsafe.get el (Js.string name) : int Js.opt)
    (fun () -> 0)
    Fn.id
;;

let bool_field el name = Js.to_bool (Js.Unsafe.get el (Js.string name))

let describe : Target.t -> Field_menu.Field.t = function
  | Field el ->
    { kind = Text_field
    ; selection = int_field el "selectionStart" < int_field el "selectionEnd"
    ; editable = not (bool_field el "readOnly" || bool_field el "disabled")
    ; secret =
        String.equal
          (String.lowercase
             (Js.to_string (Js.Unsafe.get el (Js.string "type"))))
          "password"
    }
  | Terminal el ->
    { kind = Terminal
    ; selection =
        Js.to_bool
          (Js.Unsafe.meth_call (terminal_handle el) "hasSelection" [||])
    ; editable = true
    ; secret = false
    }
;;

let install ~schedule =
  (* Ahead of every other listener, so that none leaves it to the browser;
     prigh's own menus still open from theirs. *)
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       (Dom.Event.make "contextmenu")
       (Dom.handler (fun (ev : Dom_html.mouseEvent Js.t) ->
          let clicked = Js.Opt.to_option ev##.target in
          let found = Option.bind clicked ~f:find in
          match
            Field_menu.decide
              ~browser_shell:(Desktop_page.in_browser ())
              (Option.map found ~f:describe)
          with
          | Native -> Js._true
          | Suppress ->
            Dom.preventDefault ev;
            Js._true
          | Open info ->
            Dom.preventDefault ev;
            (* The field's own: not also a menu of what it is in. *)
            Dom_html.stopPropagation ev;
            target := found;
            let x = Float.to_int (Js.to_float ev##.clientX)
            and y = Float.to_int (Js.to_float ev##.clientY) in
            let anchor : Prigh_web.Context_menu.Anchor.t =
              match clicked with
              | Some el when x = 0 && y = 0 ->
                (* From the keyboard: under the field's top left corner (the
                   terminal's input follows its cursor). *)
                let r = el##getBoundingClientRect in
                Point
                  { x = Float.to_int (Js.to_float r##.left)
                  ; y = Float.to_int (Js.to_float r##.top) + 24
                  }
              | _ -> Point { x; y }
            in
            schedule
              (Prigh_web.App.Action.Open_field_menu { field = info; anchor });
            Js._false))
       Js._true
     : Dom_html.event_listener_id)
;;

let selected_text el =
  let value = Js.to_string (Js.Unsafe.get el (Js.string "value")) in
  let units = Js.string value in
  Js.to_string
    (units##substring
       (int_field el "selectionStart")
       (int_field el "selectionEnd"))
;;

(* [text] in place of the selection, as typing it would: the field's undo
   and its input event (which Bonsai's state follows). *)
let replace (el : Dom_html.element Js.t) text =
  let command = if String.is_empty text then "delete" else "insertText" in
  let done_ =
    try
      Js.to_bool
        (Js.Unsafe.meth_call
           Dom_html.document
           "execCommand"
           [| Js.Unsafe.inject (Js.string command)
            ; Js.Unsafe.inject Js._false
            ; Js.Unsafe.inject (Js.string text)
           |])
    with
    | _ -> false
  in
  if not done_
  then (
    ignore
      (Js.Unsafe.meth_call
         el
         "setRangeText"
         [| Js.Unsafe.inject (Js.string text)
          ; Js.Unsafe.inject (int_field el "selectionStart")
          ; Js.Unsafe.inject (int_field el "selectionEnd")
          ; Js.Unsafe.inject (Js.string "end")
         |]
       : Js.Unsafe.any);
    let event =
      Js.Unsafe.new_obj
        Js.Unsafe.global##._Event
        [| Js.Unsafe.inject (Js.string "input")
         ; Js.Unsafe.obj [| "bubbles", Js.Unsafe.inject Js._true |]
        |]
    in
    ignore (Js.Unsafe.meth_call el "dispatchEvent" [| event |] : bool Js.t))
;;

let read_clipboard () : string option Async_kernel.Deferred.t =
  let clipboard =
    Js.Unsafe.get
      (Js.Unsafe.get Dom_html.window (Js.string "navigator"))
      (Js.string "clipboard")
  in
  if
    Js.Optdef.test clipboard
    && Js.Opt.test clipboard
    && Js.Optdef.test (Js.Unsafe.get clipboard (Js.string "readText"))
  then (
    let result = Async_kernel.Ivar.create () in
    let answer text = Async_kernel.Ivar.fill_if_empty result text in
    (try
       ignore
         (Js.Unsafe.meth_call
            (Js.Unsafe.meth_call clipboard "readText" [||] : Js.Unsafe.any)
            "then"
            [| Js.Unsafe.inject
                 (Js.wrap_callback (fun text ->
                    answer (Some (Js.to_string text))))
             ; Js.Unsafe.inject (Js.wrap_callback (fun _ -> answer None))
            |]
          : Js.Unsafe.any)
     with
     | _ -> answer None);
    Async_kernel.Ivar.read result)
  else Async_kernel.Deferred.return None
;;

let copy_selection text ~key =
  let%map.Async_kernel.Deferred copied = Browser.copy_to_clipboard text in
  Option.some_if
    (not copied)
    (sprintf "Could not copy to the clipboard: press %s instead." key)
;;

let edit_field el (item : Field_menu.Item.t option) =
  let open Async_kernel in
  el##focus;
  match item with
  | None -> return None
  | Some Select_all ->
    ignore (Js.Unsafe.meth_call el "select" [||] : Js.Unsafe.any);
    return None
  | Some ((Cut | Copy) as item) ->
    let%map failed = copy_selection (selected_text el) ~key:"Ctrl+C" in
    (match item, failed with
     | Cut, None ->
       el##focus;
       replace el ""
     | (Cut | Copy | Paste | Select_all), _ -> ());
    failed
  | Some Paste ->
    (match%map.Deferred read_clipboard () with
     | None ->
       Some
         "The browser would not let prigh read the clipboard: press Ctrl+V to \
          paste."
     | Some text ->
       el##focus;
       replace el text;
       None)
;;

let edit_terminal el (item : Field_menu.Item.t option) =
  let open Async_kernel in
  let term = terminal_handle el in
  let call name args =
    ignore (Js.Unsafe.meth_call term name args : Js.Unsafe.any)
  in
  call "focus" [||];
  match item with
  | None | Some Cut -> return None
  | Some Select_all ->
    call "selectAll" [||];
    return None
  | Some Copy ->
    copy_selection
      (Js.to_string (Js.Unsafe.meth_call term "selection" [||]))
      ~key:"Ctrl+Shift+C"
  | Some Paste ->
    (match%map.Deferred read_clipboard () with
     | None ->
       Some
         "The browser would not let prigh read the clipboard: press \
          Ctrl+Shift+V to paste."
     | Some text ->
       call "paste" [| Js.Unsafe.inject (Js.string text) |];
       None)
;;

let edit (item : Field_menu.Item.t option) =
  match !target with
  | None -> Async_kernel.return None
  | Some t
    when not
           (Js.to_bool
              (Js.Unsafe.get (Target.element t) (Js.string "isConnected"))) ->
    target := None;
    Async_kernel.return None
  | Some (Field el) -> edit_field el item
  | Some (Terminal el) -> edit_terminal el item
;;
