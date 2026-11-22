open! Import
open Js_of_ocaml

module Pattern = struct
  type t =
    | Digits
    | Chars
    | Digits_and_chars
  [@@deriving enumerate, sexp_of, compare, equal]

  let regexp = function
    | Digits -> "^\\d+$"
    | Chars -> "^[a-zA-Z]+$"
    | Digits_and_chars -> "^[a-zA-Z0-9]+$"
  ;;

  let matches t s =
    String.for_all s ~f:(fun c ->
      match t with
      | Digits -> Char.is_digit c
      | Chars -> Char.is_alpha c
      | Digits_and_chars -> Char.is_alphanum c)
  ;;
end

type t =
  { value : string
  ; selection : (int * int) option
  }

module Selection = struct
  type direction =
    | Forward
    | Backward

  (* input-otp's selectionchange handler: a collapsed caret inside the code
     selects the character at it (or before it, moving back), so that typing
     replaces it and the slot shows as active; [prev] is the last selection
     it settled on. [None]: leave the browser's selection. *)
  let adjust ~value ~max_length ~prev (s, e) =
    let len = String.length value in
    if len = 0 || s <> e || (s = len && len < max_length)
    then None
    else if s = 0
    then Some (0, 1, Forward)
    else if s = max_length
    then Some (s - 1, s, Backward)
    else if max_length > 1 && len > 1
    then (
      let dir, shift =
        match prev with
        | None -> Forward, 0
        | Some (p0, p1) ->
          if s < p1
          then (
            let was_caret = p0 = p1 && p0 < max_length in
            Backward, if was_caret then 0 else -1)
          else Forward, 0
      in
      Some (s + shift, s + shift + 1, dir))
    else None
  ;;

  let direction_name : direction -> string = function
    | Forward -> "forward"
    | Backward -> "backward"
  ;;
end

let style fields =
  Attr.style
    (Css_gen.concat (List.map fields ~f:(fun (field, value) -> Css_gen.create ~field ~value)))
;;

(* On the event's [<input>]; tests' fake events have none. *)
let on_target (ev : Dom_html.event Js.t) f =
  Effect.of_thunk (fun () ->
    match (Js.Unsafe.get ev "target" : Dom_html.inputElement Js.t) with
    | exception _ -> ()
    | input -> (try f input with _ -> ()))
;;

let set_input_value ev value = on_target ev (fun input -> input##.value := Js.string value)

(* Follows the input's selection (document's selectionchange), adjusting it
   as input-otp does, and reports it. *)
module Selection_hook = Vdom.Attr.Hooks.Make (struct
    module Input = struct
      type t = (int * int) option -> unit Effect.t

      let sexp_of_t _ = Sexp.Atom "<fun>"
      let combine _ second = second
    end

    module State = struct
      type t =
        { mutable on_change : Input.t
        ; mutable prev : (int * int) option
        ; mutable listener : Dom_html.event_listener_id option
        }
    end

    let init on_change _ = { State.on_change; prev = None; listener = None }

    let sync (state : State.t) (input : Dom_html.inputElement Js.t) =
      let active = Js.Opt.to_option Dom_html.document##.activeElement in
      let focused =
        Option.value_map active ~default:false ~f:(fun a ->
          phys_equal (a :> Dom.node Js.t) (input :> Dom.node Js.t))
      in
      let report sel = Effect.Expert.handle_non_dom_event_exn (state.on_change sel) in
      if not focused
      then (
        state.prev <- None;
        report None)
      else (
        let s = input##.selectionStart
        and e = input##.selectionEnd in
        let sel =
          match
            Selection.adjust
              ~value:(Js.to_string input##.value)
              ~max_length:input##.maxLength
              ~prev:state.prev
              (s, e)
          with
          | None -> s, e
          | Some (s, e, dir) ->
            let () =
              (Js.Unsafe.coerce input)##setSelectionRange
                s
                e
                (Js.string (Selection.direction_name dir))
            in
            s, e
        in
        report (Some sel);
        state.prev <- Some sel)
    ;;

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun _ (state : State.t) (el : Dom_html.element Js.t) ->
          let input : Dom_html.inputElement Js.t = Js.Unsafe.coerce el in
          state.listener
          <- Some
               (Dom_html.addEventListener
                  Dom_html.document
                  (Dom_html.Event.make "selectionchange")
                  (Dom_html.handler (fun _ ->
                     sync state input;
                     Js._true))
                  Js._true))
    ;;

    let update ~old_input:_ ~new_input (state : State.t) _ = state.on_change <- new_input

    let destroy _ (state : State.t) _ =
      Option.iter state.listener ~f:Dom_html.removeEventListener
    ;;
  end)

let selection_hook on_change =
  Vdom.Attr.create_hook "loon-input-otp-selection" (Selection_hook.create on_change)
;;

(* input-otp on focus: the last character selected (or the caret after it). *)
let select_last ev ~max_length =
  on_target ev (fun input ->
    let len = input##.value##.length in
    input##setSelectionRange (Int.min len (max_length - 1)) len)
;;

(* Deleting moves the caret without a selectionchange in some browsers. *)
let selection_changed =
  Effect.of_thunk (fun () ->
    try
      ignore
        (Dom_html.document##dispatchEvent
           (Js.Unsafe.new_obj (Js.Unsafe.global##._Event) [| Js.Unsafe.inject (Js.string "selectionchange") |])
          : bool Js.t)
    with
    | _ -> ())
;;

let view
  ?(attrs = [])
  ?class_
  ?container_class
  ?id
  ?(disabled = false)
  ?pattern
  ~max_length
  ~value
  ~selection
  ~on_change
  ~on_selection_change
  children
  =
  let t = { value; selection } in
  let on_input ev s =
    let s = String.prefix s max_length in
    match pattern with
    | Some p when (not (String.is_empty s)) && not (Pattern.matches p s) ->
      set_input_value ev value
    | _ ->
      (* The caret follows the typing, at the end of the code (the browser's
         selectionchange then confirms it). *)
      let caret =
        match selection with
        | None -> Effect.Ignore
        | Some _ ->
          let n = String.length s in
          on_selection_change
            (Some
               (match Selection.adjust ~value:s ~max_length ~prev:None (n, n) with
                | Some (s, e, _) -> s, e
                | None -> n, n))
      in
      if String.length s < String.length value
      then Effect.Many [ on_change s; caret; selection_changed ]
      else Effect.Many [ on_change s; caret ]
  in
  let opt f = Option.value_map ~default:[] ~f:(fun x -> [ f x ]) in
  Node.div
    ~attrs:
      [ data "input-otp-container" "true"
      ; Cn.with_class
          ?class_:container_class
          [ "gap-2 flex items-center has-disabled:opacity-50" ]
      ; style
          [ "position", "relative"
          ; "cursor", if disabled then "default" else "text"
          ; "user-select", "none"
          ; "-webkit-user-select", "none"
          ; "pointer-events", "none"
          ]
      ]
    (children t
     @ [ Node.div
           ~attrs:[ style [ "position", "absolute"; "inset", "0"; "pointer-events", "none" ] ]
           [ Node.input
               ~attrs:
                 ([ Attr.create "autocomplete" "one-time-code"
                  ; Cn.with_class ?class_ [ "disabled:cursor-not-allowed" ]
                  ; data "input-otp" "true"
                  ; Attr.create
                      "inputmode"
                      (match pattern with
                       | None | Some Digits -> "numeric"
                       | Some (Chars | Digits_and_chars) -> "text")
                  ; Attr.create "maxlength" (Int.to_string max_length)
                  ; data "slot" "input-otp"
                  ; Attr.value value
                  ; style
                      [ "position", "absolute"
                      ; "inset", "0"
                      ; "width", "100%"
                      ; "height", "100%"
                      ; "display", "flex"
                      ; "text-align", "left"
                      ; "opacity", "1"
                      ; "color", "transparent"
                      ; "pointer-events", "all"
                      ; "background", "transparent"
                      ; "caret-color", "transparent"
                      ; "border", "0 solid transparent"
                      ; "outline", "0 solid transparent"
                      ; "box-shadow", "none"
                      ; "line-height", "1"
                      ; "letter-spacing", "-.5em"
                      ; "font-size", "36px"
                      ; "font-family", "monospace"
                      ; "font-variant-numeric", "tabular-nums"
                      ]
                  ; Attr.on_input on_input
                  ; Attr.create "spellcheck" "false"
                  ; Attr.on_focus (fun ev ->
                      Effect.Many
                        [ on_selection_change
                            (Some
                               ( Int.min (String.length value) (max_length - 1)
                               , String.length value ))
                        ; select_last (ev :> Dom_html.event Js.t) ~max_length
                        ])
                  ; Attr.on_blur (fun _ -> on_selection_change None)
                  ; selection_hook on_selection_change
                  ]
                  @ opt (fun p -> Attr.create "pattern" (Pattern.regexp p)) pattern
                  @ opt Attr.id id
                  @ (if disabled then [ Attr.disabled ] else [])
                  @ attrs)
               ()
           ]
       ])
;;

let component
  ?attrs
  ?class_
  ?container_class
  ?id
  ?disabled
  ?pattern
  ?(default_value = "")
  ~max_length
  children
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let value, set = Bonsai.state default_value ~equal:String.equal graph in
  let selection, set_selection =
    Bonsai.state None ~equal:[%equal: (int * int) option] graph
  in
  let%arr value and set and selection and set_selection and children in
  { Control.value
  ; set
  ; view =
      view
        ?attrs
        ?class_
        ?container_class
        ?id
        ?disabled
        ?pattern
        ~max_length
        ~value
        ~selection
        ~on_change:set
        ~on_selection_change:set_selection
        children
  }
;;

let group ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "input-otp-group"
       ; Cn.with_class ?class_ [ "has-aria-invalid:ring-destructive/20 \
                                  dark:has-aria-invalid:ring-destructive/40 \
                                  has-aria-invalid:border-destructive rounded-lg \
                                  has-aria-invalid:ring-3 flex items-center" ]
       ]
       @ attrs)
    children
;;

let slot ?(attrs = []) ?class_ t ~index =
  let char =
    if index < String.length t.value then Some (String.of_char t.value.[index]) else None
  in
  let active =
    match t.selection with
    | None -> false
    | Some (s, e) -> (s = e && index = s) || (index >= s && index < e)
  in
  Node.div
    ~attrs:
      ([ data "slot" "input-otp-slot"
       ; data "active" (Bool.to_string active)
       ; Cn.with_class
           ?class_
           [ "dark:bg-input/30 border-input data-[active=true]:border-ring \
              data-[active=true]:ring-ring/50 \
              data-[active=true]:aria-invalid:ring-destructive/20 \
              dark:data-[active=true]:aria-invalid:ring-destructive/40 \
              aria-invalid:border-destructive \
              data-[active=true]:aria-invalid:border-destructive size-8 border-y \
              border-r text-sm transition-all outline-none first:rounded-l-lg \
              first:border-l last:rounded-r-lg data-[active=true]:ring-3 relative flex \
              items-center justify-center data-[active=true]:z-10"
           ]
       ]
       @ attrs)
    (Option.value_map char ~default:[] ~f:(fun c -> [ Node.text c ])
     @
     if active && Option.is_none char
     then
       [ Node.div
           ~attrs:
             [ Attr.class_
                 "pointer-events-none absolute inset-0 flex items-center justify-center"
             ]
           [ Node.div ~attrs:[ Attr.class_ "animate-caret-blink bg-foreground h-4 w-px \
                                            duration-1000" ] [] ]
       ]
     else [])
;;

let separator ?(attrs = []) () =
  Node.div
    ~attrs:
      ([ data "slot" "input-otp-separator"
       ; Attr.class_ "[&_svg:not([class*='size-'])]:size-4 flex items-center"
       ; Attr.role "separator"
       ]
       @ attrs)
    [ Icon.view Minus ]
;;
