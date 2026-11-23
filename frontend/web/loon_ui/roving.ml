open! Import

module Orientation = struct
  type t =
    | Horizontal
    | Vertical
  [@@deriving enumerate, sexp_of, compare, equal]

  let name t = String.lowercase (Sexp.to_string (sexp_of_t t))
end

let move ~enabled ~current (key : Js_of_ocaml.Dom_html.Keyboard_code.t) =
  let n = List.length enabled in
  let enabled = Array.of_list enabled in
  let rec step i delta remaining =
    if remaining = 0
    then None
    else (
      let i = (i + delta + n) % n in
      if enabled.(i) then Some i else step i delta (remaining - 1))
  in
  let first () = step (-1) 1 n in
  let last () = step n (-1) n in
  if n = 0
  then None
  else (
    match key with
    | ArrowRight | ArrowDown -> step current 1 n
    | ArrowLeft | ArrowUp -> step current (-1) n
    | Home -> first ()
    | End -> last ()
    | _ -> None)
;;

let in_browser () = Js_of_ocaml.Js.Optdef.test (Js_of_ocaml.Js.Unsafe.global##.document : _ Js_of_ocaml.Js.Optdef.t)

let next ev ~group ~item ~fallback ~current =
  let open Js_of_ocaml in
  let key = Dom_util.key ev in
  if in_browser ()
  then (
    let target : Dom_html.element Js.t = Js.Unsafe.get ev "currentTarget" in
    let root : Dom_html.element Js.t Js.Opt.t =
      Js.Unsafe.meth_call target "closest" [| Js.Unsafe.inject (Js.string group) |]
    in
    match Js.Opt.to_option root with
    | None -> None
    | Some root ->
      let els = Dom.list_of_nodeList (root##querySelectorAll (Js.string item)) in
      let enabled =
        List.map els ~f:(fun el ->
          not (Js.to_bool (el##hasAttribute (Js.string "disabled"))))
      in
      let current =
        List.findi els ~f:(fun _ el -> phys_equal (Js.Unsafe.coerce el) target)
        |> Option.value_map ~default:0 ~f:fst
      in
      Option.map (move ~enabled ~current key) ~f:(fun i ->
        let el = List.nth_exn els i in
        let value =
          Js.Opt.case (el##getAttribute (Js.string "value")) (fun () -> "") Js.to_string
        in
        value, Effect.of_thunk (fun () -> el##focus)))
  else (
    let current =
      List.findi fallback ~f:(fun _ (v, _) -> String.equal v current)
      |> Option.value_map ~default:0 ~f:fst
    in
    Option.map
      (move ~enabled:(List.map fallback ~f:snd) ~current key)
      ~f:(fun i -> fst (List.nth_exn fallback i), Effect.Ignore))
;;

(* Radix's RovingFocusGroup tab stop, kept in the DOM: the group is the tab
   stop until an item is focused, which then is (the vdom renders every item
   [tabindex="-1"] and never changes it, so setting it here lasts). *)
module Tab_stop = struct
  open Js_of_ocaml

  let items (group : Dom_html.element Js.t) ~item =
    Dom.list_of_nodeList (group##querySelectorAll (Js.string item))
    |> List.filter ~f:(fun (el : Dom_html.element Js.t) ->
      not (Js.to_bool (el##hasAttribute (Js.string "disabled"))))
  ;;

  let current_target ev : Dom_html.element Js.t = Js.Unsafe.get ev "currentTarget"
  let target ev : Dom_html.element Js.t = Js.Unsafe.get ev "target"

  let group_attrs ~item ~active ~focusable =
    [ Attr.tabindex (if focusable then 0 else -1)
    ; Attr.on_focus (fun ev ->
        if not (in_browser ())
        then Effect.Ignore
        else
          Effect.of_thunk (fun () ->
            let group = current_target ev in
            (* Focus on the group itself (Tab into it): to its active item,
               the last focused, or the first. *)
            if phys_equal (target ev) group
            then (
              let items = items group ~item in
              let pick =
                List.find items ~f:(fun el ->
                  Js.to_bool (el##matches (Js.string active)))
                |> Option.first_some
                     (List.find items ~f:(fun el ->
                        Js.Opt.case
                          (el##getAttribute (Js.string "tabindex"))
                          (fun () -> false)
                          (fun s -> String.equal (Js.to_string s) "0")))
                |> Option.first_some (List.hd items)
              in
              Option.iter pick ~f:(fun el -> el##focus))))
    ; Attr.on_focusout (fun ev ->
        if not (in_browser ())
        then Effect.Ignore
        else
          Effect.of_thunk (fun () ->
            let group = current_target ev in
            if focusable then group##setAttribute (Js.string "tabindex") (Js.string "0")))
    ]
  ;;

  let item_attrs ~group ~item =
    [ Attr.tabindex (-1)
    ; Attr.on_focus (fun ev ->
        if not (in_browser ())
        then Effect.Ignore
        else
          Effect.of_thunk (fun () ->
            let el = current_target ev in
            let root : Dom_html.element Js.t Js.Opt.t =
              Js.Unsafe.meth_call el "closest" [| Js.Unsafe.inject (Js.string group) |]
            in
            Js.Opt.iter root (fun root ->
              List.iter
                (Dom.list_of_nodeList (root##querySelectorAll (Js.string item)))
                ~f:(fun (other : Dom_html.element Js.t) ->
                  other##setAttribute
                    (Js.string "tabindex")
                    (Js.string (if phys_equal other el then "0" else "-1"))))))
    ; Attr.on_keydown (fun ev ->
        (* Shift+Tab out of the group: past the group, not into it. *)
        if in_browser ()
           && Js.to_bool ev##.shiftKey
           && (match Dom_util.key ev with
               | Tab -> true
               | _ -> false)
        then
          Effect.of_thunk (fun () ->
            let root : Dom_html.element Js.t Js.Opt.t =
              Js.Unsafe.meth_call
                (current_target ev)
                "closest"
                [| Js.Unsafe.inject (Js.string group) |]
            in
            Js.Opt.iter root (fun root ->
              root##setAttribute (Js.string "tabindex") (Js.string "-1")))
        else Effect.Ignore)
    ]
  ;;
end
