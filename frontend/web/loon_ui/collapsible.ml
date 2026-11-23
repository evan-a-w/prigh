open! Import
open Js_of_ocaml

type t =
  { open_ : bool
  ; on_change : bool -> unit Effect.t
  ; id : string
  ; disabled : bool
  }

let is_open t = t.open_

let class_attr = function
  | None -> []
  | Some class_ -> [ Cn.attr [ class_ ] ]
;;

let state open_ = if open_ then "open" else "closed"
let content_id t = t.id ^ "-content"

(* Radix's Presence and CollapsibleContent's measuring, on the DOM: the
   content's size goes into --radix-collapsible-content-{height,width}
   (measured with its animation off), and a closing content is shown again
   until its animation ends. Virtual_dom sets [hidden] first, so the work
   waits for a microtask. *)
module Presence = struct
  let microtask f =
    Js.Unsafe.fun_call
      Js.Unsafe.global##.queueMicrotask
      [| Js.Unsafe.inject (Js.wrap_callback f) |]
  ;;

  let measure ~accordion (el : Dom_html.element Js.t) =
    el##.style##setProperty
      (Js.string "animation-name")
      (Js.string "none")
      Js.undefined
    |> ignore;
    let names = "collapsible" :: (if accordion then [ "accordion" ] else []) in
    (* The variables off while measuring: a child sized by them (the
       accordion content's inner div) would keep their old value. *)
    List.iter names ~f:(fun name ->
      List.iter [ "height"; "width" ] ~f:(fun dim ->
        el##.style##removeProperty (Js.string ("--radix-" ^ name ^ "-content-" ^ dim))
        |> ignore));
    let rect = el##getBoundingClientRect in
    let px f = Js.string (sprintf "%gpx" (Js.to_float f)) in
    let set name value =
      el##.style##setProperty (Js.string name) value Js.undefined |> ignore
    in
    List.iter names ~f:(fun name ->
        set ("--radix-" ^ name ^ "-content-height") (px rect##.height);
        set ("--radix-" ^ name ^ "-content-width") (px rect##.width))
  ;;

  let animate (el : Dom_html.element Js.t) =
    el##.style##removeProperty (Js.string "animation-name") |> ignore
  ;;

  let is_closed (el : Dom_html.element Js.t) =
    Js.Opt.case
      (el##getAttribute (Js.string "data-state"))
      (fun () -> false)
      (fun s -> String.equal (Js.to_string s) "closed")
  ;;

  let hide el =
    if is_closed el then el##setAttribute (Js.string "hidden") (Js.string "")
  ;;

  let change ~accordion (el : Dom_html.element Js.t) ~open_ =
    if not open_ then el##removeAttribute (Js.string "hidden");
    measure ~accordion el;
    animate el;
    if not open_
    then (
      let animation =
        Js.to_string (Dom_html.window##getComputedStyle el)##.animationName
      in
      if String.equal animation "none" || String.is_empty animation
      then hide el
      else (
        let listener = ref None in
        listener
        := Some
             (Dom_html.addEventListener
                el
                (Dom_html.Event.make "animationend")
                (Dom_html.handler (fun ev ->
                   if Js.Unsafe.equals (Js.Unsafe.get ev "target") el
                   then (
                     Option.iter !listener ~f:Dom_html.removeEventListener;
                     hide el);
                   Js._true))
                Js._false)))
  ;;

  module Input = struct
    type t =
      { open_ : bool
      ; accordion : bool
      }
    [@@deriving sexp_of]

    let combine _ b = b
  end

  module Hook = Vdom.Attr.Hooks.Make (struct
      module State = struct
        type t = { mutable open_ : bool }
      end

      module Input = Input

      let init (input : Input.t) _ = { State.open_ = input.open_ }

      (* No animation for what is open from the start (Radix prevents the
         mount animation too): it stays off until the first change. *)
      let on_mount =
        `Schedule_immediately_after_this_dom_patch_completes
          (fun (input : Input.t) _ el -> if input.open_ then measure ~accordion:input.accordion el)
      ;;

      let update ~old_input:_ ~new_input:({ open_; accordion } : Input.t) (state : State.t) el =
        if not (Bool.equal state.open_ open_)
        then (
          state.open_ <- open_;
          microtask (fun () -> change ~accordion el ~open_))
      ;;

      let destroy _ _ _ = ()
    end)

  let attr ?(accordion = false) ~open_ () =
    Vdom.Attr.create_hook
      "loon-collapsible-presence"
      (Hook.create { Input.open_; accordion })
  ;;
end

let presence = Presence.attr

let create ?(disabled = false) ?(id = "collapsible") ~open_ ~on_change () =
  { open_; on_change; id; disabled }
;;

let root_attrs t =
  data "state" (state t.open_) :: (if t.disabled then [ data "disabled" "" ] else [])
;;

let view ?(attrs = []) ?class_ ?disabled ?id ~open_ ~on_change children =
  let t = create ?disabled ?id ~open_ ~on_change () in
  Node.div
    ~attrs:
      ([ data "state" (state open_); data "slot" "collapsible" ]
       @ class_attr class_
       @ (if t.disabled then [ data "disabled" "" ] else [])
       @ attrs)
    (children t)
;;

let component
      ?attrs
      ?class_
      ?disabled
      ?(default_open = false)
      children
      (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let open_, set_open = Bonsai.state default_open graph in
  let id = Bonsai.path_id graph in
  let%arr open_
  and set_open
  and id
  and children in
  view ?attrs ?class_ ?disabled ~id ~open_ ~on_change:set_open children
;;

let trigger_attrs t =
  [ aria "controls" (content_id t)
  ; aria_bool "expanded" t.open_
  ; data "state" (state t.open_)
  ]
  @
  if t.disabled
  then [ data "disabled" ""; Attr.disabled ]
  else [ Attr.on_click (fun _ -> t.on_change (not t.open_)) ]
;;

let trigger_child ?(attrs = []) t node =
  Slot.merge
    ~defaults:
      [ "type", Attr.type_ "button"; "data-slot", data "slot" "collapsible-trigger" ]
    (trigger_attrs t @ attrs)
    node
;;

let trigger ?(attrs = []) ?class_ t children =
  Node.button
    ~attrs:
      ([ Attr.type_ "button" ]
       @ trigger_attrs t
       @ [ data "slot" "collapsible-trigger" ]
       @ class_attr class_
       @ attrs)
    children
;;

let content ?(attrs = []) ?class_ t children =
  Node.div
    ~attrs:
      ([ data "state" (state t.open_); Attr.id (content_id t) ]
       @ (if t.disabled then [ data "disabled" "" ] else [])
       @ (if t.open_ then [] else [ Attr.hidden ])
       @ [ data "slot" "collapsible-content"; Presence.attr ~open_:t.open_ () ]
       @ class_attr class_
       @ attrs)
    children
;;
