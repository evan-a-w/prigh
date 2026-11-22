open! Import
open Js_of_ocaml

type t =
  { id : string
  ; open_ : bool
  ; on_open_change : bool -> unit Effect.t
  }

let view ?(id = "tooltip") ~open_ ~on_open_change f =
  Node.fragment (f { id; open_; on_open_change })
;;

let component ?(default_open = false) f (local_ graph) =
  let open Bonsai.Let_syntax in
  let open_, set_open = Bonsai.state default_open graph in
  let id = Bonsai.path_id graph in
  let%arr open_ and set_open and id and f in
  view ~id ~open_ ~on_open_change:set_open f
;;

let trigger_id t = t.id ^ "-trigger"
let content_id t = t.id ^ "-content"
let text_id t = t.id ^ "-tooltip"
let state t = data "state" (if t.open_ then "delayed-open" else "closed")
let set t o = if Bool.equal o t.open_ then Effect.Ignore else t.on_open_change o

(* Whether the pointer went to an element inside the one with id [id]. *)
let went_into (ev : Dom_html.mouseEvent Js.t) id =
  match
    ( Js.Opt.to_option ev##.relatedTarget
    , Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) )
  with
  | Some target, Some el -> Js.to_bool ((Js.Unsafe.coerce el)##contains target)
  | _ -> false
;;

let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

let focus_visible (ev : Dom_html.focusEvent Js.t) =
  if not (in_browser ())
  then true
  else (
    Js.Opt.case
      ev##.target
      (fun () -> true)
      (fun target ->
        try Js.to_bool ((Js.Unsafe.coerce target)##matches (Js.string ":focus-visible")) with
        | _ -> true))
;;

let trigger ?(attrs = []) t node =
  Slot.merge
    ~defaults:[ "data-slot", data "slot" "tooltip-trigger" ]
    ([ Attr.id (trigger_id t); state t ]
     @ (if t.open_ then [ aria "describedby" (text_id t) ] else [])
     @ [ Attr.on_mousemove (fun _ -> set t true)
       ; Attr.on_mouseleave (fun ev ->
           if in_browser () && went_into ev (content_id t)
           then Effect.Ignore
           else set t false)
       ; Attr.on_focus (fun ev -> if focus_visible ev then set t true else Effect.Ignore)
       ; Attr.on_blur (fun _ -> set t false)
       ; Attr.on_pointerdown (fun _ -> set t false)
       ]
     @ attrs)
    node
;;

let content_class =
  "data-open:animate-in data-open:fade-in-0 data-open:zoom-in-95 \
   data-[state=delayed-open]:animate-in data-[state=delayed-open]:fade-in-0 \
   data-[state=delayed-open]:zoom-in-95 data-closed:animate-out data-closed:fade-out-0 \
   data-closed:zoom-out-95 data-[side=bottom]:slide-in-from-top-2 \
   data-[side=left]:slide-in-from-right-2 data-[side=right]:slide-in-from-left-2 \
   data-[side=top]:slide-in-from-bottom-2 inline-flex items-center gap-1.5 rounded-md \
   px-3 py-1.5 text-xs has-data-[slot=kbd]:pr-1.5 **:data-[slot=kbd]:relative \
   **:data-[slot=kbd]:isolate **:data-[slot=kbd]:z-50 **:data-[slot=kbd]:rounded-sm \
   z-50 w-fit max-w-xs origin-(--radix-tooltip-content-transform-origin) bg-foreground \
   text-background"
;;

let arrow_class =
  "size-2.5 rotate-45 rounded-[2px] z-50 translate-y-[calc(-50%_-_2px)] bg-foreground \
   fill-foreground"
;;

let visually_hidden =
  "position:absolute;border:0;width:1px;height:1px;padding:0;margin:-1px;\
   overflow:hidden;clip:rect(0, 0, 0, 0);white-space:nowrap;word-wrap:normal"
;;

let content
  ?(attrs = [])
  ?class_
  ?(side = Popper.Side.Top)
  ?(side_offset = 0.)
  ?(align = Popper.Align.Center)
  ?align_offset
  ?avoid_collisions
  t
  children
  =
  if not t.open_
  then Node.none
  else (
    let close (_ : Dismiss.Reason.t) = set t false in
    let content =
      Node.div
        ~attrs:
          ([ data "slot" "tooltip-content"
           ; Cn.with_class ?class_ [ content_class ]
           ; Attr.id (content_id t)
           ; state t
           ; Attr.on_mouseleave (fun ev ->
               if in_browser () && went_into ev (trigger_id t)
               then Effect.Ignore
               else set t false)
           ]
           @ Popper.content_attrs ~side ~align ()
           @ attrs)
          (children
           @ [ Node.span
                 ~attrs:
                   [ Attr.id (text_id t)
                   ; Attr.role "tooltip"
                   ; Attr.create "style" visually_hidden
                   ]
                 children
             ; Popper.arrow ~class_:arrow_class ()
             ])
    in
    Node.div
      ~attrs:
        [ Attr.style (Css_gen.create ~field:"display" ~value:"contents")
        ; Presence.attr
        ; Dismiss.attr ~inside:[ trigger_id t ] ~on_dismiss:close ()
        ]
      [ Popper.wrapper
          ~name:"tooltip"
          ~side
          ~side_offset
          ~align
          ?align_offset
          ?avoid_collisions
          ~anchor:(Element (trigger_id t))
          content
      ])
;;
