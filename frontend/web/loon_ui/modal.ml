open! Import

type t =
  { id : string
  ; trigger_id : string
  ; open_ : bool
  ; on_open_change : bool -> unit Effect.t
  }

let trigger_id t = t.trigger_id
let content_id t = t.id ^ "-content"
let title_id t = t.id ^ "-title"
let description_id t = t.id ^ "-description"
let state_attr t = data "state" (if t.open_ then "open" else "closed")

let view ?(id = "dialog") ?trigger_id ~open_ ~on_open_change f =
  let trigger_id = Option.value trigger_id ~default:(id ^ "-trigger") in
  Node.fragment (f { id; trigger_id; open_; on_open_change })
;;

let component ?(default_open = false) f (local_ graph) =
  let open Bonsai.Let_syntax in
  let open_, set_open = Bonsai.state default_open graph in
  let id = Bonsai.path_id graph in
  let%arr open_ and set_open and id and f in
  view ~id ~open_ ~on_open_change:set_open f
;;

let trigger ~slot ?(attrs = []) t node =
  Slot.merge
    ~defaults:[ "type", Attr.type_ "button"; "data-slot", data "slot" slot ]
    ([ Attr.id (trigger_id t)
     ; aria "haspopup" "dialog"
     ; aria_bool "expanded" t.open_
     ; aria "controls" (content_id t)
     ; state_attr t
     ; Attr.on_click (fun _ -> t.on_open_change (not t.open_))
     ]
     @ attrs)
    node
;;

let close ?slot ?(attrs = []) t node =
  Slot.merge
    ~defaults:
      (("type", Attr.type_ "button")
       :: Option.value_map slot ~default:[] ~f:(fun s -> [ "data-slot", data "slot" s ]))
    ([ Attr.on_click (fun _ -> t.on_open_change false) ] @ attrs)
    node
;;

let pointer_events_auto = Attr.style (Css_gen.create ~field:"pointer-events" ~value:"auto")

let overlay ~slot ~class_ t =
  Node.div ~attrs:[ data "slot" slot; Attr.class_ class_; state_attr t; pointer_events_auto ] []
;;

let content
      ?(dismissible = true)
      ?(auto_focus = true)
      ?initial_focus
      ?(role = "dialog") ~overlay ~content_attrs t children =
  if not t.open_
  then Node.none
  else (
    let id = content_id t in
    let close = t.on_open_change false in
    (* The page's pointerdown outside (Dismiss) closes it in a browser; the
       click also does, where there is no pointer (tests). *)
    let overlay =
      if dismissible then Slot.merge [ Attr.on_click (fun _ -> close) ] overlay else overlay
    in
    let content =
      Node.div
        ~attrs:
          ([ Attr.role role
           ; Attr.id id
           ; aria "describedby" (description_id t)
           ; aria "labelledby" (title_id t)
           ; state_attr t
           ; Attr.tabindex (-1)
           ; pointer_events_auto
           ; Scroll.lock
           ; Focus.on_mount
               (match initial_focus with
                | _ when not auto_focus -> `Keep
                | Some s -> `Selector s
                | None -> `First_tabbable)
           ; Attr.on_keydown (fun ev ->
               Option.value (Dismiss.escape_keydown ~on_escape:close ev) ~default:Effect.Ignore)
           ; Dismiss.attr
               ~modal:true
               ~on_dismiss:(fun (reason : Dismiss.Reason.t) ->
                 match reason with
                 | Escape -> close
                 | Pointer_outside | Focus_outside ->
                   if dismissible then close else Effect.Ignore)
               ()
           ]
           @ content_attrs)
        children
    in
    Node.div
      ~attrs:
        (Presence.attr :: Portal.fullscreen)
      [ overlay; Focus.guard ~id `Start; content; Focus.guard ~id `End ])
;;

let title ~slot ~class_ ?(attrs = []) t children =
  Node.h2 ~attrs:([ data "slot" slot; Attr.class_ class_; Attr.id (title_id t) ] @ attrs) children
;;

let description ~slot ~class_ ?(attrs = []) t children =
  Node.p
    ~attrs:([ data "slot" slot; Attr.class_ class_; Attr.id (description_id t) ] @ attrs)
    children
;;
