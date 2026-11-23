open! Import

type t =
  { id : string
  ; trigger_id : string
  ; modal : bool
  ; open_ : bool
  ; on_open_change : bool -> unit Effect.t
  }

let view ?(id = "popover") ?trigger_id ?(modal = false) ~open_ ~on_open_change f =
  let trigger_id = Option.value trigger_id ~default:(id ^ "-trigger") in
  Node.fragment (f { id; trigger_id; modal; open_; on_open_change })
;;

let component ?(default_open = false) ?trigger_id ?modal f (local_ graph) =
  let open Bonsai.Let_syntax in
  let open_, set_open = Bonsai.state default_open graph in
  let id = Bonsai.path_id graph in
  let%arr open_ and set_open and id and f in
  view ~id ?trigger_id ?modal ~open_ ~on_open_change:set_open f
;;

let is_open t = t.open_

let set_open t o = if Bool.equal o t.open_ then Effect.Ignore else t.on_open_change o
let trigger_id t = t.trigger_id
let content_id t = t.id ^ "-content"
let state t = data "state" (if t.open_ then "open" else "closed")

let trigger ?(attrs = []) t node =
  Slot.merge
    ~defaults:[ "type", Attr.type_ "button"; "data-slot", data "slot" "popover-trigger" ]
    ([ Attr.id (trigger_id t)
     ; aria "haspopup" "dialog"
     ; aria_bool "expanded" t.open_
     ]
     (* Radix names the content only while it is there. *)
     @ (if t.open_ then [ aria "controls" (content_id t) ] else [])
     @ [ state t
       ; Attr.on_click (fun _ -> t.on_open_change (not t.open_))
       ]
     @ attrs)
    node
;;

let content_class =
  "bg-popover text-popover-foreground data-open:animate-in data-closed:animate-out \
   data-closed:fade-out-0 data-open:fade-in-0 data-closed:zoom-out-95 \
   data-open:zoom-in-95 data-[side=bottom]:slide-in-from-top-2 \
   data-[side=left]:slide-in-from-right-2 data-[side=right]:slide-in-from-left-2 \
   data-[side=top]:slide-in-from-bottom-2 ring-foreground/10 flex flex-col gap-2.5 \
   rounded-lg p-2.5 text-sm shadow-md ring-1 duration-100 z-50 w-72 \
   origin-(--radix-popover-content-transform-origin) outline-hidden"
;;

let content
  ?(attrs = [])
  ?class_
  ?(side = Popper.Side.Bottom)
  ?(side_offset = 4.)
  ?(align = Popper.Align.Center)
  ?align_offset
  ?avoid_collisions
  t
  children
  =
  if not t.open_
  then Node.none
  else (
    let close (reason : Dismiss.Reason.t) =
      match reason with
      | Escape -> Effect.Many [ t.on_open_change false; Focus.restore (trigger_id t) ]
      | Pointer_outside | Focus_outside ->
        (* Radix: a non-modal popover closed from outside leaves the focus
           where that put it. *)
        if t.modal
        then t.on_open_change false
        else Effect.Many [ Focus.no_restore; t.on_open_change false ]
    in
    let id = content_id t in
    let content =
      Node.div
        ~attrs:
          ([ data "slot" "popover-content"
           ; Cn.with_class ?class_ [ content_class ]
           ; Attr.id id
           ; Attr.role "dialog"
           ; state t
           ; Attr.tabindex (-1)
           ; Focus.on_mount `First_tabbable
           ; Attr.on_keydown (fun ev ->
               match Dismiss.escape_keydown ~on_escape:(close Escape) ev with
               | Some effect -> effect
               | None -> Focus.loop_tab ev)
           ]
           @ Popper.content_attrs ~side ~align ()
           @ attrs)
        children
    in
    Node.div
      ~attrs:
        ([ Attr.style (Css_gen.create ~field:"display" ~value:"contents")
         ; Presence.attr
         ; Dismiss.attr
             ~inside:[ trigger_id t ]
             ~focus_outside:(not t.modal)
             ~modal:t.modal
             ~on_dismiss:close
             ()
         ]
         @ if t.modal then [ Scroll.lock ] else [])
      ((if t.modal then [ Focus.guard ~id `Start ] else [])
       @ [ Popper.wrapper
             ~name:"popover"
             ~side
             ~side_offset
             ~align
             ?align_offset
             ?avoid_collisions
             ~anchor:(Element (trigger_id t))
             content
         ]
       @ if t.modal then [ Focus.guard ~id `End ] else []))
;;

let header ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:
      ([ data "slot" "popover-header"; Cn.with_class ?class_ [ "flex flex-col gap-0.5 \
                                                                text-sm" ] ]
       @ attrs)
    children
;;

let title ?(attrs = []) ?class_ children =
  Node.div
    ~attrs:([ data "slot" "popover-title"; Cn.with_class ?class_ [ "font-medium" ] ] @ attrs)
    children
;;

let description ?(attrs = []) ?class_ children =
  Node.p
    ~attrs:
      ([ data "slot" "popover-description"
       ; Cn.with_class ?class_ [ "text-muted-foreground" ]
       ]
       @ attrs)
    children
;;
