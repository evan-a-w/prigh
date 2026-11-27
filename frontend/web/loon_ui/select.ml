open! Import
open Js_of_ocaml

module Size = struct
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Sm -> "sm"
  ;;
end

module Position = struct
  type t =
    | Item_aligned
    | Popper
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Item_aligned -> "item-aligned"
    | Popper -> "popper"
  ;;
end

module State = struct
  type t =
    { open_ : bool
    ; highlighted : string option
    }
  [@@deriving sexp_of, equal]

  let closed = { open_ = false; highlighted = None }
end

type part =
  | Group of
      { attrs : Attr.t list
      ; class_ : string option
      ; parts : part list
      }
  | Label of
      { attrs : Attr.t list
      ; class_ : string option
      ; children : Node.t list
      }
  | Item of
      { attrs : Attr.t list
      ; class_ : string option
      ; value : string
      ; text_value : string
      ; disabled : bool
      ; children : Node.t list
      }
  | Separator of
      { attrs : Attr.t list
      ; class_ : string option
      }

let group ?(attrs = []) ?class_ parts = Group { attrs; class_; parts }
let label ?(attrs = []) ?class_ children = Label { attrs; class_; children }

let item ?(attrs = []) ?class_ ?text_value ?(disabled = false) ~value children =
  let text_value =
    Option.value_or_thunk text_value ~default:(fun () -> List_nav.text_of_nodes children)
  in
  Item { attrs; class_; value; text_value; disabled; children }
;;

let separator ?(attrs = []) ?class_ () = Separator { attrs; class_ }

module Trigger = struct
  type t =
    { attrs : Attr.t list
    ; class_ : string option
    ; id : string option
    ; size : Size.t
    ; placeholder : string option
    ; value_class : string option
    }
end

module Content = struct
  type t =
    { attrs : Attr.t list
    ; class_ : string option
    ; position : Position.t
    ; side : Popper.Side.t
    ; side_offset : float
    ; align : Popper.Align.t
    ; align_offset : float
    ; parts : part list
    }
end

let trigger
  ?(attrs = [])
  ?class_
  ?id
  ?(size = Size.Default)
  ?placeholder
  ?value_class
  ()
  : Trigger.t
  =
  { attrs; class_; id; size; placeholder; value_class }
;;

let content
  ?(attrs = [])
  ?class_
  ?(position = Position.Item_aligned)
  ?(side = Popper.Side.Bottom)
  ?(side_offset = 0.)
  ?(align = Popper.Align.Center)
  ?(align_offset = 0.)
  parts
  : Content.t
  =
  { attrs; class_; position; side; side_offset; align; align_offset; parts }
;;

let trigger_class =
  "border-input data-placeholder:text-muted-foreground dark:bg-input/30 \
   dark:hover:bg-input/50 focus-visible:border-ring focus-visible:ring-ring/50 \
   aria-invalid:ring-destructive/20 dark:aria-invalid:ring-destructive/40 \
   aria-invalid:border-destructive dark:aria-invalid:border-destructive/50 gap-1.5 \
   rounded-lg border bg-transparent py-2 pr-2 pl-2.5 text-sm transition-colors \
   select-none focus-visible:ring-3 aria-invalid:ring-3 data-[size=default]:h-8 \
   data-[size=sm]:h-7 data-[size=sm]:rounded-[min(var(--radius-md),10px)] \
   *:data-[slot=select-value]:gap-1.5 [&_svg:not([class*='size-'])]:size-4 flex w-fit \
   items-center justify-between whitespace-nowrap outline-none \
   disabled:cursor-not-allowed disabled:opacity-50 \
   *:data-[slot=select-value]:line-clamp-1 *:data-[slot=select-value]:flex \
   *:data-[slot=select-value]:items-center [&_svg]:pointer-events-none [&_svg]:shrink-0"
;;

let content_class =
  "bg-popover text-popover-foreground data-open:animate-in data-closed:animate-out \
   data-closed:fade-out-0 data-open:fade-in-0 data-closed:zoom-out-95 \
   data-open:zoom-in-95 data-[side=bottom]:slide-in-from-top-2 \
   data-[side=left]:slide-in-from-right-2 data-[side=right]:slide-in-from-left-2 \
   data-[side=top]:slide-in-from-bottom-2 ring-foreground/10 min-w-36 rounded-lg \
   shadow-md ring-1 duration-100 cn-menu-target relative z-50 \
   max-h-(--radix-select-content-available-height) \
   origin-(--radix-select-content-transform-origin) overflow-x-hidden overflow-y-auto \
   data-[align-trigger=true]:animate-none"
;;

let popper_content_class =
  "data-[side=bottom]:translate-y-1 data-[side=left]:-translate-x-1 \
   data-[side=right]:translate-x-1 data-[side=top]:-translate-y-1"
;;

let viewport_class =
  "data-[position=popper]:h-(--radix-select-trigger-height) \
   data-[position=popper]:w-full \
   data-[position=popper]:min-w-(--radix-select-trigger-width)"
;;

let item_class =
  "focus:bg-accent focus:text-accent-foreground \
   not-data-[variant=destructive]:focus:**:text-accent-foreground gap-1.5 rounded-md \
   py-1 pr-8 pl-1.5 text-sm [&_svg:not([class*='size-'])]:size-4 *:[span]:last:flex \
   *:[span]:last:items-center *:[span]:last:gap-2 relative flex w-full cursor-default \
   items-center outline-hidden select-none data-disabled:pointer-events-none \
   data-disabled:opacity-50 [&_svg]:pointer-events-none [&_svg]:shrink-0"
;;


let rec items_of parts =
  List.concat_map parts ~f:(function
    | Item { value; text_value; disabled; children; _ } ->
      [ value, text_value, disabled, children ]
    | Group { parts; _ } -> items_of parts
    | Label _ | Separator _ -> [])
;;

(* Radix's scroll buttons: shown while the viewport can scroll their way;
   resting the pointer on one scrolls. *)
module Scroll_buttons = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t =
        { mutable listeners : Dom.event_listener_id list
        ; mutable timer : Dom_html.interval_id option
        }
    end

    module Input = struct
      (* [true]: the popper position, which leaves the buttons to this hook;
         the item-aligned one shows them as it places the content. *)
      type t = bool [@@deriving sexp_of]

      let combine _ second = second
    end

    let init _ _ = { State.listeners = []; timer = None }

    let query (el : Dom_html.element Js.t) s =
      Js.Opt.to_option (el##querySelector (Js.string s))
    ;;

    let set_hidden (el : Dom_html.element Js.t option) hidden =
      Option.iter el ~f:(fun el ->
        if hidden
        then el##setAttribute (Js.string "hidden") (Js.string "")
        else el##removeAttribute (Js.string "hidden"))
    ;;

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun is_popper (state : State.t) (content : Dom_html.element Js.t) ->
          match query content "[data-radix-select-viewport]" with
          | None -> ()
          | Some vp ->
            let up = query content "[data-slot=select-scroll-up-button]" in
            let down = query content "[data-slot=select-scroll-down-button]" in
            let dir = ref 0. in
            let stop () =
              Option.iter state.timer ~f:(fun t -> Dom_html.window##clearInterval t);
              state.timer <- None;
              dir := 0.
            in
            let update () =
              let top = Js.to_float vp##.scrollTop in
              let max_top = Float.of_int (vp##.scrollHeight - vp##.clientHeight) in
              let hide_up = Float.(top <= 0.) in
              let hide_down = Float.(round_up top >= max_top) in
              set_hidden up hide_up;
              set_hidden down hide_down;
              if (hide_up && Float.(!dir < 0.)) || (hide_down && Float.(!dir > 0.))
              then stop ()
            in
            let listen target event f =
              Dom_html.addEventListener
                target
                event
                (Dom_html.handler (fun _ ->
                   f ();
                   Js._true))
                Js._false
            in
            let scroll_by button dy =
              match button with
              | None -> []
              | Some b ->
                let start () =
                  if Option.is_none state.timer
                  then (
                    dir := dy;
                    state.timer
                    <- Some
                         (Dom_html.window##setInterval
                            (Js.wrap_callback (fun () ->
                               vp##.scrollTop := Js.float (Js.to_float vp##.scrollTop +. dy)))
                            (Js.float 50.)))
                in
                [ listen b Dom_html.Event.pointerdown start
                ; listen b Dom_html.Event.pointermove start
                ; listen b Dom_html.Event.pointerup stop
                ; listen b (Dom_html.Event.make "pointercancel") stop
                ; listen b Dom_html.Event.pointerleave stop
                ]
            in
            state.listeners
            <- (listen vp Dom_html.Event.scroll update :: scroll_by up (-32.))
               @ scroll_by down 32.;
            if is_popper
            then
              ignore
                (Dom_html.window##requestAnimationFrame
                   (Js.wrap_callback (fun (_ : Js.number_t) ->
                      ignore
                        (Dom_html.window##requestAnimationFrame
                           (Js.wrap_callback (fun (_ : Js.number_t) -> update ())))))
                 : Dom_html.animation_frame_request_id))
    ;;

    let update ~old_input:_ ~new_input:_ _ _ = ()

    let destroy _ (state : State.t) _ =
      Option.iter state.timer ~f:(fun t -> Dom_html.window##clearInterval t);
      state.timer <- None;
      List.iter state.listeners ~f:Dom_html.removeEventListener;
      state.listeners <- []
    ;;
  end)

let view
  ?(id = "select")
  ?name
  ?(disabled = false)
  ?(required = false)
  ?(invalid = false)
  ~value
  ~on_value_change
  ~state
  ~on_state_change
  (trigger : Trigger.t)
  (content : Content.t)
  =
  let state : State.t = state in
  let value : string option = value in
  let trigger_id = Option.value trigger.id ~default:(id ^ "-trigger") in
  let content_id = id ^ "-content" in
  let items = items_of content.parts in
  let enabled =
    List.filter_map items ~f:(fun (v, text, disabled, _) ->
      if disabled then None else Some (v, text))
  in
  let selected =
    Option.bind value ~f:(fun v ->
      List.find items ~f:(fun (v', _, _, _) -> String.equal v v'))
  in
  let item_id v =
    (* ids from values, which may hold any character *)
    let i =
      List.findi items ~f:(fun _ (v', _, _, _) -> String.equal v v')
      |> Option.value_map ~default:0 ~f:fst
    in
    sprintf "%s-item-%d" id i
  in
  let open_with highlighted =
    let highlighted =
      match highlighted with
      | Some v -> Some v
      | None -> Option.map (List.hd enabled) ~f:fst
    in
    on_state_change { State.open_ = true; highlighted }
  in
  let close =
    Effect.Many [ on_state_change State.closed; Focus.restore trigger_id ]
  in
  let choose v =
    Effect.Many
      [ (if Option.equal String.equal value (Some v) then Effect.Ignore else on_value_change v)
      ; close
      ]
  in
  let typeahead current key =
    match List_nav.letter key with
    | None -> None
    | Some _ -> List_nav.move ~equal:String.equal ~items:enabled ~current key
  in
  let trigger_node =
    let t = trigger in
    Node.button
      ~attrs:
        ([ Attr.type_ "button"
         ; Attr.role "combobox"
         ; aria "controls" content_id
         ; aria_bool "expanded" state.open_
         ]
         @ (if required then [ aria "required" "true" ] else [])
         @ [ aria "autocomplete" "none"
           ; Attr.id trigger_id
           ; Attr.create "dir" "ltr"
           ; data "state" (if state.open_ then "open" else "closed")
           ]
         @ (if disabled then [ Attr.disabled; data "disabled" "" ] else [])
         @ (if Option.is_none selected then [ data "placeholder" "" ] else [])
         @ (if invalid then [ aria "invalid" "true" ] else [])
         @ [ data "slot" "select-trigger"
           ; data "size" (Size.name t.size)
           ; Cn.with_class ?class_:t.class_ [ trigger_class ]
           ; Attr.on_click (fun _ ->
               if state.open_ then close else open_with value)
           ; Attr.on_keydown (fun ev ->
               match Dom_util.key ev with
               | Enter | Space | ArrowDown | ArrowUp ->
                 Effect.Many [ Dom_util.prevent_default ev; open_with value ]
               | key ->
                 (* Radix: typing on the closed trigger picks the item *)
                 (match typeahead value key with
                  | Some v when not state.open_ -> on_value_change v
                  | _ -> Effect.Ignore))
           ]
         @ t.attrs)
      [ Node.span
          ~attrs:
            ([ data "slot" "select-value"; Attr.style (Css_gen.create ~field:"pointer-events" ~value:"none") ]
             @ Option.value_map t.value_class ~default:[] ~f:(fun c -> [ Attr.class_ c ]))
          (match selected with
           | Some (_, _, _, children) -> children
           | None -> [ Node.text (Option.value t.placeholder ~default:"") ])
      ; Icon.view ~class_:"text-muted-foreground size-4 pointer-events-none" Chevron_down
      ]
  in
  let content_node =
    if not state.open_
    then Node.none
    else (
      let c = content in
      let highlight v = on_state_change { state with highlighted = Some v } in
      (* Radix's onItemLeave: no item highlighted, the content focused. *)
      let leave =
        if Option.is_none state.highlighted
        then Effect.Ignore
        else
          Effect.Many
            [ on_state_change { state with highlighted = None }
            ; Dom_util.focus_selector_now (sprintf "[id=%S]" content_id)
            ]
      in
      let rec render parts =
        List.map parts ~f:(function
          | Group { attrs; class_; parts } ->
            Node.div
              ~attrs:
                ([ Attr.role "group"
                 ; data "slot" "select-group"
                 ; Cn.with_class ?class_ [ "scroll-my-1 p-1" ]
                 ]
                 @ attrs)
              (render parts)
          | Label { attrs; class_; children } ->
            Node.div
              ~attrs:
                ([ data "slot" "select-label"
                 ; Cn.with_class ?class_ [ "text-muted-foreground px-1.5 py-1 text-xs" ]
                 ]
                 @ attrs)
              children
          | Separator { attrs; class_ } ->
            Node.div
              ~attrs:
                ([ aria "hidden" "true"
                 ; data "slot" "select-separator"
                 ; Cn.with_class ?class_ [ "bg-border -mx-1 my-1 h-px \
                                            pointer-events-none" ]
                 ]
                 @ attrs)
              []
          | Item { attrs; class_; value = v; text_value = _; disabled; children } ->
            let checked = Option.equal String.equal value (Some v) in
            let highlighted = Option.equal String.equal state.highlighted (Some v) in
            let text_id = item_id v ^ "-text" in
            Node.div
              ~attrs:
                ([ Attr.role "option"
                 ; aria "labelledby" text_id
                 ]
                 @ (if highlighted then [ data "highlighted" "" ] else [])
                 @ [ aria_bool "selected" (checked && highlighted)
                   ; data "state" (if checked then "checked" else "unchecked")
                   ]
                 @ (if disabled then [ aria "disabled" "true"; data "disabled" "" ] else [])
                 @ [ Attr.tabindex (-1)
                   ; Attr.id (item_id v)
                   ; data "slot" "select-item"
                   ; Cn.with_class ?class_ [ item_class ]
                   ; Focus.when_ highlighted
                   ]
                 @ (if disabled
                    then [ Attr.on_pointermove (fun _ -> leave) ]
                    else
                      [ Attr.on_pointermove (fun _ ->
                          if highlighted then Effect.Ignore else highlight v)
                      ; Attr.on_pointerleave (fun _ ->
                          if highlighted then leave else Effect.Ignore)
                      ; Attr.on_click (fun _ -> choose v)
                      ])
                 @ attrs)
              [ Node.span
                  ~attrs:[ Attr.class_ "pointer-events-none absolute right-2 flex \
                                        size-4 items-center justify-center" ]
                  (if checked
                   then
                     [ Icon.view
                         ~class_:"pointer-events-none"
                         Check
                     ]
                   else [])
              ; Node.span ~attrs:[ Attr.id text_id ] children
              ])
      in
      let on_keydown ev =
        let key = Dom_util.key ev in
        match key with
        | Escape -> Effect.Many [ Dom_util.prevent_default ev; Dom_util.stop ev; close ]
        | Tab -> Dom_util.prevent_default ev
        | Enter | Space ->
          Effect.Many
            [ Dom_util.prevent_default ev
            ; (match state.highlighted with
               | Some v -> choose v
               | None -> Effect.Ignore)
            ]
        | _ ->
          (match
             List_nav.move ~loop:false ~equal:String.equal ~items:enabled ~current:state.highlighted key
           with
           | Some v ->
             Effect.Many
               [ Dom_util.prevent_default ev
               ; highlight v
               ; Scroll.into_view (sprintf "[id=%S]" (item_id v))
               ]
           | None -> Effect.Ignore)
      in
      let is_popper = Position.equal c.position Popper in
      let content_el =
        Node.div
          ~attrs:
            ([ data "slot" "select-content"
             ; data "align-trigger" (Bool.to_string (not is_popper))
             ; Attr.role "listbox"
             ; Attr.id content_id
             ; data "state" "open"
             ; Attr.create "dir" "ltr"
             ]
             @ (if is_popper
                then Popper.content_attrs ~side:c.side ~align:c.align ()
                else [])
             @ [ Cn.with_class
                   ?class_:c.class_
                   (content_class :: (if is_popper then [ popper_content_class ] else []))
               ; Attr.tabindex (-1)
               ; Attr.style
                   (Css_gen.concat
                      [ Css_gen.create ~field:"box-sizing" ~value:"border-box"
                      ; Css_gen.create ~field:"display" ~value:"flex"
                      ; Css_gen.create ~field:"flex-direction" ~value:"column"
                      ; Css_gen.create ~field:"outline" ~value:"none"
                      ])
               ; Focus.on_mount `Self
               ; Attr.on_keydown on_keydown
               ; Vdom.Attr.create_hook "loon-select-scroll" (Scroll_buttons.create is_popper)
               ]
             @ c.attrs)
          [ Node.div
              ~attrs:
                [ data "slot" "select-scroll-up-button"
                ; aria "hidden" "true"
                ; Attr.hidden
                ; Attr.class_ "bg-popover z-10 flex cursor-default items-center \
                               justify-center py-1 [&_svg:not([class*='size-'])]:size-4"
                ; Attr.style (Css_gen.create ~field:"flex-shrink" ~value:"0")
                ; Attr.on_pointermove (fun _ -> leave)
                ]
              [ Icon.view Chevron_up ]
          ; (* Radix's: the viewport scrolls with no scrollbar, by the
               buttons, the wheel or touch. *)
            Node.create
              "style"
              [ Node.text
                  "[data-radix-select-viewport]{scrollbar-width:none;-ms-overflow-style:none;-webkit-overflow-scrolling:touch;}[data-radix-select-viewport]::-webkit-scrollbar{display:none}"
              ]
          ; Node.div
              ~attrs:
                [ Attr.create "data-radix-select-viewport" ""
                ; Attr.role "presentation"
                ; data "position" (if is_popper then "popper" else "item-aligned")
                ; Attr.class_ viewport_class
                ; Attr.style
                    (Css_gen.concat
                       [ Css_gen.create ~field:"position" ~value:"relative"
                       ; Css_gen.create ~field:"flex" ~value:"1"
                       ; Css_gen.create ~field:"overflow" ~value:"hidden auto"
                       ])
                ]
              (render c.parts)
          ; Node.div
              ~attrs:
                [ data "slot" "select-scroll-down-button"
                ; aria "hidden" "true"
                ; Attr.hidden
                ; Attr.class_ "bg-popover z-10 flex cursor-default items-center \
                               justify-center py-1 [&_svg:not([class*='size-'])]:size-4"
                ; Attr.style (Css_gen.create ~field:"flex-shrink" ~value:"0")
                ; Attr.on_pointermove (fun _ -> leave)
                ]
              [ Icon.view Chevron_down ]
          ]
      in
      Node.div
        ~attrs:
          [ Attr.style (Css_gen.create ~field:"display" ~value:"contents")
          ; Dismiss.attr
              ~inside:[ trigger_id ]
              ~modal:true
              ~on_dismiss:(fun (reason : Dismiss.Reason.t) ->
                match reason with
                | Escape -> close
                | Pointer_outside | Focus_outside -> on_state_change State.closed)
              ()
          ; Scroll.lock
          ]
        [ (if is_popper
           then
             Popper.wrapper
               ~name:"select"
               ~side:c.side
               ~side_offset:c.side_offset
               ~align:c.align
               ~align_offset:c.align_offset
               ~collision_padding:10.
               ~anchor:(Element trigger_id)
               content_el
           else Popper.item_aligned_wrapper ~name:"select" ~anchor:trigger_id content_el)
        ])
  in
  let native =
    match name with
    | None -> Node.none
    | Some name ->
      Node.select
        ~attrs:
          ([ aria "hidden" "true"
           ; Attr.tabindex (-1)
           ; Attr.name name
           ; Attr.create
               "style"
               "position:absolute;border:0;width:1px;height:1px;padding:0;margin:-1px;\
                overflow:hidden;clip:rect(0, 0, 0, 0);white-space:nowrap"
           ]
           @ (if required then [ Attr.create "required" "" ] else [])
           @ (if disabled then [ Attr.disabled ] else []))
        (Node.option ~attrs:[ Attr.value "" ] []
         :: List.map items ~f:(fun (v, text, _, _) ->
           Node.option
             ~attrs:
               ([ Attr.value v ]
                @ if Option.equal String.equal value (Some v) then [ Attr.selected ] else [])
             [ Node.text text ]))
  in
  Node.fragment [ trigger_node; content_node; native ]
;;

let component ?name ?disabled ?required ?default_value trigger content (local_ graph) =
  let open Bonsai.Let_syntax in
  let value, set_value = Bonsai.state_opt ?default_model:default_value graph in
  let state, set_state = Bonsai.state State.closed graph in
  let id = Bonsai.path_id graph in
  let%arr value and set_value and state and set_state and id and trigger and content in
  ( value
  , view
      ~id
      ?name
      ?disabled
      ?required
      ~value
      ~on_value_change:(fun v -> set_value (Some v))
      ~state
      ~on_state_change:set_state
      trigger
      content )
;;
