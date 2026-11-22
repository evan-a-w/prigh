open! Import
open Js_of_ocaml

type t =
  { id : string
  ; value : string option
  ; previous : string option
  ; on_change : string option -> unit Effect.t
  ; viewport : bool
  ; (* Filled in while the root's children are rendered: the open content
       when it goes in the viewport, and the indicator for the list to place. *)
    viewport_content : (unit -> Node.t) option ref
  ; indicator : Node.t option ref
  }

let trigger_id t value = t.id ^ "-trigger-" ^ value
let content_id t value = t.id ^ "-content-" ^ value
let is_open t value = Option.equal String.equal t.value (Some value)

(* Radix's delays: opening on hover waits unless a content is open; leaving
   closes after a moment, unless the pointer comes back. *)
let open_delay = 200
let close_delay = 150
let set t value = Effect.Many [ After_render.cancel t.id; t.on_change value ]

let close_and_refocus t =
  match t.value with
  | None -> Effect.Ignore
  | Some value ->
    Effect.Many
      [ set t None
      ; Dom_util.focus_selector (sprintf "[id=%S]" (trigger_id t value))
      ]
;;

(* DOM helpers for keyboard focus: the focusable elements in [container],
   leaving out those inside contents when [skip_content]. *)
let focusable_selector =
  "a[href], button:not([disabled]), input:not([disabled]), \
   [tabindex]:not([tabindex='-1'])"
;;

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option ((Js.Unsafe.coerce el)##closest (Js.string selector))
;;

let in_content el =
  Option.is_some (closest el "[data-slot=navigation-menu-content]")
;;

let focusables (container : Dom_html.element Js.t) ~skip_content =
  Dom.list_of_nodeList
    (container##querySelectorAll (Js.string focusable_selector))
  |> List.filter ~f:(fun el -> (not skip_content) || not (in_content el))
;;

let active_element () : Dom_html.element Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce Dom_html.document)##.activeElement
;;

let index_of_active els =
  match active_element () with
  | None -> None
  | Some active ->
    List.findi els ~f:(fun _ el ->
      phys_equal (el :> Dom_html.element Js.t) active)
    |> Option.map ~f:fst
;;

(* Moves focus within [container] for an arrow key (forward or back), Home
   or End; [true] if it did. *)
let move_focus
      container
      ~skip_content
      ~(key : Dom_html.Keyboard_code.t)
      ~forward
      ~back
  =
  let els = focusables container ~skip_content in
  let n = List.length els in
  if n = 0
  then false
  else (
    let current = index_of_active els in
    let target =
      if List.mem forward key ~equal:Poly.equal
      then
        Some
          (match current with
           | None -> 0
           | Some i -> Int.min (i + 1) (n - 1))
      else if List.mem back key ~equal:Poly.equal
      then
        Some
          (match current with
           | None -> n - 1
           | Some i -> Int.max (i - 1) 0)
      else (
        match key with
        | Home -> Some 0
        | End -> Some (n - 1)
        | _ -> None)
    in
    match target with
    | None -> false
    | Some i ->
      (List.nth_exn els i)##focus;
      true)
;;

let current_target (ev : #Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce ev)##.currentTarget
;;

let event_target (ev : #Dom_html.event Js.t) : Dom_html.element Js.t option =
  Js.Opt.to_option ev##.target
  |> Option.bind ~f:(fun target ->
    Js.Opt.to_option (Dom_html.CoerceTo.element target))
;;

let is_mouse (ev : Dom_html.pointerEvent Js.t) =
  String.equal (Js.to_string ev##.pointerType) "mouse"
;;

(* Pointer handlers on triggers, contents and the viewport. *)
let on_pointer_enter_trigger t value ev =
  if not (is_mouse ev)
  then Effect.Ignore
  else if is_open t value
  then After_render.cancel t.id
  else if Option.is_some t.value
  then set t (Some value)
  else After_render.schedule t.id open_delay (t.on_change (Some value))
;;

let on_pointer_leave t ev =
  if is_mouse ev && Option.is_some t.value
  then After_render.schedule t.id close_delay (t.on_change None)
  else if is_mouse ev
  then After_render.cancel t.id
  else Effect.Ignore
;;

let on_pointer_enter_content t ev =
  if is_mouse ev then After_render.cancel t.id else Effect.Ignore
;;

let hover_attrs t =
  [ Attr.on_pointerenter (on_pointer_enter_content t)
  ; Attr.on_pointerleave (on_pointer_leave t)
  ]
;;

let viewport_view t =
  match t.value, !(t.viewport_content) with
  | Some _, Some content ->
    (* The viewport takes the size of the content in it, as Radix's
       [--radix-navigation-menu-viewport-width] and [-height]. *)
    let measure (el : Dom_html.element Js.t) =
      Js.Opt.iter el##.firstChild (fun child ->
        Js.Opt.iter (Dom_html.CoerceTo.element child) (fun child ->
          let style = (Js.Unsafe.coerce el)##.style in
          let set name px =
            (style##setProperty (Js.string name) (Js.string (sprintf "%dpx" px))
             : unit)
          in
          set "--radix-navigation-menu-viewport-width" child##.offsetWidth;
          set "--radix-navigation-menu-viewport-height" child##.offsetHeight))
    in
    Node.div
      ~attrs:
        [ Attr.class_
            "absolute top-full left-0 isolate z-50 flex justify-center"
        ]
      [ Node.div
          ~attrs:
            ([ data "state" "open"
             ; data "orientation" "horizontal"
             ; data "slot" "navigation-menu-viewport"
             ; Attr.class_
                 "bg-popover text-popover-foreground data-open:animate-in \
                  data-closed:animate-out data-closed:zoom-out-90 data-open:zoom-in-90 \
                  ring-foreground/10 rounded-lg shadow ring-1 duration-100 \
                  origin-top-center relative mt-1.5 \
                  h-(--radix-navigation-menu-viewport-height) w-full overflow-hidden \
                  md:w-(--radix-navigation-menu-viewport-width)"
             ; After_render.attr ~name:"navigation-menu-viewport" measure
             ]
             @ hover_attrs t)
          [ content () ]
      ]
  | _ -> Node.none
;;

let view
      ?(attrs = [])
      ?class_
      ?(viewport = true)
      ?(id = "navigation-menu")
      ?previous
      ~value
      ~on_change
      children
  =
  let t =
    { id
    ; value
    ; previous
    ; on_change
    ; viewport
    ; viewport_content = ref None
    ; indicator = ref None
    }
  in
  let children = children t in
  let inside ev =
    match event_target ev with
    | None -> true
    | Some target -> Option.is_some (closest target (sprintf "[id=%S]" id))
  in
  let on_click ev =
    (* Choosing a link closes the menu. *)
    match event_target ev with
    | Some target
      when Option.is_some (closest target "[data-slot=navigation-menu-link]") ->
      set t None
    | _ -> Effect.Ignore
  in
  let on_keydown ev =
    match Dom_util.key ev with
    | Escape when Option.is_some t.value ->
      Effect.Many [ Dom_util.stop ev; close_and_refocus t ]
    | _ -> Effect.Ignore
  in
  let dismiss =
    match value with
    | None -> []
    | Some _ ->
      [ Attr.Global_listeners.mousedown ~phase:Capture ~f:(fun ev ->
          if inside ev then Effect.Ignore else set t None)
      ; Attr.Global_listeners.focusin ~phase:Capture ~f:(fun ev ->
          if inside ev then Effect.Ignore else set t None)
      ]
  in
  Node.create
    "nav"
    ~attrs:
      ([ Attr.id id
       ; aria "label" "Main"
       ; data "orientation" "horizontal"
       ; Attr.create "dir" "ltr"
       ; data "slot" "navigation-menu"
       ; data "viewport" (Bool.to_string viewport)
       ; Cn.with_class
           ?class_
           [ "group/navigation-menu relative flex max-w-max flex-1 items-center \
              justify-center"
           ]
       ; Attr.on_click on_click
       ; Attr.on_keydown on_keydown
       ]
       @ dismiss
       @ attrs)
    (children @ if viewport then [ viewport_view t ] else [])
;;

let component
      ?attrs
      ?class_
      ?viewport
      ?default_value
      children
      (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let state, set_state =
    Bonsai.state
      (default_value, None)
      graph
      ~equal:[%equal: string option * string option]
  in
  let id = Bonsai.path_id graph in
  let%arr value, previous = state
  and set_state
  and id
  and children in
  view
    ?attrs
    ?class_
    ?viewport
    ~id
    ?previous
    ~value
    ~on_change:(fun next ->
      (* The content open before, to slide from; none when opening from
         closed (Radix's previousValue). *)
      set_state (next, value))
    children
;;

let list ?(attrs = []) ?class_ t children =
  let on_keydown ev =
    match event_target ev, current_target ev with
    | Some target, Some list when not (in_content target) ->
      if
        move_focus
          list
          ~skip_content:true
          ~key:(Dom_util.key ev)
          ~forward:[ ArrowRight ]
          ~back:[ ArrowLeft ]
      then Dom_util.prevent_default ev
      else Effect.Ignore
    | _ -> Effect.Ignore
  in
  let ul =
    Node.ul
      ~attrs:
        ([ data "orientation" "horizontal"
         ; Attr.create "dir" "ltr"
         ; data "slot" "navigation-menu-list"
         ; Cn.with_class
             ?class_
             [ "gap-0 group flex flex-1 list-none items-center justify-center" ]
         ; Attr.on_keydown on_keydown
         ]
         @ attrs)
      children
  in
  (* Radix's list sits in a positioned track that also holds the
     indicator. *)
  Node.div
    ~attrs:[ Attr.style (Css_gen.position `Relative) ]
    (ul :: Option.to_list !(t.indicator))
;;

let item ?(attrs = []) ?class_ children =
  Node.li
    ~attrs:
      ([ data "slot" "navigation-menu-item"
       ; Cn.with_class ?class_ [ "relative" ]
       ]
       @ attrs)
    children
;;

let trigger_base =
  "hover:bg-muted focus:bg-muted data-open:hover:bg-muted data-open:focus:bg-muted \
   data-open:bg-muted/50 focus-visible:ring-ring/50 data-popup-open:bg-muted/50 \
   data-popup-open:hover:bg-muted rounded-lg px-2.5 py-1.5 text-sm font-medium \
   transition-all focus-visible:ring-3 focus-visible:outline-1 disabled:opacity-50 \
   group/navigation-menu-trigger inline-flex h-9 w-max items-center justify-center \
   outline-none disabled:pointer-events-none"
;;

let trigger_style ?class_ () = Cn.cn (trigger_base :: Option.to_list class_)

let trigger ?(attrs = []) ?class_ ?(disabled = false) t ~value children =
  let open_ = is_open t value in
  let on_keydown ev =
    let enter () =
      Effect.Many
        [ Dom_util.prevent_default ev
        ; Effect.of_thunk (fun () ->
            Option.iter
              (After_render.by_id (content_id t value))
              ~f:(fun content ->
                match focusables content ~skip_content:false with
                | first :: _ -> first##focus
                | [] -> ()))
        ]
    in
    match Dom_util.key ev with
    | ArrowDown when open_ -> enter ()
    | Tab when open_ && not (Js.to_bool ev##.shiftKey) -> enter ()
    | _ -> Effect.Ignore
  in
  Node.button
    ~attrs:
      ([ Attr.id (trigger_id t value)
       ; Attr.type_ "button"
       ; data "state" (if open_ then "open" else "closed")
       ; aria_bool "expanded" open_
       ]
       (* Radix names the content only while it is there. *)
       @ (if open_ then [ aria "controls" (content_id t value) ] else [])
       @ [ data "slot" "navigation-menu-trigger"
         ; Cn.with_class ?class_ [ trigger_base; "group" ]
         ; Attr.on_keydown on_keydown
         ; Attr.on_pointerenter (on_pointer_enter_trigger t value)
         ; Attr.on_pointerleave (on_pointer_leave t)
         ]
       @ (if disabled
          then [ Attr.disabled; data "disabled" "" ]
          else
            [ Attr.on_click (fun _ ->
                set t (if open_ then None else Some value))
            ])
       @ attrs)
    (children
     @ [ Node.text " "
       ; Icon.view
           ~class_:
             "relative top-px ml-1 size-3 transition duration-300 \
              group-data-open/navigation-menu-trigger:rotate-180 \
              group-data-popup-open/navigation-menu-trigger:rotate-180"
           Chevron_down
       ])
;;

let content ?(attrs = []) ?class_ t ~value children =
  if not (is_open t value)
  then Node.none
  else (
    let build () =
      let motion =
        match t.previous with
        | None -> None
        | Some previous when String.equal previous value -> None
        | Some previous ->
          (* Which trigger comes first in the page (the children's order of
             evaluation is not the page's). *)
          (match
             ( After_render.by_id (trigger_id t value)
             , After_render.by_id (trigger_id t previous) )
           with
           | Some el, Some prev ->
             let following =
               (Js.Unsafe.coerce prev)##compareDocumentPosition el land 4 <> 0
             in
             Some (if following then "from-end" else "from-start")
           | _ -> None)
      in
      let on_keydown ev =
        match current_target ev with
        | None -> Effect.Ignore
        | Some content ->
          let key = Dom_util.key ev in
          let els = focusables content ~skip_content:false in
          let current = index_of_active els in
          let shift = Js.to_bool ev##.shiftKey in
          (match key with
           | Tab when shift && Poly.equal current (Some 0) ->
             Effect.Many
               [ Dom_util.stop ev
               ; Dom_util.focus_selector
                   (sprintf "[id=%S]" (trigger_id t value))
               ]
           | Tab
             when (not shift) && Poly.equal current (Some (List.length els - 1))
             ->
             (* Out of the content, to whatever follows its trigger in the
                list. *)
             Effect.Many
               [ Dom_util.stop ev
               ; set t None
               ; Effect.of_thunk (fun () ->
                   match
                     ( After_render.by_id t.id
                     , After_render.by_id (trigger_id t value) )
                   with
                   | Some nav, Some trigger ->
                     let all = focusables nav ~skip_content:true in
                     (match
                        List.findi all ~f:(fun _ el ->
                          phys_equal (el :> Dom_html.element Js.t) trigger)
                      with
                      | Some (i, _) ->
                        Option.iter
                          (List.nth all (i + 1))
                          ~f:(fun el -> el##focus)
                      | None -> ())
                   | _ -> ())
               ]
           | _ ->
             if
               move_focus
                 content
                 ~skip_content:false
                 ~key
                 ~forward:[ ArrowDown; ArrowRight ]
                 ~back:[ ArrowUp; ArrowLeft ]
             then Effect.Many [ Dom_util.stop ev ]
             else Effect.Ignore)
      in
      Node.div
        ~attrs:
          ([ Attr.id (content_id t value)
           ; aria "labelledby" (trigger_id t value)
           ; data "orientation" "horizontal"
           ; data "state" "open"
           ]
           @ Option.value_map motion ~default:[] ~f:(fun m ->
             [ data "motion" m ])
           @ [ data "slot" "navigation-menu-content"
             ; Cn.with_class
                 ?class_
                 [ "data-[motion^=from-]:animate-in data-[motion^=to-]:animate-out \
                    data-[motion^=from-]:fade-in data-[motion^=to-]:fade-out \
                    data-[motion=from-end]:slide-in-from-right-52 \
                    data-[motion=from-start]:slide-in-from-left-52 \
                    data-[motion=to-end]:slide-out-to-right-52 \
                    data-[motion=to-start]:slide-out-to-left-52 \
                    group-data-[viewport=false]/navigation-menu:bg-popover \
                    group-data-[viewport=false]/navigation-menu:text-popover-foreground \
                    group-data-[viewport=false]/navigation-menu:data-open:animate-in \
                    group-data-[viewport=false]/navigation-menu:data-closed:animate-out \
                    group-data-[viewport=false]/navigation-menu:data-closed:zoom-out-95 \
                    group-data-[viewport=false]/navigation-menu:data-open:zoom-in-95 \
                    group-data-[viewport=false]/navigation-menu:data-open:fade-in-0 \
                    group-data-[viewport=false]/navigation-menu:data-closed:fade-out-0 \
                    group-data-[viewport=false]/navigation-menu:ring-foreground/10 p-1 \
                    ease-[cubic-bezier(0.22,1,0.36,1)] \
                    group-data-[viewport=false]/navigation-menu:rounded-lg \
                    group-data-[viewport=false]/navigation-menu:shadow \
                    group-data-[viewport=false]/navigation-menu:ring-1 \
                    group-data-[viewport=false]/navigation-menu:duration-300 top-0 \
                    left-0 w-full group-data-[viewport=false]/navigation-menu:top-full \
                    group-data-[viewport=false]/navigation-menu:mt-1.5 \
                    group-data-[viewport=false]/navigation-menu:overflow-hidden \
                    **:data-[slot=navigation-menu-link]:focus:ring-0 \
                    **:data-[slot=navigation-menu-link]:focus:outline-none md:absolute \
                    md:w-auto"
                 ]
             ; Attr.on_keydown on_keydown
             ]
           @ (if t.viewport then [] else Presence.attr :: hover_attrs t)
           @ attrs)
        children
    in
    if t.viewport
    then (
      t.viewport_content := Some build;
      Node.none)
    else build ())
;;

let link ?(attrs = []) ?class_ ?(active = false) ~href children =
  Node.a
    ~attrs:
      ((if active then [ aria "current" "page"; data "active" "true" ] else [])
       @ [ data "slot" "navigation-menu-link"
         ; Attr.href href
         ; Cn.with_class
             ?class_
             [ "data-active:focus:bg-muted data-active:hover:bg-muted \
                data-active:bg-muted/50 focus-visible:ring-ring/50 hover:bg-muted \
                focus:bg-muted flex items-center gap-2 rounded-lg p-2 text-sm \
                transition-all outline-none focus-visible:ring-3 \
                focus-visible:outline-1 \
                in-data-[slot=navigation-menu-content]:rounded-md \
                [&_svg:not([class*='size-'])]:size-4"
             ]
         ]
       @ attrs)
    children
;;

let indicator ?(attrs = []) ?class_ t =
  (match t.value with
   | None -> t.indicator := None
   | Some value ->
     (* Under the open trigger, as Radix's: its left and width, measured. *)
     let place (el : Dom_html.element Js.t) =
       match
         ( After_render.by_id (trigger_id t value)
         , Js.Opt.to_option el##.offsetParent )
       with
       | Some trigger, Some track ->
         let r = trigger##getBoundingClientRect in
         let p = track##getBoundingClientRect in
         el##.style##.width
         := Js.string (sprintf "%.0fpx" (Js.to_float r##.width));
         el##.style##.transform
         := Js.string
              (sprintf
                 "translateX(%.0fpx)"
                 (Js.to_float r##.left -. Js.to_float p##.left))
       | _ -> ()
     in
     t.indicator
     := Some
          (Node.div
             ~attrs:
               ([ aria "hidden" "true"
                ; data "state" "visible"
                ; data "orientation" "horizontal"
                ; data "slot" "navigation-menu-indicator"
                ; Attr.style
                    (Css_gen.concat
                       [ Css_gen.position `Absolute; Css_gen.left (`Px 0) ])
                ; Cn.with_class
                    ?class_
                    [ "data-[state=visible]:animate-in data-[state=hidden]:animate-out \
                       data-[state=hidden]:fade-out data-[state=visible]:fade-in \
                       top-full z-1 flex h-1.5 items-end justify-center overflow-hidden"
                    ]
                ; After_render.attr ~name:"navigation-menu-indicator" place
                ]
                @ attrs)
             [ Node.div
                 ~attrs:
                   [ Attr.class_
                       "bg-border rounded-tl-sm shadow-md relative top-[60%] h-2 w-2 \
                        rotate-45"
                   ]
                 []
             ]));
  Node.none
;;
