open! Import
open Js_of_ocaml

module Variant = struct
  type t =
    | Default
    | Destructive
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Default -> "default"
    | Destructive -> "destructive"
  ;;
end

module Classes = struct
  type t =
    { slot : string
    ; item : string
    ; checkbox_item : string
    ; radio_item : string
    ; checkbox_indicator : string
    ; radio_indicator : string
    ; indicator_slots : bool
    ; label : string
    ; separator : string
    ; shortcut : string
    ; sub_trigger : string
    ; sub_trigger_icon : string
    ; sub_content : string
    }
end

module State = struct
  type t =
    { highlighted : int list option
    ; open_sub : int list option
    }
  [@@deriving sexp_of, equal]

  let initial = { highlighted = None; open_sub = None }
end

type t =
  { id : string
  ; classes : Classes.t
  ; state : State.t
  ; set_state : State.t -> unit Effect.t
  ; close : unit Effect.t
  ; on_edge : ([ `Left | `Right ] -> unit Effect.t) option
  }

let trigger_id id = id ^ "-trigger"
let content_id id = id ^ "-content"

let path_id id path =
  String.concat ~sep:"-" (id :: "sub" :: List.map path ~f:Int.to_string)
;;

type common =
  { text : string
  ; disabled : bool
  }

type action =
  { common : common
  ; select : unit Effect.t
  ; close_on_select : bool
  ; render : Classes.t -> Attr.t list -> Node.t
  }

module Sub_trigger = struct
  type t =
    { common : common
    ; attrs : Attr.t list
    ; class_ : string option
    ; inset : bool
    ; children : Node.t list
    }
end

type radio =
  { r_common : common
  ; value : string
  ; r_close_on_select : bool
  ; r_render : Classes.t -> checked:bool -> Attr.t list -> Node.t
  }

module Item = struct
  type t =
    | Action of action
    | Radio of radio
    | Sub of Sub_trigger.t * sub_content
    | Static of (Classes.t -> Node.t)
    | Group of
        { attrs : Attr.t list
        ; slot : string
        ; items : t list
        }

  and sub_content =
    { attrs : Attr.t list
    ; class_ : string option
    ; side_offset : float
    ; align_offset : float
    ; items : t list
    }
end

module Sub_content = struct
  type t = Item.sub_content
end

let common ?text_value ~disabled children =
  { text = Option.value text_value ~default:(List_nav.text_of_nodes children); disabled }
;;

let inset_attr inset = if inset then [ data "inset" "" ] else []
let slot (c : Classes.t) part = data "slot" (c.slot ^ "-" ^ part)

let item
  ?(attrs = [])
  ?class_
  ?(inset = false)
  ?(variant = Variant.Default)
  ?(disabled = false)
  ?text_value
  ?(close_on_select = true)
  ?(on_select = Effect.Ignore)
  ?href
  children
  : Item.t
  =
  Action
    { common = common ?text_value ~disabled children
    ; select = on_select
    ; close_on_select
    ; render =
        (fun c nav ->
          let element ~attrs =
            match href with
            | None -> Node.div ~attrs
            | Some href -> Node.a ~attrs:(Attr.href href :: attrs)
          in
          element
            ~attrs:
              ([ slot c "item" ]
               @ inset_attr inset
               @ [ data "variant" (Variant.name variant)
                 ; Cn.with_class ?class_ [ c.item ]
                 ; Attr.role "menuitem"
                 ]
               @ nav
               @ attrs)
            children)
    }
;;

let indicator (c : Classes.t) ~kind ~checked icon =
  Node.span
    ~attrs:
      ((if c.indicator_slots then [ slot c (kind ^ "-item-indicator") ] else [])
       @ [ Attr.class_
             (match kind with
              | "radio" -> c.radio_indicator
              | _ -> c.checkbox_indicator)
         ])
    (if checked then [ Node.span ~attrs:[ data "state" "checked" ] [ icon ] ] else [])
;;

let checked_state checked = data "state" (if checked then "checked" else "unchecked")

let checkbox_item
  ?(attrs = [])
  ?class_
  ?(disabled = false)
  ?text_value
  ?(close_on_select = true)
  ~checked
  ~on_checked_change
  children
  : Item.t
  =
  Action
    { common = common ?text_value ~disabled children
    ; select = on_checked_change (not checked)
    ; close_on_select
    ; render =
        (fun c nav ->
          Node.div
            ~attrs:
              ([ slot c "checkbox-item"
               ; Cn.with_class ?class_ [ c.checkbox_item ]
               ; Attr.role "menuitemcheckbox"
               ; aria_bool "checked" checked
               ; checked_state checked
               ]
               @ nav
               @ attrs)
            (indicator c ~kind:"checkbox" ~checked (Icon.view Check) :: children))
    }
;;

let radio_item
  ?(attrs = [])
  ?class_
  ?indicator:indicator_icon
  ?(disabled = false)
  ?text_value
  ?(close_on_select = true)
  ~value
  children
  : Item.t
  =
  Radio
    { r_common = common ?text_value ~disabled children
    ; value
    ; r_close_on_select = close_on_select
    ; r_render =
        (fun c ~checked nav ->
          Node.div
            ~attrs:
              ([ slot c "radio-item"
               ; Cn.with_class ?class_ [ c.radio_item ]
               ; Attr.role "menuitemradio"
               ; aria_bool "checked" checked
               ; checked_state checked
               ]
               @ nav
               @ attrs)
            (indicator
               c
               ~kind:"radio"
               ~checked
               (match indicator_icon with
                | Some icon -> icon
                | None -> Icon.view Check)
             :: children))
    }
;;

let radio_action ?value ?(on_value_change = fun _ -> Effect.Ignore) (r : radio) =
  let checked = Option.value_map value ~default:false ~f:(String.equal r.value) in
  { common = r.r_common
  ; select = on_value_change r.value
  ; close_on_select = r.r_close_on_select
  ; render = (fun c nav -> r.r_render c ~checked nav)
  }
;;

let radio_group ?(attrs = []) ~value ~on_value_change (items : Item.t list) : Item.t =
  let rec resolve (items : Item.t list) =
    List.map items ~f:(fun (item : Item.t) : Item.t ->
      match item with
      | Radio r -> Action (radio_action ~value ~on_value_change r)
      | Group g -> Group { g with items = resolve g.items }
      | Action _ | Sub _ | Static _ -> item)
  in
  Group { attrs = Attr.role "group" :: attrs; slot = "radio-group"; items = resolve items }
;;

let label ?(attrs = []) ?class_ ?(inset = false) children : Item.t =
  Static
    (fun c ->
      Node.div
        ~attrs:
          ([ slot c "label" ]
           @ inset_attr inset
           @ [ Cn.with_class ?class_ [ c.label ] ]
           @ attrs)
        children)
;;

let separator ?(attrs = []) ?class_ () : Item.t =
  Static
    (fun c ->
      Node.div
        ~attrs:
          ([ slot c "separator"
           ; Attr.role "separator"
           ; aria "orientation" "horizontal"
           ; Cn.with_class ?class_ [ c.separator ]
           ]
           @ attrs)
        [])
;;

let group ?(attrs = []) items : Item.t =
  Group { attrs = Attr.role "group" :: attrs; slot = "group"; items }
;;

let sub trigger content : Item.t = Sub (trigger, content)

let sub_trigger
  ?(attrs = [])
  ?class_
  ?(inset = false)
  ?(disabled = false)
  ?text_value
  children
  : Sub_trigger.t
  =
  { common = common ?text_value ~disabled children; attrs; class_; inset; children }
;;

let sub_content ?(attrs = []) ?class_ ?(side_offset = 0.) ?(align_offset = 0.) items
  : Sub_content.t
  =
  { attrs; class_; side_offset; align_offset; items }
;;

let node n : Item.t = Static (fun _ -> n)

let shortcut (c : Classes.t) ?(attrs = []) ?class_ children =
  Node.span
    ~attrs:([ slot c "shortcut"; Cn.with_class ?class_ [ c.shortcut ] ] @ attrs)
    children
;;

(* The items of one level that the keyboard and pointer reach, in order:
   groups are flattened; labels and separators are not among them. *)
type entry =
  | E_action of action
  | E_sub of Sub_trigger.t * Item.sub_content

let rec entries (items : Item.t list) =
  List.concat_map items ~f:(function
    | Action a -> [ E_action a ]
    | Radio r -> [ E_action (radio_action r) ]
    | Sub (t, c) -> [ E_sub (t, c) ]
    | Static _ -> []
    | Group { items; _ } -> entries items)
;;

let entry_common = function
  | E_action a -> a.common
  | E_sub (t, _) -> t.common
;;

let enabled items =
  List.filter_mapi (entries items) ~f:(fun i e ->
    let c = entry_common e in
    if c.disabled then None else Some (i, c.text))
;;

let first_highlighted items =
  { State.initial with
    highlighted = Option.map (List.hd (enabled items)) ~f:(fun (i, _) -> [ i ])
  }
;;

let last_highlighted items =
  { State.initial with
    highlighted = Option.map (List.last (enabled items)) ~f:(fun (i, _) -> [ i ])
  }
;;

let parent path = List.drop_last path |> Option.value ~default:[]

let rec is_prefix ~prefix path =
  match prefix, path with
  | [], _ -> true
  | x :: xs, y :: ys -> x = y && is_prefix ~prefix:xs ys
  | _ :: _, [] -> false
;;

(* The open sub-menus that stay open when [path] is highlighted: those whose
   triggers are on its way from the top. *)
let trim open_sub ~path =
  match open_sub with
  | None -> None
  | Some open_sub ->
    let p = parent path in
    let rec common a b =
      match a, b with
      | x :: xs, y :: ys when x = y -> x :: common xs ys
      | _ -> []
    in
    (match common open_sub p with
     | [] -> None
     | l -> Some l)
;;

let level_keydown t ~prefix items ev =
  let state = t.state in
  let entries = Array.of_list (entries items) in
  let current =
    match state.highlighted with
    | Some h when List.equal Int.equal (parent h) prefix && not (List.is_empty h) ->
      List.last h
    | _ -> None
  in
  let highlight i =
    let path = prefix @ [ i ] in
    t.set_state { highlighted = Some path; open_sub = trim state.open_sub ~path }
  in
  let open_sub i (sub : Item.sub_content) =
    let path = prefix @ [ i ] in
    let first = Option.map (List.hd (enabled sub.items)) ~f:(fun (j, _) -> path @ [ j ]) in
    t.set_state { highlighted = first; open_sub = Some path }
  in
  let handled e = Some (Effect.Many [ Dom_util.stop ev; e ]) in
  let key = Dom_util.key ev in
  let result =
    match key, Option.map current ~f:(fun i -> i, entries.(i)) with
    | Escape, _ -> handled t.close
    | Tab, _ -> Some (Dom_util.prevent_default ev)
    | (Enter | Space), Some (_, E_action a) when not a.common.disabled ->
      handled
        (Effect.Many [ a.select; (if a.close_on_select then t.close else Effect.Ignore) ])
    | (Enter | Space | ArrowRight), Some (i, E_sub (trig, sub)) when not trig.common.disabled
      -> handled (open_sub i sub)
    | ArrowLeft, _ when not (List.is_empty prefix) ->
      handled
        (t.set_state
           { highlighted = Some prefix; open_sub = trim state.open_sub ~path:prefix })
    | ArrowLeft, _ -> Option.map t.on_edge ~f:(fun f -> Effect.Many [ Dom_util.stop ev; f `Left ])
    (* In a sub-menu too, as Radix's menubar: on an item that opens nothing,
       ArrowRight goes to the next menu. *)
    | ArrowRight, _ ->
      Option.map t.on_edge ~f:(fun f -> Effect.Many [ Dom_util.stop ev; f `Right ])
    | _ ->
      (match
         List_nav.move ~loop:false ~equal:Int.equal ~items:(enabled items) ~current key
       with
       | Some i -> handled (highlight i)
       | None -> None)
  in
  Option.value result ~default:Effect.Ignore
;;

(* At once, not after the next frame (as [Dom_util.focus_selector]): the
   pointer leaving one item for another blurs the first, then the hook of the
   one it enters focuses that. *)
let focus_content id =
  Effect.of_thunk (fun () ->
    if Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)
    then
      Option.iter
        (Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)))
        ~f:(fun el -> el##focus))
;;

(* Radix's pointer grace: leaving an open sub-menu's trigger towards the
   sub-menu, the pointer may cross other items (for 300ms, within the
   triangle from where it left to the sub-menu's near edge) without them
   taking the highlight or closing the sub-menu. *)
module Grace = struct
  type t =
    { level : string (* the items' level (its content's id) *)
    ; area : (float * float) list
    ; right : bool
    ; until : float
    }

  let current : t option ref = ref None
  let last_x = ref None
  let moving_right = ref None
  let now () = Js.to_float (new%js Js.date_now)##getTime

  let xy (ev : Dom_html.mouseEvent Js.t) =
    Js.to_float (Js.Unsafe.coerce ev)##.clientX, Js.to_float (Js.Unsafe.coerce ev)##.clientY
  ;;

  let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

  (* The pointer's horizontal direction, from the moves over the items. *)
  let track ev =
    if in_browser ()
    then (
      let x, _ = xy ev in
      (match !last_x with
       | Some x0 when Float.( <> ) x x0 -> moving_right := Some Float.(x > x0)
       | _ -> ());
      last_x := Some x)
  ;;

  let start (ev : Dom_html.mouseEvent Js.t) ~level ~sub_id =
    if in_browser ()
    then (
      match
        Js.Opt.to_option (Dom_html.document##getElementById (Js.string sub_id))
      with
      | None -> current := None
      | Some content ->
        let r = content##getBoundingClientRect in
        let x, y = xy ev in
        let left = Js.to_float r##.left
        and right_edge = Js.to_float r##.right
        and top = Js.to_float r##.top
        and bottom = Js.to_float r##.bottom in
        let right = Float.(left >= x) in
        let near, far, bleed =
          if right then left, right_edge, -5. else right_edge, left, 5.
        in
        current
        := Some
             { level
             ; area = [ x +. bleed, y; near, top; far, top; far, bottom; near, bottom ]
             ; right
             ; until = now () +. 300.
             })
  ;;

  let inside (px, py) polygon =
    let a = Array.of_list polygon in
    let n = Array.length a in
    let inside = ref false in
    let j = ref (n - 1) in
    for i = 0 to n - 1 do
      let xi, yi = a.(i)
      and xj, yj = a.(!j) in
      if Bool.( <> ) Float.(yi > py) Float.(yj > py)
         && Float.(px < ((xj -. xi) *. (py -. yi) /. (yj -. yi)) +. xi)
      then inside := not !inside;
      j := i
    done;
    !inside
  ;;

  let moving_to_sub ev ~level =
    in_browser ()
    &&
    match !current with
    | Some g when not (String.equal g.level level) -> false
    | Some g when Float.(now () < g.until) ->
      Option.equal Bool.equal !moving_right (Some g.right) && inside (xy ev) g.area
    | Some _ ->
      current := None;
      false
    | None -> false
  ;;
end

let rec render_level t ~prefix ~level_id ~subs (items : Item.t list) =
  let state = t.state in
  let counter = ref (-1) in
  let nav_attrs ~(common : common) ~path ~on_click ~sub_open =
    let highlighted =
      Option.value_map state.highlighted ~default:false ~f:(List.equal Int.equal path)
    in
    let on_move =
      if common.disabled
      then
        if highlighted
        then
          Effect.Many
            [ t.set_state { state with highlighted = None }; focus_content level_id ]
        else Effect.Ignore
      else (
        let open_sub =
          match sub_open with
          | `Sub -> Some path
          | `No -> trim state.open_sub ~path
        in
        let next = { State.highlighted = Some path; open_sub } in
        if State.equal next state then Effect.Ignore else t.set_state next)
    in
    [ Attr.tabindex (-1)
    ; data "orientation" "vertical"
    ; Attr.create "data-radix-collection-item" ""
    ; Attr.on_mousemove (fun ev ->
        let to_sub = Grace.moving_to_sub ev ~level:level_id in
        Grace.track ev;
        if to_sub then Effect.Ignore else on_move)
    ; Attr.on_mouseleave (fun ev ->
        match sub_open with
        | `Sub when Option.exists state.open_sub ~f:(fun o -> is_prefix ~prefix:path o) ->
          (* The sub-menu stays, its trigger highlighted, while the pointer
             heads for it. *)
          Grace.start ev ~level:level_id ~sub_id:(path_id t.id path);
          Effect.Ignore
        | _ ->
          if highlighted && not (Grace.moving_to_sub ev ~level:level_id)
          then
            Effect.Many
              [ t.set_state { state with highlighted = None }; focus_content level_id ]
          else Effect.Ignore)
    ; Focus.when_ highlighted
    ]
    @ (if highlighted then [ data "highlighted" "" ] else [])
    @ (if common.disabled
       then [ aria "disabled" "true"; data "disabled" "" ]
       else [ Attr.on_click (fun _ -> on_click) ])
  in
  let go_action a =
    incr counter;
    let path = prefix @ [ !counter ] in
    let on_click =
      Effect.Many [ a.select; (if a.close_on_select then t.close else Effect.Ignore) ]
    in
    a.render t.classes (nav_attrs ~common:a.common ~path ~on_click ~sub_open:`No)
  in
  let rec go (items : Item.t list) =
    List.map items ~f:(fun (item : Item.t) ->
      match item with
      | Static f -> f t.classes
      | Group { attrs; slot = s; items } ->
        Node.div ~attrs:(slot t.classes s :: attrs) (go items)
      | Radio r -> go_action (radio_action r)
      | Action a -> go_action a
      | Sub (trig, content) ->
        incr counter;
        let path = prefix @ [ !counter ] in
        let is_open =
          Option.value_map state.open_sub ~default:false ~f:(fun o ->
            is_prefix ~prefix:path o)
        in
        let trigger_id = path_id t.id path ^ "-trigger" in
        let sub_id = path_id t.id path in
        let on_click =
          t.set_state { highlighted = Some path; open_sub = Some path }
        in
        if is_open
        then (
          let sub_node =
            Popper.wrapper
              ~name:t.classes.slot
              ~side:Right
              ~align:Start
              ~side_offset:content.side_offset
              ~align_offset:content.align_offset
              ~anchor:(Element trigger_id)
              (Node.div
                 ~attrs:
                   ([ slot t.classes "sub-content"
                    ; Cn.with_class ?class_:content.class_ [ t.classes.sub_content ]
                    ; Attr.id sub_id
                    ; Attr.role "menu"
                    ; aria "orientation" "vertical"
                    ; aria "labelledby" trigger_id
                    ; data "state" "open"
                    ; data "orientation" "vertical"
                    ; Attr.tabindex (-1)
                    ; Attr.style (Css_gen.create ~field:"outline" ~value:"none")
                    ; Attr.on_keydown (level_keydown t ~prefix:path content.items)
                    ]
                    @ Popper.content_attrs ~side:Right ~align:Start ()
                    @ content.attrs)
                 (render_level t ~prefix:path ~level_id:sub_id ~subs content.items))
          in
          subs := sub_node :: !subs);
        Node.div
          ~attrs:
            ([ slot t.classes "sub-trigger" ]
             @ inset_attr trig.inset
             @ [ Cn.with_class ?class_:trig.class_ [ t.classes.sub_trigger ]
               ; Attr.id trigger_id
               ; Attr.role "menuitem"
               ; aria "haspopup" "menu"
               ; aria_bool "expanded" is_open
               ]
             @ (if is_open then [ aria "controls" sub_id ] else [])
             @ [ data "state" (if is_open then "open" else "closed") ]
             @ nav_attrs ~common:trig.common ~path ~on_click ~sub_open:`Sub
             @ trig.attrs)
          (trig.children
           @ [ Icon.view ~class_:t.classes.sub_trigger_icon Chevron_right ]))
  in
  go items
;;

let content
  ?(attrs = [])
  ?class_
  ?(side = Popper.Side.Bottom)
  ?(side_offset = 0.)
  ?(align = Popper.Align.Center)
  ?(align_offset = 0.)
  ?avoid_collisions
  ?(modal = true)
  ?(inside = [])
  ?(labelled_by = true)
  ?on_dismiss
  ~base_class
  ~anchor
  t
  items
  =
  let subs = ref [] in
  let id = content_id t.id in
  let children = render_level t ~prefix:[] ~level_id:id ~subs items in
  let top =
    Popper.wrapper
      ~name:t.classes.slot
      ~side
      ~side_offset
      ~align
      ~align_offset
      ?avoid_collisions
      ~anchor
      (Node.div
         ~attrs:
           ([ slot t.classes "content"
            ; Cn.with_class ?class_ [ base_class ]
            ; Attr.id id
            ; Attr.role "menu"
            ; aria "orientation" "vertical"
            ]
            @ (if labelled_by then [ aria "labelledby" (trigger_id t.id) ] else [])
            @ [ data "state" "open"
              ; Attr.tabindex (-1)
              ; Attr.style (Css_gen.create ~field:"outline" ~value:"none")
              ; Attr.on_keydown (level_keydown t ~prefix:[] items)
              ; Focus.on_mount `Self
              ]
            @ Popper.content_attrs ~side ~align ()
            @ attrs)
         children)
  in
  Node.div
    ~attrs:
      ([ Attr.style (Css_gen.create ~field:"display" ~value:"contents")
       ; Presence.attr
       ; Dismiss.attr
           ~inside
           ~modal
           ~on_dismiss:
             (Option.value on_dismiss ~default:(fun (_ : Dismiss.Reason.t) -> t.close))
           ()
       ]
     (* Radix's modal menus lock the page's scroll (RemoveScroll). *)
     @ if modal then [ Scroll.lock ] else [])
    (top :: !subs)
;;
