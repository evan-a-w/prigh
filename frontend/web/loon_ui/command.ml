open! Import

module State = struct
  type t =
    { search : string
    ; value : string option
    }
  [@@deriving sexp_of, equal]

  let empty = { search = ""; value = None }
end

module Item = struct
  type t =
    { attrs : Attr.t list
    ; class_ : string option
    ; value : string
    ; keywords : string list
    ; disabled : bool
    ; force_mount : bool
    ; on_select : unit Effect.t
    ; children : Node.t list
    }
end

module Group = struct
  type t =
    { attrs : Attr.t list
    ; class_ : string option
    ; heading : string option
    ; value : string option
    ; force_mount : bool
    ; parts : part list
    }

  and part =
    | Input of
        { attrs : Attr.t list
        ; class_ : string option
        ; placeholder : string option
        ; disabled : bool
        ; auto_focus : bool
        }
    | List of
        { attrs : Attr.t list
        ; class_ : string option
        ; parts : part list
        }
    | Empty of
        { attrs : Attr.t list
        ; class_ : string option
        ; children : Node.t list
        }
    | Group of t
    | Separator of
        { attrs : Attr.t list
        ; class_ : string option
        ; always_render : bool
        }
    | Item of Item.t
    | Static of Node.t
end

type part = Group.part

let input ?(attrs = []) ?class_ ?placeholder ?(disabled = false) ?(auto_focus = false) ()
  : part
  =
  Input { attrs; class_; placeholder; disabled; auto_focus }
;;

let list ?(attrs = []) ?class_ parts : part = List { attrs; class_; parts }
let empty ?(attrs = []) ?class_ children : part = Empty { attrs; class_; children }

let group ?(attrs = []) ?class_ ?heading ?value ?(force_mount = false) parts : part =
  Group { attrs; class_; heading; value; force_mount; parts }
;;

let separator ?(attrs = []) ?class_ ?(always_render = false) () : part =
  Separator { attrs; class_; always_render }
;;

let item
  ?(attrs = [])
  ?class_
  ?value
  ?(keywords = [])
  ?(disabled = false)
  ?(force_mount = false)
  ?(on_select = Effect.Ignore)
  children
  : part
  =
  let value =
    String.strip (Option.value_or_thunk value ~default:(fun () -> List_nav.text_of_nodes children))
  in
  Item { attrs; class_; value; keywords; disabled; force_mount; on_select; children }
;;

let static node : part = Static node

let shortcut ?(attrs = []) ?class_ children =
  Node.span
    ~attrs:
      ([ data "slot" "command-shortcut"
       ; Cn.with_class ?class_ [ "text-muted-foreground \
                                  group-data-selected/command-item:text-foreground \
                                  ml-auto text-xs tracking-widest" ]
       ]
       @ attrs)
    children
;;

let default_filter value ~search ~keywords = Command_score.score ~keywords value ~search
let cmdk name = Attr.create ("cmdk-" ^ name) ""

let sr_only =
  Attr.create
    "style"
    "position:absolute;width:1px;height:1px;padding:0;margin:-1px;overflow:hidden;\
     clip:rect(0, 0, 0, 0);white-space:nowrap;border-width:0"
;;

(* Every item in the tree, in order, with its index (its id). *)
let rec items_of (parts : part list) =
  List.concat_map parts ~f:(fun (p : part) ->
    match p with
    | Item i -> [ i ]
    | List { parts; _ } | Group { parts; _ } -> items_of parts
    | Input _ | Empty _ | Separator _ | Static _ -> [])
;;

let view
  ?(attrs = [])
  ?class_
  ?(id = "command")
  ?(label = "")
  ?(should_filter = true)
  ?(filter = default_filter)
  ?(loop = false)
  ~state
  ~on_change
  parts
  =
  let state : State.t = state in
  let input_id = id ^ "-input" in
  let list_id = id ^ "-list" in
  let label_id = id ^ "-label" in
  let searching = should_filter && not (String.is_empty state.search) in
  (* Scores, by physical item; items with score 0 are filtered out. *)
  let all = items_of parts in
  let indexed = List.mapi all ~f:(fun i item -> item, i) in
  let index_of (item : Item.t) =
    List.find_map_exn indexed ~f:(fun (i, n) -> Option.some_if (phys_equal i item) n)
  in
  let item_id item = sprintf "%s-item-%d" id (index_of item) in
  let score (item : Item.t) =
    if not searching
    then 1.
    else filter item.value ~search:state.search ~keywords:item.keywords
  in
  let visible (item : Item.t) = item.force_mount || Float.(score item > 0.) in
  let by_score (a : Item.t) (b : Item.t) = Float.compare (score b) (score a) in
  (* cmdk's sort: items by score within their group (or the list), then the
     groups by their best item, after everything else. *)
  let rec arrange (parts : part list) =
    if not searching
    then parts
    else (
      let items =
        List.filter_map parts ~f:(function
          | Item i -> Some i
          | _ -> None)
        |> List.stable_sort ~compare:by_score
      in
      let items = ref items in
      let others, groups =
        List.partition_map parts ~f:(fun (p : part) ->
          match p with
          | Group g -> Second g
          | Item _ ->
            (match !items with
             | i :: rest ->
               items := rest;
               First (Group.Item i : part)
             | [] -> First p)
          | List l -> First (List { l with parts = arrange l.parts })
          | _ -> First p)
      in
      let groups =
        List.map groups ~f:(fun g -> { g with parts = arrange g.parts })
        |> List.map ~f:(fun g ->
          let best =
            List.fold (items_of g.parts) ~init:0. ~f:(fun acc i -> Float.max acc (score i))
          in
          best, g)
        |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Float.compare b a)
        |> List.map ~f:(fun (_, g) -> (Group g : part))
      in
      others @ groups)
  in
  let parts = arrange parts in
  let ordered = List.filter (items_of parts) ~f:visible in
  let count = List.length ordered in
  let selectable = List.filter ordered ~f:(fun i -> not i.disabled) in
  let selected =
    match state.value with
    | Some v ->
      (match List.find selectable ~f:(fun i -> String.equal i.value v) with
       | Some i -> Some i
       | None -> List.hd selectable)
    | None -> List.hd selectable
  in
  let is_selected (item : Item.t) =
    Option.value_map selected ~default:false ~f:(fun s -> phys_equal s item)
  in
  let select (item : Item.t) =
    Effect.Many
      [ on_change { state with value = Some item.value }
      ; Scroll.into_view (sprintf "[id=%S]" (item_id item))
      ]
  in
  let move key =
    let arr = Array.of_list selectable in
    let n = Array.length arr in
    if n = 0
    then Effect.Ignore
    else (
      let current =
        Option.bind selected ~f:(fun s ->
          Array.findi arr ~f:(fun _ i -> phys_equal i s) |> Option.map ~f:fst)
      in
      let target =
        match key, current with
        | `First, _ -> Some 0
        | `Last, _ -> Some (n - 1)
        | `Next, None | `Prev, None -> Some 0
        | `Next, Some i ->
          if i + 1 < n then Some (i + 1) else if loop then Some 0 else None
        | `Prev, Some i ->
          if i > 0 then Some (i - 1) else if loop then Some (n - 1) else None
      in
      match target with
      | Some j -> select arr.(j)
      | None -> Effect.Ignore)
  in
  let on_keydown (ev : Js_of_ocaml.Dom_html.keyboardEvent Js_of_ocaml.Js.t) =
    let ctrl = Js_of_ocaml.Js.to_bool ev##.ctrlKey in
    let handled e = Effect.Many [ Dom_util.prevent_default ev; e ] in
    match Dom_util.key ev with
    | ArrowDown -> handled (move (if Js_of_ocaml.Js.to_bool ev##.metaKey then `Last else `Next))
    | ArrowUp -> handled (move (if Js_of_ocaml.Js.to_bool ev##.metaKey then `First else `Prev))
    | KeyN | KeyJ when ctrl -> handled (move `Next)
    | KeyP | KeyK when ctrl -> handled (move `Prev)
    | Home -> handled (move `First)
    | End -> handled (move `Last)
    | Enter ->
      (match selected with
       | Some item -> handled item.on_select
       | None -> Effect.Ignore)
    | _ -> Effect.Ignore
  in
  let rec render (parts : part list) =
    List.map parts ~f:(fun (p : part) ->
      match p with
      | Static node -> node
      | Input { attrs; class_; placeholder; disabled; auto_focus } ->
        Node.div
          ~attrs:
            [ data "slot" "command-input-wrapper"; Attr.class_ "p-1 pb-0" ]
          [ Input_group.root
              ~class_:"bg-input/30 border-input/30 h-8! rounded-lg! shadow-none! \
                       *:data-[slot=input-group-addon]:pl-2!"
              [ Node.input
              ~attrs:
                ([ cmdk "input"
                 ; Attr.create "autocomplete" "off"
                 ; Attr.create "autocorrect" "off"
                 ; Attr.create "spellcheck" "false"
                 ; aria "autocomplete" "list"
                 ; Attr.role "combobox"
                 ; aria "expanded" "true"
                 ; aria "controls" list_id
                 ; aria "labelledby" label_id
                 ]
                 @ Option.value_map selected ~default:[] ~f:(fun i ->
                   [ aria "activedescendant" (item_id i) ])
                 @ [ Attr.id input_id
                   ; Attr.type_ "text"
                   ; data "slot" "command-input"
                   ; Cn.with_class
                       ?class_
                       [ "w-full text-sm outline-hidden disabled:cursor-not-allowed \
                          disabled:opacity-50"
                       ]
                   ; Attr.string_property "value" state.search
                   ; Attr.on_input (fun _ search -> on_change { State.search; value = None })
                   ]
                 @ Option.value_map placeholder ~default:[] ~f:(fun p -> [ Attr.placeholder p ])
                 @ (if disabled then [ Attr.disabled ] else [])
                 @ (if auto_focus then [ Focus.when_ true ] else [])
                 @ attrs)
                  ()
              ; Input_group.addon [ Icon.view ~class_:"size-4 shrink-0 opacity-50" Search ]
              ]
          ]
      | List { attrs; class_; parts } ->
        Node.div
          ~attrs:
            ([ cmdk "list"; Attr.role "listbox"; Attr.tabindex (-1) ]
             @ Option.value_map selected ~default:[] ~f:(fun i ->
               [ aria "activedescendant" (item_id i) ])
             @ [ aria "label" "Suggestions"
               ; Attr.id list_id
               ; data "slot" "command-list"
               ; Cn.with_class
                   ?class_
                   [ "no-scrollbar max-h-72 scroll-py-1 outline-none overflow-x-hidden \
                      overflow-y-auto" ]
               ]
             @ attrs)
          [ Node.div ~attrs:[ cmdk "list-sizer" ] (render parts) ]
      | Empty { attrs; class_; children } ->
        if count > 0
        then Node.none
        else
          Node.div
            ~attrs:
              ([ cmdk "empty"
               ; Attr.role "presentation"
               ; data "slot" "command-empty"
               ; Cn.with_class ?class_ [ "py-6 text-center text-sm" ]
               ]
               @ attrs)
            children
      | Separator { attrs; class_; always_render } ->
        if always_render || String.is_empty state.search
        then
          Node.div
            ~attrs:
              ([ cmdk "separator"
               ; Attr.role "separator"
               ; data "slot" "command-separator"
               ; Cn.with_class ?class_ [ "bg-border -mx-1 h-px" ]
               ]
               @ attrs)
            []
        else Node.none
      | Group g ->
        let shown = g.force_mount || List.exists (items_of g.parts) ~f:visible in
        let heading_id =
          sprintf "%s-group-%d" id (Option.value_map (List.hd (items_of g.parts)) ~default:0 ~f:index_of)
        in
        Node.div
          ~attrs:
            ([ cmdk "group"; Attr.role "presentation" ]
             @ Option.value_map
                 (Option.first_some g.value g.heading)
                 ~default:[]
                 ~f:(fun v -> [ data "value" v ])
             @ (if shown then [] else [ Attr.hidden ])
             @ [ data "slot" "command-group"
               ; Cn.with_class
                   ?class_:g.class_
                   [ "text-foreground **:[[cmdk-group-heading]]:text-muted-foreground \
                      overflow-hidden p-1 **:[[cmdk-group-heading]]:px-2 \
                      **:[[cmdk-group-heading]]:py-1.5 \
                      **:[[cmdk-group-heading]]:text-xs \
                      **:[[cmdk-group-heading]]:font-medium"
                   ]
               ]
             @ g.attrs)
          ((match g.heading with
            | Some h ->
              [ Node.div
                  ~attrs:[ cmdk "group-heading"; aria "hidden" "true"; Attr.id heading_id ]
                  [ Node.text h ]
              ]
            | None -> [])
           @ [ Node.div
                 ~attrs:
                   ([ cmdk "group-items"; Attr.role "group" ]
                    @ Option.value_map g.heading ~default:[] ~f:(fun _ ->
                      [ aria "labelledby" heading_id ]))
                 (render g.parts)
             ])
      | Item item ->
        if not (visible item)
        then Node.none
        else (
          let sel = is_selected item in
          Node.div
            ~attrs:
              ([ cmdk "item"
               ; Attr.role "option"
               ; aria_bool "disabled" item.disabled
               ; aria_bool "selected" sel
               ; data "disabled" (Bool.to_string item.disabled)
               ; data "selected" (Bool.to_string sel)
               ; data "value" item.value
               ; Attr.id (item_id item)
               ; data "slot" "command-item"
               ; Cn.with_class
                   ?class_:item.class_
                   [ "data-selected:bg-muted data-selected:text-foreground \
                      data-selected:*:[svg]:text-foreground relative flex \
                      cursor-default items-center gap-2 rounded-sm px-2 py-1.5 text-sm \
                      outline-hidden select-none \
                      in-data-[slot=dialog-content]:rounded-lg! \
                      [&_svg:not([class*='size-'])]:size-4 group/command-item \
                      data-[disabled=true]:pointer-events-none \
                      data-[disabled=true]:opacity-50 [&_svg]:pointer-events-none \
                      [&_svg]:shrink-0"
                   ]
               ]
               @ (if item.disabled
                  then []
                  else
                    [ Attr.on_pointermove (fun _ ->
                        if sel
                        then Effect.Ignore
                        else on_change { state with value = Some item.value })
                    ; Attr.on_click (fun _ -> item.on_select)
                    ])
               @ item.attrs)
            (item.children
             @ [ Icon.view
                   ~class_:
                     "ml-auto opacity-0 \
                      group-has-data-[slot=command-shortcut]/command-item:hidden \
                      group-data-[checked=true]/command-item:opacity-100"
                   Check
               ])))
  in
  Node.div
    ~attrs:
      ([ Attr.tabindex (-1)
       ; cmdk "root"
       ; data "slot" "command"
       ; Cn.with_class
           ?class_
           [ "bg-popover text-popover-foreground rounded-xl! p-1 flex size-full \
              flex-col overflow-hidden" ]
       ; Attr.on_keydown on_keydown
       ]
       @ attrs)
    (Node.label
       ~attrs:[ cmdk "label"; Attr.for_ input_id; Attr.id label_id; sr_only ]
       [ Node.text label ]
     :: render parts)
;;

let component ?attrs ?class_ ?label ?should_filter ?filter ?loop parts (local_ graph) =
  let open Bonsai.Let_syntax in
  let state, set_state = Bonsai.state State.empty graph in
  let id = Bonsai.path_id graph in
  let%arr state and set_state and id and parts in
  view
    ?attrs
    ?class_
    ~id
    ?label
    ?should_filter
    ?filter
    ?loop
    ~state
    ~on_change:set_state
    parts
;;

let dialog
  ?(id = "command-dialog")
  ?(title = "Command Palette")
  ?(description = "Search for a command to run...")
  ?class_
  ?show_close_button
  ?label
  ?should_filter
  ?filter
  ?loop
  ~open_
  ~on_open_change
  ~state
  ~on_change
  parts
  =
  Dialog.view
    ~id
    ~open_
    ~on_open_change:(fun o ->
      if o
      then on_open_change true
      else Effect.Many [ on_open_change false; on_change State.empty ])
    (fun d ->
      [ Dialog.header
          ~class_:"sr-only"
          [ Dialog.title d [ Node.text title ]; Dialog.description d [ Node.text description ] ]
      ; Dialog.content
          ~class_:
            (Cn.cn
               [ "rounded-xl! top-1/3 translate-y-0 overflow-hidden p-0"
               ; Option.value class_ ~default:""
               ])
          ~show_close_button:(Option.value show_close_button ~default:false)
          d
          [ view
              ~id:(id ^ "-command")
              ?label
              ?should_filter
              ?filter
              ?loop
              ~state
              ~on_change
              parts
          ]
      ])
;;
