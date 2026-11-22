open! Import

type t =
  { value : string option
  ; on_change : string -> unit Effect.t
  ; disabled : bool
  ; required : bool
  ; (* The items, in the order they are created (for tests, which have no
       DOM; OCaml evaluates a list literal last first, so tests make them with
       [List.map]). *)
    mutable items : (string * bool) list
  }

let view
  ?(attrs = [])
  ?class_
  ?orientation
  ?(disabled = false)
  ?(required = false)
  ~value
  ~on_change
  children
  =
  let t = { value; on_change; disabled; required; items = [] } in
  let children = children t in
  Node.div
    ~attrs:
      ([ Attr.role "radiogroup"
       ; aria_bool "required" required
       ; Attr.create "dir" "ltr"
       ; data "slot" "radio-group"
       ; Cn.with_class ?class_ [ "grid gap-2 w-full" ]
       ]
       @ Option.value_map orientation ~default:[] ~f:(fun o ->
         [ aria "orientation" (Roving.Orientation.name o) ])
       @ (if disabled then [ data "disabled" "" ] else [])
       @ attrs)
    children
;;

let component
  ?attrs
  ?class_
  ?orientation
  ?disabled
  ?required
  ?default_value
  children
  (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let value, set = Bonsai.state default_value ~equal:[%equal: string option] graph in
  let%arr value and set and children in
  { Control.value
  ; set
  ; view =
      view
        ?attrs
        ?class_
        ?orientation
        ?disabled
        ?required
        ~value
        ~on_change:(fun v -> set (Some v))
        children
  }
;;

let item ?(attrs = []) ?class_ ?id ?(disabled = false) ?(invalid = false) t ~value =
  let disabled = disabled || t.disabled in
  t.items <- t.items @ [ value, not disabled ];
  let checked = [%equal: string option] t.value (Some value) in
  (* The tab stop: the checked item, else every enabled item until one is
     checked (Radix focuses the first). *)
  let tab_stop = checked || Option.is_none t.value in
  let state = data "state" (if checked then "checked" else "unchecked") in
  let disabled_attrs = if disabled then [ data "disabled" "" ] else [] in
  Node.button
    ~attrs:
      ([ Attr.type_ "button"
       ; Attr.role "radio"
       ; aria_bool "checked" checked
       ; state
       ; Attr.value value
       ; Attr.tabindex (if tab_stop then 0 else -1)
       ; data "slot" "radio-group-item"
       ; Cn.with_class
           ?class_
           [ "border-input dark:bg-input/30 data-checked:bg-primary \
              data-checked:text-primary-foreground dark:data-checked:bg-primary \
              data-checked:border-primary aria-invalid:aria-checked:border-primary \
              aria-invalid:border-destructive focus-visible:border-ring \
              focus-visible:ring-ring/50 aria-invalid:ring-destructive/20 \
              dark:aria-invalid:ring-destructive/40 \
              dark:aria-invalid:border-destructive/50 flex size-4 rounded-full \
              focus-visible:ring-3 aria-invalid:ring-3 \
              group-has-[:focus-visible]/field-label:ring-0 \
              group-has-[:focus-visible]/field-label:not-data-checked:border-input \
              group-has-[:focus-visible]/field-label:data-checked:border-primary \
              group/radio-group-item peer relative aspect-square shrink-0 border \
              outline-none after:absolute after:-inset-x-3 after:-inset-y-2 \
              disabled:cursor-not-allowed disabled:opacity-50"
           ]
       ; Attr.on_keydown (fun ev ->
           match Dom_util.key ev with
           | Enter -> Dom_util.prevent_default ev
           | _ ->
             (match
                Roving.next
                  ev
                  ~group:"[role=radiogroup]"
                  ~item:"[role=radio]"
                  ~fallback:t.items
                  ~current:value
              with
              | None -> Effect.Ignore
              | Some (next, focus) ->
                Effect.Many [ Dom_util.prevent_default ev; focus; t.on_change next ]))
       ]
       @ Option.value_map id ~default:[] ~f:(fun id -> [ Attr.id id ])
       @ (if t.required then [ aria "required" "true" ] else [])
       @ (if invalid then [ aria "invalid" "true" ] else [])
       @ disabled_attrs
       @ (if disabled
          then [ Attr.disabled ]
          else [ Attr.on_click (fun _ -> if checked then Effect.Ignore else t.on_change value) ])
       @ attrs)
    (if checked
     then
       [ Node.span
           ~attrs:
             ([ state
              ; data "slot" "radio-group-indicator"
              ; Attr.class_ "flex size-4 items-center justify-center"
              ]
              @ disabled_attrs)
           [ Node.span ~attrs:[ Attr.class_ "bg-primary-foreground absolute top-1/2 \
                                             left-1/2 size-2 -translate-x-1/2 \
                                             -translate-y-1/2 rounded-full" ] [] ]
       ]
     else [])
;;
