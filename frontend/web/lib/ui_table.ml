open! Core
open! Import
module Table = Ui_spec.Table
module Cell = Ui_spec.Cell

let format (c : Table.Column.t) (cell : Cell.t) =
  match cell with
  | Null -> "—"
  | Bool true -> "✓"
  | Bool false -> "✗"
  | Text s ->
    (match c.format, Float.of_string_opt (String.strip s) with
     | (Number | Percent | Currency), Some f ->
       (match c.format with
        | Percent -> Ui_format.percent ?decimals:c.decimals f
        | Currency -> Ui_format.currency ?decimals:c.decimals ~code:c.currency f
        | _ -> Ui_format.number ?decimals:c.decimals f)
     | _ -> s)
  | Number f ->
    (match c.format with
     | Percent -> Ui_format.percent ?decimals:c.decimals f
     | Currency -> Ui_format.currency ?decimals:c.decimals ~code:c.currency f
     | Number | Text | Md | Date -> Ui_format.number ?decimals:c.decimals f)
;;

let numeric (c : Table.Column.t) =
  match c.format with
  | Number | Percent | Currency -> true
  | Text | Date | Md -> false
;;

let parse_sort = function
  | None -> None
  | Some s ->
    (match String.rsplit2 s ~on:':' with
     | Some (key, "asc") -> Some (key, `Asc)
     | Some (key, "desc") -> Some (key, `Desc)
     | _ -> None)
;;

let as_number : Cell.t -> float option = function
  | Number f -> Some f
  | Text s -> Float.of_string_opt (String.strip s)
  | Bool b -> Some (if b then 1. else 0.)
  | Null -> None
;;

let compare_cells (a : Cell.t) (b : Cell.t) =
  match as_number a, as_number b with
  | Some x, Some y -> Float.compare x y
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None ->
    let text : Cell.t -> string = function
      | Text s -> s
      | _ -> ""
    in
    String.Caseless.compare (text a) (text b)
;;

let sort (t : Table.t) ~sort =
  match parse_sort sort with
  | None -> t.rows
  | Some (key, dir) ->
    let is_null row =
      match Table.cell row key with
      | Null -> true
      | _ -> false
    in
    let nulls, values = List.partition_tf t.rows ~f:is_null in
    let sorted =
      List.stable_sort values ~compare:(fun a b ->
        let c = compare_cells (Table.cell a key) (Table.cell b key) in
        match dir with
        | `Asc -> c
        | `Desc -> -c)
    in
    sorted @ nulls
;;

let align_cls (c : Table.Column.t) =
  match c.align with
  | Left -> "left"
  | Right -> "right"
  | Center -> "center"
;;

let view (ctx : Ui_context.t) ~path (t : Table.t) =
  let state = Ui_context.get ctx ~path "sort" in
  let current = parse_sort state in
  let header (c : Table.Column.t) =
    let dir =
      match current with
      | Some (key, dir) when String.equal key c.key -> Some dir
      | _ -> None
    in
    let aria =
      match dir with
      | Some `Asc -> "ascending"
      | Some `Desc -> "descending"
      | None -> "none"
    in
    let attrs =
      [ Attr.class_ (align_cls c); Attr.create "scope" "col" ]
      @ if t.sortable then [ Attr.create "aria-sort" aria ] else []
    in
    if not t.sortable
    then Node.th ~attrs [ Node.text c.label ]
    else (
      let next =
        match dir with
        | Some `Asc -> "desc"
        | Some `Desc -> "asc"
        | None -> if numeric c then "desc" else "asc"
      in
      Node.th
        ~attrs
        [ Node.button
            ~attrs:
              [ Attr.type_ "button"
              ; Attr.class_ "ui-sort"
              ; Attr.title (sprintf "Sort by %s" c.label)
              ; Attr.on_click (fun _ ->
                  Ui_context.set ctx ~path "sort" (c.key ^ ":" ^ next))
              ]
            [ Node.span [ Node.text c.label ]
            ; Node.span
                ~attrs:
                  [ Attr.class_ "ui-sort-mark"; Attr.create "aria-hidden" "true" ]
                [ Node.text
                    (match dir with
                     | Some `Asc -> "▲"
                     | Some `Desc -> "▼"
                     | None -> "▲")
                ]
            ]
        ])
  in
  let cell row (c : Table.Column.t) =
    let value = Table.cell row c.key in
    let content =
      match c.format, value with
      | Md, Text s -> Ui_context.md s
      | _, Null -> Node.span ~attrs:[ Attr.class_ "ui-null" ] [ Node.text "—" ]
      | _, Bool b ->
        Node.span
          ~attrs:
            [ Attr.class_ (if b then "ui-yes" else "ui-no")
            ; Attr.create "aria-label" (if b then "yes" else "no")
            ]
          [ Node.text (format c value) ]
      | _ -> Node.text (format c value)
    in
    Node.td ~attrs:[ Attr.class_ (align_cls c) ] [ content ]
  in
  let rows = sort t ~sort:state in
  Node.figure
    ~attrs:[ Attr.class_ "ui-table" ]
    [ (match t.title with
       | Some title -> Node.div ~attrs:[ Attr.class_ "ui-title" ] [ Node.text title ]
       | None -> Node.none)
    ; Node.div
        ~attrs:[ Attr.class_ "ui-table-scroll"; Attr.tabindex 0; Attr.role "region"
               ; Attr.create "aria-label" (Option.value t.title ~default:"Table") ]
        [ Node.table
            [ Node.thead [ Node.tr (List.map t.columns ~f:header) ]
            ; Node.tbody
                (List.map rows ~f:(fun row ->
                   Node.tr (List.map t.columns ~f:(cell row))))
            ]
        ]
    ; (match t.caption with
       | Some caption ->
         Node.figcaption ~attrs:[ Attr.class_ "ui-caption" ] [ Ui_context.md caption ]
       | None -> Node.none)
    ]
;;
