open! Import

module Sort_direction = struct
  type t =
    | Asc
    | Desc
  [@@deriving enumerate, sexp_of, compare, equal]

  let name = function
    | Asc -> "asc"
    | Desc -> "desc"
  ;;
end

module State = struct
  type t =
    { sorting : (string * Sort_direction.t) list
    ; column_filters : string String.Map.t
    ; column_visibility : bool String.Map.t
    ; row_selection : String.Set.t
    ; page_index : int
    ; page_size : int
    }
  [@@deriving sexp_of, equal]

  let default =
    { sorting = []
    ; column_filters = String.Map.empty
    ; column_visibility = String.Map.empty
    ; row_selection = String.Set.empty
    ; page_index = 0
    ; page_size = 10
    }
  ;;
end

module Row = struct
  type 'row t =
    { id : string
    ; index : int
    ; original : 'row
    }
end

type 'row t =
  { columns : 'row column list
  ; state : State.t
  ; on_change : (State.t -> State.t) -> unit Effect.t
  ; core_rows : 'row Row.t list
  ; filtered_rows : 'row Row.t list
  ; sorted_rows : 'row Row.t list
  }

and 'row column =
  { id : string
  ; header : ('row t -> Node.t list) option
  ; cell : 'row t -> 'row Row.t -> Node.t list
  ; sort : ('row -> 'row -> int) option
  ; filter : ('row -> string) option
  ; hideable : bool
  }

module Column = struct
  type 'row t = 'row column

  let create ?header ?sort ?filter ?(hideable = true) ~id ~cell () =
    { id; header; cell; sort; filter; hideable }
  ;;

  let id (t : _ t) = t.id
end

let sort_text f a b =
  String.compare (String.lowercase (f a)) (String.lowercase (f b))
;;

let find_column columns id =
  List.find columns ~f:(fun (c : _ column) -> String.equal c.id id)
;;

let filtered ~columns ~(state : State.t) rows =
  let filters =
    Map.to_alist state.column_filters
    |> List.filter_map ~f:(fun (id, value) ->
      match find_column columns id with
      | Some { filter = Some text; _ } when not (String.is_empty value) ->
        let needle = String.lowercase value in
        Some
          (fun row ->
            String.is_substring (String.lowercase (text row)) ~substring:needle)
      | _ -> None)
  in
  List.filter rows ~f:(fun (row : _ Row.t) ->
    List.for_all filters ~f:(fun keep -> keep row.original))
;;

let sorted ~columns ~(state : State.t) rows =
  let compares =
    List.filter_map state.sorting ~f:(fun (id, direction) ->
      match find_column columns id with
      | Some { sort = Some compare; _ } ->
        Some
          (match direction with
           | Asc -> compare
           | Desc -> fun a b -> compare b a)
      | _ -> None)
  in
  if List.is_empty compares
  then rows
  else
    List.stable_sort rows ~compare:(fun (a : _ Row.t) (b : _ Row.t) ->
      List.find_map compares ~f:(fun compare ->
        match compare a.original b.original with
        | 0 -> None
        | c -> Some c)
      |> Option.value ~default:0)
;;

let create ?get_row_id ~columns ~state ~on_change data =
  let core_rows =
    List.mapi data ~f:(fun index original ->
      let id =
        match get_row_id with
        | Some f -> f original
        | None -> Int.to_string index
      in
      { Row.id; index; original })
  in
  let filtered_rows = filtered ~columns ~state core_rows in
  let sorted_rows = sorted ~columns ~state filtered_rows in
  { columns; state; on_change; core_rows; filtered_rows; sorted_rows }
;;

let component
      ?get_row_id
      ?(default_state = State.default)
      ~columns
      data
      (local_ graph)
  =
  let open Bonsai.Let_syntax in
  let state, update = Bonsai.state' ~equal:State.equal default_state graph in
  let%arr state
  and update
  and columns
  and data in
  create ?get_row_id ~columns ~state ~on_change:update data
;;

let state t = t.state
let update t f = t.on_change f

(* Columns *)

let all_columns t = t.columns

let is_visible t id =
  Map.find t.state.column_visibility id |> Option.value ~default:true
;;

let visible_columns t = List.filter t.columns ~f:(fun c -> is_visible t c.id)
let column t id = find_column t.columns id
let can_hide (c : _ column) = c.hideable

let toggle_visibility t id visible =
  update t (fun s ->
    { s with
      column_visibility = Map.set s.column_visibility ~key:id ~data:visible
    })
;;

let header t (c : _ column) =
  match c.header with
  | Some f -> f t
  | None -> [ Node.text c.id ]
;;

let cell t (c : _ column) row = c.cell t row

(* Filtering *)

let filter_value t id =
  Map.find t.state.column_filters id |> Option.value ~default:""
;;

let set_filter_value t id value =
  update t (fun s ->
    { s with
      column_filters =
        (if String.is_empty value
         then Map.remove s.column_filters id
         else Map.set s.column_filters ~key:id ~data:value)
    ; page_index = 0
    })
;;

(* Sorting *)

let is_sorted t id = List.Assoc.find t.state.sorting id ~equal:String.equal

let toggle_sorting ?desc t id =
  update t (fun s ->
    let current = List.Assoc.find s.sorting id ~equal:String.equal in
    let next : Sort_direction.t option =
      match desc, current with
      | Some true, _ -> Some Desc
      | Some false, _ -> Some Asc
      | None, None -> Some Asc
      | None, Some Asc -> Some Desc
      | None, Some Desc -> None
    in
    { s with
      sorting = Option.value_map next ~default:[] ~f:(fun d -> [ id, d ])
    ; page_index = 0
    })
;;

(* Pagination *)

let page_count t =
  let n = List.length t.sorted_rows in
  let size = Int.max 1 t.state.page_size in
  Int.max 1 ((n + size - 1) / size)
;;

let page_index t =
  Int.clamp_exn t.state.page_index ~min:0 ~max:(page_count t - 1)
;;

let can_previous_page t = page_index t > 0
let can_next_page t = page_index t < page_count t - 1

let set_page_index t index =
  update t (fun s -> { s with page_index = Int.max 0 index })
;;

let previous_page t = set_page_index t (page_index t - 1)

let next_page t =
  set_page_index t (Int.min (page_index t + 1) (page_count t - 1))
;;

let set_page_size t size =
  update t (fun s -> { s with page_size = Int.max 1 size; page_index = 0 })
;;

(* Rows *)

let core_rows t = t.core_rows
let filtered_rows t = t.filtered_rows

let rows t =
  let size = Int.max 1 t.state.page_size in
  List.drop t.sorted_rows (page_index t * size)
  |> fun rows -> List.take rows size
;;

(* Selection *)

let is_selected t (row : _ Row.t) = Set.mem t.state.row_selection row.id

let toggle_selected t (row : _ Row.t) selected =
  update t (fun s ->
    { s with
      row_selection =
        (if selected
         then Set.add s.row_selection row.id
         else Set.remove s.row_selection row.id)
    })
;;

let filtered_selected_rows t = List.filter t.filtered_rows ~f:(is_selected t)

let is_all_page_rows_selected t =
  let page = rows t in
  (not (List.is_empty page)) && List.for_all page ~f:(is_selected t)
;;

let is_some_page_rows_selected t =
  (not (is_all_page_rows_selected t)) && List.exists (rows t) ~f:(is_selected t)
;;

let toggle_all_page_rows_selected t selected =
  let ids = List.map (rows t) ~f:(fun (r : _ Row.t) -> r.id) in
  update t (fun s ->
    { s with
      row_selection =
        List.fold
          ids
          ~init:s.row_selection
          ~f:(if selected then Set.add else Set.remove)
    })
;;
