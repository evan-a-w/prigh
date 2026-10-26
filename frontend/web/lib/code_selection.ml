open! Core
open! Import
module Row = Diff_view.Row
module Side = Review_comment.Side
module Range = Review_comment.Range

module Point = struct
  type t =
    { side : Side.t
    ; line : int
    ; offset : int
    }
  [@@deriving sexp_of, equal]
end

let key (row : Row.t) =
  match row with
  | Line { kind = `Removed; old_line = Some n; _ } -> Some (Side.Old, n)
  | Line { kind = `Same | `Added; new_line = Some n; _ } -> Some (Side.New, n)
  | Line _ | Hunk _ -> None
;;

let text (row : Row.t) =
  match row with
  | Line { text; _ } -> text
  | Hunk _ -> ""
;;

let removed (row : Row.t) =
  match row with
  | Line { kind = `Removed; _ } -> true
  | Line _ | Hunk _ -> false
;;

let resolve rows ~lines ~(start : Point.t) ~(end_ : Point.t) =
  let open Option.Let_syntax in
  let rows = Array.of_list rows in
  let index (p : Point.t) =
    Array.findi rows ~f:(fun _ row ->
      match key row with
      | Some (side, n) -> Side.equal side p.side && n = p.line
      | None -> false)
    |> Option.map ~f:fst
  in
  let byte i (p : Point.t) =
    Utf16.byte_offset (text rows.(i)) ~utf16:p.offset
  in
  let%bind i = index start in
  let%bind j = index end_ in
  let a = byte i start in
  let b = byte j end_ in
  let (i, a), (j, b) =
    if [%compare: int * int] (i, a) (j, b) <= 0
    then (i, a), (j, b)
    else (j, b), (i, a)
  in
  let len k = String.length (text rows.(k)) in
  let keyed k = Option.is_some (key rows.(k)) in
  let next k ~until =
    Sequence.range (k + 1) (until + 1) |> Sequence.find ~f:keyed
  in
  let prev k ~until =
    Sequence.range until k ~stop:`exclusive
    |> Sequence.to_list_rev
    |> List.find ~f:keyed
  in
  (* From the end of a line: the next one's start; to a line's start: the
     end of the one before. *)
  let i, a =
    match next i ~until:j with
    | Some k when a >= len i -> k, 0
    | _ -> i, a
  in
  let j, b =
    match prev j ~until:i with
    | Some k when b = 0 && j > i -> k, len k
    | _ -> j, b
  in
  let between = List.filter (List.range i (j + 1)) ~f:keyed in
  let old = List.for_all between ~f:(fun k -> removed rows.(k)) in
  let line k = Option.value_map (key rows.(k)) ~default:0 ~f:snd in
  let%bind i, a, j, b =
    if old
    then (
      (* As far as the old lines run on without a gap (another hunk). *)
      let j, b =
        List.fold_until
          (List.tl between |> Option.value ~default:[])
          ~init:i
          ~f:(fun last k ->
            if line k = line last + 1 then Continue k else Stop (last, len last))
          ~finish:(fun last -> if last = j then j, b else last, len last)
      in
      Some (i, a, j, b))
    else (
      (* Without the removed lines at either end. *)
      let kept = List.filter between ~f:(fun k -> not (removed rows.(k))) in
      let%bind i' = List.hd kept in
      let%map j' = List.last kept in
      i', (if i' = i then a else 0), j', if j' = j then b else len j')
  in
  let%bind () = Option.some_if (i < j || (i = j && a < b)) () in
  let side = if old then Side.Old else Side.New in
  let first = line i in
  let last = line j in
  let%map anchor =
    let shown =
      Array.to_list rows
      |> List.filter_map ~f:(fun row ->
        match key row with
        | Some (s, n) when Side.equal s side && n >= first && n <= last ->
          Some (n, text row)
        | Some _ | None -> None)
      |> Int.Map.of_alist_reduce ~f:(fun a _ -> a)
    in
    List.range first (last + 1)
    |> List.map ~f:(fun n ->
      match Map.find shown n with
      | Some text -> Some text
      | None ->
        Option.bind lines ~f:(fun lines ->
          if n - 1 < Array.length lines then Some lines.(n - 1) else None))
    |> Option.all
    |> Option.map ~f:(String.concat ~sep:"\n")
  in
  let columns =
    if a = 0 && b = len j
    then None
    else Some { Review_comment.Columns.start_col = a; end_col = b }
  in
  { Range.first; last; side; columns }, anchor
;;

let whole_lines rows ~lines ~(from : Point.t) ~(to_ : Point.t) =
  let index (p : Point.t) =
    List.findi rows ~f:(fun _ row ->
      Option.equal [%equal: Side.t * int] (key row) (Some (p.side, p.line)))
    |> Option.map ~f:fst
  in
  let start, end_ =
    match index from, index to_ with
    | Some a, Some b when a > b -> to_, from
    | _ -> from, to_
  in
  resolve
    rows
    ~lines
    ~start:{ start with offset = 0 }
    ~end_:{ end_ with offset = Int.max_value }
;;

let covered (range : Range.t) (row : Row.t) =
  match key row with
  | Some (side, n)
    when Side.equal side range.side && n >= range.first && n <= range.last ->
    let len = String.length (text row) in
    let start, end_ =
      match range.columns with
      | None -> 0, len
      | Some { start_col; end_col } ->
        ( (if n = range.first then Int.min start_col len else 0)
        , if n = range.last then Int.min end_col len else len )
    in
    Some (start, Int.max start end_)
  | Some _ | None -> None
;;
