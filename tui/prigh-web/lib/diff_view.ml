open! Core
open! Import

module Row = struct
  type t =
    | Hunk of string
    | Line of
        { kind : [ `Same | `Added | `Removed ]
        ; old_line : int option
        ; new_line : int option
        ; text : string
        }
end

let is_unified text = String.is_prefix text ~prefix:"--- "

(* "@@ -12,7 +12,8 @@" *)
let hunk_starts line =
  let number s =
    String.chop_prefix s ~prefix:"-"
    |> Option.first_some (String.chop_prefix s ~prefix:"+")
    |> Option.bind ~f:(fun s ->
      Int.of_string_opt (List.hd_exn (String.split s ~on:',')))
  in
  match String.split line ~on:' ' with
  | "@@" :: old :: new_ :: _ -> Option.both (number old) (number new_)
  | _ -> None
;;

let unified_rows text =
  let old_n = ref 0 in
  let new_n = ref 0 in
  List.filter_map (String.split_lines text) ~f:(fun line ->
    if
      String.is_prefix line ~prefix:"--- "
      || String.is_prefix line ~prefix:"+++ "
    then None
    else if String.is_prefix line ~prefix:"@@"
    then (
      Option.iter (hunk_starts line) ~f:(fun (o, n) ->
        old_n := o;
        new_n := n);
      Some (Row.Hunk line))
    else (
      let text = String.drop_prefix line 1 in
      let next r =
        let v = !r in
        incr r;
        Some v
      in
      match String.prefix line 1 with
      | "+" ->
        Some
          (Line { kind = `Added; old_line = None; new_line = next new_n; text })
      | "-" ->
        Some
          (Line
             { kind = `Removed; old_line = next old_n; new_line = None; text })
      | "\\" -> None
      | _ ->
        let old_line = next old_n in
        Some (Line { kind = `Same; old_line; new_line = next new_n; text })))
;;

let edit_rows edits =
  let many = List.length edits > 1 in
  List.concat_mapi edits ~f:(fun i (old, new_) ->
    let rows =
      List.map (Line_diff.diff ~old ~new_) ~f:(fun line ->
        let kind, text =
          match (line : Line_diff.Line.t) with
          | Same t -> `Same, t
          | Added t -> `Added, t
          | Removed t -> `Removed, t
        in
        Row.Line { kind; old_line = None; new_line = None; text })
    in
    if many then Row.Hunk (sprintf "edit %d" (i + 1)) :: rows else rows)
;;

let counts rows =
  List.fold rows ~init:(0, 0) ~f:(fun (a, r) (row : Row.t) ->
    match row with
    | Line { kind = `Added; _ } -> a + 1, r
    | Line { kind = `Removed; _ } -> a, r + 1
    | Line { kind = `Same; _ } | Hunk _ -> a, r)
;;

let unified_counts text = counts (unified_rows text)
let edits_counts edits = counts (edit_rows edits)

(* Each line's tokens, the old and the new side each carrying its state
   from line to line (and starting afresh at each hunk). *)
let highlighted lang rows =
  let initial = Highlight.State.initial in
  let _, _, rev =
    List.fold
      rows
      ~init:(initial, initial, [])
      ~f:(fun (old, new_, acc) (row : Row.t) ->
        match row with
        | Hunk _ -> initial, initial, None :: acc
        | Line { kind; text; _ } ->
          let tokens state = Highlight.line lang state text in
          (match kind with
           | `Same ->
             let _, old = tokens old in
             let t, new_ = tokens new_ in
             old, new_, Some t :: acc
           | `Added ->
             let t, new_ = tokens new_ in
             old, new_, Some t :: acc
           | `Removed ->
             let t, old = tokens old in
             old, new_, Some t :: acc))
  in
  List.rev rev
;;

let table ?lang ~numbers rows =
  let td cls text = Node.td ~attrs:[ Attr.class_ cls ] [ Node.text text ] in
  let tokens =
    match lang with
    | Some lang -> highlighted lang rows
    | None -> List.map rows ~f:(fun _ -> None)
  in
  let num = Option.value_map ~default:"" ~f:Int.to_string in
  Node.table
    ~attrs:[ Attr.class_ "diff" ]
    [ Node.tbody
        (List.map2_exn rows tokens ~f:(fun (row : Row.t) tokens ->
           match row with
           | Hunk text ->
             Node.tr
               ~attrs:[ Attr.class_ "hunk" ]
               [ Node.td
                   ~attrs:
                     [ Attr.create "colspan" (if numbers then "4" else "2") ]
                   [ Node.text text ]
               ]
           | Line { kind; old_line; new_line; text } ->
             let cls, sign =
               match kind with
               | `Same -> "same", " "
               | `Added -> "add", "+"
               | `Removed -> "del", "-"
             in
             Node.tr
               ~attrs:[ Attr.class_ cls ]
               ((if numbers
                 then [ td "ln" (num old_line); td "ln" (num new_line) ]
                 else [])
                @ [ td "sign" sign
                  ; (match tokens with
                     | Some tokens ->
                       Node.td
                         ~attrs:[ Attr.class_ "text" ]
                         (Highlight_view.spans tokens)
                     | None -> td "text" text)
                  ])))
    ]
;;

let default_max_rows = 8

let view ?(max_rows = default_max_rows) ?lang ~fold:(fold, key) ~numbers rows =
  let count = List.length rows in
  let shown, toggle =
    if count <= max_rows + 1
    then rows, Node.none
    else
      ( (if Fold.expanded fold key then rows else List.take rows max_rows)
      , Fold.button fold key ~total:count ~shown:max_rows )
  in
  Chat_html.div "diff-view" [ table ?lang ~numbers shown; toggle ]
;;

let unified ?max_rows ?lang ~fold text =
  view ?max_rows ?lang ~fold ~numbers:true (unified_rows text)
;;

let hunks ?lang ~fold lines =
  let rows =
    List.map
      (unified_rows (String.concat ~sep:"\n" lines))
      ~f:(fun (row : Row.t) ->
        match row with
        | Hunk _ -> row
        | Line l -> Line { l with old_line = None; new_line = None })
  in
  view ?lang ~fold ~numbers:false rows
;;

let edits ?lang ~fold edits = view ?lang ~fold ~numbers:false (edit_rows edits)
