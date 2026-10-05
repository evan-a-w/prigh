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

let table ~numbers rows =
  let td cls text = Node.td ~attrs:[ Attr.class_ cls ] [ Node.text text ] in
  let num = Option.value_map ~default:"" ~f:Int.to_string in
  Node.table
    ~attrs:[ Attr.class_ "diff" ]
    [ Node.tbody
        (List.map rows ~f:(fun (row : Row.t) ->
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
                @ [ td "sign" sign; td "text" text ])))
    ]
;;

let view ~numbers ~max_rows rows =
  let count = List.length rows in
  let shown, rest =
    if count <= max_rows + 1
    then rows, []
    else List.take rows max_rows, List.drop rows max_rows
  in
  Chat_html.div
    "diff-view"
    [ table ~numbers shown
    ; (match rest with
       | [] -> Node.none
       | rest ->
         Node.details
           ~attrs:[ Attr.class_ "more" ]
           [ Node.summary
               [ Node.text (Chat_html.plural (List.length rest) "more line") ]
           ; table ~numbers rest
           ])
    ]
;;

let unified ?(max_rows = 40) text =
  view ~numbers:true ~max_rows (unified_rows text)
;;

let edits ?(max_rows = 40) edits =
  view ~numbers:false ~max_rows (edit_rows edits)
;;
