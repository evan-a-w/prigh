open! Core
open! Import

module Gap = struct
  type t =
    { id : int
    ; first : int
    ; hidden : int
    ; before : bool
    ; after : bool
    }
  [@@deriving sexp_of, equal]
end

module Row = struct
  type t =
    | Hunk of string
    | Gap of Gap.t
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
    | Line { kind = `Same; _ } | Hunk _ | Gap _ -> a, r)
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
        | Hunk _ | Gap _ -> initial, initial, None :: acc
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

let hunk_header text =
  match String.chop_prefix text ~prefix:"@@ " with
  | None -> [ Node.text text ]
  | Some rest ->
    (match String.substr_index rest ~pattern:" @@" with
     | None -> [ Node.text text ]
     | Some i ->
       let range = "@@ " ^ String.prefix rest i ^ " @@" in
       let context = String.strip (String.drop_prefix rest (i + 3)) in
       [ Node.span ~attrs:[ Attr.class_ "hunk-range" ] [ Node.text range ] ]
       @
       if String.is_empty context
       then []
       else
         [ Node.text " "
         ; Node.span ~attrs:[ Attr.class_ "hunk-context" ] [ Node.text context ]
         ])
;;

(* One number per row: the new one, or the old one of a removed line. *)
let number_cell (kind : [ `Same | `Added | `Removed ]) ~old_line ~new_line =
  match kind, old_line, new_line with
  | `Removed, Some n, _ ->
    Node.td ~attrs:[ Attr.class_ "ln old" ] [ Node.text (Int.to_string n) ]
  | (`Same | `Added), _, Some n | `Removed, None, Some n ->
    Node.td ~attrs:[ Attr.class_ "ln" ] [ Node.text (Int.to_string n) ]
  | _, _, None -> Node.td ~attrs:[ Attr.class_ "ln" ] []
;;

let tab_width = 8

let hang text =
  let rec indent i col ~tabs =
    if i >= String.length text
    then col, tabs
    else (
      match text.[i] with
      | ' ' -> indent (i + 1) (col + 1) ~tabs
      | '\t' ->
        indent (i + 1) ((col / tab_width * tab_width) + tab_width) ~tabs:true
      | _ -> col, tabs)
  in
  match indent 0 0 ~tabs:false with
  | col, false -> col + 2
  | col, true -> col / tab_width * tab_width
;;

let default_hang = 2

let text_attrs text =
  match hang text with
  | n when n = default_hang -> [ Attr.class_ "text" ]
  | n -> [ Attr.class_ "text"; Attr.create "style" (sprintf "--hang: %dch" n) ]
;;

let word_changes rows =
  let rows = Array.of_list rows in
  let n = Array.length rows in
  let changes = Array.create ~len:n [] in
  let is kind i =
    i < n
    &&
    match rows.(i) with
    | Row.Line l -> Poly.equal l.kind kind
    | Hunk _ | Gap _ -> false
  in
  let text i =
    match rows.(i) with
    | Row.Line l -> l.text
    | Hunk _ | Gap _ -> ""
  in
  let rec run i =
    if i < n
    then
      if is `Removed i
      then (
        let rec past kind j = if is kind j then past kind (j + 1) else j in
        let removed_end = past `Removed i in
        let added_end = past `Added removed_end in
        let pairs = Int.min (removed_end - i) (added_end - removed_end) in
        for k = 0 to pairs - 1 do
          let o = i + k
          and a = removed_end + k in
          match Word_diff.ranges ~old:(text o) ~new_:(text a) with
          | Some (old, new_) ->
            changes.(o) <- old;
            changes.(a) <- new_
          | None -> ()
        done;
        run (Int.max added_end (i + 1)))
      else run (i + 1)
  in
  run 0;
  Array.to_list changes
;;

let pieces tokens ~cuts =
  List.fold
    tokens
    ~init:(0, [])
    ~f:(fun (pos, acc) ({ kind; text } : Highlight.Token.t) ->
      let len = String.length text in
      let inside = List.filter cuts ~f:(fun c -> c > pos && c < pos + len) in
      let bounds = (pos :: inside) @ [ pos + len ] in
      let rec split acc = function
        | a :: (b :: _ as rest) ->
          split
            (( a
             , { Highlight.Token.kind
               ; text = String.sub text ~pos:(a - pos) ~len:(b - a)
               } )
             :: acc)
            rest
        | [ _ ] | [] -> acc
      in
      pos + len, split acc bounds)
  |> snd
  |> List.rev
;;

let in_change (changed : Word_diff.Range.t list) at =
  List.exists changed ~f:(fun r -> r.start <= at && at < r.end_)
;;

let changed_spans tokens ~(changed : Word_diff.Range.t list) =
  match changed with
  | [] -> Highlight_view.spans tokens
  | changed ->
    let cuts = List.concat_map changed ~f:(fun r -> [ r.start; r.end_ ]) in
    List.group (pieces tokens ~cuts) ~break:(fun (a, _) (b, _) ->
      Bool.( <> ) (in_change changed a) (in_change changed b))
    |> List.concat_map ~f:(fun group ->
      let spans = Highlight_view.spans (List.map group ~f:snd) in
      if in_change changed (fst (List.hd_exn group))
      then [ Node.span ~attrs:[ Attr.class_ "wd" ] spans ]
      else spans)
;;

let gap_text (g : Gap.t) =
  if g.hidden = 1 then "1 line hidden" else sprintf "%d lines hidden" g.hidden
;;

let table ?lang ~numbers rows =
  let tokens =
    match lang with
    | Some lang -> highlighted lang rows
    | None -> List.map rows ~f:(fun _ -> None)
  in
  let changes = word_changes rows in
  Node.table
    ~attrs:[ Attr.class_ "diff" ]
    [ Node.tbody
        (List.map3_exn
           rows
           tokens
           changes
           ~f:(fun (row : Row.t) tokens changed ->
             match row with
             | Gap g ->
               Node.tr
                 ~attrs:[ Attr.class_ "hunk" ]
                 [ Node.td
                     ~attrs:
                       [ Attr.create "colspan" (if numbers then "3" else "2") ]
                     [ Node.text (gap_text g) ]
                 ]
             | Hunk text ->
               Node.tr
                 ~attrs:[ Attr.class_ "hunk" ]
                 [ Node.td
                     ~attrs:
                       [ Attr.create "colspan" (if numbers then "3" else "2") ]
                     (hunk_header text)
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
                   then [ number_cell kind ~old_line ~new_line ]
                   else [])
                  @ [ Node.td
                        ~attrs:
                          [ Attr.class_ "sign"
                          ; Attr.create "aria-hidden" "true"
                          ]
                        [ Node.text sign ]
                    ; Node.td
                        ~attrs:(text_attrs text)
                        (changed_spans
                           (Option.value
                              tokens
                              ~default:
                                [ { Highlight.Token.kind = Plain; text } ])
                           ~changed)
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
        | Hunk _ | Gap _ -> row
        | Line l -> Line { l with old_line = None; new_line = None })
  in
  view ?lang ~fold ~numbers:false rows
;;

let edits ?lang ~fold edits = view ?lang ~fold ~numbers:false (edit_rows edits)
