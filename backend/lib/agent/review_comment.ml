open! Core
open! Import

module Side = struct
  type t =
    | Old
    | New
  [@@deriving sexp, jsonaf, equal]

  let to_string = function
    | Old -> "old"
    | New -> "new"
  ;;

  let of_string = function
    | "old" -> Ok Old
    | "new" -> Ok New
    | s -> Or_error.errorf "unknown side %S: old or new" s
  ;;
end

module Columns = struct
  type t =
    { start_col : int
    ; end_col : int
    }
  [@@deriving sexp, jsonaf, equal]
end

module Range = struct
  type t =
    { first : int
    ; last : int
    ; side : Side.t
          [@default Side.New]
          [@sexp_drop_default.equal]
          [@jsonaf_drop_default.equal]
    ; columns : Columns.t option [@sexp.option] [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, equal]

  let lines ~first ~last = { first; last; side = New; columns = None }

  let to_string t =
    let lines =
      if t.first = t.last
      then Int.to_string t.first
      else sprintf "%d-%d" t.first t.last
    in
    match t.side with
    | New -> lines
    | Old -> "old " ^ lines
  ;;

  let anchor_lines anchor = Array.of_list (String.split_lines anchor)

  let selected t ~anchor =
    match t.columns with
    | None -> anchor
    | Some { start_col; end_col } ->
      let lines = anchor_lines anchor in
      let n = Array.length lines in
      if n = 0
      then ""
      else if n = 1
      then String.sub lines.(0) ~pos:start_col ~len:(end_col - start_col)
      else
        String.concat
          ~sep:"\n"
          ((String.drop_prefix lines.(0) start_col
            :: List.init (n - 2) ~f:(fun i -> lines.(i + 1)))
           @ [ String.prefix lines.(n - 1) end_col ])
  ;;

  let validate t ~anchor =
    if t.first < 1 || t.last < t.first
    then
      Or_error.errorf
        "lines %d-%d are not a range of lines: the first is 1 or more and the \
         last no smaller"
        t.first
        t.last
    else (
      match t.columns with
      | None -> Ok ()
      | Some { start_col; end_col } ->
        let lines = anchor_lines anchor in
        let n = Array.length lines in
        if n <> t.last - t.first + 1
        then
          Or_error.errorf
            "a selection's anchor needs the text of its %d lines, not %d"
            (t.last - t.first + 1)
            n
        else if
          start_col < 0
          || start_col > String.length lines.(0)
          || end_col < 0
          || end_col > String.length lines.(n - 1)
        then
          Or_error.errorf
            "columns %d-%d are outside the selected lines: they are byte \
             offsets into the first and the last line"
            start_col
            end_col
        else if n = 1 && end_col <= start_col
        then
          Or_error.errorf
            "columns %d-%d select nothing: select some text first"
            start_col
            end_col
        else Ok ())
  ;;
end

module Author = struct
  type t =
    | User
    | Agent
  [@@deriving sexp, jsonaf, equal]
end

module Edit = struct
  type t =
    { old_text : string
    ; new_text : string
    }
  [@@deriving sexp, jsonaf, equal]
end

module Proposal = struct
  module State = struct
    type t =
      | Pending
      | Accepted
      | Rejected
      | Failed of string
    [@@deriving sexp, jsonaf, equal]

    let to_string = function
      | Pending -> "pending"
      | Accepted -> "accepted"
      | Rejected -> "rejected"
      | Failed _ -> "failed"
    ;;
  end

  type t =
    { edits : Edit.t list
    ; state : State.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Post = struct
  type t =
    { author : Author.t
    ; text : string
    ; at : float
    ; proposal : Proposal.t option [@jsonaf.option]
    ; delivered : bool
    }
  [@@deriving sexp, jsonaf, equal]
end

module Status = struct
  type t =
    | Open
    | Resolved
  [@@deriving sexp, jsonaf, equal]

  let to_string = function
    | Open -> "open"
    | Resolved -> "resolved"
  ;;

  let of_string = function
    | "open" -> Ok Open
    | "resolved" -> Ok Resolved
    | s -> Or_error.errorf "unknown status %S: open or resolved" s
  ;;
end

type t =
  { id : string
  ; root : string
  ; path : string
  ; range : Range.t
  ; anchor : string
  ; status : Status.t
  ; thread : Post.t list
  }
[@@deriving sexp, jsonaf, equal]

let file t = Filename.concat t.root t.path

let unsent t =
  List.exists t.thread ~f:(fun (p : Post.t) ->
    Author.equal p.author User && not p.delivered)
;;

(* 1-based, as editors count: the characters before [byte], plus one. *)
let column line ~byte =
  1
  + String.count (String.prefix line byte) ~f:(fun c ->
    Char.to_int c land 0xC0 <> 0x80)
;;

let numbered t =
  let lines = Range.anchor_lines t.anchor in
  let n = Array.length lines in
  let width = String.length (Int.to_string (t.range.first + n)) in
  Array.to_list lines
  |> List.mapi ~f:(fun i line ->
    let line =
      match t.range.columns with
      | None -> line
      | Some { start_col; end_col } ->
        let line =
          if i = n - 1
          then
            String.prefix line end_col ^ "⟧" ^ String.drop_prefix line end_col
          else line
        in
        if i = 0
        then
          String.prefix line start_col ^ "⟦" ^ String.drop_prefix line start_col
        else line
    in
    sprintf "%*d  %s" width (t.range.first + i) line)
  |> String.concat ~sep:"\n"
;;

let selection_attr t =
  match t.range.columns with
  | None -> ""
  | Some { start_col; end_col } ->
    let lines = Range.anchor_lines t.anchor in
    let line i = if i < Array.length lines then lines.(i) else "" in
    sprintf
      " selection=\"%d:%d-%d:%d\""
      t.range.first
      (column (line 0) ~byte:start_col)
      t.range.last
      (column (line (Array.length lines - 1)) ~byte:end_col - 1)
;;

let post_text (p : Post.t) =
  let who =
    match p.author with
    | User -> "user"
    | Agent -> "you"
  in
  let attrs =
    (if p.delivered then "" else " new=\"true\"")
    ^
    match p.proposal with
    | Some { state; _ } ->
      sprintf " proposal=\"%s\"" (Proposal.State.to_string state)
    | None -> ""
  in
  sprintf "<post from=\"%s\"%s>\n%s\n</post>" who attrs (String.strip p.text)
;;

let to_prompt t =
  String.concat
    ~sep:"\n"
    ([ sprintf
         "<comment id=\"%s\" file=\"%s\" lines=\"%s\"%s%s status=\"%s\">"
         t.id
         (file t)
         (Range.to_string { t.range with side = New })
         (match t.range.side with
          | New -> ""
          | Old -> " side=\"old\"")
         (selection_attr t)
         (Status.to_string t.status)
     ; "<code>"
     ; numbered t
     ; "</code>"
     ]
     @ (match t.range.columns with
        | None -> []
        | Some _ ->
          [ "<selected>"
          ; Range.selected t.range ~anchor:t.anchor
          ; "</selected>"
          ])
     @ List.map t.thread ~f:post_text
     @ [ "</comment>" ])
;;
