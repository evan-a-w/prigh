open! Core
open! Import

module File = struct
  module Status = struct
    type t =
      | Added
      | Deleted
      | Modified
      | Binary
    [@@deriving sexp_of, equal]
  end

  type t =
    { path : string
    ; status : Status.t
    ; added : int
    ; removed : int
    ; hunks : string list
    }
  [@@deriving sexp_of, equal]
end

(* [diff --git a/PATH b/PATH]: the paths are equal (no renames), so the
   second half is the path even when it contains spaces. *)
let path_of_header line =
  let rest = String.drop_prefix line (String.length "diff --git ") in
  let n = String.length rest in
  (* "a/P b/P": length 2|P| + 5. *)
  if n >= 5 && (n - 5) mod 2 = 0
  then String.drop_prefix rest (((n - 5) / 2) + 5)
  else (
    match String.substr_index rest ~pattern:" b/" with
    | Some i -> String.drop_prefix rest (i + 3)
    | None -> rest)
;;

let file_of_lines header lines =
  let in_hunks = ref false in
  let status = ref File.Status.Modified in
  let hunks = ref [] in
  let added = ref 0 in
  let removed = ref 0 in
  List.iter lines ~f:(fun line ->
    if !in_hunks
    then (
      hunks := line :: !hunks;
      if String.is_prefix line ~prefix:"+"
      then incr added
      else if String.is_prefix line ~prefix:"-"
      then incr removed)
    else if String.is_prefix line ~prefix:"@@"
    then (
      in_hunks := true;
      hunks := [ line ])
    else if String.is_prefix line ~prefix:"new file mode"
    then status := Added
    else if String.is_prefix line ~prefix:"deleted file mode"
    then status := Deleted
    else if String.is_prefix line ~prefix:"Binary files"
    then status := Binary);
  { File.path = path_of_header header
  ; status = !status
  ; added = !added
  ; removed = !removed
  ; hunks = List.rev !hunks
  }
;;

let parse diff =
  let rec go acc current lines =
    let flush acc =
      match current with
      | None -> acc
      | Some (header, rev_lines) -> file_of_lines header (List.rev rev_lines) :: acc
    in
    match lines with
    | [] -> List.rev (flush acc)
    | line :: rest when String.is_prefix line ~prefix:"diff --git " ->
      go (flush acc) (Some (line, [])) rest
    | line :: rest ->
      (match current with
       | None -> go acc None rest
       | Some (header, rev_lines) -> go acc (Some (header, line :: rev_lines)) rest)
  in
  go [] None (String.split_lines diff)
;;

let against (d : P.Git_diff.t) =
  if String.equal d.against "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  then "nothing (no commits yet)"
  else if String.length d.against = 40
  then String.prefix d.against 8
  else d.against
;;

let summary files =
  match files with
  | [] -> "no changes"
  | files ->
    let added = List.sum (module Int) files ~f:(fun f -> f.File.added) in
    let removed = List.sum (module Int) files ~f:(fun f -> f.File.removed) in
    sprintf
      "%d file%s changed, +%d −%d"
      (List.length files)
      (if List.length files = 1 then "" else "s")
      added
      removed
;;

let gray = Style.fg Gray
let dim = Style.dim Style.plain

let line_style line =
  if String.is_prefix line ~prefix:"@@"
  then Style.fg Cyan
  else if String.is_prefix line ~prefix:"+"
  then Style.fg Green
  else if String.is_prefix line ~prefix:"-"
  then Style.fg Red
  else gray
;;

let render (d : P.Git_diff.t) : Content.t =
  let files = parse d.diff in
  let head : Content.Line.t =
    [ { text = "diff "; style = Style.bold Style.plain }
    ; { text = d.root; style = Style.plain }
    ; { text = " against " ^ against d; style = dim }
    ; { text = "  " ^ summary files; style = gray }
    ]
  in
  let file (f : File.t) : Content.t =
    let chip =
      match f.status with
      | Added -> sprintf "new  +%d" f.added
      | Deleted -> sprintf "deleted  −%d" f.removed
      | Binary -> "binary"
      | Modified -> sprintf "+%d −%d" f.added f.removed
    in
    ([ { Content.Span.text = f.path; style = Style.bold Style.plain }
     ; { text = "  " ^ chip; style = dim }
     ]
     : Content.Line.t)
    :: List.map f.hunks ~f:(fun line ->
      Content.Line.of_string ~style:(line_style line) ("  " ^ line))
  in
  head
  :: List.concat_map files ~f:file
  @
  if d.truncated
  then
    [ Content.Line.of_string
        ~style:(Style.fg Yellow)
        "the diff is cut short: /diff PATH shows one path, or run git diff in \
         the terminal"
    ]
  else []
;;
