open! Core
open! Import

let grammar =
  {|start: begin_patch hunk+ end_patch
begin_patch: "*** Begin Patch" LF
end_patch: "*** End Patch" LF?

hunk: add_hunk | delete_hunk | update_hunk
add_hunk: "*** Add File: " filename LF add_line+
delete_hunk: "*** Delete File: " filename LF
update_hunk: "*** Update File: " filename LF change_move? change?

filename: /(.+)/
add_line: "+" /(.*)/ LF -> line

change_move: "*** Move to: " filename LF
change: (change_context | change_line)+ eof_line?
change_context: ("@@" | "@@ " /(.+)/) LF
change_line: ("+" | "-" | " ") /(.*)/ LF
eof_line: "*** End of File" LF

%import common.LF
|}
;;

let description =
  {|Use the `apply_patch` tool to edit files.
Your patch language is a stripped-down, file-oriented diff format designed to be easy to parse and safe to apply. You can think of it as a high-level envelope:

*** Begin Patch
[ one or more file sections ]
*** End Patch

Within that envelope, you get a sequence of file operations.
You MUST include a header to specify the action you are taking.
Each operation starts with one of three headers:

*** Add File: <path> - create a new file. Every following line is a + line (the initial contents).
*** Delete File: <path> - remove an existing file. Nothing follows.
*** Update File: <path> - patch an existing file in place (optionally with a rename).

May be immediately followed by *** Move to: <new path> if you want to rename the file.
Then one or more "hunks", each introduced by @@ (optionally followed by a hunk header).
Within a hunk each line starts with:
- " " (a space) for a context line, unchanged
- "-" for a line to remove
- "+" for a line to add

For instructions on [context_before] and [context_after]:
- By default, show 3 lines of code immediately above and 3 lines immediately below each change. If a change is within 3 lines of a previous change, do NOT duplicate the first change's [context_after] lines in the second change's [context_before] lines.
- If 3 lines of context is insufficient to uniquely identify the snippet of code within the file, use the @@ operator to indicate the class or function to which the snippet belongs. For instance, we might have:
@@ class BaseClass
[3 lines of pre-context]
-[old_code]
+[new_code]
[3 lines of post-context]

- If a code block is repeated so many times in a class or function such that even a single @@ statement and 3 lines of context cannot uniquely identify the snippet of code, you can use multiple @@ statements to jump to the right context. For instance:

@@ class BaseClass
@@     def method():
[3 lines of pre-context]
-[old_code]
+[new_code]
[3 lines of post-context]

- Hunks must appear in the order their changes occur in the file.
- If a hunk changes the last lines of the file, end it with *** End of File.

The full grammar definition is below:
Patch := Begin { FileOp } End
Begin := "*** Begin Patch" NEWLINE
End := "*** End Patch" NEWLINE
FileOp := AddFile | DeleteFile | UpdateFile
AddFile := "*** Add File: " path NEWLINE { "+" line NEWLINE }
DeleteFile := "*** Delete File: " path NEWLINE
UpdateFile := "*** Update File: " path NEWLINE [ MoveTo ] { Hunk }
MoveTo := "*** Move to: " newPath NEWLINE
Hunk := "@@" [ header ] NEWLINE { HunkLine } [ "*** End of File" NEWLINE ]
HunkLine := (" " | "-" | "+") text NEWLINE

A full patch can combine several operations:

*** Begin Patch
*** Add File: hello.txt
+Hello world
*** Update File: src/app.py
*** Move to: src/main.py
@@ def greet():
-print("Hi")
+print("Hello, world!")
*** Delete File: obsolete.txt
*** End Patch

It is important to remember:

- You must include a header with your intended action (Add/Delete/Update)
- You must prefix new lines with `+` even when creating a new file
- Use paths relative to the working directory (absolute paths also work, but prefer relative ones)|}
;;

module Chunk = struct
  type t =
    { context : string option
    ; old_lines : string list
    ; new_lines : string list
    ; end_of_file : bool
    }
  [@@deriving sexp_of]
end

module Operation = struct
  type t =
    | Add of
        { path : string
        ; contents : string
        }
    | Delete of { path : string }
    | Update of
        { path : string
        ; move_to : string option
        ; chunks : Chunk.t list
        }
  [@@deriving sexp_of]

  let path = function
    | Add { path; _ } | Delete { path } | Update { path; _ } -> path
  ;;
end

let begin_marker = "*** Begin Patch"
let end_marker = "*** End Patch"
let add_marker = "*** Add File: "
let delete_marker = "*** Delete File: "
let update_marker = "*** Update File: "
let move_marker = "*** Move to: "
let eof_marker = "*** End of File"

let split_lines text =
  String.split text ~on:'\n'
  |> List.map ~f:(String.chop_suffix_if_exists ~suffix:"\r")
;;

module Line = struct
  type t =
    { number : int
    ; text : string
    }
end

exception Parse_error of string

let fail (line : Line.t) fmt =
  Printf.ksprintf
    (fun s -> raise (Parse_error (sprintf "line %d: %s" line.number s)))
    fmt
;;

let is_blank s = String.is_empty (String.strip s)

(* The delimiter of a heredoc opened at the end of [line], as in
   [apply_patch <<'EOF']. *)
let heredoc_delimiter line =
  match String.substr_index line ~pattern:"<<" with
  | None -> None
  | Some i ->
    let delimiter =
      String.drop_prefix line (i + 2)
      |> String.strip
      |> String.chop_prefix_if_exists ~prefix:"-"
      |> String.strip ~drop:(fun c -> Char.equal c '\'' || Char.equal c '"')
    in
    if
      (not (String.is_empty delimiter))
      && String.for_all delimiter ~f:(fun c ->
        Char.is_alphanum c || Char.equal c '_')
    then Some delimiter
    else None
;;

(* Drops surrounding blank lines and a heredoc wrapper. *)
let envelope text =
  let lines =
    split_lines text
    |> List.mapi ~f:(fun i text -> { Line.number = i + 1; text })
    |> List.drop_while ~f:(fun (l : Line.t) -> is_blank l.text)
    |> List.rev
    |> List.drop_while ~f:(fun (l : Line.t) -> is_blank l.text)
    |> List.rev
    |> Array.of_list
  in
  let n = Array.length lines in
  let stripped i = String.strip lines.(i).text in
  if n = 0
  then raise (Parse_error "the patch is empty: expected '*** Begin Patch'");
  if n >= 4 && not (String.equal (stripped 0) begin_marker)
  then (
    match heredoc_delimiter (stripped 0) with
    | Some delimiter when String.equal (stripped (n - 1)) delimiter ->
      Array.sub lines ~pos:1 ~len:(n - 2)
    | _ -> lines)
  else lines
;;

let parse_chunk lines ~pos ~stop ~allow_missing_context =
  let first : Line.t = lines.(pos) in
  let context, start =
    if String.equal first.text "@@"
    then None, pos + 1
    else if String.is_prefix first.text ~prefix:"@@ "
    then Some (String.drop_prefix first.text 3), pos + 1
    else if allow_missing_context
    then None, pos
    else
      fail
        first
        "expected update hunk to start with a @@ context marker, got: '%s'"
        first.text
  in
  if start >= stop
  then fail lines.(start) "update hunk does not contain any lines";
  if Option.is_some context && String.is_prefix lines.(start).text ~prefix:"@@"
  then
    (* One of several @@ lines that narrow down the location. *)
    ( { Chunk.context; old_lines = []; new_lines = []; end_of_file = false }
    , start )
  else (
    let old_lines = Queue.create () in
    let new_lines = Queue.create () in
    let rec loop i ~parsed =
      if i >= stop
      then i, false
      else (
        let line : Line.t = lines.(i) in
        if String.equal (String.rstrip line.text) eof_marker
        then
          if parsed = 0
          then fail line "update hunk does not contain any lines"
          else i + 1, true
        else (
          let continue () = loop (i + 1) ~parsed:(parsed + 1) in
          let rest () = String.drop_prefix line.text 1 in
          match
            if String.is_empty line.text then None else Some line.text.[0]
          with
          | None ->
            Queue.enqueue old_lines "";
            Queue.enqueue new_lines "";
            continue ()
          | Some ' ' ->
            Queue.enqueue old_lines (rest ());
            Queue.enqueue new_lines (rest ());
            continue ()
          | Some '-' ->
            Queue.enqueue old_lines (rest ());
            continue ()
          | Some '+' ->
            Queue.enqueue new_lines (rest ());
            continue ()
          | _ ->
            if parsed = 0
            then
              fail
                line
                "unexpected line found in update hunk: '%s'. Every line should \
                 start with ' ' (context line), '+' (added line), or '-' \
                 (removed line)"
                line.text
            else i, false))
    in
    let next, end_of_file = loop start ~parsed:0 in
    ( { Chunk.context
      ; old_lines = Queue.to_list old_lines
      ; new_lines = Queue.to_list new_lines
      ; end_of_file
      }
    , next ))
;;

let parse_path (line : Line.t) ~marker header =
  let path = String.strip (String.drop_prefix header (String.length marker)) in
  if String.is_empty path
  then fail line "missing path after '%s'" (String.rstrip marker);
  path
;;

(* Parses the operation starting at [lines.(pos)], returning it and the
   position after it. *)
let parse_operation lines ~pos ~stop =
  let line : Line.t = lines.(pos) in
  let header = String.strip line.text in
  if String.is_prefix header ~prefix:add_marker
  then (
    let path = parse_path line ~marker:add_marker header in
    let rec take i acc =
      if i < stop && String.is_prefix lines.(i).Line.text ~prefix:"+"
      then take (i + 1) (String.drop_prefix lines.(i).text 1 :: acc)
      else i, List.rev acc
    in
    let next, added = take (pos + 1) [] in
    let contents = List.map added ~f:(fun l -> l ^ "\n") |> String.concat in
    Operation.Add { path; contents }, next)
  else if String.is_prefix header ~prefix:delete_marker
  then Delete { path = parse_path line ~marker:delete_marker header }, pos + 1
  else if String.is_prefix header ~prefix:update_marker
  then (
    let path = parse_path line ~marker:update_marker header in
    let move_to, start =
      let next = pos + 1 in
      if next < stop && String.is_prefix lines.(next).text ~prefix:move_marker
      then (
        let header = String.strip lines.(next).text in
        Some (parse_path lines.(next) ~marker:move_marker header), next + 1)
      else None, next
    in
    let rec chunks i acc =
      if i >= stop
      then i, List.rev acc
      else if is_blank lines.(i).Line.text
      then chunks (i + 1) acc
      else if String.is_prefix lines.(i).text ~prefix:"***"
      then i, List.rev acc
      else (
        let chunk, next =
          parse_chunk
            lines
            ~pos:i
            ~stop
            ~allow_missing_context:(List.is_empty acc)
        in
        chunks next (chunk :: acc))
    in
    let next, chunks = chunks start [] in
    if List.is_empty chunks
    then fail line "Update File hunk for path '%s' is empty" path;
    Update { path; move_to; chunks }, next)
  else
    fail
      line
      "invalid hunk header '%s': valid headers are '*** Add File: {path}', \
       '*** Delete File: {path}', '*** Update File: {path}'"
      header
;;

let parse text =
  match
    let lines = envelope text in
    let n = Array.length lines in
    let first = lines.(0) in
    let last = lines.(n - 1) in
    if not (String.equal (String.strip first.text) begin_marker)
    then
      fail
        first
        "expected '*** Begin Patch', got '%s'"
        (String.strip first.text);
    if n < 2 || not (String.equal (String.strip last.text) end_marker)
    then
      fail
        last
        "expected '*** End Patch' as the last line of the patch, got '%s'"
        (String.strip last.text);
    let stop = n - 1 in
    let rec operations i acc =
      if i >= stop
      then List.rev acc
      else if is_blank lines.(i).text
      then operations (i + 1) acc
      else (
        let operation, next = parse_operation lines ~pos:i ~stop in
        operations next (operation :: acc))
    in
    match operations 1 [] with
    | [] ->
      fail
        last
        "the patch contains no file operations: expected '*** Add File: \
         {path}', '*** Delete File: {path}' or '*** Update File: {path}' \
         before '*** End Patch'"
    | operations -> operations
  with
  | operations -> Ok operations
  | exception Parse_error message -> Or_error.error_string message
;;

let paths text =
  let markers =
    List.map
      [ add_marker; delete_marker; update_marker; move_marker ]
      ~f:String.rstrip
  in
  split_lines text
  |> List.filter_map ~f:(fun line ->
    let line = String.strip line in
    List.find_map markers ~f:(fun prefix ->
      String.chop_prefix line ~prefix
      |> Option.map ~f:String.strip
      |> Option.filter ~f:(Fn.non String.is_empty)))
  |> List.stable_dedup ~compare:String.compare
;;

module Change = struct
  type t =
    | Added of
        { path : string
        ; contents : string
        }
    | Deleted of
        { path : string
        ; before : string
        }
    | Updated of
        { path : string
        ; move_to : string option
        ; before : string
        ; after : string
        }
  [@@deriving sexp_of]
end

let normalise s = String.strip (Lenient_match.ascii_punctuation s)

let matchers =
  [ Fn.id; (fun s -> String.rstrip s); (fun s -> String.strip s); normalise ]
;;

(* The index of the first occurrence of [pattern] in [lines] at or after
   [start], trying each matcher in turn over the whole range. With
   [end_of_file], a match at the very end of the file is preferred. *)
let seek_sequence lines pattern ~start ~end_of_file =
  let n = Array.length lines in
  let m = Array.length pattern in
  if m = 0
  then Some start
  else if m > n
  then None
  else (
    let search ~from =
      List.find_map matchers ~f:(fun key ->
        let pattern = Array.map pattern ~f:key in
        let lines = lazy (Array.map lines ~f:key) in
        let rec loop i =
          if i > n - m
          then None
          else if
            Array.for_alli pattern ~f:(fun k p ->
              String.equal (force lines).(i + k) p)
          then Some i
          else loop (i + 1)
        in
        loop from)
    in
    let at_end =
      if end_of_file && n - m >= start then search ~from:(n - m) else None
    in
    match at_end with
    | Some _ -> at_end
    | None -> search ~from:start)
;;

let file_lines contents =
  match List.rev (String.split contents ~on:'\n') with
  | "" :: rest -> List.rev rest
  | lines -> List.rev lines
;;

let update_contents ~path ~before chunks =
  let open Or_error.Let_syntax in
  let lines = Array.of_list (file_lines before) in
  let n = Array.length lines in
  let%map _, replacements =
    List.fold_result
      chunks
      ~init:(0, [])
      ~f:(fun (index, replacements) (chunk : Chunk.t) ->
        let%bind index =
          match chunk.context with
          | None -> Ok index
          | Some context ->
            (match
               seek_sequence lines [| context |] ~start:index ~end_of_file:false
             with
             | Some i -> Ok (i + 1)
             | None ->
               Or_error.errorf "Failed to find context '%s' in %s" context path)
        in
        if List.is_empty chunk.old_lines
        then (
          let at =
            if Option.is_some chunk.context
            then index
            else if n > 0 && String.is_empty lines.(n - 1)
            then n - 1
            else n
          in
          Ok (index, (at, 0, chunk.new_lines) :: replacements))
        else (
          let seek pattern =
            seek_sequence
              lines
              (Array.of_list pattern)
              ~start:index
              ~end_of_file:chunk.end_of_file
          in
          let found =
            match seek chunk.old_lines with
            | Some i -> Some (i, chunk.old_lines, chunk.new_lines)
            | None ->
              (* A blank line closing a hunk is often padding rather than a
                 line of the file. *)
              (match List.rev chunk.old_lines with
               | "" :: rev_old ->
                 let old_lines = List.rev rev_old in
                 let new_lines =
                   match List.rev chunk.new_lines with
                   | "" :: rev_new -> List.rev rev_new
                   | _ -> chunk.new_lines
                 in
                 seek old_lines
                 |> Option.map ~f:(fun i -> i, old_lines, new_lines)
               | _ -> None)
          in
          match found with
          | Some (i, old_lines, new_lines) ->
            let length = List.length old_lines in
            Ok (i + length, (i, length, new_lines) :: replacements)
          | None ->
            Or_error.errorf
              "Failed to find expected lines in %s:\n%s"
              path
              (String.concat ~sep:"\n" chunk.old_lines)))
  in
  let replacements =
    List.rev replacements
    |> List.stable_sort ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)
  in
  let output = Queue.create () in
  let pos =
    List.fold replacements ~init:0 ~f:(fun pos (start, length, new_lines) ->
      for i = pos to start - 1 do
        Queue.enqueue output lines.(i)
      done;
      Queue.enqueue_all output new_lines;
      Int.max pos (start + length))
  in
  for i = pos to n - 1 do
    Queue.enqueue output lines.(i)
  done;
  let output = Queue.to_list output in
  let output =
    match List.last output with
    | Some "" -> output
    | _ -> output @ [ "" ]
  in
  String.concat ~sep:"\n" output
;;

let apply ?(file = Fn.id) operations ~read =
  let open Or_error.Let_syntax in
  let overlay = String.Table.create () in
  let read path =
    match Hashtbl.find overlay (file path) with
    | Some contents -> contents
    | None -> read path
  in
  let apply_one : Operation.t -> Change.t Or_error.t = function
    | Add { path; contents } ->
      let change : Change.t =
        match read path with
        | None -> Added { path; contents }
        | Some before ->
          Updated { path; move_to = None; before; after = contents }
      in
      Hashtbl.set overlay ~key:(file path) ~data:(Some contents);
      Ok change
    | Delete { path } ->
      (match read path with
       | None ->
         Or_error.errorf "Failed to delete %s: the file does not exist" path
       | Some before ->
         Hashtbl.set overlay ~key:(file path) ~data:None;
         Ok (Change.Deleted { path; before }))
    | Update { path; move_to; chunks } ->
      (match read path with
       | None ->
         Or_error.errorf
           "Failed to update %s: the file does not exist (use '*** Add File: \
            %s' to create it)"
           path
           path
       | Some before ->
         let%map after = update_contents ~path ~before chunks in
         Hashtbl.set overlay ~key:(file path) ~data:None;
         Hashtbl.set
           overlay
           ~key:(file (Option.value move_to ~default:path))
           ~data:(Some after);
         Change.Updated { path; move_to; before; after })
  in
  List.fold_result operations ~init:[] ~f:(fun changes operation ->
    let%map change = apply_one operation in
    change :: changes)
  |> Or_error.map ~f:List.rev
;;
