open! Core

module Kind = struct
  type t =
    | Add
    | Update
    | Delete
  [@@deriving sexp_of, equal]

  let letter = function
    | Add -> "A"
    | Update -> "M"
    | Delete -> "D"
  ;;
end

let count_diff lines =
  List.fold lines ~init:(0, 0) ~f:(fun (added, removed) line ->
    if String.is_prefix line ~prefix:"+"
    then added + 1, removed
    else if String.is_prefix line ~prefix:"-"
    then added, removed + 1
    else added, removed)
;;

module File = struct
  type t =
    { kind : Kind.t
    ; path : string
    ; move_to : string option
    ; input : string list
    ; diff : string list option
    }
  [@@deriving sexp_of]

  let name t =
    match t.move_to with
    | Some dest -> sprintf "%s → %s" t.path dest
    | None -> t.path
  ;;

  let lines t =
    match t.kind with
    | Delete -> []
    | Add | Update -> Option.value t.diff ~default:t.input
  ;;

  let counts t = count_diff (lines t)
end

let headers =
  [ "*** Add File: ", Kind.Add
  ; "*** Update File: ", Update
  ; "*** Delete File: ", Delete
  ]
;;

let of_input input =
  let finish (file : File.t option) files =
    match file with
    | Some file when not (String.is_empty file.path) ->
      { file with input = List.rev file.input } :: files
    | Some _ | None -> files
  in
  let file, files =
    List.fold
      (String.split_lines input)
      ~init:(None, [])
      ~f:(fun (file, files) line ->
        match
          List.find_map headers ~f:(fun (prefix, kind) ->
            Option.map (String.chop_prefix line ~prefix) ~f:(fun path ->
              kind, String.strip path))
        with
        | Some (kind, path) ->
          ( Some { File.kind; path; move_to = None; input = []; diff = None }
          , finish file files )
        | None ->
          (match file, String.chop_prefix line ~prefix:"*** Move to: " with
           | Some file, Some dest ->
             Some { file with move_to = Some (String.strip dest) }, files
           | _ when String.is_prefix line ~prefix:"***" -> file, files
           | Some ({ kind = Add; _ } as f), None
             when String.is_prefix line ~prefix:"+" ->
             Some { f with input = line :: f.input }, files
           | Some ({ kind = Update; _ } as f), None ->
             let line = if String.is_empty line then " " else line in
             if
               List.exists [ "@@"; " "; "-"; "+" ] ~f:(fun prefix ->
                 String.is_prefix line ~prefix)
             then Some { f with input = line :: f.input }, files
             else file, files
           | _ -> file, files))
  in
  List.rev (finish file files)
;;

let summary_line = "Success."
let is_success text = String.is_prefix text ~prefix:summary_line

(* The result's unified diffs, as (old path, new path, lines from the first
   [@@]). *)
let diffs text =
  let path line ~prefix ~side =
    let p = String.drop_prefix line (String.length prefix) in
    Option.value (String.chop_prefix p ~prefix:side) ~default:p
  in
  let is_header = function
    | old_file :: new_file :: _ ->
      String.is_prefix old_file ~prefix:"--- "
      && String.is_prefix new_file ~prefix:"+++ "
    | _ -> false
  in
  let rec split_body acc lines =
    match lines with
    | line :: rest when not (is_header lines) -> split_body (line :: acc) rest
    | _ -> List.rev acc, lines
  in
  let rec go acc lines =
    match lines with
    | old_file :: new_file :: rest when is_header lines ->
      let body, rest = split_body [] rest in
      let body =
        List.filter body ~f:(fun line ->
          not (String.is_prefix line ~prefix:"\\"))
        |> List.rev
        |> List.drop_while ~f:String.is_empty
        |> List.rev
      in
      go
        (( path old_file ~prefix:"--- " ~side:"a/"
         , path new_file ~prefix:"+++ " ~side:"b/"
         , body )
         :: acc)
        rest
    | _ :: rest -> go acc rest
    | [] -> List.rev acc
  in
  go [] (String.split_lines text)
;;

let parse ~input ~result =
  let files = of_input input in
  match result with
  | Some text when is_success text ->
    let diffs = diffs text in
    List.map files ~f:(fun (file : File.t) ->
      match file.kind with
      | Add | Delete -> file
      | Update ->
        let dest = Option.value file.move_to ~default:file.path in
        let diff =
          List.find_map diffs ~f:(fun (old_path, new_path, lines) ->
            Option.some_if
              (String.equal new_path dest || String.equal old_path file.path)
              lines)
        in
        { file with diff })
  | Some _ | None -> files
;;

let counts files =
  List.fold files ~init:(0, 0) ~f:(fun (a, r) file ->
    let a', r' = File.counts file in
    a + a', r + r')
;;
