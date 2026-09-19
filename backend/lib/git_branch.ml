open! Core
open! Import

let parse_head contents =
  let contents = String.strip contents in
  match String.chop_prefix contents ~prefix:"ref: " with
  | Some ref ->
    Some
      (match String.chop_prefix ref ~prefix:"refs/heads/" with
       | Some branch -> branch
       | None -> ref)
  | None ->
    if String.is_empty contents
    then None
    else (
      let hash =
        match String.split contents ~on:' ' with
        | hash :: _ -> hash
        | [] -> contents
      in
      Some (String.prefix hash (Int.min 8 (String.length hash))))
;;

let find ~cwd =
  let rec git_dir dir =
    let candidate = Filename.concat dir ".git" in
    match Sys_unix.is_directory candidate with
    | `Yes -> Some candidate
    | `No | `Unknown ->
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else git_dir parent
  in
  match git_dir cwd with
  | None -> None
  | Some git_dir ->
    (match In_channel.read_all (Filename.concat git_dir "HEAD") with
     | contents -> parse_head contents
     | exception _ -> None)
;;
