open! Core
open! Import

type t =
  { root : string
  ; worktree : string
  ; branch : string option
  ; main : bool
  }
[@@deriving sexp, jsonaf, equal]

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

let read path =
  match In_channel.read_all path with
  | contents -> Some (String.strip contents)
  | exception _ -> None
;;

let realpath path =
  match Filename_unix.realpath path with
  | path -> path
  | exception _ -> path
;;

let absolute ~relative_to path =
  if Filename.is_absolute path
  then realpath path
  else realpath (Filename.concat relative_to path)
;;

let branch git_dir =
  Option.bind (read (Filename.concat git_dir "HEAD")) ~f:parse_head
;;

(* The main checkout of the repository whose git directory is [common]:
   [<root>/.git], or the bare repository itself. *)
let root_of_common common =
  if String.equal (Filename.basename common) ".git"
  then Filename.dirname common
  else common
;;

let at dir =
  let dot_git = Filename.concat dir ".git" in
  match Sys_unix.is_directory dot_git with
  | `Yes ->
    Some { root = dir; worktree = dir; branch = branch dot_git; main = true }
  | `No | `Unknown ->
    (match
       Option.bind (read dot_git) ~f:(String.chop_prefix ~prefix:"gitdir:")
     with
     | None -> None
     | Some git_dir ->
       let git_dir = absolute ~relative_to:dir (String.strip git_dir) in
       (match read (Filename.concat git_dir "commondir") with
        | Some common ->
          Some
            { root = root_of_common (absolute ~relative_to:git_dir common)
            ; worktree = dir
            ; branch = branch git_dir
            ; main = false
            }
        | None ->
          (* A submodule: a repository of its own. *)
          Some
            { root = dir; worktree = dir; branch = branch git_dir; main = true }))
;;

let find ~cwd =
  let rec go dir =
    match at dir with
    | Some t -> Some t
    | None ->
      let parent = Filename.dirname dir in
      if String.equal parent dir then None else go parent
  in
  match Sys_unix.is_directory cwd with
  | `Yes -> go (realpath cwd)
  | `No | `Unknown -> None
;;

let op = "$git_location"

let run ~cwd =
  Tool_result.ok (Json.to_string ([%jsonaf_of: t option] (find ~cwd)))
;;

let of_result (result : Tool_result.t) =
  if result.is_error
  then Or_error.error_string result.text
  else
    Or_error.try_with (fun () ->
      [%of_jsonaf: t option] (Json.parse result.text |> Or_error.ok_exn))
;;
