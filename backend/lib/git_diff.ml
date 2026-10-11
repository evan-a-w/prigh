open! Core
open! Import

module Result = struct
  type t =
    { root : string
    ; against : string
    ; diff : string
    ; truncated : bool
    }
  [@@deriving sexp, jsonaf, equal]

  let stats t =
    List.fold
      (String.split_lines t.diff)
      ~init:(0, 0, 0)
      ~f:(fun (files, added, removed) line ->
        if String.is_prefix line ~prefix:"diff --git "
        then files + 1, added, removed
        else if
          String.is_prefix line ~prefix:"+++ "
          || String.is_prefix line ~prefix:"--- "
        then files, added, removed
        else if String.is_prefix line ~prefix:"+"
        then files, added + 1, removed
        else if String.is_prefix line ~prefix:"-"
        then files, added, removed + 1
        else files, added, removed)
  ;;
end

let op = "$git_diff"
let max_bytes = 1_000_000
let max_untracked = 200

(* git's id of the empty tree, to diff against before the first commit. *)
let empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

let args ?path ?base () =
  `Object
    (List.filter_opt
       [ Option.map path ~f:(fun p -> "path", `String p)
       ; Option.map base ~f:(fun b -> "base", `String b)
       ])
;;

(* Whatever the user's git configuration says. *)
let diff_options =
  [ "diff"
  ; "--no-color"
  ; "--no-ext-diff"
  ; "--no-renames"
  ; "--src-prefix=a/"
  ; "--dst-prefix=b/"
  ]
;;

let unknown_revision = "unknown revision"
let is_unknown_revision = String.is_prefix ~prefix:unknown_revision

exception Failed of string

let run ~env ~cancel ~cwd ~arguments =
  let git ?(ok_codes = [ 0 ]) ~dir args =
    match
      Process.run_collect
        ~env
        ~cancel
        ~extra_env:[ "GIT_PAGER", "cat"; "LC_ALL", "C" ]
        ~prog:"git"
        ~args:("-C" :: dir :: "-c" :: "core.quotepath=off" :: args)
        ()
    with
    | { exit = Exited code; stdout; _ } when List.mem ok_codes code ~equal:( = )
      -> Ok stdout
    | { exit = Exited _; stderr; _ } -> Error (String.strip stderr)
    | { exit = Cancelled; _ } -> raise (Failed "[cancelled]")
    | { exit = Signaled _ | Timed_out; stderr; _ } ->
      raise (Failed ("git failed: " ^ String.strip stderr))
    | exception Eio.Io _ ->
      raise
        (Failed
           "git is not installed on the tool host: install it there (e.g. nix \
            profile install nixpkgs#git)")
  in
  let string_arg name =
    match Json.member name arguments with
    | Some (`String s) when not (String.is_empty (String.strip s)) ->
      Some (String.strip s)
    | _ -> None
  in
  try
    let root =
      match git ~dir:cwd [ "rev-parse"; "--show-toplevel" ] with
      | Ok root -> String.strip root
      | Error _ ->
        raise
          (Failed
             (sprintf
                "not a git repository: %s (git init starts one, or /cd into \
                 one)"
                cwd))
    in
    let against =
      match string_arg "base" with
      | Some base ->
        (match
           git
             ~dir:root
             [ "rev-parse"; "--verify"; "--quiet"; base ^ "^{commit}" ]
         with
         | Error _ ->
           raise
             (Failed
                (sprintf
                   "%s %S: name a branch, tag or commit (e.g. main)"
                   unknown_revision
                   base))
         | Ok _ ->
           (match git ~dir:root [ "merge-base"; "HEAD"; base ] with
            | Ok sha -> String.strip sha
            | Error _ -> base))
      | None ->
        (match git ~dir:root [ "rev-parse"; "--verify"; "--quiet"; "HEAD" ] with
         | Ok _ -> "HEAD"
         | Error _ -> empty_tree)
    in
    let pathspec =
      match string_arg "path" with
      | None -> []
      | Some path -> [ "--"; Tool.resolve ~cwd path ]
    in
    let tracked =
      match git ~dir:root (diff_options @ [ against ] @ pathspec) with
      | Ok diff -> diff
      | Error e -> raise (Failed ("git diff failed: " ^ e))
    in
    let untracked =
      match
        git
          ~dir:root
          ([ "ls-files"; "--others"; "--exclude-standard"; "-z" ] @ pathspec)
      with
      | Ok out ->
        String.split out ~on:'\000' |> List.filter ~f:(Fn.non String.is_empty)
      | Error _ -> []
    in
    let buffer = Buffer.create (String.length tracked) in
    Buffer.add_string buffer tracked;
    let full () = Buffer.length buffer >= max_bytes in
    List.iteri untracked ~f:(fun i file ->
      if (not (full ())) && i < max_untracked
      then (
        match
          git
            ~ok_codes:[ 0; 1 ]
            ~dir:root
            (diff_options @ [ "--no-index"; "--"; "/dev/null"; file ])
        with
        | Ok diff -> Buffer.add_string buffer diff
        | Error _ -> ()));
    let diff = Buffer.contents buffer in
    let truncated =
      String.length diff > max_bytes || List.length untracked > max_untracked
    in
    let diff =
      if String.length diff > max_bytes
      then (
        (* At a line boundary. *)
        let cut = String.prefix diff max_bytes in
        match String.rindex cut '\n' with
        | Some i -> String.prefix cut (i + 1)
        | None -> cut)
      else diff
    in
    Tool_result.ok
      (Json.to_string
         ([%jsonaf_of: Result.t] { root; against; diff; truncated }))
  with
  | Failed message -> Tool_result.error message
;;

let of_tool_result (result : Tool_result.t) =
  if result.is_error
  then Or_error.error_string result.text
  else
    Or_error.try_with (fun () ->
      [%of_jsonaf: Result.t] (Json.parse result.text |> Or_error.ok_exn))
;;
