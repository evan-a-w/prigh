open! Core
open! Import

module Commit = struct
  type t =
    { sha : string
    ; subject : string
    }
  [@@deriving sexp, jsonaf, equal]
end

module Result = struct
  type t =
    { root : string
    ; against : string
    ; diff : string
    ; truncated : bool
    ; branch : string option [@sexp.option] [@jsonaf.option]
    ; base : string [@default "HEAD"]
    ; main : bool [@default true]
    ; branches : string list [@sexp.list] [@jsonaf.default []]
    ; remote_branches : string list [@sexp.list] [@jsonaf.default []]
    ; tags : string list [@sexp.list] [@jsonaf.default []]
    ; commits : Commit.t list [@sexp.list] [@jsonaf.default []]
    ; rev : string option [@sexp.option] [@jsonaf.option]
    ; direct : bool [@default false]
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
let max_branches = 500
let max_tags = 200
let max_commits = 20
let show_op = "$git_show"
let refs_op = "$git_refs"
let max_show_bytes = 262_144

(* git's id of the empty tree, to diff against before the first commit. *)
let empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

let args ?path ?base ?dir ?rev ?(direct = false) ?(default_base = false) () =
  `Object
    (List.filter_opt
       [ Option.map path ~f:(fun p -> "path", `String p)
       ; Option.map base ~f:(fun b -> "base", `String b)
       ; Option.map dir ~f:(fun d -> "dir", `String d)
       ; Option.map rev ~f:(fun r -> "rev", `String r)
       ; Option.some_if direct ("direct", `True)
       ; Option.some_if default_base ("default_base", `True)
       ])
;;

let show_args ?dir ~rev ~path () =
  `Object
    (List.filter_opt
       [ Option.map dir ~f:(fun d -> "dir", `String d)
       ; Some ("rev", `String rev)
       ; Some ("path", `String path)
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

let git ~env ~cancel ?(ok_codes = [ 0 ]) ~dir args =
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
;;

let string_arg arguments name =
  match Json.member name arguments with
  | Some (`String s) when not (String.is_empty (String.strip s)) ->
    Some (String.strip s)
  | _ -> None
;;

let bool_arg arguments name =
  match Json.member name arguments with
  | Some `True -> true
  | _ -> false
;;

(* The top directory of [dir]'s worktree (default: [cwd]'s). *)
let locate ~git ~cwd arguments =
  let named = string_arg arguments "dir" in
  let dir =
    match named with
    | Some dir -> Tool.resolve ~cwd dir
    | None -> cwd
  in
  match git ~dir [ "rev-parse"; "--show-toplevel" ] with
  | Ok root -> String.strip root
  | Error _ when Option.is_some named ->
    raise
      (Failed
         (sprintf
            "not a git repository: %s (name a worktree of the repository: \
             /worktree lists them)"
            dir))
  | Error _ ->
    raise
      (Failed
         (sprintf
            "not a git repository: %s (git init starts one, or /cd into one)"
            cwd))
;;

let commit_id ~git ~dir rev =
  if String.is_prefix rev ~prefix:"-"
  then None
  else (
    match
      git ~dir [ "rev-parse"; "--verify"; "--quiet"; rev ^ "^{commit}" ]
    with
    | Ok sha when not (String.is_empty (String.strip sha)) ->
      Some (String.strip sha)
    | Ok _ | Error _ -> None)
;;

(* Local branches, remote branches and tags (newest first). *)
let refs ~git ~dir =
  let list ?(sort = []) prefix ~max =
    match
      git ~dir ([ "for-each-ref"; "--format=%(refname)" ] @ sort @ [ prefix ])
    with
    | Ok out ->
      List.filter_map (String.split_lines out) ~f:(fun ref_ ->
        if String.is_suffix ref_ ~suffix:"/HEAD"
        then None
        else String.chop_prefix ref_ ~prefix:(prefix ^ "/"))
      |> Fn.flip List.take max
    | Error _ -> []
  in
  ( list "refs/heads" ~max:max_branches
  , list "refs/remotes" ~max:max_branches
  , list ~sort:[ "--sort=-creatordate" ] "refs/tags" ~max:max_tags )
;;

let unknown ~refs ~what rev =
  let local, remote, tags = refs in
  Failed
    (sprintf
       "%s %S: name a %s (%s)"
       unknown_revision
       rev
       what
       (match Edit_distance.closest (local @ remote @ tags) rev with
        | [] -> "e.g. main"
        | close -> "closest: " ^ String.concat ~sep:", " close))
;;

let commits ~git ~dir ~tip =
  match
    git
      ~dir
      [ "log"; sprintf "-%d" max_commits; "--format=%H%x09%s"; tip; "--" ]
  with
  | Ok out ->
    List.filter_map (String.split_lines out) ~f:(fun line ->
      Option.map (String.lsplit2 line ~on:'\t') ~f:(fun (sha, subject) ->
        { Commit.sha; subject }))
  | Error _ -> []
;;

let config ~git ~dir key =
  match git ~dir [ "config"; "--get"; key ] with
  | Ok v when not (String.is_empty (String.strip v)) -> Some (String.strip v)
  | Ok _ | Error _ -> None
;;

(* The base [branch] is compared with when none is named: see the mli. *)
let default_base ~git ~dir ~branch =
  let recorded =
    Option.bind branch ~f:(fun b -> config ~git ~dir (Worktree.base_key b))
  in
  let remotes =
    Option.bind branch ~f:(fun b ->
      config ~git ~dir (sprintf "branch.%s.remote" b))
    |> Option.filter ~f:(fun r -> not (String.equal r "."))
    |> Option.to_list
    |> fun rs ->
    rs
    @ List.filter [ "origin" ] ~f:(fun o ->
      not (List.mem rs o ~equal:String.equal))
  in
  let remote_heads =
    List.filter_map remotes ~f:(fun r ->
      match
        git
          ~dir
          [ "symbolic-ref"
          ; "--quiet"
          ; "--short"
          ; sprintf "refs/remotes/%s/HEAD" r
          ]
      with
      | Ok head when not (String.is_empty (String.strip head)) ->
        Some (String.strip head)
      | Ok _ | Error _ -> None)
  in
  List.find
    (Option.to_list recorded
     @ remote_heads
     @ [ "main"; "master"; "origin/main"; "origin/master" ])
    ~f:(fun base ->
      (not (Option.exists branch ~f:(String.equal base)))
      && Option.is_some (commit_id ~git ~dir base))
;;

let run ~env ~cancel ~cwd ~arguments =
  let git_codes ?ok_codes ~dir args = git ~env ~cancel ?ok_codes ~dir args in
  let git ~dir args = git_codes ~dir args in
  try
    let root = locate ~git ~cwd arguments in
    let refs = refs ~git ~dir:root in
    let branches, remote_branches, tags = refs in
    let rev =
      Option.map (string_arg arguments "rev") ~f:(fun rev ->
        match commit_id ~git ~dir:root rev with
        | Some _ -> rev
        | None -> raise (unknown ~refs ~what:"branch to review" rev))
    in
    let branch =
      match rev with
      | Some _ -> None
      | None ->
        (match
           git ~dir:root [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ]
         with
         | Ok b when not (String.is_empty (String.strip b)) ->
           Some (String.strip b)
         | Ok _ | Error _ -> None)
    in
    let main =
      Option.is_none rev
      &&
      match
        git
          ~dir:root
          [ "rev-parse"
          ; "--path-format=absolute"
          ; "--git-dir"
          ; "--git-common-dir"
          ]
      with
      | Ok out ->
        (match String.split_lines out with
         | [ git_dir; common ] -> String.equal git_dir common
         | _ -> true)
      | Error _ -> true
    in
    let tip = Option.value rev ~default:"HEAD" in
    let base =
      match string_arg arguments "base", rev with
      | Some "HEAD", Some rev ->
        raise
          (Failed
             (sprintf
                "%s is not checked out, so it has no uncommitted changes: \
                 compare it with a branch (e.g. --base main)"
                rev))
      | Some "HEAD", None -> None
      | Some base, _ ->
        if Option.is_none (commit_id ~git ~dir:root base)
        then raise (unknown ~refs ~what:"branch, tag or commit" base);
        Some base
      | None, Some rev ->
        (match default_base ~git ~dir:root ~branch:(Some rev) with
         | Some base -> Some base
         | None ->
           raise
             (Failed
                (sprintf
                   "nothing to compare %s with: name a base (e.g. --base main)"
                   rev)))
      | None, None when bool_arg arguments "default_base" && not main ->
        default_base ~git ~dir:root ~branch
      | None, None -> None
    in
    let direct = Option.is_some base && bool_arg arguments "direct" in
    let against, base =
      match base with
      | None ->
        ( (if Option.is_some (commit_id ~git ~dir:root "HEAD")
           then "HEAD"
           else empty_tree)
        , "HEAD" )
      | Some base when direct ->
        Option.value (commit_id ~git ~dir:root base) ~default:base, base
      | Some base ->
        ( (match git ~dir:root [ "merge-base"; tip; base ] with
           | Ok sha -> String.strip sha
           | Error _ -> base)
        , base )
    in
    let pathspec =
      match string_arg arguments "path" with
      | None -> []
      | Some path -> [ "--"; Tool.resolve ~cwd path ]
    in
    let tracked =
      match
        git
          ~dir:root
          (diff_options @ [ against ] @ Option.to_list rev @ pathspec)
      with
      | Ok diff -> diff
      | Error e -> raise (Failed ("git diff failed: " ^ e))
    in
    let untracked =
      match rev with
      | Some _ -> []
      | None ->
        (match
           git
             ~dir:root
             ([ "ls-files"; "--others"; "--exclude-standard"; "-z" ] @ pathspec)
         with
         | Ok out ->
           String.split out ~on:'\000'
           |> List.filter ~f:(Fn.non String.is_empty)
         | Error _ -> [])
    in
    let buffer = Buffer.create (String.length tracked) in
    Buffer.add_string buffer tracked;
    let full () = Buffer.length buffer >= max_bytes in
    List.iteri untracked ~f:(fun i file ->
      if (not (full ())) && i < max_untracked
      then (
        match
          git_codes
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
         ([%jsonaf_of: Result.t]
            { root
            ; against
            ; diff
            ; truncated
            ; branch
            ; base
            ; main
            ; branches
            ; remote_branches
            ; tags
            ; commits = commits ~git ~dir:root ~tip
            ; rev
            ; direct
            }))
  with
  | Failed message -> Tool_result.error message
;;

module Shown = struct
  type t =
    { text : string
    ; truncated : bool
    }
  [@@deriving sexp, jsonaf, equal]
end

let show ~env ~cancel ~cwd ~arguments =
  let git ~dir args = git ~env ~cancel ~dir args in
  try
    let root = locate ~git ~cwd arguments in
    let required name =
      match string_arg arguments name with
      | Some v -> v
      | None -> raise (Failed (sprintf "missing argument %S" name))
    in
    let rev = required "rev" in
    let path = required "path" in
    if Option.is_none (commit_id ~git ~dir:root rev)
    then raise (unknown ~refs:(refs ~git ~dir:root) ~what:"branch" rev);
    match git ~dir:root [ "cat-file"; "blob"; rev ^ ":" ^ path ] with
    | Error _ -> raise (Failed (sprintf "%s has no file %s" rev path))
    | Ok text ->
      let truncated = String.length text > max_show_bytes in
      let text =
        if truncated
        then (
          let cut = String.prefix text max_show_bytes in
          match String.rindex cut '\n' with
          | Some i -> String.prefix cut (i + 1)
          | None -> cut)
        else text
      in
      if String.mem text '\000' then raise (Failed "not a text file");
      let text = Utf8.sanitize text in
      Tool_result.ok
        (Json.to_string ([%jsonaf_of: Shown.t] { text; truncated }))
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

module Refs = struct
  type t =
    { branches : string list
    ; remote_branches : string list
    ; tags : string list
    }
  [@@deriving sexp, jsonaf, equal]
end

let refs_args ?dir () =
  `Object (Option.to_list (Option.map dir ~f:(fun d -> "dir", `String d)))
;;

let list_refs ~env ~cancel ~cwd ~arguments =
  let git ~dir args = git ~env ~cancel ~dir args in
  try
    let root = locate ~git ~cwd arguments in
    let branches, remote_branches, tags = refs ~git ~dir:root in
    Tool_result.ok
      (Json.to_string
         ([%jsonaf_of: Refs.t] { branches; remote_branches; tags }))
  with
  | Failed message -> Tool_result.error message
;;

let refs_of_tool_result (result : Tool_result.t) =
  if result.is_error
  then Or_error.error_string result.text
  else
    Or_error.try_with (fun () ->
      [%of_jsonaf: Refs.t] (Json.parse result.text |> Or_error.ok_exn))
;;

let shown_of_tool_result (result : Tool_result.t) =
  if result.is_error
  then Or_error.error_string result.text
  else
    Or_error.try_with (fun () ->
      [%of_jsonaf: Shown.t] (Json.parse result.text |> Or_error.ok_exn))
;;
