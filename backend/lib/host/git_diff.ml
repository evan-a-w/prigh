open! Core
open! Import

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

(* git's id of the empty tree, to diff against before the first commit. *)
let empty_tree = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

let args ?path ?base ?dir ?(default_base = false) () =
  `Object
    (List.filter_opt
       [ Option.map path ~f:(fun p -> "path", `String p)
       ; Option.map base ~f:(fun b -> "base", `String b)
       ; Option.map dir ~f:(fun d -> "dir", `String d)
       ; Option.some_if default_base ("default_base", `True)
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
  let bool_arg name =
    match Json.member name arguments with
    | Some `True -> true
    | _ -> false
  in
  let verify ~dir rev =
    Core.Result.is_ok
      (git ~dir [ "rev-parse"; "--verify"; "--quiet"; rev ^ "^{commit}" ])
  in
  let config ~dir key =
    match git ~dir [ "config"; "--get"; key ] with
    | Ok v when not (String.is_empty (String.strip v)) -> Some (String.strip v)
    | Ok _ | Error _ -> None
  in
  let default_base ~dir ~branch =
    let recorded =
      Option.bind branch ~f:(fun b -> config ~dir (Worktree.base_key b))
    in
    let remotes =
      Option.bind branch ~f:(fun b ->
        config ~dir (sprintf "branch.%s.remote" b))
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
        (not (Option.exists branch ~f:(String.equal base))) && verify ~dir base)
  in
  try
    let dir =
      match string_arg "dir" with
      | Some dir -> Tool.resolve ~cwd dir
      | None -> cwd
    in
    let root =
      match git ~dir [ "rev-parse"; "--show-toplevel" ] with
      | Ok root -> String.strip root
      | Error _ when Option.is_some (string_arg "dir") ->
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
                "not a git repository: %s (git init starts one, or /cd into \
                 one)"
                cwd))
    in
    let branch =
      match git ~dir:root [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ] with
      | Ok b when not (String.is_empty (String.strip b)) ->
        Some (String.strip b)
      | Ok _ | Error _ -> None
    in
    let main =
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
    let merge_base base =
      match git ~dir:root [ "merge-base"; "HEAD"; base ] with
      | Ok sha -> String.strip sha
      | Error _ -> base
    in
    let head () =
      match git ~dir:root [ "rev-parse"; "--verify"; "--quiet"; "HEAD" ] with
      | Ok _ -> "HEAD"
      | Error _ -> empty_tree
    in
    let against, base =
      match string_arg "base" with
      | Some "HEAD" -> head (), "HEAD"
      | Some base ->
        if not (verify ~dir:root base)
        then
          raise
            (Failed
               (sprintf
                  "%s %S: name a branch, tag or commit (e.g. main)"
                  unknown_revision
                  base));
        merge_base base, base
      | None when bool_arg "default_base" && not main ->
        (match default_base ~dir:root ~branch with
         | Some base -> merge_base base, base
         | None -> head (), "HEAD")
      | None -> head (), "HEAD"
    in
    let branches =
      match
        git
          ~dir:root
          [ "for-each-ref"
          ; "--format=%(refname)"
          ; "refs/heads"
          ; "refs/remotes"
          ]
      with
      | Ok out ->
        List.filter_map (String.split_lines out) ~f:(fun ref_ ->
          if String.is_suffix ref_ ~suffix:"/HEAD"
          then None
          else
            Option.first_some
              (String.chop_prefix ref_ ~prefix:"refs/heads/")
              (String.chop_prefix ref_ ~prefix:"refs/remotes/"))
        |> fun l -> List.take l max_branches
      | Error _ -> []
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
         ([%jsonaf_of: Result.t]
            { root; against; diff; truncated; branch; base; main; branches }))
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
