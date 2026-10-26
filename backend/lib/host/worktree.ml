open! Core
open! Import

type t =
  { path : string
  ; branch : string option
  ; head : string option
  ; main : bool
  ; managed : bool
  ; locked : bool
  ; prunable : bool
  ; changes : int option
  ; upstream : string option
  ; ahead : int
  ; behind : int
  }
[@@deriving sexp, jsonaf, equal]

type worktree = t [@@deriving sexp, jsonaf, equal]

let contains t ~dir =
  String.equal dir t.path || String.is_prefix dir ~prefix:(t.path ^ "/")
;;

module Listing = struct
  type t =
    { root : string
    ; project_dir : string
    ; worktrees : worktree list
    }
  [@@deriving sexp, jsonaf, equal]
end

module Created = struct
  type t =
    { worktree : worktree
    ; new_branch : bool
    ; root : string
    ; cwd : string
    }
  [@@deriving sexp, jsonaf, equal]
end

let slug branch =
  let mapped =
    String.map branch ~f:(fun c ->
      if Char.is_alphanum c || Char.equal c '.' || Char.equal c '_'
      then c
      else '-')
  in
  let collapsed =
    String.concat
      ~sep:"-"
      (List.filter (String.split mapped ~on:'-') ~f:(Fn.non String.is_empty))
  in
  match String.lstrip collapsed ~drop:(Char.equal '.') with
  | "" -> "worktree"
  | s -> s
;;

exception Failed of string

let fail fmt = ksprintf (fun s -> raise (Failed s)) fmt

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
    fail "git failed: %s" (String.strip stderr)
  | exception Eio.Io _ ->
    fail
      "git is not installed on the tool host: install it there (e.g. nix \
       profile install nixpkgs#git)"
;;

let git_exn ~env ~cancel ~dir ~what args =
  match git ~env ~cancel ~dir args with
  | Ok out -> out
  | Error e -> fail "%s failed: %s" what e
;;

let realpath path =
  match Filename_unix.realpath path with
  | path -> path
  | exception _ -> path
;;

(* [git worktree list --porcelain]: blocks separated by empty lines. *)
module Porcelain = struct
  type t =
    { path : string
    ; head : string option
    ; branch : string option
    ; bare : bool
    ; locked : bool
    ; prunable : bool
    }

  let parse out =
    String.split_lines out
    |> List.group ~break:(fun _ line -> String.is_empty line)
    |> List.filter_map ~f:(fun lines ->
      let lines = List.filter lines ~f:(Fn.non String.is_empty) in
      let field name =
        List.find_map lines ~f:(fun line ->
          if String.equal line name
          then Some ""
          else String.chop_prefix line ~prefix:(name ^ " "))
      in
      Option.map (field "worktree") ~f:(fun path ->
        { path
        ; head =
            Option.filter (field "HEAD") ~f:(fun h ->
              not (String.for_all h ~f:(Char.equal '0')))
        ; branch =
            Option.map (field "branch") ~f:(fun b ->
              String.chop_prefix_if_exists b ~prefix:"refs/heads/")
        ; bare = Option.is_some (field "bare")
        ; locked = Option.is_some (field "locked")
        ; prunable = Option.is_some (field "prunable")
        }))
  ;;
end

let parse_track track =
  List.fold
    (String.split track ~on:',')
    ~init:(0, 0)
    ~f:(fun (ahead, behind) part ->
      match String.split (String.strip part) ~on:' ' with
      | [ "ahead"; n ] -> Option.value (Int.of_string_opt n) ~default:0, behind
      | [ "behind"; n ] -> ahead, Option.value (Int.of_string_opt n) ~default:0
      | _ -> ahead, behind)
;;

let marker = ".prigh-repo"

(* [<dir>/<name>], [<dir>/<name>-2], ...: the first one recording [common]
   as its repository, or else the first one free. *)
let project_dir ~dir ~name ~common =
  let candidate i =
    Filename.concat dir (if i = 1 then name else sprintf "%s-%d" name i)
  in
  let owner path =
    match In_channel.read_all (Filename.concat path marker) with
    | s -> `Repo (String.strip s)
    | exception _ ->
      (match Sys_unix.file_exists path with
       | `Yes -> `Other
       | `No | `Unknown -> `Free)
  in
  let rec go i first_free =
    if i > 100
    then Option.value first_free ~default:(candidate 1)
    else (
      let path = candidate i in
      match owner path with
      | `Repo r when String.equal r common -> path
      | `Free when Option.is_none first_free -> go (i + 1) (Some path)
      | `Repo _ | `Other | `Free -> go (i + 1) first_free)
  in
  go 1 None
;;

let expand dir ~cwd = realpath (Tool.resolve ~cwd dir)

let common_dir ~env ~cancel ~cwd =
  match
    git
      ~env
      ~cancel
      ~dir:cwd
      [ "rev-parse"; "--path-format=absolute"; "--git-common-dir" ]
  with
  | Ok dir -> realpath (String.strip dir)
  | Error _ ->
    fail "not a git repository: %s (git init starts one, or /cd into one)" cwd
;;

let changes ~env ~cancel path =
  match
    git
      ~env
      ~cancel
      ~dir:path
      [ "status"; "--porcelain"; "--untracked-files=normal" ]
  with
  | Ok out -> Some (List.length (String.split_lines out))
  | Error _ -> None
;;

let list_exn ~env ~cancel ~cwd ~dir =
  let common = common_dir ~env ~cancel ~cwd in
  let entries =
    git_exn
      ~env
      ~cancel
      ~dir:cwd
      ~what:"git worktree list"
      [ "worktree"; "list"; "--porcelain" ]
    |> Porcelain.parse
  in
  let tracking =
    match
      git
        ~env
        ~cancel
        ~dir:cwd
        [ "for-each-ref"
        ; "--format=%(refname:short)%00%(upstream:short)%00%(upstream:track,nobracket)"
        ; "refs/heads"
        ]
    with
    | Error _ -> String.Map.empty
    | Ok out ->
      String.split_lines out
      |> List.filter_map ~f:(fun line ->
        match String.split line ~on:'\000' with
        | [ branch; upstream; track ] when not (String.is_empty upstream) ->
          Some (branch, (upstream, parse_track track))
        | _ -> None)
      |> String.Map.of_alist_reduce ~f:Fn.const
  in
  let root =
    match entries with
    | main :: _ -> main.path
    | [] -> Filename.dirname common
  in
  let name =
    String.chop_suffix_if_exists (Filename.basename root) ~suffix:".git"
  in
  let project_dir = project_dir ~dir:(expand dir ~cwd) ~name ~common in
  let worktrees =
    List.mapi entries ~f:(fun i (e : Porcelain.t) ->
      let upstream, (ahead, behind) =
        match Option.bind e.branch ~f:(Map.find tracking) with
        | Some (upstream, counts) -> Some upstream, counts
        | None -> None, (0, 0)
      in
      { path = e.path
      ; branch = e.branch
      ; head = e.head
      ; main = i = 0
      ; managed = String.is_prefix e.path ~prefix:(project_dir ^ "/")
      ; locked = e.locked
      ; prunable = e.prunable
      ; changes =
          (if e.prunable || e.bare then None else changes ~env ~cancel e.path)
      ; upstream
      ; ahead
      ; behind
      })
  in
  { Listing.root; project_dir; worktrees }, common
;;

let protect f =
  try Ok (f ()) with
  | Failed message -> Or_error.error_string message
;;

let list ~env ~cancel ~cwd ~dir =
  protect (fun () -> fst (list_exn ~env ~cancel ~cwd ~dir))
;;

let rev_exists ~env ~cancel ~dir rev =
  Result.is_ok
    (git ~env ~cancel ~dir [ "rev-parse"; "--verify"; "--quiet"; rev ])
;;

let free_path project_dir slug =
  let candidate i =
    Filename.concat project_dir (if i = 1 then slug else sprintf "%s-%d" slug i)
  in
  let rec go i =
    let path = candidate i in
    match Sys_unix.file_exists path with
    | `No -> path
    | `Yes | `Unknown -> go (i + 1)
  in
  go 1
;;

(* [branch], else [branch-2], [branch-3], ...: the first neither a local
   branch nor checked out. *)
let fresh_branch ~env ~cancel ~cwd ~(listing : Listing.t) branch =
  let taken name =
    rev_exists ~env ~cancel ~dir:cwd ("refs/heads/" ^ name)
    || List.exists listing.worktrees ~f:(fun w ->
      Option.exists w.branch ~f:(String.equal name))
  in
  let rec go i =
    let name = if i = 1 then branch else sprintf "%s-%d" branch i in
    if taken name && i < 1000 then go (i + 1) else name
  in
  go 1
;;

(* [cwd]'s subdirectory of its checkout, in [path] when it exists there. *)
let counterpart ~env ~cancel ~cwd path =
  match git ~env ~cancel ~dir:cwd [ "rev-parse"; "--show-prefix" ] with
  | Ok prefix ->
    (match String.strip prefix with
     | "" -> path
     | prefix ->
       let sub =
         Filename.concat path (String.rstrip ~drop:(Char.equal '/') prefix)
       in
       (match Sys_unix.is_directory sub with
        | `Yes -> sub
        | `No | `Unknown -> path))
  | Error _ -> path
;;

let base_key branch = sprintf "branch.%s.prighBase" branch

(* The branch [rev] names (local or remote, short), if it is one. *)
let branch_name ~env ~cancel ~dir rev =
  match git ~env ~cancel ~dir [ "rev-parse"; "--symbolic-full-name"; rev ] with
  | Ok full ->
    let full = String.strip full in
    Option.first_some
      (String.chop_prefix full ~prefix:"refs/heads/")
      (String.chop_prefix full ~prefix:"refs/remotes/")
  | Error _ -> None
;;

let create ?(fresh = false) ~env ~cancel ~cwd ~dir ~branch ~base () =
  protect (fun () ->
    let branch = String.strip branch in
    let base =
      Option.filter (Option.map base ~f:String.strip) ~f:(fun b ->
        not (String.is_empty b))
    in
    (match
       git ~env ~cancel ~dir:cwd [ "check-ref-format"; "--branch"; branch ]
     with
     | Ok _ -> ()
     | Error _ ->
       fail "not a valid branch name: %S (e.g. feat/my-change)" branch);
    let listing, common = list_exn ~env ~cancel ~cwd ~dir in
    let branch =
      if fresh then fresh_branch ~env ~cancel ~cwd ~listing branch else branch
    in
    (match
       List.find listing.worktrees ~f:(fun w ->
         Option.exists w.branch ~f:(String.equal branch))
     with
     | Some w ->
       fail
         "branch %s is already checked out at %s: open that worktree instead"
         branch
         w.path
     | None -> ());
    let exists =
      rev_exists ~env ~cancel ~dir:cwd ("refs/heads/" ^ branch ^ "^{commit}")
    in
    let path = free_path listing.project_dir (slug branch) in
    let add_args, new_branch =
      match exists, base with
      | true, Some _ ->
        fail
          "branch %s already exists: leave out the base to check it out, or \
           pick a new name"
          branch
      | true, None -> [ path; branch ], false
      | false, Some base ->
        if not (rev_exists ~env ~cancel ~dir:cwd (base ^ "^{commit}"))
        then
          fail
            "unknown revision %S: name a branch, tag or commit (e.g. main)"
            base;
        [ "-b"; branch; path; base ], true
      | false, None ->
        let remotes =
          match
            git
              ~env
              ~cancel
              ~dir:cwd
              [ "for-each-ref"
              ; "--format=%(refname:short)"
              ; "refs/remotes/*/" ^ branch
              ]
          with
          | Ok out -> String.split_lines out
          | Error _ -> []
        in
        (match remotes with
         | [ remote ] -> [ "--track"; "-b"; branch; path; remote ], true
         | _ -> [ "-b"; branch; path; "HEAD" ], true)
    in
    Core_unix.mkdir_p listing.project_dir;
    Out_channel.write_all
      (Filename.concat listing.project_dir marker)
      ~data:(common ^ "\n");
    ignore
      (git_exn
         ~env
         ~cancel
         ~dir:cwd
         ~what:"git worktree add"
         ("worktree" :: "add" :: "--quiet" :: add_args)
       : string);
    (match add_args with
     | [ "-b"; _; _; from ] ->
       Option.iter (branch_name ~env ~cancel ~dir:cwd from) ~f:(fun from ->
         ignore
           (git ~env ~cancel ~dir:cwd [ "config"; base_key branch; from ]
            : (string, string) Result.t))
     | _ -> ());
    let listing, _ = list_exn ~env ~cancel ~cwd ~dir in
    let real = realpath path in
    match
      List.find listing.worktrees ~f:(fun w ->
        String.equal w.path real || String.equal w.path path)
    with
    | Some worktree ->
      { Created.worktree
      ; new_branch
      ; root = listing.root
      ; cwd = counterpart ~env ~cancel ~cwd worktree.path
      }
    | None -> fail "git worktree add did not create %s" path)
;;

let remove ?(keep = []) ~env ~cancel ~cwd ~dir ~path ~force () =
  protect (fun () ->
    let listing, _ = list_exn ~env ~cancel ~cwd ~dir in
    let wanted = Tool.resolve ~cwd path in
    let real = realpath wanted in
    let worktree =
      match
        List.find listing.worktrees ~f:(fun w ->
          String.equal w.path real || String.equal w.path wanted)
      with
      | Some w -> w
      | None ->
        fail "not a worktree of %s: %s (/worktree lists them)" listing.root path
    in
    if worktree.main
    then
      fail
        "%s is the repository's main worktree: it cannot be removed"
        worktree.path;
    List.iter keep ~f:(fun kept ->
      let real = realpath (Tool.resolve ~cwd kept) in
      if contains worktree ~dir:real
      then
        fail
          "%s is in use: %s, inside it, is where the agent or the session \
           works (work elsewhere first; the session moves with /cd)"
          worktree.path
          real);
    if worktree.prunable
    then
      ignore
        (git_exn
           ~env
           ~cancel
           ~dir:listing.root
           ~what:"git worktree prune"
           [ "worktree"; "prune" ]
         : string)
    else (
      if not force
      then (
        (match worktree.changes with
         | Some n when n > 0 ->
           fail
             "%s has uncommitted changes (%d %s): commit or stash them first, \
              or force the removal"
             worktree.path
             n
             (if n = 1 then "file" else "files")
         | _ -> ());
        if worktree.locked
        then
          fail
            "%s is locked: git worktree unlock %s, or force the removal"
            worktree.path
            worktree.path);
      let force_args =
        if not force
        then []
        else if worktree.locked
        then [ "--force"; "--force" ]
        else [ "--force" ]
      in
      ignore
        (git_exn
           ~env
           ~cancel
           ~dir:listing.root
           ~what:"git worktree remove"
           ([ "worktree"; "remove" ] @ force_args @ [ worktree.path ])
         : string));
    worktree)
;;

let list_op = "$worktree_list"
let create_op = "$worktree_create"
let remove_op = "$worktree_remove"

let is_op name =
  List.mem [ list_op; create_op; remove_op ] name ~equal:String.equal
;;

let list_args ~dir = `Object [ "dir", `String dir ]

let create_args ?(fresh = false) ~dir ~branch ~base () =
  `Object
    ([ "dir", `String dir; "branch", `String branch ]
     @ Option.value_map base ~default:[] ~f:(fun b -> [ "base", `String b ])
     @ if fresh then [ "fresh", `True ] else [])
;;

let remove_args ?(keep = []) ~dir ~path ~force () =
  `Object
    ([ "dir", `String dir
     ; "path", `String path
     ; ("force", if force then `True else `False)
     ]
     @
     if List.is_empty keep
     then []
     else [ "keep", `Array (List.map keep ~f:(fun d -> `String d)) ])
;;

let run ~env ~cancel ~cwd ~name ~arguments =
  let string name =
    match Json.member name arguments with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let dir = Option.value (string "dir") ~default:"~/.prigh/worktrees" in
  let reply to_json = function
    | Ok v -> Tool_result.ok (Json.to_string (to_json v))
    | Error e -> Tool_result.error (Error.to_string_hum e)
  in
  if String.equal name list_op
  then reply [%jsonaf_of: Listing.t] (list ~env ~cancel ~cwd ~dir)
  else if String.equal name create_op
  then (
    match string "branch" with
    | None -> Tool_result.error "missing branch"
    | Some branch ->
      let fresh =
        match Json.member "fresh" arguments with
        | Some `True -> true
        | _ -> false
      in
      reply
        [%jsonaf_of: Created.t]
        (create ~fresh ~env ~cancel ~cwd ~dir ~branch ~base:(string "base") ()))
  else if String.equal name remove_op
  then (
    match string "path" with
    | None -> Tool_result.error "missing path"
    | Some path ->
      let force =
        match Json.member "force" arguments with
        | Some `True -> true
        | _ -> false
      in
      let keep =
        match Json.member "keep" arguments with
        | Some (`Array dirs) ->
          List.filter_map dirs ~f:(function
            | `String d -> Some d
            | _ -> None)
        | _ -> []
      in
      reply
        [%jsonaf_of: t]
        (remove ~keep ~env ~cancel ~cwd ~dir ~path ~force ()))
  else Tool_result.error (sprintf "unknown worktree operation %S" name)
;;

let of_result of_json (result : Tool_result.t) =
  if result.is_error
  then Or_error.error_string result.text
  else
    Or_error.try_with (fun () ->
      of_json (Json.parse result.text |> Or_error.ok_exn))
;;

let listing_of_result = of_result [%of_jsonaf: Listing.t]
let created_of_result = of_result [%of_jsonaf: Created.t]
let removed_of_result = of_result [%of_jsonaf: t]

let op_tool name =
  { Tool.spec =
      { Tool_spec.name
      ; description = "git worktrees"
      ; parameters = `Object []
      ; parallel_safe = true
      ; destructive = false
      ; on_host = true
      ; freeform = None
      }
  ; run =
      (fun (context : Tool.Context.t) arguments ->
        run
          ~env:context.env
          ~cancel:context.cancel
          ~cwd:context.cwd
          ~name
          ~arguments)
  }
;;

let list_tool = op_tool list_op
let create_tool = op_tool create_op
let remove_tool = op_tool remove_op
