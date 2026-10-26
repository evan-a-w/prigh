open! Core
open! Import

module File = struct
  type t =
    { path : string
    ; orig : string option [@sexp.option] [@jsonaf.option]
    ; index : string option [@sexp.option] [@jsonaf.option]
    ; worktree : string option [@sexp.option] [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, equal]
end

module Status = struct
  type t =
    { root : string
    ; branch : string option [@sexp.option] [@jsonaf.option]
    ; head : string option [@sexp.option] [@jsonaf.option]
    ; upstream : string option [@sexp.option] [@jsonaf.option]
    ; ahead : int
    ; behind : int
    ; files : File.t list
    }
  [@@deriving sexp, jsonaf, equal]

  let change c =
    match c with
    | '.' -> None
    | c -> Some (String.of_char c)
  ;;

  (* [git status --porcelain=v2 --branch -z]. *)
  let parse ~root out =
    let t =
      { root
      ; branch = None
      ; head = None
      ; upstream = None
      ; ahead = 0
      ; behind = 0
      ; files = []
      }
    in
    let field line n =
      List.nth (String.split line ~on:' ') n |> Option.value ~default:""
    in
    (* The fields before the path, which may hold spaces. *)
    let path_after line n =
      let rec drop s n =
        if n = 0
        then s
        else (
          match String.lsplit2 s ~on:' ' with
          | Some (_, rest) -> drop rest (n - 1)
          | None -> "")
      in
      drop line n
    in
    let xy line =
      let xy = field line 1 in
      if String.length xy = 2 then change xy.[0], change xy.[1] else None, None
    in
    let rec go t = function
      | [] -> { t with files = List.rev t.files }
      | "" :: rest -> go t rest
      | line :: rest when String.is_prefix line ~prefix:"# " ->
        let t =
          match String.lsplit2 (String.drop_prefix line 2) ~on:' ' with
          | Some ("branch.oid", "(initial)") -> t
          | Some ("branch.oid", oid) -> { t with head = Some oid }
          | Some ("branch.head", "(detached)") -> t
          | Some ("branch.head", b) -> { t with branch = Some b }
          | Some ("branch.upstream", u) -> { t with upstream = Some u }
          | Some ("branch.ab", ab) ->
            (match String.split ab ~on:' ' with
             | [ a; b ] ->
               let n s =
                 Option.value
                   (Int.of_string_opt (String.drop_prefix s 1))
                   ~default:0
               in
               { t with ahead = n a; behind = n b }
             | _ -> t)
          | _ -> t
        in
        go t rest
      | line :: rest when String.is_prefix line ~prefix:"1 " ->
        let index, worktree = xy line in
        go
          { t with
            files =
              { File.path = path_after line 8; orig = None; index; worktree }
              :: t.files
          }
          rest
      | line :: rest when String.is_prefix line ~prefix:"2 " ->
        let index, worktree = xy line in
        let orig, rest =
          match rest with
          | orig :: rest -> Some orig, rest
          | [] -> None, []
        in
        go
          { t with
            files =
              { path = path_after line 9; orig; index; worktree } :: t.files
          }
          rest
      | line :: rest when String.is_prefix line ~prefix:"u " ->
        go
          { t with
            files =
              { path = path_after line 10
              ; orig = None
              ; index = Some "U"
              ; worktree = Some "U"
              }
              :: t.files
          }
          rest
      | line :: rest when String.is_prefix line ~prefix:"? " ->
        go
          { t with
            files =
              { path = String.drop_prefix line 2
              ; orig = None
              ; index = None
              ; worktree = Some "?"
              }
              :: t.files
          }
          rest
      | _ :: rest -> go t rest
    in
    go t (String.split out ~on:'\000')
  ;;
end

module Commit = struct
  type t =
    { sha : string
    ; subject : string
    ; status : Status.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Push = struct
  type t =
    { remote : string
    ; branch : string
    ; status : Status.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Commit_context = struct
  type t =
    { branch : string option [@sexp.option] [@jsonaf.option]
    ; diff : string
    ; recent : string list
    ; conventions : (string * string) list
    }
  [@@deriving sexp, jsonaf, equal]
end

module Pr_context = struct
  type t =
    { branch : string option [@sexp.option] [@jsonaf.option]
    ; base : string
    ; commits : (string * string) list
    ; diff : string
    ; conventions : (string * string) list
    ; template : (string * string) option [@sexp.option] [@jsonaf.option]
    }
  [@@deriving sexp, jsonaf, equal]
end

module Request = struct
  type t =
    | Status
    | Stage of string list
    | Unstage of string list
    | Commit of
        { message : string
        ; amend : bool
        ; paths : string list
        }
    | Push
    | Commit_context
    | Pr_context of { base : string option }
  [@@deriving sexp_of, equal]

  let to_json ?dir t =
    let s v = `String v in
    let paths ps = `Array (List.map ps ~f:s) in
    let fields =
      match t with
      | Status -> [ "action", s "status" ]
      | Stage ps -> [ "action", s "stage"; "paths", paths ps ]
      | Unstage ps -> [ "action", s "unstage"; "paths", paths ps ]
      | Commit { message; amend; paths = ps } ->
        [ "action", s "commit"
        ; "message", s message
        ; ("amend", if amend then `True else `False)
        ; "paths", paths ps
        ]
      | Push -> [ "action", s "push" ]
      | Commit_context -> [ "action", s "commit_context" ]
      | Pr_context { base } ->
        ("action", s "pr_context")
        :: Option.value_map base ~default:[] ~f:(fun b -> [ "base", s b ])
    in
    `Object
      (fields @ Option.value_map dir ~default:[] ~f:(fun d -> [ "dir", s d ]))
  ;;
end

let op = "$git_actions"
let max_diff_bytes = 40_000

exception Failed = Git_forge.Failed

let fail fmt = ksprintf (fun s -> raise (Failed s)) fmt

module Repo = struct
  type t =
    { runner : Git_forge.Runner.t
    ; root : string
    }

  let git ?timeout t args =
    t.runner.git
      ?timeout
      ~dir:t.root
      ("-c" :: "core.quotepath=off" :: "--literal-pathspecs" :: args)
  ;;

  let git_exn ?timeout t args =
    match git ?timeout t args with
    | Ok out -> out
    | Error e -> fail "git %s: %s" (List.hd_exn args) e
  ;;

  let open_ ~runner ~cwd ~dir =
    let dir = Option.value_map dir ~default:cwd ~f:(Tool.resolve ~cwd) in
    match
      runner.Git_forge.Runner.git ~dir [ "rev-parse"; "--show-toplevel" ]
    with
    | Ok root -> { runner; root = String.strip root }
    | Error _ ->
      fail
        "not a git repository: %s (name a worktree of the repository: \
         /worktree lists them)"
        dir
  ;;

  let status t =
    git_exn
      t
      [ "status"; "--porcelain=v2"; "--branch"; "-z"; "--untracked-files=all" ]
    |> Status.parse ~root:t.root
  ;;

  let has_head t =
    Result.is_ok (git t [ "rev-parse"; "--verify"; "--quiet"; "HEAD" ])
  ;;

  let staged_anything t =
    match git t [ "diff"; "--cached"; "--quiet" ] with
    | Ok _ -> false
    | Error _ -> true
  ;;

  let read_file t path =
    let file = Filename.concat t.root path in
    match Sys_unix.is_file file with
    | `Yes -> Option.try_with (fun () -> In_channel.read_all file)
    | `No | `Unknown -> None
  ;;
end

(* A path relative to the worktree's top directory that stays inside it. *)
let check_path path =
  let parts = String.split path ~on:'/' in
  let rec depth d = function
    | [] -> d >= 0
    | ".." :: rest -> d > 0 && depth (d - 1) rest
    | ("" | ".") :: rest -> depth d rest
    | _ :: rest -> depth (d + 1) rest
  in
  if String.is_empty (String.strip path)
  then fail "an empty path: name files relative to the worktree's top directory"
  else if Filename.is_absolute path
  then
    fail
      "%s is absolute: name files relative to the worktree's top directory"
      path
  else if String.mem path '\000' || not (depth 0 parts)
  then fail "%s is outside the worktree: name files inside it" path
  else path
;;

let check_paths = function
  | [] -> fail "no files given: tick the files to stage"
  | paths -> List.map paths ~f:check_path
;;

let stage repo paths =
  let paths = check_paths paths in
  ignore (Repo.git_exn repo ("add" :: "--all" :: "--" :: paths) : string)
;;

let unstage repo paths =
  let paths = check_paths paths in
  let status = Repo.status repo in
  (* A rename is staged as its two paths: unstaging it frees both. *)
  let paths =
    List.concat_map paths ~f:(fun p ->
      p
      :: List.filter_map status.files ~f:(fun (f : File.t) ->
        if String.equal f.path p then f.orig else None))
    |> List.dedup_and_sort ~compare:String.compare
  in
  let args =
    if Repo.has_head repo
    then "reset" :: "--quiet" :: "HEAD" :: "--" :: paths
    else
      "rm"
      :: "--cached"
      :: "--quiet"
      :: "-r"
      :: "--ignore-unmatch"
      :: "--"
      :: paths
  in
  ignore (Repo.git_exn repo args : string)
;;

let identity_hint e =
  if
    String.is_substring e ~substring:"tell me who you are"
    || String.is_substring e ~substring:"empty ident"
  then
    " (set who commits on the tool host: git config --global user.name NAME \
     and git config --global user.email EMAIL)"
  else ""
;;

let commit repo ~message ~amend ~paths =
  let status = Repo.status repo in
  if Option.is_none status.branch
  then
    fail
      "HEAD is detached in %s: commit on a branch (git switch -c NAME there \
       makes one)"
      repo.root;
  if amend && Option.is_none status.head
  then fail "there is no commit to amend yet: commit without Amend first";
  if not (List.is_empty paths) then stage repo paths;
  let message = String.strip message in
  if (not amend) && not (Repo.staged_anything repo)
  then
    fail
      "nothing is staged to commit: tick the files to commit (or stage them \
       with git add)";
  if (not amend) && String.is_empty message
  then fail "the commit message is empty: write one (or Generate one)";
  let args =
    [ "commit"; "--quiet" ]
    @ (if amend then [ "--amend" ] else [])
    @ if String.is_empty message then [ "--no-edit" ] else [ "-m"; message ]
  in
  (match Repo.git ~timeout:(Time_ns.Span.of_sec 120.) repo args with
   | Ok _ -> ()
   | Error e -> fail "git commit failed: %s%s" e (identity_hint e));
  let sha = Repo.git_exn repo [ "rev-parse"; "HEAD" ] |> String.strip in
  let subject =
    Repo.git_exn repo [ "log"; "-1"; "--format=%s" ] |> String.strip
  in
  { Commit.sha; subject; status = Repo.status repo }
;;

let push_hint e =
  let has s = String.is_substring e ~substring:s in
  if has "[rejected]" || has "non-fast-forward" || has "fetch first"
  then
    " (the remote has commits this branch does not: pull first, e.g. git pull \
     --rebase, then push again)"
  else if
    has "Authentication failed"
    || has "could not read Username"
    || has "Permission denied"
    || has "terminal prompts disabled"
  then
    " (the tool host has no git credentials for this remote: set up an SSH key \
     or a credential helper there; prigh never asks for passwords)"
  else ""
;;

let push repo =
  let info = Git_forge.info ~runner:repo.Repo.runner ~cwd:repo.root in
  let branch =
    match info.branch with
    | Some b -> b
    | None ->
      fail
        "HEAD is detached in %s: push a branch (git switch -c NAME there makes \
         one)"
        repo.root
  in
  if Option.is_none info.head then fail "there is nothing to push: commit first";
  let remote =
    match info.upstream_remote, info.remotes with
    | Some r, _ when not (String.equal r ".") -> r
    | _, [] ->
      fail
        "%s has no remote to push to: add one (git remote add origin URL)"
        repo.root
    | _, remotes ->
      (match
         List.find remotes ~f:(fun (r : Git_forge.Remote.t) ->
           String.equal r.name "origin")
       with
       | Some r -> r.name
       | None -> (List.hd_exn remotes).name)
  in
  (* The branch it tracks there, if it has one. *)
  let target =
    match
      Repo.git repo [ "config"; "--get"; sprintf "branch.%s.merge" branch ]
    with
    | Ok merge ->
      (match String.chop_prefix (String.strip merge) ~prefix:"refs/heads/" with
       | Some b when not (String.is_empty b) -> b
       | Some _ | None -> branch)
    | Error _ -> branch
  in
  match
    Git_forge.push_branch
      ~runner:repo.runner
      ~root:repo.root
      ~remote
      ~branch:target
  with
  | Ok () -> { Push.remote; branch = target; status = Repo.status repo }
  | Error e ->
    fail "git push %s %s was refused: %s%s" remote target e (push_hint e)
;;

(* ---- what a model drafts from -------------------------------------- *)

let heading line =
  let hashes = String.length (String.take_while line ~f:(Char.equal '#')) in
  if
    hashes > 0
    && hashes <= 6
    && String.length line > hashes
    && Char.equal line.[hashes] ' '
  then Some (hashes, String.strip (String.drop_prefix line hashes))
  else None
;;

let mentions ~topic text =
  let text = " " ^ String.lowercase text ^ " " in
  let words =
    String.map text ~f:(fun c -> if Char.is_alphanum c then c else ' ')
  in
  List.exists topic ~f:(fun t ->
    String.is_substring words ~substring:(" " ^ String.lowercase t ^ " "))
;;

let sections ~topic text =
  let rec go acc in_section = function
    | [] -> List.rev acc
    | line :: rest ->
      (match heading line, in_section with
       | Some (level, title), _ when mentions ~topic title ->
         go (line :: acc) (Some level) rest
       | Some (level, _), Some within when level <= within -> go acc None rest
       | _, Some _ -> go (line :: acc) in_section rest
       | _, None -> go acc None rest)
  in
  match go [] None (String.split_lines text) with
  | [] -> None
  | lines -> Some (String.strip (String.concat ~sep:"\n" lines))
;;

let guidance_files =
  [ "AGENTS.md"
  ; "CLAUDE.md"
  ; "CONTRIBUTING.md"
  ; ".github/CONTRIBUTING.md"
  ; "docs/CONTRIBUTING.md"
  ]
;;

let max_guidance = 6_000

let conventions repo ~topic =
  List.filter_map guidance_files ~f:(fun path ->
    Option.bind (Repo.read_file repo path) ~f:(sections ~topic)
    |> Option.map ~f:(fun text -> path, String.prefix text max_guidance))
;;

let templates =
  [ ".github/pull_request_template.md"
  ; ".github/PULL_REQUEST_TEMPLATE.md"
  ; "pull_request_template.md"
  ; "PULL_REQUEST_TEMPLATE.md"
  ; "docs/pull_request_template.md"
  ; ".forgejo/pull_request_template.md"
  ; ".gitea/pull_request_template.md"
  ]
;;

let diff_options = [ "diff"; "--no-color"; "--no-ext-diff"; "--no-renames" ]

let commit_context repo =
  let status = Repo.status repo in
  if not (Repo.staged_anything repo)
  then
    fail "nothing is staged to describe: tick (stage) the files to commit first";
  let diff =
    Repo.git_exn repo (diff_options @ [ "--cached" ])
    |> Diff_digest.digest ~max_bytes:max_diff_bytes
  in
  let recent =
    match Repo.git repo [ "log"; "-15"; "--format=%s" ] with
    | Ok out ->
      String.split_lines out |> List.filter ~f:(Fn.non String.is_empty)
    | Error _ -> []
  in
  { Commit_context.branch = status.branch
  ; diff
  ; recent
  ; conventions = conventions repo ~topic:[ "commit"; "commits" ]
  }
;;

let verify repo rev =
  Result.is_ok
    (Repo.git repo [ "rev-parse"; "--verify"; "--quiet"; rev ^ "^{commit}" ])
;;

(* The base a pull request merges into when none is given: the branch the
   worktree was made from, the remote's default, main or master. *)
let default_base repo ~branch =
  let config key =
    match Repo.git repo [ "config"; "--get"; key ] with
    | Ok v when not (String.is_empty (String.strip v)) -> Some (String.strip v)
    | Ok _ | Error _ -> None
  in
  let recorded =
    Option.bind branch ~f:(fun b -> config (Worktree.base_key b))
  in
  let remote_head =
    match
      Repo.git
        repo
        [ "symbolic-ref"; "--quiet"; "--short"; "refs/remotes/origin/HEAD" ]
    with
    | Ok h when not (String.is_empty (String.strip h)) -> Some (String.strip h)
    | Ok _ | Error _ -> None
  in
  List.find
    (Option.to_list recorded
     @ Option.to_list remote_head
     @ [ "origin/main"; "origin/master"; "main"; "master" ])
    ~f:(fun b ->
      (not (Option.exists branch ~f:(String.equal b))) && verify repo b)
;;

let pr_context repo ~base =
  let status = Repo.status repo in
  let base_ref =
    match base with
    | Some base ->
      (match List.find [ "origin/" ^ base; base ] ~f:(verify repo) with
       | Some r -> r
       | None ->
         fail
           "unknown base %S: name a branch of the repository (e.g. main)"
           base)
    | None ->
      (match default_base repo ~branch:status.branch with
       | Some b -> b
       | None ->
         fail
           "no base branch found: name the branch the pull request merges into")
  in
  let commits =
    Repo.git_exn
      repo
      [ "log"; "--reverse"; "--format=%s%x00%b%x1e"; base_ref ^ "..HEAD" ]
    |> String.split ~on:'\x1e'
    |> List.filter_map ~f:(fun entry ->
      match String.lsplit2 (String.lstrip entry) ~on:'\000' with
      | Some (subject, body) -> Some (subject, String.strip body)
      | None -> None)
  in
  if List.is_empty commits
  then
    fail
      "%s has no commits that %s lacks: commit the changes first"
      (Option.value status.branch ~default:"HEAD")
      base_ref;
  let diff =
    Repo.git_exn repo (diff_options @ [ base_ref ^ "...HEAD" ])
    |> Diff_digest.digest ~max_bytes:max_diff_bytes
  in
  let template =
    List.find_map templates ~f:(fun path ->
      Option.map (Repo.read_file repo path) ~f:(fun text ->
        path, String.prefix text max_guidance))
  in
  { Pr_context.branch = status.branch
  ; base = Option.value base ~default:base_ref
  ; commits
  ; diff
  ; conventions =
      conventions repo ~topic:[ "pull request"; "pull requests"; "pr"; "prs" ]
  ; template
  }
;;

let run ~env ~cancel ~cwd ~arguments =
  let runner = Git_forge.runner ~env ~cancel in
  let str name =
    match Json.member name arguments with
    | Some (`String s) when not (String.is_empty (String.strip s)) -> Some s
    | _ -> None
  in
  let paths () =
    match Json.member "paths" arguments with
    | Some (`Array items) ->
      List.filter_map items ~f:(function
        | `String s -> Some s
        | _ -> None)
    | _ -> []
  in
  try
    let repo = Repo.open_ ~runner ~cwd ~dir:(str "dir") in
    let json =
      match str "action" with
      | Some "status" -> [%jsonaf_of: Status.t] (Repo.status repo)
      | Some "stage" ->
        stage repo (paths ());
        [%jsonaf_of: Status.t] (Repo.status repo)
      | Some "unstage" ->
        unstage repo (paths ());
        [%jsonaf_of: Status.t] (Repo.status repo)
      | Some "commit" ->
        [%jsonaf_of: Commit.t]
          (commit
             repo
             ~message:(Option.value (str "message") ~default:"")
             ~amend:
               (match Json.member "amend" arguments with
                | Some `True -> true
                | _ -> false)
             ~paths:(paths ()))
      | Some "push" -> [%jsonaf_of: Push.t] (push repo)
      | Some "commit_context" ->
        [%jsonaf_of: Commit_context.t] (commit_context repo)
      | Some "pr_context" ->
        [%jsonaf_of: Pr_context.t] (pr_context repo ~base:(str "base"))
      | _ -> fail "%s: unknown action" op
    in
    Tool_result.ok (Json.to_string json)
  with
  | Failed message -> Tool_result.error message
;;
