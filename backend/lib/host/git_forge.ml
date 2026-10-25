open! Core
open! Import

module Remote = struct
  type t =
    { name : string
    ; urls : string list
    }
  [@@deriving sexp, jsonaf]
end

module Info = struct
  type t =
    { root : string
    ; branch : string option
    ; upstream_remote : string option
    ; head : string option
    ; remotes : Remote.t list
    }
  [@@deriving sexp, jsonaf]
end

module Log = struct
  type t =
    { commits : (string * string) list
    ; pushed : bool
    }
  [@@deriving sexp, jsonaf]
end

module Checkout = struct
  type t =
    { path : string
    ; branch : string
    ; created : bool
    }
  [@@deriving sexp, jsonaf]
end

module Request = struct
  type t =
    | Info
    | Log of
        { remote : string
        ; base : string
        ; branch : string
        }
    | Push of
        { remote : string
        ; branch : string
        }
    | Checkout of
        { remote : string
        ; number : int
        ; worktrees_dir : string
        }
  [@@deriving sexp_of]

  let to_json t =
    let s v = `String v in
    `Object
      (match t with
       | Info -> [ "action", s "info" ]
       | Log { remote; base; branch } ->
         [ "action", s "log"
         ; "remote", s remote
         ; "base", s base
         ; "branch", s branch
         ]
       | Push { remote; branch } ->
         [ "action", s "push"; "remote", s remote; "branch", s branch ]
       | Checkout { remote; number; worktrees_dir } ->
         [ "action", s "checkout"
         ; "remote", s remote
         ; "number", `Number (Int.to_string number)
         ; "worktrees_dir", s worktrees_dir
         ])
  ;;
end

let op = "$git_forge"

exception Failed of string

let git ~env ~cancel ?(timeout = Time_ns.Span.of_sec 30.) ~dir args =
  match
    Process.run_collect
      ~env
      ~cancel
      ~timeout
      ~extra_env:
        [ "GIT_PAGER", "cat"
        ; "LC_ALL", "C"
        ; "GIT_TERMINAL_PROMPT", "0"
        ; "GIT_ASKPASS", "true"
        ; "SSH_ASKPASS", "true"
        ; "GIT_SSH_COMMAND", "ssh -o BatchMode=yes"
        ]
      ~prog:"git"
      ~args:("-C" :: dir :: args)
      ()
  with
  | { exit = Exited 0; stdout; _ } -> Ok stdout
  | { exit = Exited _; stderr; _ } -> Error (String.strip stderr)
  | { exit = Cancelled; _ } -> raise (Failed "[cancelled]")
  | { exit = Timed_out; _ } ->
    raise (Failed (sprintf "git %s timed out" (String.concat ~sep:" " args)))
  | { exit = Signaled _; stderr; _ } ->
    raise (Failed ("git failed: " ^ String.strip stderr))
  | exception Eio.Io _ ->
    raise
      (Failed
         "git is not installed on the tool host: install it there (e.g. nix \
          profile install nixpkgs#git)")
;;

module Runner = struct
  type t =
    { git :
        ?timeout:Time_ns.Span.t
        -> dir:string
        -> string list
        -> (string, string) Result.t
    }
end

let ok_or_fail ~what = function
  | Ok out -> out
  | Error e -> raise (Failed (sprintf "%s: %s" what e))
;;

let first_line s = String.strip s |> String.split_lines |> List.hd

let info ~(runner : Runner.t) ~cwd =
  let git = runner.git in
  let root =
    match git ~dir:cwd [ "rev-parse"; "--show-toplevel" ] with
    | Ok root -> String.strip root
    | Error _ ->
      raise (Failed (sprintf "not a git repository: %s (/cd into one)" cwd))
  in
  let branch =
    match git ~dir:root [ "symbolic-ref"; "--quiet"; "--short"; "HEAD" ] with
    | Ok b -> first_line b
    | Error _ -> None
  in
  let head =
    match git ~dir:root [ "rev-parse"; "--verify"; "--quiet"; "HEAD" ] with
    | Ok sha -> first_line sha
    | Error _ -> None
  in
  let upstream_remote =
    Option.bind branch ~f:(fun b ->
      match
        git ~dir:root [ "config"; "--get"; sprintf "branch.%s.remote" b ]
      with
      | Ok r -> first_line r
      | Error _ -> None)
  in
  let remotes =
    match git ~dir:root [ "remote" ] with
    | Error _ -> []
    | Ok names ->
      List.filter_map (String.split_lines names) ~f:(fun name ->
        let name = String.strip name in
        let url args =
          Option.bind (Result.ok (git ~dir:root args)) ~f:first_line
        in
        match
          List.filter_opt
            [ url [ "config"; "--get"; sprintf "remote.%s.url" name ]
            ; url [ "remote"; "get-url"; name ]
            ]
          |> List.remove_consecutive_duplicates ~equal:String.equal
        with
        | [] -> None
        | urls ->
          if String.is_empty name then None else Some { Remote.name; urls })
  in
  { Info.root; branch; upstream_remote; head; remotes }
;;

let verify ~(runner : Runner.t) ~dir rev =
  let git = runner.git in
  Result.is_ok
    (git ~dir [ "rev-parse"; "--verify"; "--quiet"; rev ^ "^{commit}" ])
;;

let log ~(runner : Runner.t) ~cwd ~remote ~base ~branch =
  let git = runner.git in
  let root = (info ~runner ~cwd).root in
  let base_ref =
    List.find [ remote ^ "/" ^ base; base ] ~f:(verify ~runner ~dir:root)
  in
  let commits =
    match base_ref with
    | None -> []
    | Some base_ref ->
      git
        ~dir:root
        [ "log"; "--reverse"; "--format=%s%x00%b%x1e"; base_ref ^ "..HEAD" ]
      |> ok_or_fail ~what:"git log"
      |> String.split ~on:'\x1e'
      |> List.filter_map ~f:(fun entry ->
        match String.lsplit2 (String.lstrip entry) ~on:'\000' with
        | Some (subject, body) -> Some (subject, String.strip body)
        | None -> None)
  in
  let tracking = remote ^ "/" ^ branch in
  let pushed =
    verify ~runner ~dir:root tracking
    &&
    match git ~dir:root [ "rev-list"; "--count"; tracking ^ "..HEAD" ] with
    | Ok n -> String.equal (String.strip n) "0"
    | Error _ -> false
  in
  { Log.commits; pushed }
;;

let push ~(runner : Runner.t) ~cwd ~remote ~branch =
  let git = runner.git in
  let root = (info ~runner ~cwd).root in
  match
    git
      ~timeout:(Time_ns.Span.of_sec 120.)
      ~dir:root
      [ "push"; "--set-upstream"; remote; "HEAD:refs/heads/" ^ branch ]
  with
  | Ok _ -> `Object [ "pushed", `True ]
  | Error e ->
    raise
      (Failed
         (sprintf
            "git push %s %s failed on the tool host: %s (push it yourself with \
             git push -u %s %s, then create the pull request without pushing)"
            remote
            branch
            e
            remote
            branch))
;;

let checkout ~env ~cancel ~(runner : Runner.t) ~cwd ~remote ~number ~dir =
  let git = runner.git in
  let root = (info ~runner ~cwd).root in
  let or_fail = function
    | Ok v -> v
    | Error e -> raise (Failed (Error.to_string_hum e))
  in
  let branch = sprintf "pr-%d" number in
  let _ : string =
    git
      ~timeout:(Time_ns.Span.of_sec 120.)
      ~dir:root
      [ "fetch"; remote; sprintf "refs/pull/%d/head" number ]
    |> ok_or_fail
         ~what:
           (sprintf
              "git fetch %s refs/pull/%d/head (is #%d a pull request of %s's \
               repository? /pr lists them)"
              remote
              number
              number
              remote)
  in
  let fetched =
    git ~dir:root [ "rev-parse"; "FETCH_HEAD" ]
    |> ok_or_fail ~what:"git rev-parse FETCH_HEAD"
    |> String.strip
  in
  let fast_forward path =
    let _ : string =
      git ~dir:path [ "merge"; "--ff-only"; fetched ]
      |> ok_or_fail
           ~what:
             (sprintf
                "%s cannot fast-forward to the pull request (commit or drop \
                 its changes there, or rename the branch: git branch -m %s \
                 OTHER)"
                path
                branch)
    in
    ()
  in
  let listing = Worktree.list ~env ~cancel ~cwd:root ~dir |> or_fail in
  match
    List.find listing.worktrees ~f:(fun (w : Worktree.t) ->
      Option.equal String.equal w.branch (Some branch))
  with
  | Some existing ->
    fast_forward existing.path;
    { Checkout.path = existing.path; branch; created = false }
  | None ->
    let existed = verify ~runner ~dir:root ("refs/heads/" ^ branch) in
    let created =
      Worktree.create
        ~env
        ~cancel
        ~cwd:root
        ~dir
        ~branch
        ~base:(if existed then None else Some fetched)
      |> or_fail
    in
    if existed then fast_forward created.worktree.path;
    { path = created.worktree.path; branch; created = true }
;;

let run ~env ~cancel ~cwd ~arguments =
  let runner =
    { Runner.git =
        (fun ?timeout ~dir args -> git ~env ~cancel ?timeout ~dir args)
    }
  in
  let str name =
    match Json.member name arguments with
    | Some (`String s) when not (String.is_empty s) -> s
    | _ -> raise (Failed (sprintf "%s: missing %S" op name))
  in
  try
    let json =
      match Json.member "action" arguments with
      | Some (`String "info") -> [%jsonaf_of: Info.t] (info ~runner ~cwd)
      | Some (`String "log") ->
        [%jsonaf_of: Log.t]
          (log
             ~runner
             ~cwd
             ~remote:(str "remote")
             ~base:(str "base")
             ~branch:(str "branch"))
      | Some (`String "push") ->
        push ~runner ~cwd ~remote:(str "remote") ~branch:(str "branch")
      | Some (`String "checkout") ->
        let number =
          match Option.bind (Json.member "number" arguments) ~f:Json.int with
          | Some n -> n
          | None -> raise (Failed (sprintf "%s: missing \"number\"" op))
        in
        [%jsonaf_of: Checkout.t]
          (checkout
             ~env
             ~cancel
             ~runner
             ~cwd
             ~remote:(str "remote")
             ~number
             ~dir:(str "worktrees_dir"))
      | _ -> raise (Failed (sprintf "%s: unknown action" op))
    in
    Tool_result.ok (Json.to_string json)
  with
  | Failed message -> Tool_result.error message
;;

let result (r : Tool_result.t) ~of_json =
  if r.is_error
  then Or_error.error_string r.text
  else
    Or_error.try_with (fun () -> of_json (Json.parse r.text |> Or_error.ok_exn))
;;
