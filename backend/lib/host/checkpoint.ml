open! Core
open! Import

let ref_prefix = "refs/prigh/checkpoints/"
let session_prefix ~session = ref_prefix ^ session ^ "/"

module Snapshot = struct
  type t =
    { commit : string option [@jsonaf.option]
    ; tree : string
    ; root : string
    }
  [@@deriving sexp, jsonaf, equal]
end

module Captured = struct
  type t =
    | Snapshot of Snapshot.t
    | Not_git of string
  [@@deriving sexp, jsonaf, equal]
end

exception Failed of string

let fail fmt = ksprintf (fun s -> raise (Failed s)) fmt

let identity =
  [ "GIT_AUTHOR_NAME", "prigh"
  ; "GIT_AUTHOR_EMAIL", "prigh@localhost"
  ; "GIT_COMMITTER_NAME", "prigh"
  ; "GIT_COMMITTER_EMAIL", "prigh@localhost"
  ]
;;

(* git in [dir]; [index]: a temporary index instead of the repository's. *)
let git ~env ~cancel ?index ?stdin ?(ok_codes = [ 0 ]) ~dir args =
  let extra_env =
    [ "GIT_PAGER", "cat"; "LC_ALL", "C"; "GIT_OPTIONAL_LOCKS", "0" ]
    @ identity
    @ Option.value_map index ~default:[] ~f:(fun i -> [ "GIT_INDEX_FILE", i ])
  in
  match
    Process.run_collect
      ~env
      ~cancel
      ~extra_env
      ?stdin
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
  | exception Eio.Io _ -> fail "%s" (Git_missing.message ())
;;

let git_exn ~env ~cancel ?index ?stdin ~dir ~what args =
  match git ~env ~cancel ?index ?stdin ~dir args with
  | Ok out -> out
  | Error e -> fail "git %s failed: %s" what e
;;

let line s = String.strip s

(* The repository's top, or why there is none. *)
let root ~env ~cancel ~cwd =
  match
    Process.run_collect
      ~env
      ~cancel
      ~extra_env:[ "LC_ALL", "C" ]
      ~prog:"git"
      ~args:[ "-C"; cwd; "rev-parse"; "--show-toplevel" ]
      ()
  with
  | { exit = Exited 0; stdout; _ } -> Ok (line stdout)
  | { exit = Cancelled; _ } -> raise (Failed "[cancelled]")
  | { stderr; _ } ->
    let stderr = String.strip stderr in
    Error
      (if String.is_substring stderr ~substring:"not a git repository"
       then sprintf "%s is not in a git repository" cwd
       else sprintf "git rev-parse in %s failed: %s" cwd stderr)
  | exception Eio.Io _ -> Error (Git_missing.message ())
;;

let root_exn ~env ~cancel ~cwd =
  match root ~env ~cancel ~cwd with
  | Ok r -> r
  | Error e -> fail "%s" e
;;

let with_temp_index f =
  let path = Filename_unix.temp_file "prigh-checkpoint" ".index" in
  (* git wants a missing file or a valid index, not an empty file. *)
  (try Core_unix.unlink path with
   | _ -> ());
  Exn.protect
    ~f:(fun () -> f path)
    ~finally:(fun () ->
      List.iter
        [ path; path ^ ".lock" ]
        ~f:(fun p ->
          try Core_unix.unlink p with
          | _ -> ()))
;;

(* A temporary index with the repository's as its start: its stat cache
   spares [git add] re-reading every file. The copy keeps the original's
   times: git takes an entry modified no earlier than its index file as
   possibly stale and re-reads it, so a copy stamped now would pass a file
   changed in the same tick as unchanged. *)
let seed_index ~env ~cancel ~root ~index =
  match git ~env ~cancel ~dir:root [ "rev-parse"; "--git-path"; "index" ] with
  | Error _ -> ()
  | Ok p ->
    let p = line p in
    let p = if Filename.is_relative p then root ^/ p else p in
    (try
       let data = In_channel.read_all p in
       let stats = Core_unix.stat p in
       Out_channel.write_all index ~data;
       Core_unix.utimes index ~access:stats.st_atime ~modif:stats.st_mtime
     with
     | _ -> ())
;;

(* The working tree's tree, through a temporary index. *)
let worktree_tree ~env ~cancel ~root =
  with_temp_index (fun index ->
    seed_index ~env ~cancel ~root ~index;
    let add () = git ~env ~cancel ~index ~dir:root [ "add"; "-A"; "--"; "." ] in
    (match add () with
     | Ok _ -> ()
     | Error _ ->
       (* A copy of an index caught mid-write: start from nothing. *)
       (try Core_unix.unlink index with
        | _ -> ());
       ignore
         (git_exn
            ~env
            ~cancel
            ~index
            ~dir:root
            ~what:"add"
            [ "add"; "-A"; "--"; "." ]
          : string));
    line
      (git_exn
         ~env
         ~cancel
         ~index
         ~dir:root
         ~what:"write-tree"
         [ "write-tree" ]))
;;

let check_ref ref =
  if not (String.is_prefix ref ~prefix:ref_prefix)
  then fail "checkpoint refs are under %s, not %s" ref_prefix ref
;;

let protect f =
  match f () with
  | v -> Ok v
  | exception Failed s -> Or_error.error_string s
;;

let capture ~env ~cancel ~cwd ~ref =
  protect (fun () ->
    match root ~env ~cancel ~cwd with
    | Error why -> Captured.Not_git why
    | Ok root ->
      Option.iter ref ~f:check_ref;
      let tree = worktree_tree ~env ~cancel ~root in
      let commit =
        Option.map ref ~f:(fun ref ->
          let commit =
            line
              (git_exn
                 ~env
                 ~cancel
                 ~dir:root
                 ~what:"commit-tree"
                 [ "commit-tree"; tree; "-m"; "prigh checkpoint" ])
          in
          ignore
            (git_exn
               ~env
               ~cancel
               ~dir:root
               ~what:"update-ref"
               [ "update-ref"; ref; commit ]
             : string);
          commit)
      in
      Snapshot { commit; tree; root })
;;

let tree_of ~env ~cancel ~root rev =
  line
    (git_exn
       ~env
       ~cancel
       ~dir:root
       ~what:"rev-parse"
       [ "rev-parse"; "--verify"; rev ^ "^{tree}" ])
;;

module Change = struct
  type t =
    | Gone (** in the source, not in the target: removed *)
    | Written (** in the target: written from it *)
end

(* [git diff-tree -z --name-status] from [a] to [b]: the paths whose
   content differs, with whether [b] has them. *)
let diff ~env ~cancel ~root a b =
  let out =
    git_exn
      ~env
      ~cancel
      ~dir:root
      ~what:"diff-tree"
      [ "diff-tree"; "-r"; "-z"; "--no-renames"; "--name-status"; a; b ]
  in
  let rec pairs acc = function
    | status :: path :: rest ->
      let change : Change.t =
        if String.is_prefix status ~prefix:"D" then Gone else Written
      in
      pairs ((path, change) :: acc) rest
    | [ "" ] | [] -> List.rev acc
    | [ junk ] -> fail "unexpected git diff-tree output: %S" junk
  in
  pairs [] (String.split out ~on:'\000')
;;

let changed ~env ~cancel ~cwd ~from ~to_ =
  protect (fun () ->
    let root = root_exn ~env ~cancel ~cwd in
    let a = tree_of ~env ~cancel ~root from in
    let b =
      match to_ with
      | Some to_ -> tree_of ~env ~cancel ~root to_
      | None -> worktree_tree ~env ~cancel ~root
    in
    if String.equal a b
    then []
    else
      List.map (diff ~env ~cancel ~root a b) ~f:fst
      |> List.sort ~compare:String.compare)
;;

let rec remove_empty_dirs ~root dir =
  if (not (String.equal dir root)) && String.is_prefix dir ~prefix:root
  then (
    match Core_unix.rmdir dir with
    | () -> remove_empty_dirs ~root (Filename.dirname dir)
    | exception _ -> ())
;;

let show_files files =
  let n = List.length files in
  let shown = List.take files 10 in
  String.concat ~sep:", " shown
  ^ if n > 10 then sprintf " and %d more" (n - 10) else ""
;;

let restore ~env ~cancel ~cwd ~to_ ~expect =
  protect (fun () ->
    let root = root_exn ~env ~cancel ~cwd in
    let expected = tree_of ~env ~cancel ~root expect in
    let target = tree_of ~env ~cancel ~root to_ in
    let now = worktree_tree ~env ~cancel ~root in
    if not (String.equal now expected)
    then (
      let files = List.map (diff ~env ~cancel ~root expected now) ~f:fst in
      fail
        "files changed since the agent's last turn: %s"
        (show_files (List.sort files ~compare:String.compare)));
    if String.equal now target
    then []
    else (
      let changes = diff ~env ~cancel ~root now target in
      (* The files the target lacks go first: a directory may take the
         place of one. *)
      List.iter changes ~f:(fun (path, change) ->
        match change with
        | Written -> ()
        | Gone ->
          let full = root ^/ path in
          (try Core_unix.unlink full with
           | Core_unix.Unix_error (ENOENT, _, _) -> ()
           | Core_unix.Unix_error (e, _, _) ->
             fail "cannot remove %s: %s" path (Core_unix.Error.message e));
          remove_empty_dirs ~root (Filename.dirname full));
      let written =
        List.filter_map changes ~f:(fun (path, change) ->
          match change with
          | Written -> Some path
          | Gone -> None)
      in
      if not (List.is_empty written)
      then
        with_temp_index (fun index ->
          ignore
            (git_exn
               ~env
               ~cancel
               ~index
               ~dir:root
               ~what:"read-tree"
               [ "read-tree"; target ]
             : string);
          (* A directory where a file goes: it held only files the target
             lacks, removed above. *)
          ignore
            (git_exn
               ~env
               ~cancel
               ~index
               ~dir:root
               ~what:"checkout-index"
               ~stdin:
                 (String.concat (List.map written ~f:(fun p -> p ^ "\000")))
               [ "checkout-index"; "-f"; "-z"; "--stdin" ]
             : string));
      let after = worktree_tree ~env ~cancel ~root in
      if not (String.equal after target)
      then (
        let files = List.map (diff ~env ~cancel ~root target after) ~f:fst in
        fail
          "restored the checkpoint, but these files still differ from it: %s"
          (show_files files));
      List.map changes ~f:fst |> List.sort ~compare:String.compare))
;;

let drop ~env ~cancel ~cwd ~prefix =
  protect (fun () ->
    check_ref prefix;
    match root ~env ~cancel ~cwd with
    | Error _ -> 0
    | Ok root ->
      let refs =
        git_exn
          ~env
          ~cancel
          ~dir:root
          ~what:"for-each-ref"
          [ "for-each-ref"; "--format=%(refname)"; prefix ]
        |> String.split_lines
        |> List.filter ~f:(fun r ->
          (not (String.is_empty r)) && String.is_prefix r ~prefix)
      in
      if not (List.is_empty refs)
      then
        ignore
          (git_exn
             ~env
             ~cancel
             ~dir:root
             ~what:"update-ref"
             ~stdin:
               (String.concat
                  (List.map refs ~f:(fun r -> "delete " ^ r ^ "\n")))
             [ "update-ref"; "--stdin" ]
           : string);
      List.length refs)
;;

let op = "$checkpoint"

let run ~env ~cancel ~cwd ~arguments =
  let string name =
    match Json.member name arguments with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let need name f =
    match string name with
    | Some s -> f s
    | None -> Tool.Result.error (sprintf "%s: missing %s" op name)
  in
  let reply to_json = function
    | Ok v -> Tool.Result.ok (Json.to_string (to_json v))
    | Error e -> Tool.Result.error (Error.to_string_hum e)
  in
  let files l = `Array (List.map l ~f:(fun s -> `String s)) in
  match string "action" with
  | Some "capture" ->
    reply
      [%jsonaf_of: Captured.t]
      (capture ~env ~cancel ~cwd ~ref:(string "ref"))
  | Some "changed" ->
    need "from" (fun from ->
      reply files (changed ~env ~cancel ~cwd ~from ~to_:(string "to")))
  | Some "restore" ->
    need "to" (fun to_ ->
      need "expect" (fun expect ->
        reply files (restore ~env ~cancel ~cwd ~to_ ~expect)))
  | Some "drop" ->
    need "prefix" (fun prefix ->
      reply
        (fun n -> `Number (Int.to_string n))
        (drop ~env ~cancel ~cwd ~prefix))
  | Some other -> Tool.Result.error (sprintf "%s: unknown action %s" op other)
  | None -> Tool.Result.error (sprintf "%s: missing action" op)
;;

module Client = struct
  let call exec fields of_json =
    let (result : Tool.Result.t) = exec (`Object fields) in
    if result.is_error
    then (
      (* An older prigh on the host: it does not know the op. *)
      let text = result.text in
      if String.is_substring text ~substring:"unknown host tool"
      then
        Or_error.error_string
          "the tool host's prigh is too old to keep checkpoints: update prigh \
           there"
      else Or_error.error_string text)
    else (
      match Json.of_string result.text with
      | json -> Or_error.try_with (fun () -> of_json json)
      | exception exn -> Or_error.of_exn exn)
  ;;

  let opt name = function
    | None -> []
    | Some v -> [ name, `String v ]
  ;;

  let files = function
    | `Array l ->
      List.map l ~f:(function
        | `String s -> s
        | _ -> failwith "expected file names")
    | _ -> failwith "expected a list of files"
  ;;

  let capture exec ~ref =
    call
      exec
      ([ "action", `String "capture" ] @ opt "ref" ref)
      [%of_jsonaf: Captured.t]
  ;;

  let changed exec ~from ~to_ =
    call
      exec
      ([ "action", `String "changed"; "from", `String from ] @ opt "to" to_)
      files
  ;;

  let restore exec ~to_ ~expect =
    call
      exec
      [ "action", `String "restore"
      ; "to", `String to_
      ; "expect", `String expect
      ]
      files
  ;;

  let drop exec ~prefix =
    call
      exec
      [ "action", `String "drop"; "prefix", `String prefix ]
      (function
        | `Number n -> Int.of_string n
        | _ -> failwith "expected a count")
  ;;
end
