open! Core

module Session_ref = struct
  type t =
    { id : string
    ; path : string
    ; name : string option
    ; live : bool
    ; running : bool
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind id = Json.string_field j "id" in
    let%bind path = Json.string_field j "path" in
    let%bind name = Json.string_opt_field j "name" in
    let%bind live = Json.bool_default_field j "live" ~default:false in
    let%map running = Json.bool_default_field j "running" ~default:false in
    { id; path; name; live; running }
  ;;
end

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
  ; current : bool
  ; sessions : Session_ref.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let flag name = Json.bool_default_field j name ~default:false in
  let count name =
    match Json.field j name with
    | None -> Ok 0
    | Some _ -> Json.int_field j name
  in
  let%bind path = Json.string_field j "path" in
  let%bind branch = Json.string_opt_field j "branch" in
  let%bind head = Json.string_opt_field j "head" in
  let%bind main = flag "main" in
  let%bind managed = flag "managed" in
  let%bind locked = flag "locked" in
  let%bind prunable = flag "prunable" in
  let%bind changes =
    match Json.field j "changes" with
    | None -> Ok None
    | Some _ -> Json.int_field j "changes" >>| Option.some
  in
  let%bind upstream = Json.string_opt_field j "upstream" in
  let%bind ahead = count "ahead" in
  let%bind behind = count "behind" in
  let%bind current = flag "current" in
  let%map sessions =
    Json.optional_list_field j "sessions" ~f:Session_ref.of_json
  in
  { path
  ; branch
  ; head
  ; main
  ; managed
  ; locked
  ; prunable
  ; changes
  ; upstream
  ; ahead
  ; behind
  ; current
  ; sessions
  }
;;

let label t =
  match t.branch, t.head with
  | Some branch, _ -> branch
  | None, Some head -> String.prefix head 8
  | None, None -> "(detached)"
;;

let dirty t = Option.exists t.changes ~f:(fun n -> n > 0)

module Listing = struct
  type nonrec t =
    { root : string
    ; project_dir : string
    ; worktrees : t list
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind root = Json.string_field j "root" in
    let%bind project_dir = Json.string_field j "project_dir" in
    let%map worktrees = Json.list_field j "worktrees" ~f:of_json in
    { root; project_dir; worktrees }
  ;;
end

module Created = struct
  type nonrec t =
    { worktree : t
    ; new_branch : bool
    ; root : string option
    ; cwd : string option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind worktree = Json.object_field j "worktree" >>= of_json in
    let%bind new_branch =
      Json.bool_default_field j "new_branch" ~default:false
    in
    let%bind root = Json.string_opt_field j "root" in
    let%map cwd = Json.string_opt_field j "cwd" in
    { worktree; new_branch; root; cwd }
  ;;
end

let removed_of_json j =
  Or_error.bind (Json.object_field j "worktree") ~f:of_json
;;
