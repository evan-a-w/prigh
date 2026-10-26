open! Core

module File = struct
  type t =
    { path : string
    ; orig : string option
    ; index : string option
    ; worktree : string option
    }
  [@@deriving sexp_of, equal]

  let staged t = Option.is_some t.index
  let untracked t = Option.equal String.equal t.worktree (Some "?")
  let unstaged t = Option.is_some t.worktree

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind path = Json.string_field j "path" in
    let%bind orig = Json.string_opt_field j "orig" in
    let%bind index = Json.string_opt_field j "index" in
    let%map worktree = Json.string_opt_field j "worktree" in
    { path; orig; index; worktree }
  ;;
end

type t =
  { root : string
  ; branch : string option
  ; head : string option
  ; upstream : string option
  ; ahead : int
  ; behind : int
  ; files : File.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind root = Json.string_field j "root" in
  let%bind branch = Json.string_opt_field j "branch" in
  let%bind head = Json.string_opt_field j "head" in
  let%bind upstream = Json.string_opt_field j "upstream" in
  let int name =
    match Json.field j name with
    | None -> Ok 0
    | Some _ -> Json.int_field j name
  in
  let%bind ahead = int "ahead" in
  let%bind behind = int "behind" in
  let%map files = Json.optional_list_field j "files" ~f:File.of_json in
  { root; branch; head; upstream; ahead; behind; files }
;;

let counts t =
  List.fold t.files ~init:(0, 0, 0) ~f:(fun (s, u, n) f ->
    ( (if File.staged f then s + 1 else s)
    , (if File.unstaged f && not (File.untracked f) then u + 1 else u)
    , if File.untracked f then n + 1 else n ))
;;

let find t path =
  List.find t.files ~f:(fun f ->
    String.equal f.path path || Option.equal String.equal f.orig (Some path))
;;

module Commit = struct
  type nonrec t =
    { sha : string
    ; subject : string
    ; status : t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind sha = Json.string_field j "sha" in
    let%bind subject = Json.string_field j "subject" in
    let%map status = Or_error.bind (Json.object_field j "status") ~f:of_json in
    { sha; subject; status }
  ;;
end

module Push = struct
  type nonrec t =
    { remote : string
    ; branch : string
    ; status : t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind remote = Json.string_field j "remote" in
    let%bind branch = Json.string_field j "branch" in
    let%map status = Or_error.bind (Json.object_field j "status") ~f:of_json in
    { remote; branch; status }
  ;;
end
