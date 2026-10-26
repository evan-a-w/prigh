open! Core

module Commit = struct
  type t =
    { sha : string
    ; subject : string
    }
  [@@deriving sexp_of, equal]

  let short t = String.prefix t.sha 8
end

module Target = struct
  type t =
    | Dir of string option
    | Rev of string
  [@@deriving sexp_of, equal, compare]
end

type t =
  { root : string
  ; against : string
  ; diff : string
  ; truncated : bool
  ; branch : string option
  ; base : string
  ; main : bool
  ; branches : string list
  ; remote_branches : string list
  ; tags : string list
  ; commits : Commit.t list
  ; rev : string option
  ; direct : bool
  }
[@@deriving sexp_of, equal]

let review_root t =
  match t.rev with
  | Some rev -> "branch:" ^ rev
  | None -> t.root
;;

let params ?path ?base ?(direct = false) ?(target = Target.Dir None) () =
  List.filter_opt
    [ Option.map path ~f:(fun p -> "path", Json.str p)
    ; Option.map base ~f:(fun b -> "base", Json.str b)
    ; (match target with
       | Dir dir -> Option.map dir ~f:(fun d -> "dir", Json.str d)
       | Rev rev -> Some ("rev", Json.str rev))
    ; Option.some_if direct ("direct", Json.bool true)
    ]
;;

let show_params ~rev ~path = [ "rev", Json.str rev; "path", Json.str path ]

let strings j name =
  Json.optional_list_field j name ~f:(function
    | `String s -> Ok s
    | _ -> Or_error.errorf "%s: each is a string" name)
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind root = Json.string_field j "root" in
  let%bind against = Json.string_field j "against" in
  let%bind diff = Json.string_field j "diff" in
  let%bind truncated = Json.bool_default_field j "truncated" ~default:false in
  let%bind branch = Json.string_opt_field j "branch" in
  let%bind base = Json.string_opt_field j "base" in
  let%bind main = Json.bool_default_field j "main" ~default:true in
  let%bind branches = strings j "branches" in
  let%bind remote_branches = strings j "remote_branches" in
  let%bind tags = strings j "tags" in
  let%bind commits =
    Json.optional_list_field j "commits" ~f:(fun c ->
      let%bind sha = Json.string_field c "sha" in
      let%map subject = Json.string_field c "subject" in
      { Commit.sha; subject })
  in
  let%bind rev = Json.string_opt_field j "rev" in
  let%map direct = Json.bool_default_field j "direct" ~default:false in
  { root
  ; against
  ; diff
  ; truncated
  ; branch
  ; base = Option.value base ~default:"HEAD"
  ; main
  ; branches
  ; remote_branches
  ; tags
  ; commits
  ; rev
  ; direct
  }
;;

module Refs = struct
  type t =
    { branches : string list
    ; remote_branches : string list
    ; tags : string list
    }
  [@@deriving sexp_of, equal]

  let empty = { branches = []; remote_branches = []; tags = [] }

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind branches = strings j "branches" in
    let%bind remote_branches = strings j "remote_branches" in
    let%map tags = strings j "tags" in
    { branches; remote_branches; tags }
  ;;
end

let refs (t : t) =
  { Refs.branches = t.branches; remote_branches = t.remote_branches; tags = t.tags }
;;

let shown_of_json j =
  let open Or_error.Let_syntax in
  let%bind text = Json.string_field j "text" in
  let%map truncated = Json.bool_default_field j "truncated" ~default:false in
  text, truncated
;;
