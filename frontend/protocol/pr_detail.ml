open! Core

type t =
  { repo : Forge_repo.t
  ; pr : Pull_request.t
  ; checks : Pr_checks.t option
  ; diff : string
  ; truncated : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind repo = Json.object_field j "repo" >>= Forge_repo.of_json in
  let%bind pr = Json.object_field j "pr" >>= Pull_request.of_json in
  let%bind checks =
    match Json.field j "checks" with
    | None -> Ok None
    | Some c -> Pr_checks.of_json c >>| Option.some
  in
  let%bind diff = Json.string_opt_field j "diff" >>| Option.value ~default:"" in
  let%map truncated = Json.bool_default_field j "truncated" ~default:false in
  { repo; pr; checks; diff; truncated }
;;

let git_diff t : Git_diff.t =
  { root = Forge_repo.slug t.repo
  ; against = t.pr.base
  ; diff = t.diff
  ; truncated = t.truncated
  ; branch = Some t.pr.head
  ; base = t.pr.base
  ; main = true
  ; branches = []
  }
;;
