open! Core

type t =
  { repo : Forge_repo.t
  ; prs : Pull_request.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind repo = Json.object_field j "repo" >>= Forge_repo.of_json in
  let%map prs = Json.list_field j "prs" ~f:Pull_request.of_json in
  { repo; prs }
;;
