open! Core

module Problem = struct
  type t =
    | Not_installed
    | Not_logged_in
    | Login_refused
    | Unreachable
    | Failed
  [@@deriving sexp_of, equal]

  let of_string = function
    | "not_installed" -> Not_installed
    | "not_logged_in" -> Not_logged_in
    | "login_refused" -> Login_refused
    | "unreachable" -> Unreachable
    | _ -> Failed
  ;;
end

module Fix = struct
  type t =
    | Login of { provider : string }
    | Engines
    | Retry
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    match%bind Json.string_field j "kind" with
    | "login" ->
      let%map provider = Json.string_field j "provider" in
      Some (Login { provider })
    | "engines" -> Ok (Some Engines)
    | "retry" -> Ok (Some Retry)
    | _ -> Ok None
  ;;

  let label = function
    | Login _ -> "Log in again"
    | Engines -> "Settings → Engines"
    | Retry -> "Retry"
  ;;

  let command = function
    | Login { provider } -> "/login " ^ provider
    | Engines -> "/adapters refresh"
    | Retry -> "/adapters retry"
  ;;
end

type t =
  | Loading
  | Failed of
      { problem : Problem.t
      ; summary : string
      ; message : string
      ; fixes : Fix.t list
      }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  match%bind Json.string_field j "state" with
  | "failed" ->
    let%bind problem = Json.string_field j "problem" in
    let%bind summary = Json.string_field j "summary" in
    let%bind message = Json.string_field j "message" in
    let%map fixes = Json.optional_list_field j "fixes" ~f:Fix.of_json in
    Failed
      { problem = Problem.of_string problem
      ; summary
      ; message
      ; fixes = List.filter_opt fixes
      }
  | _ -> Ok Loading
;;

let chip = function
  | Loading -> "loading models…"
  | Failed { summary; _ } -> summary
;;
