open! Core

type t =
  { cwd : string
  ; host : string option
  ; host_name : string option
  ; repo : Repo.t option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind host, host_name =
    match Json.field j "host" with
    | None | Some `Null -> Ok (None, None)
    | Some h ->
      let%bind id = Json.string_field h "id" in
      let%map name = Json.string_opt_field h "name" in
      Some id, name
  in
  let%map repo =
    match Json.field j "repo" with
    | None | Some `Null -> Ok None
    | Some r -> Repo.of_json r >>| Option.some
  in
  { cwd; host; host_name; repo }
;;

let field j =
  match Json.field j "home" with
  | None | Some `Null -> Ok None
  | Some h -> Or_error.map (of_json h) ~f:Option.some
;;

let label t ~tilde =
  match t.repo with
  | Some { branch = Some branch; _ } -> branch
  | Some { branch = None; _ } | None -> tilde t.cwd
;;
