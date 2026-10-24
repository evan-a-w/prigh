open! Core

module Choice = struct
  type t =
    { label : string
    ; description : string option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind label = Json.string_field j "label" in
    let%map description = Json.string_opt_field j "description" in
    { label; description }
  ;;
end

type t =
  { question : string
  ; header : string option
  ; choices : Choice.t list
  ; multiple : bool
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind question = Json.string_field j "question" in
  let%bind header = Json.string_opt_field j "header" in
  let%bind choices = Json.optional_list_field j "options" ~f:Choice.of_json in
  let%map multiple =
    match Json.field j "multiple" with
    | None -> Ok false
    | Some _ -> Json.bool_field j "multiple"
  in
  { question; header; choices; multiple }
;;
