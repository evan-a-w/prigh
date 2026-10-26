open! Core

module Kind = struct
  type t =
    | Commit_message
    | Pr
    | Branch_name
    | Session_title
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Commit_message -> "commit_message"
    | Pr -> "pr"
    | Branch_name -> "branch_name"
    | Session_title -> "session_title"
  ;;
end

type t =
  { gen_id : string
  ; text : string
  ; title : string option
  ; body : string option
  ; model : string
  ; cost_usd : float
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind gen_id = Json.string_field j "gen_id" in
  let%bind text = Json.string_field j "text" in
  let%bind title = Json.string_opt_field j "title" in
  let%bind body = Json.string_opt_field j "body" in
  let%bind model =
    Or_error.map (Json.string_opt_field j "model") ~f:(Option.value ~default:"")
  in
  let%map cost_usd =
    match Json.field j "cost_usd" with
    | None -> Ok 0.
    | Some _ -> Json.float_field j "cost_usd"
  in
  { gen_id; text; title; body; model; cost_usd }
;;

let split_pr text =
  let text = String.lstrip text in
  match String.lsplit2 text ~on:'\n' with
  | None -> String.strip text, ""
  | Some (title, rest) -> String.strip title, String.strip rest
;;
