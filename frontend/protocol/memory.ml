open! Core

module Note = struct
  type t =
    { id : string
    ; title : string option
    ; text : string
    ; saved : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind id = Json.string_field j "id" in
    let%bind title = Json.string_opt_field j "title" in
    let%bind text = Json.string_field j "text" in
    let%map saved = Json.string_field j "saved" in
    { id; title; text; saved }
  ;;

  let date t = String.prefix t.saved 10
end

type t =
  { enabled : bool
  ; notes : Note.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind enabled = Json.bool_default_field j "enabled" ~default:true in
  let%map notes = Json.optional_list_field j "notes" ~f:Note.of_json in
  { enabled; notes }
;;

let save_params ?id ?title text =
  let opt key = Option.value_map ~default:[] ~f:(fun v -> [ key, Json.str v ]) in
  opt "id" id @ opt "title" title @ [ "text", Json.str text ]
;;

let note_of_reply j = Or_error.bind (Json.object_field j "note") ~f:Note.of_json
let forget_params ~id = [ "id", Json.str id ]
let set_enabled_params enabled = [ "enabled", Json.bool enabled ]
