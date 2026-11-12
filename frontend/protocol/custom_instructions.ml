open! Core
module Note = Memory.Note

module Limits = struct
  type t =
    { text : int
    ; title : int
    ; count : int
    }
  [@@deriving sexp_of, equal]

  let default = { text = 1000; title = 80; count = 50 }

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind text = Json.int_field j "text" in
    let%bind title = Json.int_field j "title" in
    let%map count = Json.int_field j "count" in
    { text; title; count }
  ;;
end

type t =
  { enabled : bool
  ; notes : Note.t list
  ; limits : Limits.t
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind enabled = Json.bool_default_field j "enabled" ~default:true in
  let%bind notes = Json.optional_list_field j "notes" ~f:Memory.Note.of_json in
  let%map limits =
    match Json.field j "limits" with
    | Some limits -> Limits.of_json limits
    | None -> Ok Limits.default
  in
  { enabled; notes; limits }
;;

let heading =
  "Custom instructions (the user's standing instructions; follow them):"
;;

let tokens = function
  | [] -> 0
  | notes ->
    let bytes =
      List.sum
        (module Int)
        notes
        ~f:(fun (n : Note.t) ->
          String.length n.id
          + 4
          + Option.value_map n.title ~default:0 ~f:(fun t ->
            String.length t + 3)
          + String.length n.text)
    in
    (String.length heading + bytes + 3) / 4
;;

let save_params ?id ?title text =
  let opt key =
    Option.value_map ~default:[] ~f:(fun v -> [ key, Json.str v ])
  in
  opt "id" id @ opt "title" title @ [ "text", Json.str text ]
;;

let note_of_reply j = Or_error.bind (Json.object_field j "note") ~f:Note.of_json
let remove_params ~id = [ "id", Json.str id ]
let set_enabled_params enabled = [ "enabled", Json.bool enabled ]
