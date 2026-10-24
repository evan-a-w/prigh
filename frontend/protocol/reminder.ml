open! Core

module Trigger = struct
  type t =
    | On_stop of
        { max_repeats : int
        ; repeats : int
        }
    | Every_turns of int
  [@@deriving sexp_of, equal]

  let describe = function
    | On_stop { max_repeats; repeats } ->
      sprintf "on stop %d/%d" repeats max_repeats
    | Every_turns 1 -> "every turn"
    | Every_turns n -> sprintf "every %d turns" n
  ;;
end

type t =
  { name : string
  ; text : string
  ; trigger : Trigger.t
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind name = Json.string_field j "name" in
  let%bind text = Json.string_field j "text" in
  let%map trigger =
    match Json.field j "every_turns" with
    | Some _ ->
      let%map n = Json.int_field j "every_turns" in
      Trigger.Every_turns n
    | None ->
      let%bind max_repeats = Json.int_field j "max_repeats" in
      let%map repeats = Json.int_field j "repeats" in
      Trigger.On_stop { max_repeats; repeats }
  in
  { name; text; trigger }
;;

let find l name = List.find l ~f:(fun t -> String.equal t.name name)

let first_line t =
  String.split_lines t.text
  |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
  |> Option.value_map ~default:"" ~f:String.strip
;;
