open! Core
open! Import

module Trigger = struct
  type t =
    | On_stop of { max_repeats : int }
    | Every_turns of int
  [@@deriving sexp_of, equal]
end

type t =
  { name : string option
  ; text : string
  ; trigger : Trigger.t
  }
[@@deriving sexp_of, equal]

let default_max_repeats = 3
let keys = [ "name"; "text"; "on"; "every_turns"; "max_repeats" ]

let of_json (json : Json.t) =
  let open Or_error.Let_syntax in
  match json with
  | `Object fields ->
    let find name = List.Assoc.find fields ~equal:String.equal name in
    let%bind () =
      match
        List.find fields ~f:(fun (key, _) ->
          not (List.mem keys key ~equal:String.equal))
      with
      | None -> Ok ()
      | Some (key, _) ->
        (match Edit_distance.closest ~n:1 keys key with
         | [ close ] when Edit_distance.caseless close key <= 3 ->
           Or_error.errorf "unknown key %S; did you mean %S?" key close
         | _ ->
           Or_error.errorf
             "unknown key %S (known: %s)"
             key
             (String.concat ~sep:", " keys))
    in
    let positive name =
      match find name with
      | None | Some `Null -> Ok None
      | Some (`Number n) ->
        (match Int.of_string_opt n with
         | Some n when n >= 1 -> Ok (Some n)
         | Some _ | None ->
           Or_error.errorf "%S must be a whole number, 1 or more" name)
      | Some _ -> Or_error.errorf "%S must be a whole number, 1 or more" name
    in
    let%bind name =
      match find "name" with
      | None | Some `Null | Some (`String "") -> Ok None
      | Some (`String s) -> Ok (Some s)
      | Some _ -> Or_error.error_string "\"name\" must be a string"
    in
    let%bind text =
      match find "text" with
      | Some (`String s) when not (String.is_empty (String.strip s)) -> Ok s
      | _ ->
        Or_error.error_string
          "\"text\" must be a non-empty string: what to remind the agent of"
    in
    let%bind every_turns = positive "every_turns" in
    let%bind max_repeats = positive "max_repeats" in
    let%bind on_stop =
      match find "on" with
      | None | Some `Null -> Ok false
      | Some (`String "stop") -> Ok true
      | Some _ -> Or_error.error_string "\"on\" must be \"stop\""
    in
    let%map trigger =
      match on_stop, every_turns, max_repeats with
      | true, None, max_repeats ->
        Ok
          (Trigger.On_stop
             { max_repeats =
                 Option.value max_repeats ~default:default_max_repeats
             })
      | false, Some n, None -> Ok (Trigger.Every_turns n)
      | false, Some _, Some _ ->
        Or_error.error_string
          "\"max_repeats\" only goes with \"on\": \"stop\", not with \
           \"every_turns\""
      | true, Some _, _ ->
        Or_error.error_string
          "has both \"on\": \"stop\" and \"every_turns\"; keep one"
      | false, None, _ ->
        Or_error.error_string
          "needs \"on\": \"stop\" (when the agent ends its turn) or \
           \"every_turns\": N"
    in
    { name; text; trigger }
  | _ ->
    Or_error.error_string
      "must be an object such as {\"text\": \"...\", \"on\": \"stop\"}"
;;

let to_json t : Json.t =
  let number n = `Number (Int.to_string n) in
  `Object
    (List.concat
       [ Option.value_map t.name ~default:[] ~f:(fun n -> [ "name", `String n ])
       ; [ "text", `String t.text ]
       ; (match t.trigger with
          | On_stop { max_repeats } ->
            [ "on", `String "stop"; "max_repeats", number max_repeats ]
          | Every_turns n -> [ "every_turns", number n ])
       ])
;;

let label t =
  match t.name with
  | Some name -> name
  | None ->
    (match String.split_lines (String.strip t.text) with
     | line :: _ -> line
     | [] -> "")
;;

let message_text t ~repeat =
  let when_ =
    match t.trigger, repeat with
    | Every_turns 1, _ -> "every turn"
    | Every_turns n, _ -> sprintf "every %d turns" n
    | On_stop { max_repeats }, repeat ->
      sprintf
        "as you ended your turn (%d of %d)"
        (Option.value repeat ~default:1)
        max_repeats
  in
  let name = Option.value_map t.name ~default:"" ~f:(fun n -> ": " ^ n) in
  sprintf "[prigh reminder%s, %s] %s" name when_ t.text
;;

module Tracker = struct
  type reminder = t [@@deriving sexp_of]

  type t =
    { mutable turns : int
    ; last_fired : int Int.Table.t
      (** the turn each periodic reminder, by its index, last fired at *)
    ; mutable stops : int (** eligible stops since the user last spoke *)
    }

  let create () = { turns = 0; last_fired = Int.Table.create (); stops = 0 }

  let reset t =
    t.turns <- 0;
    Hashtbl.clear t.last_fired;
    t.stops <- 0
  ;;

  let user_spoke t = t.stops <- 0
  let turn_ended t = t.turns <- t.turns + 1

  let due t (reminders : reminder list) =
    List.filter_mapi reminders ~f:(fun i r ->
      match r.trigger with
      | On_stop _ -> None
      | Every_turns n ->
        let last = Option.value (Hashtbl.find t.last_fired i) ~default:0 in
        if t.turns - last >= n
        then (
          Hashtbl.set t.last_fired ~key:i ~data:t.turns;
          Some r)
        else None)
  ;;

  module Stop = struct
    type t =
      { fire : (reminder * int) list
      ; used_up : reminder list
      }
    [@@deriving sexp_of]
  end

  let stopped t (reminders : reminder list) =
    t.stops <- t.stops + 1;
    let on_stop =
      List.filter_map reminders ~f:(fun r ->
        match r.trigger with
        | On_stop { max_repeats } -> Some (r, max_repeats)
        | Every_turns _ -> None)
    in
    { Stop.fire =
        List.filter_map on_stop ~f:(fun (r, max) ->
          Option.some_if (t.stops <= max) (r, t.stops))
    ; used_up =
        List.filter_map on_stop ~f:(fun (r, max) ->
          Option.some_if (t.stops = max + 1) r)
    }
  ;;
end
