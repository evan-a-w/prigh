open! Core
open! Import

module Trigger = struct
  type t =
    | On_stop of { max_repeats : int }
    | Every_turns of int
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { name : string
  ; text : string
  ; trigger : Trigger.t
  }
[@@deriving sexp, jsonaf, equal]

let default_max_repeats = 3

let number name =
  match String.chop_prefix name ~prefix:"r" with
  | Some n -> Option.value (Int.of_string_opt n) ~default:0
  | None -> 0
;;

let create ~taken ~text (trigger : Trigger.t) =
  let text = String.strip text in
  match trigger with
  | _ when String.is_empty text ->
    Or_error.error_string
      "say what to remind the agent of, e.g. /remind stop run the tests before \
       you stop"
  | On_stop { max_repeats } when max_repeats < 1 ->
    Or_error.error_string
      "the number of repeats must be 1 or more, e.g. /remind stop 5 TEXT"
  | Every_turns n when n < 1 ->
    Or_error.error_string
      "the number of turns must be 1 or more, e.g. /remind every 20 TEXT"
  | On_stop _ | Every_turns _ ->
    let last =
      List.fold taken ~init:0 ~f:(fun acc name -> Int.max acc (number name))
    in
    Ok { name = sprintf "r%d" (last + 1); text; trigger }
;;

let label t =
  match String.split_lines t.text with
  | line :: _ -> sprintf "%s: %s" t.name line
  | [] -> t.name
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
  sprintf "[prigh reminder: %s, %s] %s" t.name when_ t.text
;;

module Tracker = struct
  type reminder = t [@@deriving sexp_of]

  type t =
    { mutable turns : int
    ; last_fired : int String.Table.t
      (** the turn each periodic reminder, by name, last fired at *)
    ; mutable stops : int (** eligible stops since the user last spoke *)
    }

  let create () = { turns = 0; last_fired = String.Table.create (); stops = 0 }

  let reset t =
    t.turns <- 0;
    Hashtbl.clear t.last_fired;
    t.stops <- 0
  ;;

  let user_spoke t = t.stops <- 0
  let turn_ended t = t.turns <- t.turns + 1
  let stops t = t.stops

  let due t (reminders : reminder list) =
    List.filter reminders ~f:(fun r ->
      match r.trigger with
      | On_stop _ -> false
      | Every_turns n ->
        (match Hashtbl.find t.last_fired r.name with
         | None ->
           Hashtbl.set t.last_fired ~key:r.name ~data:t.turns;
           false
         | Some last when t.turns - last >= n ->
           Hashtbl.set t.last_fired ~key:r.name ~data:t.turns;
           true
         | Some _ -> false))
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
