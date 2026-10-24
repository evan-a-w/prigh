open! Core
open! Import

module Action = struct
  type t =
    | Pick of P.Reminder.t list
    | Add of (string * P.Json.t) list
    | Remove of string
    | Clear of int
    | Info of string
    | Error of string
  [@@deriving sexp_of]
end

let usage = "/remind [stop [N] TEXT|every N TEXT|rm NAME|off]"

let none_yet =
  "no reminders yet: /remind stop TEXT or /remind every N TEXT adds one"
;;

let not_loaded = "the session has not loaded yet; try again in a moment"

let stop_usage =
  "usage: /remind stop [N] TEXT: reminded when it ends its turn, at most N \
   times in a row (default 3)"
;;

let every_usage =
  "usage: /remind every N TEXT: reminded every N turns, e.g. /remind every 20 \
   check the disk space"
;;

let subcommands =
  [ "stop", "stop [N] TEXT: remind the agent when it ends its turn"
  ; "every", "every N TEXT: remind the agent every N turns"
  ; "rm", "rm NAME: remove a reminder"
  ; "off", "remove every reminder"
  ]
;;

let pick : P.Reminder.t list option -> Action.t = function
  | None -> Error not_loaded
  | Some [] -> Info none_yet
  | Some reminders -> Pick reminders
;;

(* The first word (up to any whitespace, newlines too) and what follows it,
   stripped. *)
let split_word s =
  let s = String.lstrip s in
  match String.lfindi s ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> s, ""
  | Some i -> String.prefix s i, String.strip (String.drop_prefix s i)
;;

let count word =
  match Int.of_string_opt word with
  | Some n when String.for_all word ~f:Char.is_digit -> Some n
  | Some _ | None -> None
;;

let add ~text trigger : Action.t = Add (("text", P.Json.str text) :: trigger)

let stop rest : Action.t =
  let word, after = split_word rest in
  match count word with
  | _ when String.is_empty word -> Error stop_usage
  | Some _ when String.is_empty after -> Error stop_usage
  | Some n when n < 1 -> Error "N must be 1 or more: /remind stop N TEXT"
  | Some n ->
    add ~text:after [ "on", P.Json.str "stop"; "max_repeats", P.Json.int n ]
  | None -> add ~text:(String.strip rest) [ "on", P.Json.str "stop" ]
;;

let every rest : Action.t =
  let word, after = split_word rest in
  match count word with
  | Some n when n >= 1 && not (String.is_empty after) ->
    add ~text:after [ "every_turns", P.Json.int n ]
  | Some _ when not (String.is_empty after) ->
    Error "N must be 1 or more: /remind every N TEXT"
  | Some _ | None -> Error every_usage
;;

let names reminders = List.map reminders ~f:(fun (r : P.Reminder.t) -> r.name)

let unknown reminders name : Action.t =
  match reminders with
  | [] -> Error (sprintf "no reminder %S; %s" name none_yet)
  | _ ->
    let names = names reminders in
    let near =
      List.filter names ~f:(fun n -> Edit_distance.distance n name <= 1)
      @ Fuzzy.rank ~query:name names ~key:Fn.id
      |> List.stable_dedup ~compare:String.compare
    in
    Error
      (sprintf
         "no reminder %S; %s (/remind lists them)"
         name
         (match near with
          | [] -> "the session's are " ^ String.concat ~sep:", " names
          | near -> sprintf "did you mean %s?" (String.concat ~sep:", " near)))
;;

let remove reminders rest : Action.t =
  match split_word rest with
  | "", _ -> Error "usage: /remind rm NAME (/remind lists them)"
  | name, "" ->
    (match reminders with
     | None -> Error not_loaded
     | Some reminders ->
       if Option.is_some (P.Reminder.find reminders name)
       then Remove name
       else unknown reminders name)
  | _ -> Error "usage: /remind rm NAME (/remind lists them)"
;;

let run ~reminders rest : Action.t =
  let word, after = split_word rest in
  match word with
  | "" -> pick reminders
  | "stop" -> stop after
  | "every" -> every after
  | "rm" -> remove reminders after
  | "off" when String.is_empty after ->
    (match reminders with
     | None -> Error not_loaded
     | Some [] -> Info "no reminders to clear"
     | Some reminders -> Clear (List.length reminders))
  | "off" -> Error "usage: /remind off (removes every reminder)"
  | word ->
    (match
       List.find subcommands ~f:(fun (s, _) ->
         Edit_distance.distance s word <= 2)
     with
     | Some (close, _) ->
       Error
         (sprintf
            "unknown /remind %S; did you mean %s? usage: %s"
            word
            close
            usage)
     | None -> Error ("usage: " ^ usage))
;;

let rpc : Action.t -> _ = function
  | Add params -> Some ("add_reminder", params)
  | Remove name -> Some ("remove_reminder", [ "name", P.Json.str name ])
  | Clear _ -> Some ("clear_reminders", [])
  | Pick _ | Info _ | Error _ -> None
;;

let describe (r : P.Reminder.t) =
  sprintf
    "%s · %s · %s"
    r.name
    (P.Reminder.Trigger.describe r.trigger)
    (P.Reminder.first_line r)
;;

let added result =
  match P.Reminder.of_json result with
  | Ok r ->
    sprintf
      "reminder %s added (%s): %s · /remind rm %s removes it"
      r.name
      (match r.trigger with
       | On_stop { max_repeats; _ } ->
         sprintf "when the agent ends its turn, up to %d in a row" max_repeats
       | Every_turns _ -> P.Reminder.Trigger.describe r.trigger)
      (P.Reminder.first_line r)
      r.name
  | Error _ -> "reminder added (/remind lists them)"
;;

let removed name = sprintf "reminder %s removed" name

let cleared = function
  | 1 -> "the reminder was removed"
  | n -> sprintf "all %d reminders removed" n
;;

let status_part = function
  | [] -> None
  | reminders -> Some (sprintf "remind:%d" (List.length reminders))
;;

let complete ~reminders rest =
  match String.lfindi rest ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> Some (0, subcommands)
  | Some i ->
    let word = String.prefix rest i in
    let after = String.drop_prefix rest i in
    let name = String.lstrip after in
    if String.equal word "rm" && not (String.exists name ~f:Char.is_whitespace)
    then
      Some
        ( String.length rest - String.length name
        , List.map reminders ~f:(fun (r : P.Reminder.t) ->
            ( r.name
            , sprintf
                "%s · %s"
                (P.Reminder.Trigger.describe r.trigger)
                (P.Reminder.first_line r) )) )
    else None
;;
