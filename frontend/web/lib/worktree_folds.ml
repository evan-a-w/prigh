open! Core
open! Import

type t =
  { hours : int
  ; folded : String.Set.t
  ; unfolded : String.Set.t
  }
[@@deriving sexp_of, equal]

let default_hours = 12
let max_hours = 720

let empty =
  { hours = default_hours
  ; folded = String.Set.empty
  ; unfolded = String.Set.empty
  }
;;

let hours t = t.hours

let parse_hours text =
  let word = String.lowercase (String.strip text) in
  let number =
    List.find_map [ " hours"; "hours"; " hour"; "hour"; "h" ] ~f:(fun suffix ->
      String.chop_suffix word ~suffix)
    |> Option.value ~default:word
    |> String.strip
  in
  match word with
  | "off" | "never" -> Ok 0
  | _ ->
    (match Int.of_string_opt number with
     | Some n when n >= 0 && n <= max_hours -> Ok n
     | Some _ | None ->
       Error
         (sprintf
            "“%s” isn't a number of hours from 0 to %d: type one like 12, or \
             off to fold every session's worktrees."
            (String.strip text)
            max_hours))
;;

let hours_string = function
  | 0 -> "off"
  | 1 -> "1 hour"
  | n -> sprintf "%d hours" n
;;

let recent t ~now (s : Session_summary.t) =
  t.hours > 0
  &&
  match now with
  | None -> true
  | Some now ->
    (match Rel_time.parse (Option.value s.updated_at ~default:s.created_at) with
     | None -> false
     | Some used ->
       Time_ns.Span.( <= )
         (Time_ns.diff now used)
         (Time_ns.Span.of_hr (Float.of_int t.hours)))
;;

let shown t ~now (s : Session_summary.t) =
  if Set.mem t.folded s.id
  then false
  else Set.mem t.unfolded s.id || recent t ~now s
;;

let toggle t ~now (s : Session_summary.t) =
  if shown t ~now s
  then
    { t with
      folded = Set.add t.folded s.id
    ; unfolded = Set.remove t.unfolded s.id
    }
  else
    { t with
      folded = Set.remove t.folded s.id
    ; unfolded = Set.add t.unfolded s.id
    }
;;

let ids_pref ids =
  if Set.is_empty ids
  then None
  else Some (`Array (List.map (Set.to_list ids) ~f:(fun id -> `String id)))
;;

let ids_of_pref : Json.t option -> String.Set.t = function
  | Some (`Array items) ->
    String.Set.of_list
      (List.filter_map items ~f:(function
         | `String s -> Some s
         | _ -> None))
  | _ -> String.Set.empty
;;

let folded_pref t = ids_pref t.folded
let unfolded_pref t = ids_pref t.unfolded

let hours_pref t =
  if t.hours = default_hours then None else Some (Json.int t.hours)
;;

let set_folded t value = { t with folded = ids_of_pref value }
let set_unfolded t value = { t with unfolded = ids_of_pref value }

let set_hours t hours =
  { t with hours = Int.clamp_exn hours ~min:0 ~max:max_hours }
;;

let set_hours_pref t (value : Json.t option) =
  match value with
  | Some (`Number n) ->
    (match Float.of_string_opt n with
     | Some h when Float.is_finite h -> set_hours t (Float.iround_nearest_exn h)
     | _ -> { t with hours = default_hours })
  | _ -> { t with hours = default_hours }
;;
