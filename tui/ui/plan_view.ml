open! Core
open! Import

let status (plan : P.Plan.t) =
  let total = List.length plan.steps in
  let completed = P.Plan.completed plan in
  if completed >= total
  then None
  else (
    let current =
      Option.value_map (P.Plan.current plan) ~default:"" ~f:(fun s ->
        ": " ^ s.step)
    in
    Some (sprintf "plan %d/%d%s" completed total current))
;;

let mark : P.Plan.Status.t -> string = function
  | Completed -> "✓"
  | In_progress -> "▸"
  | Pending -> "○"
;;

let lines (plan : P.Plan.t) =
  Option.to_list plan.explanation
  @ List.map plan.steps ~f:(fun s -> sprintf "%s %s" (mark s.status) s.step)
;;
