open! Core
open! Import

module Level = struct
  type t =
    | Normal
    | Warn
    | Over
  [@@deriving sexp_of, equal]
end

let money usd = sprintf "$%.2f" usd

let budget_part (s : P.State.t) =
  Option.map s.budget_usd ~f:(fun budget ->
    let level : Level.t =
      if Float.( >= ) s.cost_usd budget
      then Over
      else if Float.( >= ) s.cost_usd (0.8 *. budget)
      then Warn
      else Normal
    in
    sprintf "%s/%s" (money s.cost_usd) (money budget), level)
;;

let goal_part (g : P.Goal.t) : string * Level.t =
  match g.status with
  | Active -> sprintf "goal:%d/%d" g.continuations g.max_continuations, Warn
  | Done -> "goal:done", Normal
  | Stopped -> "goal:stopped", Over
;;

let clock at ~utc_offset =
  let _date, ofday =
    Time_ns.to_date_ofday (Time_ns.add at utc_offset) ~zone:Time_ns.Zone.utc
  in
  let parts = Time_ns.Ofday.to_parts ofday in
  sprintf "%02d:%02d" parts.hr parts.min
;;

let resume_part at ~utc_offset = "resumes " ^ clock at ~utc_offset

let describe_budget (s : P.State.t) =
  match s.budget_usd with
  | None ->
    sprintf
      "no budget (%s spent): /budget DOLLARS stops the agent starting turns \
       once the session has cost that much"
      (money s.cost_usd)
  | Some budget ->
    sprintf
      "budget %s, %s spent%s: /budget DOLLARS changes it, /budget off removes \
       it"
      (money budget)
      (money s.cost_usd)
      (if Float.( >= ) s.cost_usd budget then " (no turn starts)" else "")
;;

let describe_goal (s : P.State.t) =
  match s.goal with
  | None ->
    "no goal: /goal TEXT sets one, and the agent keeps working until it judges \
     it met"
  | Some g ->
    let state =
      match g.status with
      | Active ->
        sprintf
          "active, %d of %d automatic continuations used; /goal off clears it"
          g.continuations
          g.max_continuations
      | Done ->
        sprintf
          "done%s; /goal TEXT sets a new one"
          (Option.value_map g.summary ~default:"" ~f:(fun s -> ": " ^ s))
      | Stopped ->
        sprintf
          "stopped%s; /goal resume continues it, /goal off clears it"
          (Option.value_map g.reason ~default:"" ~f:(fun r -> sprintf " (%s)" r))
    in
    sprintf "goal: %s — %s" g.text state
;;

let describe_resume (s : P.State.t) ~utc_offset =
  match s.resume_at with
  | None ->
    "no resume is scheduled: one is when a run stops at a usage limit whose \
     reset time the provider gives"
  | Some at ->
    sprintf
      "the run stopped at a usage limit and resumes at %s: /resume now retries \
       at once, /resume cancel stays stopped"
      (clock at ~utc_offset)
;;

let parse_budget arg =
  let arg = String.strip arg in
  if String.Caseless.equal arg "off"
  then Ok None
  else (
    match
      Float.of_string_opt (String.chop_prefix_if_exists arg ~prefix:"$")
    with
    | Some usd when Float.( > ) usd 0. && Float.is_finite usd -> Ok (Some usd)
    | Some _ | None ->
      Error
        (sprintf
           "%S is not a budget: /budget DOLLARS (e.g. /budget 5) or /budget off"
           arg))
;;
