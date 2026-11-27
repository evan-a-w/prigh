open! Core
open! Import

let turns_plan_mode_on params =
  match params with
  | `Object fields ->
    (match List.Assoc.find fields "plan_mode" ~equal:String.equal with
     | Some `True -> true
     | _ -> false)
  | _ -> false
;;

let is_managed_subagent agent ~meth ~params =
  match meth with
  | "steer_agent" | "dequeue_agent" | "remove_agent_queued" ->
    (match Json.member "agent_id" params with
     | Some (`String agent_id) -> Agent.has_managed_subagent agent ~agent_id
     | _ -> false)
  | _ -> false
;;

let check agent ~meth ~params =
  if is_managed_subagent agent ~meth ~params
  then Ok ()
  else (
    let feature : Capabilities.Feature.t option =
      match meth with
      | "set_mode" when turns_plan_mode_on params -> Some Plan_mode
      | meth -> Capabilities.method_feature meth
    in
    match feature with
    | None -> Ok ()
    | Some feature -> Agent.require agent feature)
;;
