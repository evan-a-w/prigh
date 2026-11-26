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

let check agent ~meth ~params =
  let feature : Capabilities.Feature.t option =
    match meth with
    | "set_mode" when turns_plan_mode_on params -> Some Plan_mode
    | meth -> Capabilities.method_feature meth
  in
  match feature with
  | None -> Ok ()
  | Some feature -> Agent.require agent feature
;;
