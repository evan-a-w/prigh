open! Core
open! Import

let name = "exit_plan_mode"

let spec =
  { Tool_spec.name
  ; parallel_safe = false
  ; on_host = false
  ; freeform = None
  ; destructive = Never
  ; description =
      "In plan mode, presents your plan to the user and asks to leave plan \
       mode so that you can implement it. Call it once the plan is ready: the \
       user approves it (then you can edit files and run commands) or tells \
       you what to change. Only use it in plan mode."
  ; parameters =
      Tool_args.schema
        ~required:[ "plan" ]
        [ ( "plan"
          , `String
          , "The plan, in markdown: concise, the steps and the files they touch"
          )
        ]
  }
;;

let plan args =
  match args with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal "plan" with
     | Some (`String s) -> s
     | _ -> "")
  | _ -> ""
;;

(* The agent's approver answers every call; this only runs without one. *)
let run (_ : Tool.Context.t) _ =
  Tool.Result.error "plan mode is not on: there is no plan to approve"
;;

let tool = { Tool.spec; run }
