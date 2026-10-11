open! Core
open! Import

module Answer = struct
  type t =
    | Allow
    | Always
    | Deny
    | Deny_with_reason
  [@@deriving sexp_of, equal]
end

let is_plan (c : P.Pending.Confirm.t) = String.equal c.name "exit_plan_mode"

let who (c : P.Pending.Confirm.t) =
  match c.agent_id with
  | None -> ""
  | Some agent -> sprintf "[%s] " agent
;;

let title (c : P.Pending.Confirm.t) =
  let what =
    match c.name with
    | "exit_plan_mode" -> "Approve the plan and start implementing?"
    | "bash" -> sprintf "Run bash: %s?" c.summary
    | "write" -> sprintf "Write %s?" c.summary
    | "edit" -> sprintf "Edit %s?" c.summary
    | "apply_patch" -> sprintf "Apply patch to %s?" c.summary
    | name when String.is_empty c.summary -> sprintf "Run %s?" name
    | name -> sprintf "Run %s: %s?" name c.summary
  in
  who c ^ what
;;

let answers (c : P.Pending.Confirm.t) =
  if is_plan c
  then
    [ Answer.Allow, "y", "approve: leave plan mode and implement"
    ; Deny, "n", "keep planning"
    ; Deny_with_reason, "r", "keep planning, saying what to change"
    ]
  else
    [ Answer.Allow, "y", "allow once" ]
    @ (match c.rules with
       | [] -> []
       | rules ->
         [ ( Answer.Always
           , "a"
           , sprintf
               "always allow %s this session"
               (String.concat ~sep:", " rules) )
         ])
    @ [ Deny, "n", "deny"; Deny_with_reason, "r", "deny, saying why" ]
;;

let reason_question c =
  if is_plan c
  then "What should change in the plan?"
  else "Why not? (the model sees it)"
;;

let params (c : P.Pending.Confirm.t) (answer : Answer.t) ~reason =
  let allow, always =
    match answer with
    | Allow -> true, false
    | Always -> true, true
    | Deny | Deny_with_reason -> false, false
  in
  [ "call_id", P.Json.str c.call_id; "allow", P.Json.bool allow ]
  @ (if always then [ "always", P.Json.bool true ] else [])
  @ Option.value_map c.agent_id ~default:[] ~f:(fun a ->
    [ "agent_id", P.Json.str a ])
  @
  match reason with
  | Some r when (not allow) && not (String.is_empty (String.strip r)) ->
    [ "reason", P.Json.str (String.strip r) ]
  | _ -> []
;;

let acknowledgement (c : P.Pending.Confirm.t) (answer : Answer.t) =
  match is_plan c, answer with
  | true, Allow -> "plan approved"
  | true, (Deny | Deny_with_reason) -> "plan not approved: still planning"
  | false, Allow -> sprintf "allowed %s" c.name
  | false, Always ->
    sprintf "allowed %s; always: %s" c.name (String.concat ~sep:", " c.rules)
  | false, (Deny | Deny_with_reason) -> sprintf "denied %s" c.name
  | true, Always -> "plan approved"
;;
