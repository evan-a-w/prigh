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

let who (c : P.Pending.Confirm.t) =
  match c.agent_id with
  | None -> ""
  | Some agent -> sprintf "[%s] " agent
;;

let title (c : P.Pending.Confirm.t) =
  let what =
    match c.name with
    | "bash" -> sprintf "Run bash: %s?" c.summary
    | "write" -> sprintf "Write %s?" c.summary
    | "edit" -> sprintf "Edit %s?" c.summary
    | "apply_patch" -> sprintf "Apply patch to %s?" c.summary
    | "job_input" -> sprintf "Send to job %s?" c.summary
    | name when String.is_empty c.summary -> sprintf "Run %s?" name
    | name -> sprintf "Run %s: %s?" name c.summary
  in
  who c ^ what
;;

let answers (c : P.Pending.Confirm.t) =
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

let reason_question = "Why not? (the model sees it)"

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
  match answer with
  | Allow -> sprintf "allowed %s" c.name
  | Always ->
    sprintf "allowed %s; always: %s" c.name (String.concat ~sep:", " c.rules)
  | Deny | Deny_with_reason -> sprintf "denied %s" c.name
;;
