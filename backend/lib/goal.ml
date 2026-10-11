open! Core
open! Import

module Status = struct
  type t =
    | Active
    | Done of { summary : string }
    | Stopped of { reason : string }
  [@@deriving sexp, jsonaf, equal]

  let to_string = function
    | Active -> "active"
    | Done _ -> "done"
    | Stopped _ -> "stopped"
  ;;
end

type t =
  { text : string
  ; status : Status.t
  ; continuations : int
  }
[@@deriving sexp, jsonaf, equal]

let create text = { text; status = Active; continuations = 0 }

let is_active t =
  match t.status with
  | Active -> true
  | Done _ | Stopped _ -> false
;;

let default_max_continuations = 20
let tool_name = "goal_complete"

let start_message t =
  sprintf
    "[prigh goal] Work on this goal until it is fully achieved:\n\n\
     <goal>\n\
     %s\n\
     </goal>\n\n\
     You keep working across turns: when you end a turn while the goal is \
     active, you are asked to continue. Once current evidence (files, command \
     output, tests) proves every part of it is done, call %s with a short \
     summary of what was achieved."
    t.text
    tool_name
;;

let continuation_message t ~max_continuations =
  sprintf
    "[prigh goal: continuation %d of %d] Continue working toward the goal:\n\n\
     <goal>\n\
     %s\n\
     </goal>\n\n\
     Check the current state rather than relying on memory, then take the next \
     concrete step toward the full goal; do not narrow it to what is easy. If \
     every requirement is met and verified, call %s with a short summary \
     instead. If you are blocked on something only the user can resolve, say \
     so plainly and end your turn."
    t.continuations
    max_continuations
    t.text
    tool_name
;;

let describe t ~max_continuations =
  let state =
    match t.status with
    | Active ->
      sprintf
        "active, %d of %d automatic continuations used; /goal off clears it"
        t.continuations
        max_continuations
    | Done { summary } -> sprintf "done: %s" summary
    | Stopped { reason } ->
      sprintf "stopped (%s); /goal resume continues it" reason
  in
  sprintf "goal: %s\n%s" t.text state
;;
