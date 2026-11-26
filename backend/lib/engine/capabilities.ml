open! Core
open! Import

module History_op = struct
  type t =
    | Unsupported
    | Native
    | By_handoff
  [@@deriving sexp_of, equal]
end

module Compact = struct
  type t =
    | Unsupported
    | Native
    | Auto_only
    | Command
    | Summary
  [@@deriving sexp_of, equal]
end

module Steer = struct
  type t =
    | Between_turns
    | In_run
  [@@deriving sexp_of, equal]
end

module Subagents = struct
  type t =
    | Unsupported
    | Observe
    | Full
  [@@deriving sexp_of, equal]
end

type t =
  { fork : History_op.t
  ; rewind : History_op.t
  ; context_edit : bool
  ; compact : Compact.t
  ; steer : Steer.t
  ; plan_mode : bool
  ; btw : bool
  ; subagents : Subagents.t
  ; model_fallback : bool
  ; images : bool
  ; ask_user : bool
  ; rewind_note : string option [@sexp.option]
  }
[@@deriving sexp_of]

let harness =
  { fork = Native
  ; rewind = Native
  ; context_edit = true
  ; compact = Native
  ; steer = In_run
  ; plan_mode = true
  ; btw = true
  ; subagents = Full
  ; model_fallback = true
  ; images = true
  ; ask_user = true
  ; rewind_note = None
  }
;;

let none =
  { fork = Unsupported
  ; rewind = Unsupported
  ; context_edit = false
  ; compact = Unsupported
  ; steer = Between_turns
  ; plan_mode = false
  ; btw = false
  ; subagents = Observe
  ; model_fallback = false
  ; images = false
  ; ask_user = false
  ; rewind_note = None
  }
;;

module Feature = struct
  type t =
    | Fork
    | Rewind
    | Context_edit
    | Compact
    | Downsize_images
    | Btw
    | Steer_subagent
    | Plan_mode
  [@@deriving sexp_of, equal, enumerate]
end

let supports t (feature : Feature.t) =
  match feature with
  | Fork -> not (History_op.equal t.fork Unsupported)
  | Rewind -> not (History_op.equal t.rewind Unsupported)
  | Context_edit | Downsize_images -> t.context_edit
  | Compact ->
    (match t.compact with
     | Native | Command | Summary -> true
     | Unsupported | Auto_only -> false)
  | Btw -> t.btw
  | Steer_subagent -> Subagents.equal t.subagents Full
  | Plan_mode -> t.plan_mode
;;

let method_feature : string -> Feature.t option = function
  | "fork" | "clone" -> Some Fork
  | "rewind" -> Some Rewind
  | "trim" | "trim_after" | "untrim" | "delete" | "undelete" ->
    Some Context_edit
  | "compact" -> Some Compact
  | "downsize_images" -> Some Downsize_images
  | "btw" -> Some Btw
  | "steer_agent" | "dequeue_agent" | "remove_agent_queued" ->
    Some Steer_subagent
  | _ -> None
;;

let refusal t (feature : Feature.t) ~who =
  match feature with
  | Fork ->
    sprintf "%s sessions can't be forked: start a new session (/new)" who
  | Rewind when Option.is_some t.rewind_note ->
    sprintf
      "%s sessions can't rewind here: %s"
      who
      (Option.value_exn t.rewind_note)
  | Rewind ->
    if supports t Fork
    then
      sprintf
        "%s sessions can't rewind in place: fork from that message instead \
         (/fork)"
        who
    else
      sprintf
        "%s sessions can't rewind: start a new session (/new) to begin again"
        who
  | Context_edit ->
    sprintf
      "%s sessions can't trim or delete messages, the agent keeps its own \
       context: %s"
      who
      (if supports t Compact
       then "compact it instead (/compact)"
       else "start a new session (/new) for a fresh one")
  | Downsize_images ->
    sprintf
      "%s sessions can't downsize the images already sent: start a new session \
       (/new) with smaller images"
      who
  | Compact ->
    (match t.compact with
     | Auto_only ->
       sprintf
         "%s compacts its context by itself when it fills: /compact isn't \
          available"
         who
     | Native | Command | Summary | Unsupported ->
       sprintf
         "%s sessions can't be compacted: start a new session (/new) for a \
          fresh context"
         who)
  | Btw ->
    sprintf
      "%s sessions have no side questions (/btw): ask in the session instead"
      who
  | Steer_subagent ->
    sprintf
      "%s sessions show their subagents but can't steer them: steer the \
       session itself instead"
      who
  | Plan_mode ->
    sprintf
      "%s sessions have no plan mode: ask for a plan in your message instead"
      who
;;

let check t feature ~who =
  if supports t feature
  then Ok ()
  else Or_error.error_string (refusal t feature ~who)
;;
