open! Core

(* A value a newer backend adds reads as [unknown], the most restrictive:
   the frontend offers less rather than failing the whole state. *)
let enum_field j name ~cases ~default ~unknown =
  match Json.field j name with
  | None | Some `Null -> Ok default
  | Some (`String s) ->
    Ok
      (Option.value
         (List.Assoc.find cases s ~equal:String.equal)
         ~default:unknown)
  | Some _ -> Or_error.errorf "%s: expected a string" name
;;

module History_op = struct
  type t =
    | Unsupported
    | Native
    | By_handoff
  [@@deriving sexp_of, equal]

  let cases =
    [ "unsupported", Unsupported; "native", Native; "by_handoff", By_handoff ]
  ;;
end

module Compact = struct
  type t =
    | Unsupported
    | Native
    | Auto_only
  [@@deriving sexp_of, equal]

  let cases =
    [ "unsupported", Unsupported; "native", Native; "auto_only", Auto_only ]
  ;;
end

module Steer = struct
  type t =
    | Between_turns
    | In_run
  [@@deriving sexp_of, equal]

  let cases = [ "between_turns", Between_turns; "in_run", In_run ]
end

module Subagents = struct
  type t =
    | Unsupported
    | Observe
    | Full
  [@@deriving sexp_of, equal]

  let cases = [ "unsupported", Unsupported; "observe", Observe; "full", Full ]
end

module Capabilities = struct
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
  [@@deriving sexp_of, equal]

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

  (* A field a newer backend adds, or leaves out, keeps the harness's
     value. *)
  let of_json j =
    let open Or_error.Let_syntax in
    let h = harness in
    let bool name ~default = Json.bool_default_field j name ~default in
    let%bind fork =
      enum_field
        j
        "fork"
        ~cases:History_op.cases
        ~default:h.fork
        ~unknown:History_op.Unsupported
    in
    let%bind rewind =
      enum_field
        j
        "rewind"
        ~cases:History_op.cases
        ~default:h.rewind
        ~unknown:History_op.Unsupported
    in
    let%bind context_edit = bool "context_edit" ~default:h.context_edit in
    let%bind compact =
      enum_field
        j
        "compact"
        ~cases:Compact.cases
        ~default:h.compact
        ~unknown:Compact.Unsupported
    in
    let%bind steer =
      enum_field
        j
        "steer"
        ~cases:Steer.cases
        ~default:h.steer
        ~unknown:Steer.Between_turns
    in
    let%bind plan_mode = bool "plan_mode" ~default:h.plan_mode in
    let%bind btw = bool "btw" ~default:h.btw in
    let%bind subagents =
      enum_field
        j
        "subagents"
        ~cases:Subagents.cases
        ~default:h.subagents
        ~unknown:Subagents.Unsupported
    in
    let%bind model_fallback = bool "model_fallback" ~default:h.model_fallback in
    let%bind images = bool "images" ~default:h.images in
    let%bind ask_user = bool "ask_user" ~default:h.ask_user in
    let%map rewind_note = Json.string_opt_field j "rewind_note" in
    { fork
    ; rewind
    ; context_edit
    ; compact
    ; steer
    ; plan_mode
    ; btw
    ; subagents
    ; model_fallback
    ; images
    ; ask_user
    ; rewind_note
    }
  ;;
end

type t =
  { engine : string
  ; adapter : string option [@sexp.option]
  ; capabilities : Capabilities.t
  ; unavailable : string option [@sexp.option]
  }
[@@deriving sexp_of, equal]

let harness =
  { engine = "harness"
  ; adapter = None
  ; capabilities = Capabilities.harness
  ; unavailable = None
  }
;;

let is_harness t = equal t harness

let of_state_json j =
  let open Or_error.Let_syntax in
  let%bind engine = Json.string_opt_field j "engine" in
  let%bind adapter = Json.string_opt_field j "adapter" in
  let%bind capabilities =
    match Json.field j "capabilities" with
    | None | Some `Null -> Ok Capabilities.harness
    | Some c -> Capabilities.of_json c
  in
  let%map unavailable = Json.string_opt_field j "unavailable" in
  { engine = Option.value engine ~default:"harness"
  ; adapter
  ; capabilities
  ; unavailable
  }
;;

let adapter_label = function
  | "codex" -> "Codex"
  | "claude" -> "Claude Code"
  | "prigh" -> "prigh"
  | adapter ->
    (match String.chop_prefix adapter ~prefix:"acp:" with
     | Some "opencode" -> "OpenCode"
     | Some "gemini" -> "Gemini CLI"
     | Some "antigravity" -> "Antigravity"
     | Some "grok" -> "Grok Build"
     | Some "goose" -> "Goose"
     | Some name -> name ^ " (ACP)"
     | None -> adapter)
;;

let engine_label = function
  | "harness" -> "Harness"
  | "composer" -> "Composer"
  | engine -> String.capitalize engine
;;

let who t =
  match t.adapter with
  | Some adapter -> adapter_label adapter
  | None -> engine_label t.engine
;;

let model_label t ~model =
  match t.adapter with
  | Some adapter -> sprintf "%s · %s" (adapter_label adapter) model
  | None when is_harness t || String.equal t.engine "harness" -> model
  | None -> sprintf "%s · %s" (engine_label t.engine) model
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
  let c = t.capabilities in
  match feature with
  | Fork -> not (History_op.equal c.fork Unsupported)
  | Rewind -> not (History_op.equal c.rewind Unsupported)
  | Context_edit | Downsize_images -> c.context_edit
  | Compact -> Compact.equal c.compact Native
  | Btw -> c.btw
  | Steer_subagent -> Subagents.equal c.subagents Full
  | Plan_mode -> c.plan_mode
;;

(* The backend's wording ([Capabilities.refusal]), so an action refused here
   reads as one the backend refused. *)
let refusal t (feature : Feature.t) =
  let who = who t in
  match feature with
  | Fork ->
    sprintf "%s sessions can't be forked: start a new session (/new)" who
  | Rewind when Option.is_some t.capabilities.rewind_note ->
    sprintf
      "%s sessions can't rewind here: %s"
      who
      (Option.value_exn t.capabilities.rewind_note)
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
    (match t.capabilities.compact with
     | Auto_only ->
       sprintf
         "%s compacts its context by itself when it fills: /compact isn't \
          available"
         who
     | Native | Unsupported ->
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

let method_feature ~method_ ~(params : (string * Json.t) list)
  : Feature.t option
  =
  match method_ with
  | "fork" | "clone" -> Some Fork
  | "rewind" -> Some Rewind
  | "trim" | "trim_after" | "untrim" | "delete" | "undelete" ->
    Some Context_edit
  | "compact" -> Some Compact
  | "downsize_images" -> Some Downsize_images
  | "btw" -> Some Btw
  | "steer_agent" | "dequeue_agent" | "remove_agent_queued" ->
    Some Steer_subagent
  | "set_mode" ->
    (match List.Assoc.find params "plan_mode" ~equal:String.equal with
     | Some `True -> Some Plan_mode
     | _ -> None)
  | _ -> None
;;

(* What starts or feeds a run: refused while the session is unavailable. *)
let runs_agent = function
  | "prompt" | "steer" | "follow_up" | "review" | "init" | "review_submit" ->
    true
  | _ -> false
;;

let refuse t ~method_ ~params =
  match t.unavailable with
  | Some why when runs_agent method_ -> Some why
  | _ ->
    Option.bind (method_feature ~method_ ~params) ~f:(fun feature ->
      if supports t feature then None else Some (refusal t feature))
;;
