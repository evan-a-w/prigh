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
    let%map ask_user = bool "ask_user" ~default:h.ask_user in
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
