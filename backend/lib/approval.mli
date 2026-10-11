open! Core
open! Import

(** Whether a tool call may run: the session's {!Approval_mode}, whether it
    is in plan mode, the rules of [~/.prigh/config.json]'s [permissions] and
    the rules the user added for the session ("don't ask again"). *)

(** [permissions] in [config.json]: [{"allow": [...], "ask": [...], "deny":
    [...]}], each a list of {!Approval_rule}s. *)
module Rules : sig
  type t =
    { allow : Approval_rule.t list
    ; ask : Approval_rule.t list
    ; deny : Approval_rule.t list
    }
  [@@deriving sexp_of, equal]

  val empty : t
  val to_json : t -> Json.t

  (** Invalid rules are left out and returned as problems naming the list
      and saying what is wrong; a value of the wrong shape is an error. *)
  val of_json : Json.t -> (t * string list) Or_error.t
end

module Decision : sig
  type t =
    | Allow
    | Ask of { rules : Approval_rule.t list }
    (** with the rules "don't ask again" would add; none when an ask rule
        matched, since allow rules cannot override it *)
    | Deny of string (** the tool result the model sees *)
  [@@deriving sexp_of]
end

(** The tools that change files: [edit], [write], [apply_patch]. *)
val is_edit : string -> bool

(** In order: a deny rule touching the call denies it; plan mode denies file
    edits; an ask rule touching it asks; an allow rule (the config's or the
    session's) covering it allows it; plan mode asks for every other
    destructive tool; then the mode decides for destructive tools ([Ask]
    asks, [Auto_edit] asks unless it is an edit, [Auto] allows), and
    non-destructive ones run. *)
val decide
  :  mode:Approval_mode.t
  -> plan:bool
  -> rules:Rules.t
  -> session:Approval_rule.t list
  -> cwd:string
  -> home:string
  -> destructive:bool
  -> Approval_rule.Call.t
  -> Decision.t

(** The tool result of a call refused in plan mode. *)
val plan_mode_refusal : string
