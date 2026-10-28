open! Core
open! Import

(** The prigh slash commands an ACP client offers ([available_commands]):
    those that make sense in an editor's chat, each run by the RPC method
    the TUI uses for it. Any other [/name] is sent to the model as typed (a
    skill's [/name] is expanded by prigh). *)

module Command : sig
  type t =
    { name : string
    ; description : string
    ; hint : string option (** what the argument is, if it takes one *)
    }

  (** An ACP [AvailableCommand]. *)
  val to_json : t -> Json.t
end

val builtin : Command.t list

module Action : sig
  type t =
    | Call of
        { method_ : string
        ; params : (string * Json.t) list
        ; turn : bool (** it starts a run, which the prompt waits for *)
        ; ack : Json.t -> string (** what to show, given the result *)
        }
    | Reply of string (** shown as the agent's message: nothing to call *)
    | Prompt (** not one of ours: the text goes to the model *)
  [@@deriving sexp_of]
end

(** [text] is the prompt; [state] the session's last [state]. *)
val parse : state:Json.t -> string -> Action.t
