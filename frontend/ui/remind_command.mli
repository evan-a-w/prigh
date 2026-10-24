open! Core
open! Import

(** [/remind]'s arguments, shared by the terminal and browser frontends:
    [/remind] picks among the session's reminders, [/remind stop [N] TEXT]
    and [/remind every N TEXT] add one, [/remind rm NAME] and [/remind off]
    remove them. *)

module Action : sig
  type t =
    | Pick of P.Reminder.t list (** non-empty *)
    | Add of (string * P.Json.t) list
    (** [add_reminder]'s params; {!added} acknowledges the result *)
    | Remove of string (** [remove_reminder] by name *)
    | Clear of int (** [clear_reminders], removing that many *)
    | Info of string
    | Error of string
  [@@deriving sexp_of]
end

(** [/remind]'s usage line. *)
val usage : string

(** [reminders] is [None] until the state has loaded. *)
val run : reminders:P.Reminder.t list option -> string -> Action.t

(** [Pick], or what to do when there is nothing to pick. *)
val pick : P.Reminder.t list option -> Action.t

(** The RPC method and params of [Add], [Remove] and [Clear]. *)
val rpc : Action.t -> (string * (string * P.Json.t) list) option

(** The notice once [add_reminder] returned [result]. *)
val added : P.Json.t -> string

val removed : string -> string
val cleared : int -> string

(** [name · trigger · first line], for pickers and notices. *)
val describe : P.Reminder.t -> string

(** [remind:2] while there are any. *)
val status_part : P.Reminder.t list -> string option

(** What [rest] (all that follows [/remind ], without leading blanks)
    completes: the offset in [rest] of the word being completed, and the
    candidates (unranked) as [(id, detail)]: the subcommands, or the
    session's reminder names after [rm]. *)
val complete
  :  reminders:P.Reminder.t list
  -> string
  -> (int * (string * string) list) option
