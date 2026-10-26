open! Core
open! Import

(** The first tier of context compaction: old tool outputs, and large
    arguments of old tool calls, are replaced with short placeholders, while
    the conversation itself (the user's messages, the model's text,
    reasoning and tool calls) stays. It is far cheaper than a summary
    ({!Compaction}) and keeps the record of what was done.

    Masking changes the prompt's prefix, which the provider caches, so it is
    done in batches: everything before the last [keep_turns] assistant
    messages at once, only when the context is large ({!worth_masking}). A
    session records the boundary ({!Session.messages} masks up to it), so
    that a resumed session sends the same context. *)

(** Assistant messages at the end of the conversation never masked. *)
val keep_turns : int

(** Tool output (and tool-call arguments) shorter than this many bytes is
    kept. *)
val min_bytes : int

(** The message with its tool output, or its tool calls' large string
    arguments, replaced with placeholders saying what was there; unchanged
    when there is nothing large to mask. Error results are kept, and
    arguments stay valid JSON. Images in a tool result are dropped (the
    placeholder says how many there were). Masking a masked message changes
    nothing. *)
val mask : Message.t -> Message.t

module Plan : sig
  type t =
    { through : int (** the index of the last message it changes *)
    ; masked : int (** how many messages it changes *)
    ; tokens_saved : int (** estimated as {!Message.estimate_tokens} *)
    }
  [@@deriving sexp_of]
end

(** Masking every message before the last [keep_turns] (default
    {!keep_turns}) assistant messages; [None] when that changes nothing. *)
val plan : ?keep_turns:int -> Message.t list -> Plan.t option

(** [mask]s the messages up to index [through]. *)
val apply : through:int -> Message.t list -> Message.t list

(** Whether to carry out [plan] on a context of [input_tokens]: when it is
    [urgent] (the context would otherwise be summarised, or does not fit),
    or when the context is over half the model's window and masking frees at
    least a tenth of the window. The second condition keeps it from
    re-masking (and so invalidating the provider's cache) every turn. *)
val worth_masking : Model.t -> input_tokens:int -> urgent:bool -> Plan.t -> bool
