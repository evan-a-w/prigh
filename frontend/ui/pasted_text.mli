open! Core

(** Long pastes go with the message as pasted texts rather than into the
    editor: the model sees each whole, as a [<pasted_text>] block behind what
    was typed, and the frontends show a chip ([Pasted text #N], its lines),
    so the editor stays short and quick. prigh-web makes a chip of them; the
    TUI puts a token ([[Pasted text #1 +420 lines]]) in the editor. *)

(** A paste is long past either: [long_chars] characters (scalar values) or
    [long_lines] lines. *)
val long_chars : int

val long_lines : int
val is_long : string -> bool

(** The most a pasted text may be, in bytes (the backend's limit too). *)
val max_bytes : int

(** Why [text] cannot be sent as a pasted text, and what to do instead. *)
val too_large : string -> string option

(** Lines: a last one without a newline counts. *)
val count_lines : string -> int

(** [Pasted text #N], the name the backend gives the [n]th of a message. *)
val name : int -> string

(** E.g. [420 lines · 18.4 KB]. *)
val summary : lines:int -> bytes:int -> string

(** Its first line that is not blank, cut to [max] characters (default 60). *)
val preview : ?max:int -> string -> string

(** The TUI's token for one in the editor: [[Pasted text #N +L lines]]. *)
val token : n:int -> lines:int -> string

(** [text] with each [(old, new)] pair's first [old] (in [text] as given)
    replaced by [new], all at once. *)
val replace_tokens : string -> (string * string) list -> string

(** The [prompt], [steer] or [follow_up] params for [text] and the [pasted]
    texts: [pasted] when the backend takes it ([Hello_reply.pasted_text]),
    else the blocks in [text] as the backend would make them. *)
val params
  :  backend_takes:bool
  -> text:string
  -> string list
  -> (string * Prigh_protocol.Json.t) list

(** All of [text] inline, as the backend would send it: for subagents and
    backends without [pasted]. *)
val inline : text:string -> string list -> string

(** One of the TUI's: its token in the editor, and the text it stands for. *)
module Token : sig
  type t =
    { token : string
    ; text : string
    }
  [@@deriving sexp_of]
end

(** [text] to send, the tokens of [tokens] in it named [[Pasted text #K]] in
    the order they appear (or dropped, when nothing else was typed), and the
    texts they stand for, in that order. *)
val resolve : string -> Token.t list -> string * string list
