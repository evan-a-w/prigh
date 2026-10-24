open! Core

(** Lossy UTF-8 repair. Byte strings from subprocesses and files may be cut
    mid-character or not be UTF-8 at all; anything we store in a session or
    send to a provider must be valid. *)

val replacement : string

(** Replaces every byte that is not part of a well-formed UTF-8 sequence
    (including overlongs, surrogates and code points above U+10FFFF) with
    U+FFFD. Valid input is returned unchanged. *)
val sanitize : string -> string

val is_valid : string -> bool

(** Splits off a trailing sequence that could be the start of a character
    that continues in the next chunk. [(complete, pending)] where [pending]
    is at most 3 bytes; [complete ^ pending] is the input. *)
val split_incomplete_suffix : string -> string * string
