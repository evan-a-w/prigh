open! Core
open! Import

(** Codex on one of prigh's ChatGPT logins ({!Cli_credential}): its access
    token, handed to the process over stdin ({!Codex_codec.login_tokens}),
    never in its environment or a file. Codex keeps it in memory and asks
    for another when refused ({!Codex_codec.Output.Token_refresh}); prigh
    refreshes it (in the owner's store, for a shared login), and hands a
    fresh one before a turn when the one Codex has is about to expire. *)

type t [@@deriving sexp_of]

val create : Cli_credential.t -> t
val credential : t -> Cli_credential.t

(** A token for Codex, valid for ten minutes at least; [refused]: Codex was
    refused with the last one handed, so a newer one is wanted. Remembers
    what it handed. *)
val tokens : t -> refused:bool -> Codex_codec.Chatgpt_tokens.t Or_error.t

(** Whether the token last handed expires within ten minutes of [now_ms]
    (Unix ms): the next turn should get a fresh one. *)
val expiring : t -> now_ms:int -> bool

(** Why the session runs on Codex's own login instead, and what to do. *)
val fallback_notice
  :  t
  -> version:string option
  -> [ `Refused of string (** Codex refused it, saying this *)
     | `No_token of Error.t (** prigh had none to hand *)
     ]
  -> string
