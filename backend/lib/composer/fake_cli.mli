open! Core
open! Import

(** A stand-in for a harness's CLI ([codex], [claude], an ACP agent), for
    tests that drive a real backend end to end: [prigh fake-cli NAME --script
    FILE -- ARGS] behaves as the CLI run with [ARGS] would, by the script.

    A script is JSON lines in the replay fixtures' format
    ([backend/test/fixtures/codex/*.ndjson]):
    - [{"type":"expect_outbound","frame":F}] waits for a line from prigh
      that matches [F]: the same ["method"] (a JSON-RPC request or
      notification), or the same ["type"] (and [request.subtype]: Claude's
      stream-json), or, for a frame with an ["id"] and no ["method"], the
      response to the request with that id. The ids prigh gives its requests
      are remembered for the responses that follow.
    - [{"type":"emit_inbound","frame":F}] writes [F], a response's id (or a
      [control_response]'s [request_id]) replaced by the id of the request
      it answers.
    - [{"type":"exit","code":N,"stderr":S}] exits there (a crash).
    - [{"type":"command","args":[...],"exit":N,"stdout":S,"stderr":S}]:
      when the CLI is run with exactly those arguments (e.g. [--version],
      [login status]), print and exit instead.
    - [{"type":"default_reply","method":M,"result":R}] (or ["subtype"] and
      ["response"] for Claude's control requests): the answer to a request
      the script does not expect at that point; without one, an empty
      object.

    Lines that match nothing expected are answered (requests) or ignored.
    After the script's end, a request with no [default_reply] ends the CLI
    (exit 3) with a message naming it, so a test whose script has drifted
    from what prigh sends fails at once rather than waiting for a reply.
    Other types (the fixtures' [runtime_exit]) are skipped. *)

module Script : sig
  type t

  val parse : string -> t Or_error.t
end

(** What a [command] entry for these arguments says: exit code, stdout,
    stderr. *)
val command : Script.t -> string list -> (int * string * string) option

type t

val create : Script.t -> t

(** The lines to write before reading any. *)
val start : t -> string list

(** The lines to write in answer to one from prigh. *)
val handle_line : t -> string -> string list

(** Set once an [exit] entry is reached: its code and stderr. *)
val exit_requested : t -> (int * string) option

(** [prigh fake-cli]'s main: [args] are what follows [fake-cli]. *)
val main : string list -> unit
