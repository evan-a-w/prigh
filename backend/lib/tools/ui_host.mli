open! Core
open! Import

(** What [render_ui] needs from the agent running it ({!Tool.context}'s
    [ui]). *)

type t =
  { client : unit -> Client_kind.t
    (** the kind of client that sent the latest user prompt *)
  ; read_head : string -> length:int -> (int * string) Or_error.t
    (** the size and at most the first [length] bytes of the regular file
        at an absolute path on the session's tool host; the error says why
        not (missing, a directory, ...) *)
  }

(** No user that can see components, and no files. *)
val none : t
