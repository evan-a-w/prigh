open! Core
open! Import

(** What [render_ui] needs from the agent running it ({!Tool.context}'s
    [ui]). *)

module Problem : sig
  type t =
    | File of string (** what is wrong with the file: missing, a directory... *)
    | Host of string
    (** why no file can be read: no tool host, or one too old; it says what
        to do *)
end

type t =
  { client : unit -> Client_kind.t
    (** the kind of client that sent the latest user prompt *)
  ; read_head : string -> length:int -> (int * string, Problem.t) Result.t
    (** the size and at most the first [length] bytes of the regular file
        at an absolute path on the session's tool host (following
        symlinks), without waiting for a host that is away *)
  }

(** No user that can see components, and no files. *)
val none : t
