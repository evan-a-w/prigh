open! Core
open! Import

(** A tool call let through in [smart] mode ({!Approval_mode}): the approval
    model ({!Approval_judge}) allowed it, or the user did after the model
    held it. The session records it ([allowed_call] entries), and the exact
    same call (tool, arguments, host and directory) runs again without being
    judged for the rest of the session, while in [smart] mode; rules and
    plan mode still come first ({!Approval.decide}).

    A call on another host ([on_host]) the user lets run is recorded too, in
    any mode: it permits that host for the rest of the session. *)

module Call : sig
  type t =
    { tool : string
    ; arguments : string
      (** canonical JSON: an object's fields sorted by name, no spaces *)
    ; host : string (** the tool host's id *)
    ; cwd : string
    }
  [@@deriving sexp, jsonaf, equal]

  include Hashable.S with type t := t

  val create : tool:string -> arguments:Json.t -> host:string -> cwd:string -> t
end

type t =
  { call : Call.t
  ; by : string (** the approval model's key, or [you] *)
  ; reason : string option (** the model's *)
  }
[@@deriving sexp, jsonaf, equal]

val you : string

(** Why the call runs again without being judged, for [Tool_allowed]'s
    [reason]: e.g. ["this exact call earlier (runs the tests)"]. *)
val note : t -> string

(** One line for the permissions listing: the tool, its arguments, host,
    directory and who allowed it. *)
val describe : t -> host_name:(string -> string) -> string
