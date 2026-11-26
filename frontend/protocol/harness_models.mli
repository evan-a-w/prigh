open! Core

(** Where a composer session's agent CLI is with listing its models, while
    the session's model is still its placeholder ([state]'s
    [harness_models]): being asked, or why it could not and what to do. *)

module Problem : sig
  type t =
    | Not_installed
    | Not_logged_in
    | Login_refused (** prigh's login it runs on was refused *)
    | Unreachable (** its tool host is not connected *)
    | Failed
  [@@deriving sexp_of, equal]
end

module Fix : sig
  type t =
    | Login of { provider : string } (** [/login PROVIDER] *)
    | Engines (** Settings → Engines ([/adapters refresh]) *)
    | Retry (** [/adapters retry] *)
  [@@deriving sexp_of, equal]

  (** What a button for it says. *)
  val label : t -> string

  (** The command line that does it. *)
  val command : t -> string
end

type t =
  | Loading
  | Failed of
      { problem : Problem.t
      ; summary : string (** for the chip: ["not logged in on desktop"] *)
      ; message : string (** why, and what to do *)
      ; fixes : Fix.t list
      }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** What the model chip says in place of the placeholder's name. *)
val chip : t -> string
