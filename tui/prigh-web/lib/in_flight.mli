open! Core
open! Import

(** What we asked the backend for and are waiting on, so that the page shows
    every action at once: the session being opened, its transcript loading,
    prompts sent but not yet in the transcript, the model, thinking level or
    mode chosen but not yet confirmed. *)

module Session : sig
  type t =
    | Switching of string (** the path clicked *)
    | Starting (** a new session, a clone or a fork *)
  [@@deriving sexp_of, equal]
end

module Prompt : sig
  type t =
    { text : string
    ; images : Image.t list
    }
  [@@deriving sexp_of, equal]
end

(** A prompt sent as the connection went: whether it reached the agent, the
    transcript (or the queue) says once we are back. *)
module Unsure : sig
  type t =
    { prompt : Prompt.t
    ; seen : int (** user messages with its text on show when it went *)
    }
  [@@deriving sexp_of]
end

type t =
  { session : Session.t option (** until its state arrives *)
  ; messages : bool (** the session's transcript, until it arrives *)
  ; refreshing : bool
    (** a cached transcript is on show, its newest page on the way *)
  ; prompts : Prompt.t list
    (** sent, oldest first, until the transcript has them *)
  ; sending : int (** prompts, steers and follow-ups not yet answered *)
  ; unsure : Unsure.t list
  ; model : string option (** a model key, until the backend confirms it *)
  ; thinking : string option
  ; mode : Session_mode.t option (** approval mode and plan mode *)
  }
[@@deriving sexp_of]

val none : t

(** A prompt has been sent: it is shown until [prompt_shown]. *)
val sent : t -> Prompt.t option -> t

(** The backend answered a prompt, steer or follow-up; one that failed is no
    longer shown. *)
val answered : t -> failed:Prompt.t option -> t

(** The transcript has the oldest prompt shown. *)
val prompt_shown : t -> t
