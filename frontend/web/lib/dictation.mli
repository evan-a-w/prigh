open! Core

(** Dictation into the composer: what its microphone button is doing. The
    page records (app/dictation_widget.ml) and the backend transcribes
    ([transcribe], with the [dictation_model]); where the backend cannot,
    the browser's own speech recognition listens instead. *)

module Engine : sig
  type t =
    | Server (** recorded here, transcribed by the backend *)
    | Browser (** the browser's SpeechRecognition *)
  [@@deriving sexp_of, equal]
end

type t =
  | Idle
  | Starting of Engine.t (** waiting for the microphone (and permission) *)
  | Recording of
      { engine : Engine.t
      ; elapsed_ms : int
      ; level : float (** the input's loudness, 0 to 1 *)
      }
  | Transcribing (** the recording is with the backend (or the browser) *)
[@@deriving sexp_of, equal]

val active : t -> bool

(** [m:ss] *)
val elapsed : int -> string

(** The longest recording, after which it stops by itself. *)
val max_ms : int

(** Whether a [transcribe] error says no provider can transcribe (so the
    browser's recognition should listen instead). *)
val no_provider : string -> bool

(** [text] put into [draft] at the byte [cursor], with a space between it
    and a word either side; the result and the cursor after it. *)
val insert : draft:string -> cursor:int -> string -> string * int
