open! Core

(** How a login flow talks to the user. [prompt] returns [Error] when the user
    cancels; a prompt's fiber may also be cancelled by the flow itself when
    another event (the browser callback) resolves the step first. *)

module Prompt : sig
  type t =
    | Secret of { message : string }
    | Manual_code of
        { message : string
        ; placeholder : string
        }
    | Select of
        { message : string
        ; options : (string * string) list (** id, label *)
        }
  [@@deriving sexp_of]
end

module Notice : sig
  type t =
    | Auth_url of
        { url : string
        ; instructions : string
        }
    | Progress of string
  [@@deriving sexp_of]
end

type t =
  { prompt : Prompt.t -> string Or_error.t
  ; notify : Notice.t -> unit
  ; cancel : Cancellation.t
  }

val cancelled : unit -> _ Or_error.t

(** Answers prompts from a fixed list; cancels once it runs out. *)
val scripted
  :  ?notify:(Notice.t -> unit)
  -> ?cancel:Cancellation.t
  -> string list
  -> t

(** Runs [f], returning [cancelled] if [t.cancel] fires first. *)
val run : t -> f:(unit -> 'a Or_error.t) -> 'a Or_error.t
