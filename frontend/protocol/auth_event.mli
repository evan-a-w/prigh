open! Core

module Prompt : sig
  type t =
    | Secret of
        { message : string
        ; allow_empty : bool
        }
    | Text of
        { message : string
        ; placeholder : string
        ; default : string (** prefilled *)
        }
    | Manual_code of
        { message : string
        ; placeholder : string
        }
    | Select of
        { message : string
        ; options : (string * string) list
        }
  [@@deriving sexp_of, equal]

  val message : t -> string
end

type t =
  | Auth_url of
      { url : string
      ; instructions : string
      }
  | Prompt of
      { id : string
      ; prompt : Prompt.t
      }
  | Prompt_cancelled of { id : string }
  | Progress of string
  | Done of
      { provider : string
      ; method_ : string
      }
  | Failed of
      { provider : string
      ; error : string
      }
  | Logged_out of string
  | Forge_done of
      { host : string
      ; user : string option
      ; method_ : string (** [oauth] or [token] *)
      } (** signed in to a git host; [Failed] names the host otherwise *)
  | Forge_logged_out of string (** the host *)
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
