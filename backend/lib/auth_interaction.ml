open! Core
open! Import

module Prompt = struct
  type t =
    | Secret of { message : string }
    | Manual_code of
        { message : string
        ; placeholder : string
        }
    | Select of
        { message : string
        ; options : (string * string) list
        }
  [@@deriving sexp_of]
end

module Notice = struct
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

let cancelled () = Or_error.error_string "login cancelled"

let scripted ?(notify = ignore) ?(cancel = Cancellation.create ()) answers =
  let remaining = ref answers in
  let prompt _ =
    match !remaining with
    | [] -> cancelled ()
    | answer :: rest ->
      remaining := rest;
      Ok answer
  in
  { prompt; notify; cancel }
;;

let run t ~f =
  match Cancellation.protect t.cancel ~f with
  | None -> cancelled ()
  | Some result -> result
;;
