open! Core
open! Import

module Session = struct
  type t =
    | Switching of string
    | Starting
  [@@deriving sexp_of, equal]
end

module Prompt = struct
  type t =
    { text : string
    ; images : Image.t list
    }
  [@@deriving sexp_of, equal]
end

type t =
  { session : Session.t option
  ; messages : bool
  ; refreshing : bool
  ; prompts : Prompt.t list
  ; sending : int
  ; model : string option
  ; thinking : string option
  ; mode : Session_mode.t option
  }
[@@deriving sexp_of]

let none =
  { session = None
  ; messages = false
  ; refreshing = false
  ; prompts = []
  ; sending = 0
  ; model = None
  ; thinking = None
  ; mode = None
  }
;;

let loading t = Option.is_some t.session || t.messages

let sent t prompt =
  { t with
    sending = t.sending + 1
  ; prompts = t.prompts @ Option.to_list prompt
  }
;;

let answered t ~failed =
  let rec remove = function
    | [] -> []
    | p :: rest when Option.equal Prompt.equal (Some p) failed -> rest
    | p :: rest -> p :: remove rest
  in
  { t with
    sending = Int.max 0 (t.sending - 1)
  ; prompts = (if Option.is_some failed then remove t.prompts else t.prompts)
  }
;;

let prompt_shown t = { t with prompts = List.drop t.prompts 1 }
