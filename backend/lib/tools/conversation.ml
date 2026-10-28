open! Core
open! Import

type t =
  { model : Model.t
  ; thinking : Thinking.t
  ; system : string option
  ; messages : Message.t list
  }
