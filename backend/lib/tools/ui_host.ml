open! Core
open! Import

type t =
  { client : unit -> Client_kind.t
  ; read_head : string -> length:int -> (int * string) Or_error.t
  }

let none =
  { client = (fun () -> Other)
  ; read_head =
      (fun _ ~length:_ -> Or_error.error_string "no tool host to read files on")
  }
;;
