open! Core
open! Import

module Problem = struct
  type t =
    | File of string
    | Host of string
end

type t =
  { client : unit -> Client_kind.t
  ; read_head : string -> length:int -> (int * string, Problem.t) Result.t
  }

let none =
  { client = (fun () -> Other)
  ; read_head =
      (fun _ ~length:_ -> Error (Host "there is no tool host to read files on"))
  }
;;
