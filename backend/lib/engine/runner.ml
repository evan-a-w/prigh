open! Core
open! Import

module type S = Engine_intf.Runner

type t =
  | T :
      { runner : (module S with type t = 'a)
      ; value : 'a
      ; close : unit -> unit
      ; rewind : Session.t -> to_:string -> unit Or_error.t
      }
      -> t

let create ?(close = ignore) ?(rewind = fun _ ~to_:_ -> Ok ()) runner value =
  T { runner; value; close; rewind }
;;

let close (T { close; _ }) = close ()
let rewind (T { rewind; _ }) session ~to_ = rewind session ~to_
let engine (T { runner = (module R); _ }) = R.engine
let capabilities (T { runner = (module R); value; _ }) = R.capabilities value

let run_turn (T { runner = (module R); value; _ }) services ~cancel input =
  R.run_turn value services ~cancel input
;;
