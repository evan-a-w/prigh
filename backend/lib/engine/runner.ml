open! Core
open! Import

module type S = Engine_intf.Runner

type t =
  | T :
      { runner : (module S with type t = 'a)
      ; value : 'a
      ; close : unit -> unit
      }
      -> t

let create ?(close = ignore) runner value = T { runner; value; close }
let close (T { close; _ }) = close ()
let engine (T { runner = (module R); _ }) = R.engine
let capabilities (T { runner = (module R); value; _ }) = R.capabilities value

let run_turn (T { runner = (module R); value; _ }) services ~cancel input =
  R.run_turn value services ~cancel input
;;
