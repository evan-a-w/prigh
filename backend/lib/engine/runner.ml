open! Core
open! Import

module type S = Engine_intf.Runner

type t =
  | T :
      { runner : (module S with type t = 'a)
      ; value : 'a
      ; close : unit -> unit
      ; rewind : Session.t -> to_:string -> unit Or_error.t
      ; limits : unit -> Subscription_usage.Status.t option
      ; live : unit -> bool
      ; prewarm : Engine_intf.Services.t -> Engine_intf.Turn.Input.t -> unit
      ; compacted : unit -> unit
      }
      -> t

let create
      ?(close = ignore)
      ?(rewind = fun _ ~to_:_ -> Ok ())
      ?(limits = fun () -> None)
      ?(live = fun () -> false)
      ?(prewarm = fun _ _ -> ())
      ?(compacted = ignore)
      runner
      value
  =
  T { runner; value; close; rewind; limits; live; prewarm; compacted }
;;

let close (T { close; _ }) = close ()
let rewind (T { rewind; _ }) session ~to_ = rewind session ~to_
let engine (T { runner = (module R); _ }) = R.engine
let capabilities (T { runner = (module R); value; _ }) = R.capabilities value

let run_turn (T { runner = (module R); value; _ }) services ~cancel input =
  R.run_turn value services ~cancel input
;;

let models (T { runner = (module R); value; _ }) = R.models value
let context (T { runner = (module R); value; _ }) = R.context value
let limits (T { limits; _ }) = limits ()
let live (T { live; _ }) = live ()
let prewarm (T { prewarm; _ }) services input = prewarm services input
let compacted (T { compacted; _ }) = compacted ()

