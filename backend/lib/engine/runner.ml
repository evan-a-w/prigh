open! Core
open! Import

module type S = Engine_intf.Runner

type t = T : (module S with type t = 'a) * 'a -> t

let create m r = T (m, r)
let engine (T ((module R), _)) = R.engine
let capabilities (T ((module R), r)) = R.capabilities r

let run_turn (T ((module R), r)) services ~cancel input =
  R.run_turn r services ~cancel input
;;
