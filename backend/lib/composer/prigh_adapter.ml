open! Core
open! Import

module R = struct
  type t = unit -> Runner.t

  let engine = Engine_id.Composer
  let capabilities _ = Capabilities.harness

  let run_turn harness services ~cancel input =
    Runner.run_turn (harness ()) services ~cancel input
  ;;
end

let runner ~harness = Runner.create (module R) harness
