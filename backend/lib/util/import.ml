open! Core
module Json = Jsonaf
module Fiber = Eio.Fiber
module Promise = Eio.Promise
module Switch = Eio.Switch
include Jsonaf_kernel.Conv

module Env = struct
  type t = Eio_unix.Stdenv.base
end
