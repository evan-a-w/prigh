open! Core
open Prigh_ui

module Intent_key = struct
  type t = Intent.t

  let compare = Intent.compare
  let hash = Hashtbl.hash
  let sexp_of_t = Intent.sexp_of_t
end

let hit : Intent.t Hash_set.t = Hash_set.create (module Intent_key)
let record intent = Hash_set.add hit intent
let record_key key = Option.iter (Keymap.lookup key) ~f:record
let covered intent = Hash_set.mem hit intent
