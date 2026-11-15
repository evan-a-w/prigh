open! Core

(* [contents] is [flat] then [pending_rev] reversed; joining them changes the
   representation, not the value, so versions can share it. *)
type t =
  { mutable flat : string
  ; mutable pending_rev : string list
  }

let of_string flat = { flat; pending_rev = [] }
let append t s = { flat = t.flat; pending_rev = s :: t.pending_rev }

let contents t =
  (match t.pending_rev with
   | [] -> ()
   | pending ->
     t.flat <- String.concat (t.flat :: List.rev pending);
     t.pending_rev <- []);
  t.flat
;;

let sexp_of_t t = String.sexp_of_t (contents t)
