open! Core
open! Import

type t =
  { promise : unit Promise.t
  ; resolver : unit Promise.u
  }

let create () =
  let promise, resolver = Promise.create () in
  { promise; resolver }
;;

let never = create ()
let is_cancelled t = Promise.is_resolved t.promise
let cancel t = if not (is_cancelled t) then Promise.resolve t.resolver ()
let await t = Promise.await t.promise

let protect t ~f =
  if is_cancelled t
  then None
  else
    Fiber.first
      (fun () -> Some (f ()))
      (fun () ->
         await t;
         None)
;;
