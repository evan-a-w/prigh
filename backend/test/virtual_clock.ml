open! Core
open! Prigh

type t =
  { mutable now : float
  ; mutable seq : int
  ; mutable sleepers : (float * int * unit Eio.Promise.u) list
  }

let create () = { now = 0.; seq = 0; sleepers = [] }
let now t = t.now

let sleep t seconds =
  let promise, resolve = Eio.Promise.create () in
  t.seq <- t.seq + 1;
  t.sleepers <- (t.now +. seconds, t.seq, resolve) :: t.sleepers;
  Eio.Promise.await promise
;;

let trace_clock t =
  { Trace.Clock.mono = (fun () -> t.now)
  ; wall = (fun () -> 1_800_000_000. +. t.now)
  }
;;

(* Enough yields for every runnable fiber to reach its next wait, and a
   moment for IO (the scheduler only polls once nothing is runnable). *)
let settle () =
  for _ = 1 to 200 do
    Eio.Fiber.yield ()
  done;
  Eio_unix.sleep 0.0005
;;

let run t f =
  Eio.Fiber.first f (fun () ->
    let rec drive () =
      settle ();
      (match
         List.min_elt t.sleepers ~compare:(fun (a, i, _) (b, j, _) ->
           [%compare: float * int] (a, i) (b, j))
       with
       | None -> ()
       | Some ((at, seq, resolve) as _next) ->
         t.sleepers <- List.filter t.sleepers ~f:(fun (_, s, _) -> s <> seq);
         t.now <- Float.max t.now at;
         Eio.Promise.resolve resolve ());
      drive ()
    in
    drive ())
;;
