open! Core
open! Import

module For = struct
  type t =
    { host : string
    ; config : string
    }
  [@@deriving equal]
end

module In_flight = struct
  type t =
    { for_ : For.t
    ; cancel : Cancellation.t
    ; done_ : unit Promise.t
    }
end

type t =
  { mutable listed : For.t option
  ; mutable in_flight : In_flight.t option
  }

let create () = { listed = None; in_flight = None }

let current t ~host ~config =
  Option.equal For.equal t.listed (Some { For.host; config })
;;

let launch ?after t ~sw ~host ~config ~refresh =
  let for_ = { For.host; config } in
  let cancel = Cancellation.create () in
  let done_, resolve = Promise.create () in
  let flight = { In_flight.for_; cancel; done_ } in
  t.in_flight <- Some flight;
  Guarded.fork ~sw ~what:"an MCP refresh" (fun () ->
    Exn.protect
      ~f:(fun () ->
        Option.iter after ~f:(fun (a : In_flight.t) -> Promise.await a.done_);
        if (not (Cancellation.is_cancelled cancel)) && refresh ~cancel
        then t.listed <- Some for_)
      ~finally:(fun () ->
        (match t.in_flight with
         | Some f when phys_equal f flight -> t.in_flight <- None
         | Some _ | None -> ());
        Promise.resolve resolve ()));
  flight
;;

(* A refresh for other settings than the one under way follows it. *)
let refresh t ~sw ~host ~config ~refresh =
  match t.in_flight with
  | Some flight when For.equal flight.for_ { host; config } -> flight
  | after -> launch ?after t ~sw ~host ~config ~refresh
;;

let start t ~sw ~host ~config ~refresh:f =
  ignore (refresh t ~sw ~host ~config ~refresh:f : In_flight.t)
;;

let await ~cancel (flight : In_flight.t) =
  ignore
    (Cancellation.protect cancel ~f:(fun () -> Promise.await flight.done_)
     : unit option)
;;

let before_run t ~sw ~host ~config ~cancel ~refresh:f =
  if current t ~host ~config
  then start t ~sw ~host ~config ~refresh:f
  else await ~cancel (refresh t ~sw ~host ~config ~refresh:f)
;;

let cancel t =
  Option.iter t.in_flight ~f:(fun f -> Cancellation.cancel f.cancel)
;;

let wait t = Option.iter t.in_flight ~f:(fun f -> Promise.await f.done_)
