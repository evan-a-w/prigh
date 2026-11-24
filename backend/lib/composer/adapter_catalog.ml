open! Core
open! Import

module Key = struct
  type t = string * string [@@deriving sexp, compare, hash]
end

module Entry = struct
  type t =
    { catalog : Adapter.Catalog.t
    ; at : float
    }
end

type t =
  { now : unit -> float
  ; entries : (Key.t, Entry.t) Hashtbl.t
  ; asking : (Key.t, unit Promise.t) Hashtbl.t
  ; failed : (Key.t, float) Hashtbl.t
  }

let create ~now () =
  { now
  ; entries = Hashtbl.create (module Key)
  ; asking = Hashtbl.create (module Key)
  ; failed = Hashtbl.create (module Key)
  }
;;

let find t ~host adapter =
  Hashtbl.find t.entries (host, adapter)
  |> Option.map ~f:(fun (e : Entry.t) -> e.catalog)
;;

let age t ~host adapter =
  Hashtbl.find t.entries (host, adapter)
  |> Option.map ~f:(fun (e : Entry.t) -> t.now () -. e.at)
;;

let retry_after = 60.

let refresh t ~sw ~host adapter ~max_age ~fetch ~on_done =
  let key = host, adapter in
  let fresh =
    Option.exists (age t ~host adapter) ~f:(fun age -> Float.( < ) age max_age)
  in
  let failed_lately =
    Option.exists (Hashtbl.find t.failed key) ~f:(fun at ->
      Float.( < ) (t.now () -. at) retry_after)
  in
  if not (fresh || failed_lately || Hashtbl.mem t.asking key)
  then (
    let done_, resolve = Promise.create () in
    Hashtbl.set t.asking ~key ~data:done_;
    Fiber.fork ~sw (fun () ->
      let result =
        match fetch () with
        | result -> result
        | exception exn -> Or_error.of_exn exn
      in
      Hashtbl.remove t.asking key;
      match result with
      | Ok (catalog : Adapter.Catalog.t) ->
        Hashtbl.remove t.failed key;
        (* A harness that listed nothing keeps what it listed before. *)
        let catalog =
          match catalog.models, find t ~host adapter with
          | [], Some old -> { catalog with models = old.models }
          | _ -> catalog
        in
        Hashtbl.set t.entries ~key ~data:{ catalog; at = t.now () };
        Promise.resolve resolve ();
        on_done ()
      | Error _ ->
        Hashtbl.set t.failed ~key ~data:(t.now ());
        Promise.resolve resolve ()))
;;

let await t ~host adapter =
  Option.iter (Hashtbl.find t.asking (host, adapter)) ~f:Promise.await
;;
