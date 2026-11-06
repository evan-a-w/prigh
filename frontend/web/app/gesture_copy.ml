open! Core

module Early = struct
  type t =
    { text : string
    ; result : bool Async_kernel.Deferred.t
    ; at : Time_ns.t
    }
end

let early : Early.t Queue.t = Queue.create ()

let in_gesture (commands : Prigh_web.App.Command.t list) =
  List.iter commands ~f:(function
    | Copy { text; copied = _ } ->
      Queue.enqueue
        early
        { text; result = Browser.copy_to_clipboard text; at = Time_ns.now () }
    | _ -> ())
;;

let copy text =
  (* A gesture whose action Bonsai did not turn into this copy (it differed
     from the one foreseen) leaves an entry: they go stale. *)
  let now = Time_ns.now () in
  Queue.filter_inplace early ~f:(fun (e : Early.t) ->
    Time_ns.Span.( < ) (Time_ns.diff now e.at) (Time_ns.Span.of_sec 2.));
  match Queue.find early ~f:(fun (e : Early.t) -> String.equal e.text text) with
  | Some found ->
    Queue.filter_inplace early ~f:(fun e -> not (phys_equal e found));
    found.result
  | None -> Browser.copy_to_clipboard text
;;
