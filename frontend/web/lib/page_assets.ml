open! Core
open Js_of_ocaml

module State = struct
  type t =
    | Not_loaded
    | Loading of ((unit, string) Result.t -> unit) Queue.t
    | Loaded
end

type t =
  { css : string list
  ; scripts : string list
  ; mutable state : State.t
  ; mutable linked : bool
  }

let create ~css ~scripts = { css; scripts; state = Not_loaded; linked = false }

let add_to_head (el : #Dom.node Js.t) =
  Dom.appendChild Dom_html.document##.head el
;;

let script src ~k =
  let el = Dom_html.createScript Dom_html.document in
  let once = ref false in
  let finish result =
    if not !once
    then (
      once := true;
      k result)
  in
  let listen event result =
    ignore
      (Dom_html.addEventListener
         el
         event
         (Dom.handler (fun _ ->
            finish result;
            Js._true))
         Js._false
       : Dom_html.event_listener_id)
  in
  listen Dom_html.Event.load (Ok ());
  listen Dom_html.Event.error (Error src);
  el##.src := Js.string src;
  add_to_head el
;;

let load t k =
  match t.state with
  | Loaded -> k (Ok ())
  | Loading waiting -> Queue.enqueue waiting k
  | Not_loaded ->
    let waiting = Queue.singleton k in
    t.state <- Loading waiting;
    if not t.linked
    then (
      t.linked <- true;
      List.iter t.css ~f:(fun href ->
        let css = Dom_html.createLink Dom_html.document in
        css##.rel := Js.string "stylesheet";
        css##.href := Js.string href;
        add_to_head css));
    let rec next = function
      | [] -> finish (Ok ())
      | src :: rest ->
        script src ~k:(function
          | Ok () -> next rest
          | Error src -> finish (Error src))
    and finish result =
      t.state
      <- (match result with
          | Ok () -> Loaded
          | Error _ -> Not_loaded);
      Queue.iter waiting ~f:(fun k -> k result)
    in
    next t.scripts
;;

let loaded t =
  match t.state with
  | Loaded -> true
  | Not_loaded | Loading _ -> false
;;
