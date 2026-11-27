open! Core
open! Import

module Event = struct
  type t =
    { event : string option
    ; data : string
    ; id : string option
    }
  [@@deriving sexp_of]
end

type t =
  { pending : Buffer.t
  ; mutable event : string option
  ; mutable data : string list
  ; mutable id : string option
  }

let create () =
  { pending = Buffer.create 1024; event = None; data = []; id = None }
;;

let reset t =
  t.event <- None;
  t.data <- [];
  t.id <- None
;;

(* WHATWG §9.2.6: a block with no [data:] lines is ignored (keepalive frames
   such as [event: ping\n\n] must not be dispatched as empty events). *)
let take_event t =
  if List.is_empty t.data
  then (
    reset t;
    None)
  else (
    let event =
      { Event.event = t.event
      ; data = String.concat ~sep:"\n" (List.rev t.data)
      ; id = t.id
      }
    in
    reset t;
    Some event)
;;

let handle_field t ~name ~value =
  match name with
  | "data" -> t.data <- value :: t.data
  | "event" -> t.event <- Some value
  | "id" -> t.id <- Some value
  | _ -> ()
;;

let handle_line t line =
  if String.is_empty line
  then take_event t
  else if Char.equal line.[0] ':'
  then None
  else (
    let name, value =
      match String.lsplit2 line ~on:':' with
      | None -> line, ""
      | Some (name, value) ->
        name, String.chop_prefix_if_exists value ~prefix:" "
    in
    handle_field t ~name ~value;
    None)
;;

let strip_cr line = String.chop_suffix_if_exists line ~suffix:"\r"

let feed t chunk =
  Buffer.add_string t.pending chunk;
  let contents = Buffer.contents t.pending in
  let lines = String.split contents ~on:'\n' in
  let rest, complete =
    match List.rev lines with
    | rest :: complete -> rest, List.rev complete
    | [] -> "", []
  in
  Buffer.clear t.pending;
  Buffer.add_string t.pending rest;
  List.filter_map complete ~f:(fun line -> handle_line t (strip_cr line))
;;

let finish t =
  let rest = Buffer.contents t.pending in
  Buffer.clear t.pending;
  if not (String.is_empty rest)
  then ignore (handle_line t (strip_cr rest) : Event.t option);
  take_event t
;;
