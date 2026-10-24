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
  ; mutable has_fields : bool
  }

let create () =
  { pending = Buffer.create 1024
  ; event = None
  ; data = []
  ; id = None
  ; has_fields = false
  }
;;

let reset t =
  t.event <- None;
  t.data <- [];
  t.id <- None;
  t.has_fields <- false
;;

let take_event t =
  if not t.has_fields
  then None
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
  | "data" ->
    t.has_fields <- true;
    t.data <- value :: t.data
  | "event" ->
    t.has_fields <- true;
    t.event <- Some value
  | "id" ->
    t.has_fields <- true;
    t.id <- Some value
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
