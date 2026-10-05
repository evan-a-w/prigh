open! Core

type t =
  { capacity : int
  ; buffer : Buffer.t
  ; mutable dropped : bool
  ; mutable total_bytes : int
  }

let create ?(capacity = 1_048_576) () =
  { capacity; buffer = Buffer.create 1024; dropped = false; total_bytes = 0 }
;;

(* Trimming at twice the capacity keeps [add] amortised O(chunk). *)
let add t chunk =
  t.total_bytes <- t.total_bytes + String.length chunk;
  Buffer.add_string t.buffer chunk;
  if Buffer.length t.buffer > 2 * t.capacity
  then (
    let kept =
      Buffer.To_string.sub
        t.buffer
        ~pos:(Buffer.length t.buffer - t.capacity)
        ~len:t.capacity
    in
    Buffer.clear t.buffer;
    Buffer.add_string t.buffer kept;
    t.dropped <- true)
;;

let total_bytes t = t.total_bytes

let lines t =
  let len = Buffer.length t.buffer in
  let start = Int.max 0 (len - t.capacity) in
  let text = Buffer.To_string.sub t.buffer ~pos:start ~len:(len - start) in
  let lines = String.split text ~on:'\n' in
  let lines = if t.dropped || start > 0 then List.drop lines 1 else lines in
  let lines =
    match List.last lines with
    | Some "" -> List.drop_last_exn lines
    | _ -> lines
  in
  List.map lines ~f:(fun line ->
    Utf8.sanitize (String.chop_suffix_if_exists line ~suffix:"\r"))
;;

let last_line t =
  List.rev (lines t)
  |> List.find ~f:(fun line -> not (String.is_empty (String.strip line)))
;;
