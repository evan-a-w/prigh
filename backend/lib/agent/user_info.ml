open! Core
open! Import

type t =
  { path : string
  ; mutex : Eio.Mutex.t
  ; mutable subscribers : (unit -> unit) list
  }

let path ~home = Filename.concat home ".prigh/user_info.md"

let create ~home =
  { path = path ~home; mutex = Eio.Mutex.create (); subscribers = [] }
;;

let subscribe t ~f = t.subscribers <- f :: t.subscribers
let max_bytes = 4000

let get t =
  match In_channel.read_all t.path with
  | exception Sys_error _ -> ""
  | text ->
    let text = String.strip (Utf8.sanitize text) in
    if String.length text <= max_bytes
    then text
    else
      String.strip
        (fst (Utf8.split_incomplete_suffix (String.prefix text max_bytes)))
;;

let text t =
  let text = get t in
  Option.some_if (not (String.is_empty text)) text
;;

let set t text =
  let text = String.strip text in
  if String.length text > max_bytes
  then
    Or_error.errorf
      "the text about you is limited to %d bytes (this one has %d): shorten \
       it, and keep what is only sometimes needed in memory notes"
      max_bytes
      (String.length text)
  else (
    let result =
      Eio.Mutex.use_rw ~protect:false t.mutex
      @@ fun () ->
      Or_error.try_with (fun () ->
        if String.is_empty text
        then (
          match Core_unix.unlink t.path with
          | () -> ()
          | exception Core_unix.Unix_error (ENOENT, _, _) -> ())
        else (
          Core_unix.mkdir_p (Filename.dirname t.path);
          let tmp = t.path ^ ".tmp" in
          Out_channel.write_all tmp ~data:(text ^ "\n");
          Core_unix.rename ~src:tmp ~dst:t.path))
    in
    if Or_error.is_ok result
    then List.iter (List.rev t.subscribers) ~f:(fun f -> f ());
    result)
;;
