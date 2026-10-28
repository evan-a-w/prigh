open! Core
open! Import

let default_max_bytes = 2 * 1024 * 1024
let suffix = ".trace.jsonl"
let old_suffix = ".trace.old.jsonl"
let base session_path = String.chop_suffix_if_exists session_path ~suffix:".jsonl"
let current ~session_path = base session_path ^ suffix
let old ~session_path = base session_path ^ old_suffix
let paths ~session_path = [ old ~session_path; current ~session_path ]

let is_trace_file name =
  String.is_suffix name ~suffix || String.is_suffix name ~suffix:old_suffix
;;

(* Spans of a session without a file yet; past this, the oldest go. *)
let max_pending = 1000

type t =
  { max_bytes : int
  ; session_path : unit -> string option
  ; pending : string Queue.t
  ; mutable checked : string option
  (** the file whose end was checked for a line cut short by a crash *)
  }

let create ?(max_bytes = default_max_bytes) ~session_path () =
  { max_bytes; session_path; pending = Queue.create (); checked = None }
;;

let size path =
  match Core_unix.stat path with
  | { st_size; _ } -> Int64.to_int_exn st_size
  | exception _ -> 0
;;

let ends_with_newline path =
  match In_channel.with_file path ~f:(fun ic ->
    let len = In_channel.length ic in
    if Int64.(len = 0L)
    then true
    else (
      In_channel.seek ic Int64.(len - 1L);
      match In_channel.input_char ic with
      | Some '\n' | None -> true
      | Some _ -> false))
  with
  | b -> b
  | exception _ -> true
;;

let append t ~session_path lines =
  let path = current ~session_path in
  let data = String.concat (List.map lines ~f:(fun l -> l ^ "\n")) in
  let data =
    if Option.exists t.checked ~f:(String.equal path)
    then data
    else (
      t.checked <- Some path;
      if ends_with_newline path then data else "\n" ^ data)
  in
  let existing = size path in
  if existing > 0 && existing + String.length data > t.max_bytes
  then (
    Core_unix.rename ~src:path ~dst:(old ~session_path);
    t.checked <- Some path);
  Out_channel.with_file path ~append:true ~f:(fun oc ->
    Out_channel.output_string oc data)
;;

let write t span =
  let line = Json.to_string (Trace.Span.to_json span) in
  match t.session_path () with
  | None ->
    Queue.enqueue t.pending line;
    if Queue.length t.pending > max_pending
    then ignore (Queue.dequeue_exn t.pending : string)
  | Some session_path ->
    let lines = Queue.to_list t.pending @ [ line ] in
    Queue.clear t.pending;
    (* A deleted session's spans are dropped rather than left behind. *)
    if Sys_unix.file_exists_exn session_path then append t ~session_path lines
;;

let load ~session_path =
  let bad = ref 0 in
  let spans =
    List.concat_map (paths ~session_path) ~f:(fun path ->
      match In_channel.read_lines path with
      | exception _ -> []
      | lines ->
        List.filter_map lines ~f:(fun line ->
          if String.is_empty (String.strip line)
          then None
          else (
            match Result.bind (Json.parse line) ~f:Trace.Span.of_json with
            | Ok span -> Some span
            | Error _ ->
              incr bad;
              None)))
  in
  spans, !bad
;;
