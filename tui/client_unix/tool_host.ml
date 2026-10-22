open! Core
open! Async
open Prigh_client
open Prigh_protocol

module Held = struct
  type t =
    { method_ : string
    ; params : (string * Jsonaf.t) list
    }
end

type t =
  { client : Client.t
  ; spawn : unit -> Transport.t Deferred.Or_error.t
  ; on_exit : string -> unit
  ; mutable worker : Transport.t Deferred.Or_error.t option
  ; sends : unit Sequencer.t
  ; running : String.Hash_set.t
    (** execs sent to the worker, until their result *)
  ; mutable online : bool
  ; held : Held.t Queue.t (** what waits for the backend to be back *)
  ; mutable held_output : int
  ; mutable closing : bool
  }

let max_held_output = 1024 * 1024
let stderr_tail_lines = 10

let spawn_worker ~backend () =
  Stdio_transport.spawn ~prog:backend ~args:[ "tool-host" ] ()
;;

let create ?(on_exit = ignore) ~client ~spawn () =
  { client
  ; spawn
  ; on_exit
  ; worker = None
  ; sends = Sequencer.create ()
  ; running = String.Hash_set.create ()
  ; online = true
  ; held = Queue.create ()
  ; held_output = 0
  ; closing = false
  }
;;

(* While the backend is away, results and (up to [max_held_output]) output
   wait for it: the backend keeps the execs for a host that comes back with
   them running. Terminal frames are for viewers that are gone. *)
let hold t (held : Held.t) =
  match held.method_ with
  | "tool_exec_result" -> Queue.enqueue t.held held
  | "tool_exec_output" ->
    let bytes =
      match List.Assoc.find held.params "chunk" ~equal:String.equal with
      | Some (`String chunk) -> String.length chunk
      | _ -> 0
    in
    if t.held_output + bytes <= max_held_output
    then (
      t.held_output <- t.held_output + bytes;
      Queue.enqueue t.held held)
  | _ -> ()
;;

let call t method_ params =
  let held = { Held.method_; params } in
  if not t.online
  then hold t held
  else
    don't_wait_for
      (match%map Client.call t.client method_ params with
       | Ok _ -> ()
       | Error _ ->
         (* Lost with the connection; a result that made it is ignored. *)
         if not (Client.is_connected t.client)
         then (
           match method_ with
           | "tool_exec_result" -> Queue.enqueue t.held held
           | _ -> ()))
;;

let running_execs t =
  Hash_set.to_list t.running
  @ (Queue.to_list t.held
     |> List.filter_map ~f:(fun (held : Held.t) ->
       match
         held.method_, List.Assoc.find held.params "exec_id" ~equal:String.equal
       with
       | "tool_exec_result", Some (`String exec_id) -> Some exec_id
       | _ -> None))
  |> List.dedup_and_sort ~compare:String.compare
;;

let set_online t online =
  t.online <- online;
  if online
  then (
    let held = Queue.to_list t.held in
    Queue.clear t.held;
    t.held_output <- 0;
    List.iter held ~f:(fun (h : Held.t) -> call t h.method_ h.params))
;;

let fail t ~exec_id text =
  call
    t
    "tool_exec_result"
    [ "exec_id", `String exec_id; "text", `String text; "is_error", `True ]
;;

let relay t line =
  match Jsonaf.parse line with
  | Error _ -> ()
  | Ok json ->
    let field name = Jsonaf.member name json in
    let string_field name =
      match field name with
      | Some (`String s) -> s
      | _ -> ""
    in
    (match field "type", field "exec_id", field "term_id" with
     | Some (`String "output"), Some (`String exec_id), _ ->
       call
         t
         "tool_exec_output"
         [ "exec_id", `String exec_id; "chunk", `String (string_field "chunk") ]
     | Some (`String "result"), Some (`String exec_id), _ ->
       Hash_set.remove t.running exec_id;
       let is_error =
         match field "is_error" with
         | Some `True -> true
         | _ -> false
       in
       let images =
         match field "images" with
         | Some (`Array (_ :: _) as images) -> [ "images", images ]
         | _ -> []
       in
       call
         t
         "tool_exec_result"
         ([ "exec_id", `String exec_id
          ; "text", `String (string_field "text")
          ; ("is_error", if is_error then `True else `False)
          ]
          @ images)
     | Some (`String "terminal_frame"), _, Some (`String term_id) ->
       call
         t
         "terminal_frame"
         [ "term_id", `String term_id
         ; "kind", `String (string_field "kind")
         ; "data", `String (string_field "data")
         ]
     | Some (`String "terminal_closed"), _, Some (`String term_id) ->
       call t "terminal_closed" [ "term_id", `String term_id ]
     | _ -> ())
;;

(* The worker went away: what it ran is lost, and the user hears why. The
   next exec starts a new one. *)
let exited t ~lost ~stderr =
  List.iter lost ~f:(Hash_set.remove t.running);
  let why =
    match stderr with
    | [] -> ""
    | lines -> "; its last output:\n" ^ String.concat ~sep:"\n" lines
  in
  List.iter lost ~f:(fun exec_id ->
    fail t ~exec_id ("[the tool host process exited" ^ why ^ "]"));
  if not t.closing
  then
    t.on_exit
      (sprintf
         "the local tool host (prigh tool-host) exited%s; it starts again for \
          the next tool call%s"
         (match List.length lost with
          | 0 -> ""
          | n -> sprintf ", failing %d running tool call(s)" n)
         why)
;;

let worker t =
  match t.worker with
  | Some w -> w
  | None ->
    let w =
      match%map t.spawn () with
      | Error _ as e ->
        t.worker <- None;
        e
      | Ok transport ->
        let stderr = Queue.create () in
        let stderr_done =
          Pipe.iter_without_pushback transport.stderr_lines ~f:(fun line ->
            Queue.enqueue stderr line;
            if Queue.length stderr > stderr_tail_lines
            then ignore (Queue.dequeue_exn stderr : string))
        in
        (* Its lines first: a result it wrote before exiting is not lost. *)
        let lines_done =
          Pipe.iter_without_pushback transport.lines ~f:(relay t)
        in
        upon transport.closed (fun () ->
          t.worker <- None;
          let ran = Hash_set.to_list t.running in
          upon
            (Deferred.all_unit [ lines_done; stderr_done ])
            (fun () ->
               exited
                 t
                 ~lost:(List.filter ran ~f:(Hash_set.mem t.running))
                 ~stderr:(Queue.to_list stderr)));
        Ok transport
    in
    t.worker <- Some w;
    w
;;

let send t json =
  don't_wait_for
  @@ Throttle.enqueue t.sends (fun () ->
    match%map worker t with
    | Ok transport -> transport.send_line (Jsonaf.to_string json)
    | Error e ->
      (match Jsonaf.member "exec_id" json, Jsonaf.member "type" json with
       | Some (`String exec_id), _ ->
         Hash_set.remove t.running exec_id;
         fail t ~exec_id ("cannot start tool host: " ^ Error.to_string_hum e)
       | _, Some (`String "terminal_open") ->
         (match Jsonaf.member "term_id" json with
          | Some term_id -> call t "terminal_closed" [ "term_id", term_id ]
          | None -> ())
       | _ -> ()))
;;

let handle t (event : Event.t) =
  match event with
  | Tool_exec { exec_id; call_id = _; name; arguments; cwd; stdin } ->
    Hash_set.add t.running exec_id;
    send
      t
      (`Object
          ([ "type", `String "exec"
           ; "exec_id", `String exec_id
           ; "name", `String name
           ; "arguments", arguments
           ; "cwd", `String cwd
           ]
           @ if stdin then [ "stdin", `True ] else []))
  | Tool_exec_cancel exec_id ->
    send t (`Object [ "type", `String "cancel"; "exec_id", `String exec_id ])
  | Tool_exec_input { exec_id; data; close } ->
    send
      t
      (`Object
          [ "type", `String "input"
          ; "exec_id", `String exec_id
          ; "data", `String data
          ; ("close", if close then `True else `False)
          ])
  | Terminal_open { term_id; key; cwd; cols; rows } ->
    send
      t
      (`Object
          [ "type", `String "terminal_open"
          ; "term_id", `String term_id
          ; "key", `String key
          ; "cwd", `String cwd
          ; "cols", `Number (Int.to_string cols)
          ; "rows", `Number (Int.to_string rows)
          ])
  | Terminal_frame { term_id; kind; data } ->
    send
      t
      (`Object
          [ "type", `String "terminal_frame"
          ; "term_id", `String term_id
          ; "kind", `String (Event.Frame_kind.to_string kind)
          ; "data", `String data
          ])
  | Terminal_close term_id ->
    send
      t
      (`Object [ "type", `String "terminal_close"; "term_id", `String term_id ])
  | Terminal_kill key ->
    send t (`Object [ "type", `String "terminal_kill"; "key", `String key ])
  | _ -> ()
;;

let close t =
  t.closing <- true;
  match t.worker with
  | None -> ()
  | Some w ->
    upon w (function
      | Ok transport -> transport.close ()
      | Error _ -> ())
;;
