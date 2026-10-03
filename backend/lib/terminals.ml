open! Core
open! Import

module Entry = struct
  type t =
    { terminal : Terminal.t
    ; mutable idle_since : float option
    }
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; tmux : string
  ; socket : Terminal.Socket.t
  ; name_prefix : string
  ; idle_timeout : float
  ; heartbeat_timeout : float
  ; command : string list option
  ; entries : Entry.t String.Table.t
  ; create_mutex : Eio.Mutex.t
  ; mutable seq : int
  }

let create
      ~env
      ~sw
      ?tmux
      ?socket
      ?(idle_timeout = Time_ns.Span.of_min 10.)
      ?(heartbeat_timeout = Time_ns.Span.of_sec 30.)
      ?command
      ()
  =
  let tmux =
    match tmux with
    | Some tmux -> tmux
    | None ->
      Option.value
        (Option.filter (Sys.getenv "PRIGH_TMUX") ~f:(Fn.non String.is_empty))
        ~default:"tmux"
  in
  { env
  ; sw
  ; tmux
  ; socket = Option.value socket ~default:(Terminal.Socket.Name "prigh")
  ; name_prefix =
      (match socket with
       | Some _ -> ""
       | None -> sprintf "%d-" (Pid.to_int (Core_unix.getpid ())))
  ; idle_timeout = Time_ns.Span.to_sec idle_timeout
  ; heartbeat_timeout = Time_ns.Span.to_sec heartbeat_timeout
  ; command
  ; entries = String.Table.create ()
  ; create_mutex = Eio.Mutex.create ()
  ; seq = 0
  }
;;

let clock t = Eio.Stdenv.clock t.env

(* Session names go into tmux command lines, so only plain characters. *)
let session_name t key =
  t.seq <- t.seq + 1;
  sprintf
    "%st%d-%s"
    t.name_prefix
    t.seq
    (String.map (String.prefix key 48) ~f:(fun c ->
       if Char.is_alphanum c || Char.equal c '-' then c else '_'))
;;

let get_or_create t ~key ~cwd ~cols ~rows =
  Eio.Mutex.use_rw ~protect:true t.create_mutex (fun () ->
    match Hashtbl.find t.entries key with
    | Some entry when Terminal.is_alive entry.terminal -> Ok entry
    | Some _ | None ->
      Or_error.map
        (Terminal.create
           ~env:t.env
           ~sw:t.sw
           ~tmux:t.tmux
           ~socket:t.socket
           ~name:(session_name t key)
           ~cwd
           ~cols
           ~rows
           ?command:t.command
           ())
        ~f:(fun terminal ->
          let entry = { Entry.terminal; idle_since = None } in
          Hashtbl.set t.entries ~key ~data:entry;
          Fiber.fork_daemon ~sw:t.sw (fun () ->
            Promise.await (Terminal.exited terminal);
            (match Hashtbl.find t.entries key with
             | Some current when phys_equal current entry ->
               Hashtbl.remove t.entries key
             | Some _ | None -> ());
            `Stop_daemon);
          entry))
;;

let start_idle_timer t (entry : Entry.t) =
  if Terminal.viewers entry.terminal = 0
  then (
    let since = Eio.Time.now (clock t) in
    entry.idle_since <- Some since;
    Fiber.fork_daemon ~sw:t.sw (fun () ->
      Fiber.first
        (fun () ->
           Eio.Time.sleep (clock t) t.idle_timeout;
           match entry.idle_since with
           | Some s when Float.equal s since -> Terminal.kill entry.terminal
           | Some _ | None -> ())
        (fun () -> Promise.await (Terminal.exited entry.terminal));
      `Stop_daemon))
;;

module Outgoing = struct
  type t =
    | Data of string
    | Overflow
    | Exited
end

(* A socket that cannot keep up is dropped; the client reconnects and gets a
   fresh replay instead of an ever-growing backlog. *)
let queue_capacity = 1024

let control_message terminal ws text =
  match Json.parse text with
  | Ok json ->
    let field name = Json.member name json in
    (match field "type" with
     | Some (`String "ping") -> Websocket.send_text ws {|{"type":"pong"}|}
     | Some (`String "resize") ->
       (match field "cols", field "rows" with
        | Some (`Number cols), Some (`Number rows) ->
          (match Int.of_string_opt cols, Int.of_string_opt rows with
           | Some cols, Some rows -> Terminal.resize terminal ~cols ~rows
           | _ -> ())
        | _ -> ())
     | _ -> ())
  | Error _ -> ()
;;

let serve_terminal t (entry : Entry.t) ~cols ~rows ws =
  let terminal = entry.terminal in
  Switch.run
  @@ fun sw ->
  let outgoing = Eio.Stream.create queue_capacity in
  let overflowed = ref false in
  let push : Outgoing.t -> unit =
    fun item ->
    if not !overflowed
    then
      if Eio.Stream.length outgoing >= queue_capacity - 1
      then (
        overflowed := true;
        Eio.Stream.add outgoing Outgoing.Overflow)
      else Eio.Stream.add outgoing item
  in
  entry.idle_since <- None;
  let viewer =
    Terminal.attach terminal ~cols ~rows ~on_output:(fun data ->
      push (Outgoing.Data data))
  in
  Fiber.fork_daemon ~sw (fun () ->
    Promise.await (Terminal.exited terminal);
    push Outgoing.Exited;
    `Stop_daemon);
  let rec write () =
    match (Eio.Stream.take outgoing : Outgoing.t) with
    | Data data ->
      Websocket.send_binary ws data;
      if not (Websocket.is_closed ws) then write ()
    | Overflow -> ()
    | Exited -> Websocket.send_text ws {|{"type":"exit"}|}
  in
  let rec read () =
    match
      Eio.Time.with_timeout (clock t) t.heartbeat_timeout (fun () ->
        Ok (Websocket.read ws))
    with
    | Error `Timeout | Ok None -> ()
    | Ok (Some (`Binary data)) ->
      Terminal.input terminal data;
      read ()
    | Ok (Some (`Text text)) ->
      control_message terminal ws text;
      read ()
  in
  Exn.protect
    ~f:(fun () -> Fiber.first write read)
    ~finally:(fun () ->
      Terminal.detach terminal viewer;
      start_idle_timer t entry)
;;

let serve t ~key ~cwd ~cols ~rows ws =
  match get_or_create t ~key ~cwd ~cols ~rows with
  | Ok entry -> serve_terminal t entry ~cols ~rows ws
  | Error e ->
    Websocket.send_text
      ws
      (Json.to_string
         (`Object
             [ "type", `String "error"
             ; "message", `String (Error.to_string_hum e)
             ]))
;;

let live t =
  Hashtbl.to_alist t.entries
  |> List.map ~f:(fun (key, (entry : Entry.t)) ->
    key, Terminal.name entry.terminal, Terminal.viewers entry.terminal)
  |> List.sort ~compare:[%compare: string * string * int]
;;

let close_all t =
  Fiber.List.iter
    (fun (entry : Entry.t) -> Terminal.kill entry.terminal)
    (Hashtbl.data t.entries)
;;
