open! Core
open! Import

type conn =
  | Lines of
      { reader : Eio.Buf_read.t
      ; flow : Eio.Flow.two_way_ty Eio.Resource.t
      ; write_mutex : Eio.Mutex.t
      }
  | Websocket of Websocket.t

type t =
  { conn : conn
  ; abort : unit -> unit
  ; mutable lines_read : int
  ; mutable written : bool (** a line went out: [hello] is first *)
  ; mutable writing : int (** writes in progress *)
  ; mutable aborted : bool
  }

let connect ~env ~sw (address : Backend_address.t) =
  let flow, abort = Backend_address.connect_abortable ~env ~sw address in
  let conn =
    if address.lines
    then
      Lines
        { reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024)
        ; flow
        ; write_mutex = Eio.Mutex.create ()
        }
    else
      Websocket
        (Backend_address.websocket_over flow address ~path:"/ws" ~query:[])
  in
  { conn; abort; lines_read = 0; written = false; writing = 0; aborted = false }
;;

let heartbeat_id = "prigh-heartbeat"

let heartbeat_request =
  Json.to_string
    (`Object [ "id", `String heartbeat_id; "method", `String "ping" ])
;;

(* The backend's answer to [heartbeat_request], whatever it says. *)
let is_heartbeat_reply =
  String.is_prefix
    ~prefix:(sprintf {|{"type":"response","id":"%s"|} heartbeat_id)
;;

let rec read_line t =
  let line =
    match t.conn with
    | Lines { reader; _ } ->
      (match Eio.Buf_read.line reader with
       | exception (End_of_file | Eio.Io _ | Eio.Buf_read.Buffer_limit_exceeded)
         -> None
       | line -> Some line)
    | Websocket ws -> Websocket.read_text ws
  in
  match line with
  | Some line ->
    t.lines_read <- t.lines_read + 1;
    if is_heartbeat_reply line then read_line t else Some line
  | None -> None
;;

let write_line t line =
  t.written <- true;
  t.writing <- t.writing + 1;
  Exn.protect
    ~finally:(fun () -> t.writing <- t.writing - 1)
    ~f:(fun () ->
      match t.conn with
      | Lines { flow; write_mutex; _ } ->
        Eio.Mutex.use_ro write_mutex (fun () ->
          Eio.Flow.copy_string (line ^ "\n") flow)
      | Websocket ws ->
        Websocket.send_text ws line;
        if Websocket.is_closed ws then failwith "the WebSocket is closed")
;;

let close t =
  match t.conn with
  | Lines { flow; _ } ->
    (try Eio.Flow.shutdown flow `All with
     | _ -> ())
  | Websocket ws ->
    (* A close frame to a peer that is gone could wait forever. *)
    if t.aborted then t.abort () else Websocket.close ws
;;

let heartbeat
      t
      ~clock
      ?(interval = 10.)
      ?(timeout = 30.)
      ?(write_timeout = 300.)
      ~on_silent
      ()
  =
  Switch.run
  @@ fun sw ->
  Fiber.fork_daemon ~sw (fun () ->
    while true do
      Eio.Time.sleep clock interval;
      (* After [hello]: a backend serving users reads it first. A ping that
         would queue behind a write in progress would only wait too. *)
      if t.written && t.writing = 0
      then (
        try write_line t heartbeat_request with
        | Eio.Cancel.Cancelled _ as exn -> raise exn
        | _ -> ())
    done;
    `Stop_daemon);
  (* Silence counts from the last line read, but not while a write is in
     progress (the backend cannot answer a ping that waits behind it) unless
     that write is stuck for [write_timeout]. *)
  let rec watch ~seen ~quiet ~stuck =
    Eio.Time.sleep clock interval;
    if t.lines_read <> seen
    then watch ~seen:t.lines_read ~quiet:0. ~stuck:0.
    else if t.writing > 0
    then (
      let stuck = stuck +. interval in
      if Float.(stuck >= write_timeout)
      then write_timeout
      else watch ~seen ~quiet:0. ~stuck)
    else (
      let quiet = quiet +. interval in
      if Float.(quiet >= timeout) then timeout else watch ~seen ~quiet ~stuck:0.)
  in
  let seconds = watch ~seen:t.lines_read ~quiet:0. ~stuck:0. in
  (* Before the abort: what reads the connection ends with it. *)
  on_silent seconds;
  t.aborted <- true;
  t.abort ()
;;
