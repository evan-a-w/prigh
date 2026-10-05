open! Core
open! Import

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_field json name =
  match field json name with
  | Some (`String s) -> Some s
  | _ -> None
;;

let int_field json name ~default =
  match field json name with
  | Some (`Number n) -> Option.value (Int.of_string_opt n) ~default
  | _ -> default
;;

module Reply = struct
  type t =
    | Output of
        { exec_id : string
        ; chunk : string
        }
    | Result of
        { exec_id : string
        ; result : Tool.Result.t
        }
    | Terminal_frame of
        { term_id : string
        ; frame : Terminal_channel.Frame.t
        }
    | Terminal_closed of { term_id : string }

  let fields = function
    | Output { exec_id; chunk } ->
      [ "exec_id", `String exec_id; "chunk", `String chunk ]
    | Result { exec_id; result } ->
      [ "exec_id", `String exec_id
      ; "text", `String result.text
      ; ("is_error", if result.is_error then `True else `False)
      ]
      @
      if List.is_empty result.images
      then []
      else [ "images", [%jsonaf_of: Image.t list] result.images ]
    | Terminal_frame { term_id; frame } ->
      ("term_id", `String term_id) :: Terminal_channel.Frame.to_fields frame
    | Terminal_closed { term_id } -> [ "term_id", `String term_id ]
  ;;

  let worker_type = function
    | Output _ -> "output"
    | Result _ -> "result"
    | Terminal_frame _ -> "terminal_frame"
    | Terminal_closed _ -> "terminal_closed"
  ;;

  let method_ = function
    | Output _ -> "tool_exec_output"
    | Result _ -> "tool_exec_result"
    | Terminal_frame _ -> "terminal_frame"
    | Terminal_closed _ -> "terminal_closed"
  ;;
end

(* The execs and terminal viewers of one frontend or connection. *)
module Worker = struct
  type t =
    { env : Env.t
    ; sw : Switch.t
    ; terminals : Terminals.t Lazy.t
    ; mcp : Mcp_hub.t
    ; default_cwd : string
    ; send : Reply.t -> unit
    ; running : Cancellation.t String.Table.t
    ; viewers : Terminal_channel.Fed.t String.Table.t
    }

  let create ~env ~sw ~terminals ~mcp ~default_cwd ~send =
    { env
    ; sw
    ; terminals
    ; mcp
    ; default_cwd
    ; send
    ; running = String.Table.create ()
    ; viewers = String.Table.create ()
    }
  ;;

  let exec t json =
    match string_field json "exec_id", string_field json "name" with
    | Some exec_id, Some name ->
      let arguments =
        Option.value (field json "arguments") ~default:(`Object [])
      in
      (* The backend's home is not ours: instructions, skills and MCP
         servers come from this machine's. *)
      let arguments =
        match arguments with
        | `Object fields when String.is_prefix name ~prefix:"$" ->
          `Object
            (List.filter fields ~f:(fun (key, _) ->
               not (String.equal key "home")))
        | arguments -> arguments
      in
      let cwd = Option.value (string_field json "cwd") ~default:t.default_cwd in
      let cancel = Cancellation.create () in
      Hashtbl.set t.running ~key:exec_id ~data:cancel;
      Fiber.fork ~sw:t.sw (fun () ->
        let result =
          match
            Host_ops.execute
              ~mcp:(Some t.mcp)
              ~env:t.env
              ~cancel
              ~on_output:(fun chunk -> t.send (Output { exec_id; chunk }))
              ~cwd
              ~name
              ~arguments
          with
          | result -> result
          | exception exn ->
            Tool.Result.error (sprintf "%s failed: %s" name (Exn.to_string exn))
        in
        Hashtbl.remove t.running exec_id;
        t.send (Result { exec_id; result }))
    | _ -> ()
  ;;

  let error_frame message : Terminal_channel.Frame.t =
    `Text
      (Json.to_string
         (`Object [ "type", `String "error"; "message", `String message ]))
  ;;

  let terminal_open t json =
    match
      ( string_field json "term_id"
      , string_field json "key"
      , string_field json "cwd" )
    with
    | Some term_id, Some key, Some cwd ->
      let fed =
        Terminal_channel.Fed.create ~send:(fun frame ->
          t.send (Terminal_frame { term_id; frame }))
      in
      let channel = Terminal_channel.Fed.channel fed in
      Hashtbl.set t.viewers ~key:term_id ~data:fed;
      Fiber.fork ~sw:t.sw (fun () ->
        (match
           Terminals.serve
             (Lazy.force t.terminals)
             ~key
             ~cwd
             ~cols:(int_field json "cols" ~default:80)
             ~rows:(int_field json "rows" ~default:24)
             channel
         with
         | () -> ()
         | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
         | exception exn ->
           Terminal_channel.send channel (error_frame (Exn.to_string exn)));
        (match Hashtbl.find t.viewers term_id with
         | Some current when phys_equal current fed ->
           Hashtbl.remove t.viewers term_id
         | Some _ | None -> ());
        if not (Terminal_channel.is_closed channel)
        then t.send (Terminal_closed { term_id }))
    | _ -> ()
  ;;

  let viewer t json =
    Option.bind (string_field json "term_id") ~f:(Hashtbl.find t.viewers)
  ;;

  let handle t ~kind json =
    match kind with
    | "exec" -> exec t json
    | "cancel" ->
      Option.iter (string_field json "exec_id") ~f:(fun exec_id ->
        Option.iter (Hashtbl.find t.running exec_id) ~f:Cancellation.cancel)
    | "terminal_open" -> terminal_open t json
    | "terminal_frame" ->
      Option.iter (viewer t json) ~f:(fun fed ->
        Result.iter (Terminal_channel.Frame.of_json json) ~f:(fun frame ->
          Terminal_channel.Fed.push fed frame))
    | "terminal_close" ->
      Option.iter (viewer t json) ~f:(fun fed ->
        Option.iter (string_field json "term_id") ~f:(Hashtbl.remove t.viewers);
        Terminal_channel.Fed.close fed)
    | _ -> ()
  ;;

  let stop t =
    Hashtbl.iter t.running ~f:Cancellation.cancel;
    Hashtbl.iter t.viewers ~f:Terminal_channel.Fed.close;
    Hashtbl.clear t.viewers
  ;;
end

(* Lines go out in order through one writer fiber; nothing is written once
   [close] was called. *)
module Outbox = struct
  type t =
    { lines : string option Eio.Stream.t
    ; mutable closed : bool
    }

  let create ~sw ~write =
    let t = { lines = Eio.Stream.create Int.max_value; closed = false } in
    Fiber.fork ~sw (fun () ->
      let rec loop () =
        match Eio.Stream.take t.lines with
        | None -> ()
        | Some line ->
          (match write (line ^ "\n") with
           | () -> loop ()
           | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
           | exception _ -> t.closed <- true)
      in
      loop ());
    t
  ;;

  let send t json =
    if not t.closed then Eio.Stream.add t.lines (Some (Json.to_string json))
  ;;

  let close t =
    if not t.closed
    then (
      t.closed <- true;
      Eio.Stream.add t.lines None)
  ;;
end

let read_lines flow ~f =
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | exception (End_of_file | Eio.Io _) -> ()
    | line ->
      (match Json.parse line with
       | Error _ -> ()
       | Ok json -> f json);
      loop ()
  in
  loop ()
;;

let run ~env ?terminals ~input ~output () =
  Switch.run
  @@ fun sw ->
  let owned = Option.is_none terminals in
  let terminals =
    Option.value terminals ~default:(lazy (Terminals.create ~env ~sw ()))
  in
  let mcp = Mcp_hub.create ~env ~sw () in
  let outbox =
    Outbox.create ~sw ~write:(fun s -> Eio.Flow.copy_string s output)
  in
  let worker =
    Worker.create
      ~env
      ~sw
      ~terminals
      ~mcp
      ~default_cwd:(Core_unix.getcwd ())
      ~send:(fun reply ->
        Outbox.send
          outbox
          (`Object
              (("type", `String (Reply.worker_type reply)) :: Reply.fields reply)))
  in
  read_lines input ~f:(fun json ->
    Option.iter (string_field json "type") ~f:(fun kind ->
      Worker.handle worker ~kind json));
  Worker.stop worker;
  if owned && Lazy.is_val terminals
  then Terminals.close_all (Lazy.force terminals);
  Mcp_hub.close mcp;
  Outbox.close outbox
;;

let worker_kind = function
  | "tool_exec" -> Some "exec"
  | "tool_exec_cancel" -> Some "cancel"
  | ("terminal_open" | "terminal_frame" | "terminal_close") as kind -> Some kind
  | _ -> None
;;

(* One connection: [`Served] once [hello] succeeded, else why it did not. *)
let serve_connection ~env ~terminals ~mcp ~log ~address ~token ~user ~name ~cwd flow =
  Switch.run
  @@ fun sw ->
  let outbox =
    Outbox.create ~sw ~write:(fun s -> Eio.Flow.copy_string s flow)
  in
  let pending = Int.Table.create () in
  let next_id = ref 0 in
  let request meth params =
    incr next_id;
    Hashtbl.set pending ~key:!next_id ~data:meth;
    Outbox.send
      outbox
      (`Object
          [ "id", `Number (Int.to_string !next_id)
          ; "method", `String meth
          ; "params", params
          ])
  in
  Outbox.send
    outbox
    (`Object
        [ "id", `String "hello"
        ; "method", `String "hello"
        ; ( "params"
          , `Object
              (List.filter_opt
                 [ Option.map token ~f:(fun token -> "token", `String token)
                 ; Option.map user ~f:(fun user -> "user", `String user)
                 ; Some ("name", `String name)
                 ; Some ("cwd", `String cwd)
                 ; Some ("tools", `True)
                 ]) )
        ]);
  let worker =
    Worker.create ~env ~sw ~terminals ~mcp ~default_cwd:cwd ~send:(fun reply ->
      request (Reply.method_ reply) (`Object (Reply.fields reply)))
  in
  let outcome = ref (`Failed (sprintf "disconnected from %s" address)) in
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  let rec loop () =
    match Eio.Buf_read.line reader with
    | exception (End_of_file | Eio.Io _) ->
      (match !outcome with
       | `Served -> log (sprintf "disconnected from %s" address)
       | `Failed _ -> ())
    | line ->
      let continue =
        match Json.parse line with
        | Error _ -> true
        | Ok json ->
          (match string_field json "type", field json "ok" with
           | Some "response", ok ->
             let error = Option.value (string_field json "error") ~default:"" in
             (match field json "id", ok with
              | Some (`String "hello"), Some `True ->
                outcome := `Served;
                log
                  (sprintf
                     "connected to %s as %s"
                     address
                     (Option.value
                        (Option.bind (field json "result") ~f:(fun r ->
                           string_field r "client_id"))
                        ~default:"?"));
                true
              | Some (`String "hello"), _ ->
                outcome := `Failed ("hello failed: " ^ error);
                false
              | Some (`Number id), ok ->
                let meth =
                  Option.bind
                    (Int.of_string_opt id)
                    ~f:(Hashtbl.find_and_remove pending)
                in
                (match ok with
                 | Some `True -> ()
                 | _ ->
                   log
                     (sprintf
                        "%s failed: %s"
                        (Option.value meth ~default:"request")
                        error));
                true
              | _ -> true)
           | Some "event", _ ->
             Option.iter
               (Option.bind (string_field json "event") ~f:worker_kind)
               ~f:(fun kind -> Worker.handle worker ~kind json);
             true
           | _ -> true)
      in
      if continue then loop ()
  in
  loop ();
  Worker.stop worker;
  Outbox.close outbox;
  !outcome
;;

let connect
      ~env
      ?terminals
      ?(log = fun line -> eprintf "prigh tool-host: %s\n%!" line)
      ?(initial_backoff = Time_ns.Span.of_sec 0.5)
      ?(max_backoff = Time_ns.Span.of_sec 10.)
      ~host
      ~port
      ~token
      ?user
      ~name
      ~cwd
      ()
  =
  Switch.run
  @@ fun sw ->
  let terminals =
    Option.value terminals ~default:(lazy (Terminals.create ~env ~sw ()))
  in
  let mcp = Mcp_hub.create ~env ~sw () in
  let address = sprintf "%s:%d" host port in
  let net = Eio.Stdenv.net env in
  let attempt () =
    match Eio.Net.getaddrinfo_stream net host ~service:(Int.to_string port) with
    | [] -> `Failed (sprintf "cannot resolve %s" host)
    | addr :: _ ->
      Switch.run (fun sw ->
        let flow = Eio.Net.connect ~sw net addr in
        serve_connection
          ~env
          ~terminals
          ~mcp
          ~log
          ~address
          ~token
          ~user
          ~name
          ~cwd
          flow)
  in
  let rec loop backoff =
    let backoff =
      match attempt () with
      | `Served -> initial_backoff
      | `Failed message ->
        log message;
        backoff
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        log (sprintf "cannot connect to %s: %s" address (Exn.to_string exn));
        backoff
    in
    log (sprintf "retrying in %s" (Time_ns.Span.to_string_hum backoff));
    Eio.Time.sleep (Eio.Stdenv.clock env) (Time_ns.Span.to_sec backoff);
    loop (Time_ns.Span.min max_backoff (Time_ns.Span.scale backoff 2.))
  in
  loop initial_backoff
;;
