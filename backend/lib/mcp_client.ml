open! Core
open! Import

let protocol_version = "2025-06-18"

module Tool = struct
  type t =
    { name : string
    ; description : string
    ; input_schema : Json.t
    ; read_only : bool
    }
  [@@deriving sexp_of]

  let of_json json =
    match Json.member "name" json with
    | Some (`String name) ->
      Some
        { name
        ; description =
            Option.bind (Json.member "description" json) ~f:Json.string
            |> Option.value ~default:""
        ; input_schema =
            Json.member "inputSchema" json
            |> Option.value ~default:(`Object [ "type", `String "object" ])
        ; read_only =
            (match
               Option.bind
                 (Json.member "annotations" json)
                 ~f:(Json.member "readOnlyHint")
             with
             | Some `True -> true
             | _ -> false)
        }
    | _ -> None
  ;;
end

module Message = struct
  let base fields = `Object (("jsonrpc", `String "2.0") :: fields)
  let id id = `Number (Int.to_string id)

  let request ~id:n ~method_ ~params =
    base [ "id", id n; "method", `String method_; "params", params ]
  ;;

  let notification ~method_ ~params =
    base [ "method", `String method_; "params", params ]
  ;;

  let result ~id result = base [ "id", id; "result", result ]

  let error ~id ~code ~message =
    base
      [ "id", id
      ; ( "error"
        , `Object
            [ "code", `Number (Int.to_string code); "message", `String message ]
        )
      ]
  ;;

  (* Requests the server sends us: only [ping] is supported. *)
  let answer ~id ~method_ =
    match method_ with
    | "ping" -> result ~id (`Object [])
    | _ ->
      error ~id ~code:(-32601) ~message:(sprintf "method not found: %s" method_)
  ;;

  module Incoming = struct
    type t =
      | Response of
          { id : int option
          ; result : (Json.t, string) Result.t
          }
      | Request of
          { id : Json.t
          ; method_ : string
          }
      | Notification of string
      | Other

    let error_message json =
      let message =
        Option.bind (Json.member "message" json) ~f:Json.string
        |> Option.value ~default:(Json.to_string json)
      in
      match Option.bind (Json.member "code" json) ~f:Json.int with
      | Some code -> sprintf "%s (MCP error %d)" message code
      | None -> message
    ;;

    let of_json json =
      let id = Json.member "id" json in
      match Json.member "method" json, id with
      | Some (`String method_), (None | Some `Null) -> Notification method_
      | Some (`String method_), Some id -> Request { id; method_ }
      | _, _ ->
        let id =
          match id with
          | Some (`Number n | `String n) -> Int.of_string_opt n
          | _ -> None
        in
        (match Json.member "result" json, Json.member "error" json with
         | Some result, _ -> Response { id; result = Ok result }
         | None, Some error ->
           Response { id; result = Error (error_message error) }
         | None, None -> Other)
    ;;

    (* An SSE event or an HTTP body may carry a batch. *)
    let of_json_many = function
      | `Array items -> List.map items ~f:of_json
      | json -> [ of_json json ]
    ;;
  end
end

module State = struct
  type t =
    { mutable next_id : int
    ; mutable tools : Tool.t list option
    ; mutable tools_generation : int
    ; mutable failure : string option
    }

  let create () =
    { next_id = 0; tools = None; tools_generation = 0; failure = None }
  ;;

  let on_notification t method_ =
    match method_ with
    | "notifications/tools/list_changed" ->
      t.tools <- None;
      t.tools_generation <- t.tools_generation + 1
    | _ -> ()
  ;;

  let fail t reason = if Option.is_none t.failure then t.failure <- Some reason
end

module Transport = struct
  type t =
    { send : Json.t -> (unit, string) Result.t
    ; request : id:int -> Json.t -> (Json.t, string) Result.t
    ; stderr_tail : unit -> string
    ; close : unit -> unit
    }
end

module Stdio = struct
  let stderr_capacity = 4096
  let max_message_bytes = 64 * 1024 * 1024

  let environment ~extra =
    let overridden = String.Set.of_list (List.map extra ~f:fst) in
    let inherited =
      Array.to_list (Core_unix.environment ())
      |> List.filter ~f:(fun entry ->
        match String.lsplit2 entry ~on:'=' with
        | Some (key, _) -> not (Set.mem overridden key)
        | None -> true)
    in
    inherited @ List.map extra ~f:(fun (k, v) -> k ^ "=" ^ v)
  ;;

  let is_executable path =
    match Core_unix.access path [ `Exec ] with
    | Ok () -> not (Sys_unix.is_directory_exn path)
    | Error _ -> false
  ;;

  (* Resolved here rather than by Eio so that [PATH] comes from the
     server's environment and relative commands from its directory. *)
  let resolve ~(server : Mcp_config.Server.t) ~environment command =
    if String.mem command '/'
    then (
      let path =
        if Filename.is_relative command
        then Filename.concat server.dir command
        else command
      in
      if is_executable path
      then Ok path
      else
        Error
          (sprintf
             "%s does not exist or is not executable; fix \"command\" in %s"
             path
             server.source))
    else (
      let path =
        List.find_map environment ~f:(String.chop_prefix ~prefix:"PATH=")
        |> Option.value ~default:"/usr/bin:/bin"
      in
      List.find_map (String.split path ~on:':') ~f:(fun dir ->
        let candidate =
          Filename.concat (if String.is_empty dir then "." else dir) command
        in
        Option.some_if (is_executable candidate) candidate)
      |> Result.of_option
           ~error:
             (sprintf
                "the command %S was not found on PATH; install it, or fix \
                 \"command\" in %s"
                command
                server.source))
  ;;

  let tail output =
    String.strip (String.concat ~sep:"\n" (Output_tail.lines output))
  ;;

  let exit_reason status ~stderr =
    let how =
      match status with
      | `Exited n -> sprintf "exited (code %d)" n
      | `Signaled n ->
        sprintf "was killed (%s)" (Signal.to_string (Signal.of_caml_int n))
    in
    match tail stderr with
    | "" -> "the server " ^ how
    | stderr -> sprintf "the server %s: %s" how stderr
  ;;

  let pump source ~on_data =
    let buf = Cstruct.create 65536 in
    try
      while true do
        let n = Eio.Flow.single_read source buf in
        on_data (Cstruct.to_string buf ~len:n)
      done
    with
    | End_of_file -> ()
  ;;

  let connect
        ~(env : Env.t)
        ~sw
        ~(state : State.t)
        ~server
        ~command
        ~args
        ~extra_env
    =
    let environment = environment ~extra:extra_env in
    let open Result.Let_syntax in
    let%bind executable = resolve ~server ~environment command in
    let%bind () =
      if Sys_unix.is_directory_exn server.Mcp_config.Server.dir
      then Ok ()
      else Error (sprintf "its directory %s does not exist" server.dir)
    in
    let stdin_r, stdin_w = Eio_unix.pipe sw in
    let stdout_r, stdout_w = Eio_unix.pipe sw in
    let stderr_r, stderr_w = Eio_unix.pipe sw in
    let close_child_ends () =
      Eio.Resource.close stdin_r;
      Eio.Resource.close stdout_w;
      Eio.Resource.close stderr_w
    in
    let%map child =
      match
        Eio_unix.Process.spawn_unix
          ~sw
          (Eio.Stdenv.process_mgr env)
          ~cwd:Eio.Path.(Eio.Stdenv.fs env / server.dir)
          ~pgid:0
          ~fds:
            [ 0, Eio_unix.Resource.fd stdin_r, `Blocking
            ; 1, Eio_unix.Resource.fd stdout_w, `Blocking
            ; 2, Eio_unix.Resource.fd stderr_w, `Blocking
            ]
          ~env:(Array.of_list environment)
          ~executable
          (command :: args)
      with
      | child ->
        close_child_ends ();
        Ok child
      | exception exn ->
        close_child_ends ();
        Eio.Resource.close stdin_w;
        Eio.Resource.close stdout_r;
        Eio.Resource.close stderr_r;
        Error (sprintf "could not start %s: %s" command (Exn.to_string exn))
    in
    let pid = Pid.of_int (Eio.Process.pid child) in
    let kill_group () = Signal_unix.send_i Signal.kill (`Group pid) in
    let stderr = Output_tail.create ~capacity:stderr_capacity () in
    let pending = Int.Table.create () in
    let write_lock = Eio.Mutex.create () in
    let stdin_open = ref true in
    let send json =
      match state.failure with
      | Some failure -> Error failure
      | None ->
        Eio.Mutex.use_rw ~protect:true write_lock (fun () ->
          match Eio.Flow.copy_string (Json.to_string json ^ "\n") stdin_w with
          | () -> Ok ()
          | exception (Eio.Io _ as exn) ->
            Error
              (sprintf "could not write to the server: %s" (Exn.to_string exn)))
    in
    let dispatch line =
      match Json.parse line with
      | Error _ -> ()
      | Ok json ->
        List.iter (Message.Incoming.of_json_many json) ~f:(function
          | Response { id = Some id; result } ->
            Option.iter (Hashtbl.find_and_remove pending id) ~f:(fun resolver ->
              Promise.resolve resolver result)
          | Response { id = None; _ } | Other -> ()
          | Request { id; method_ } ->
            Fiber.fork ~sw (fun () ->
              ignore (send (Message.answer ~id ~method_) : _ Result.t))
          | Notification method_ -> State.on_notification state method_)
    in
    let read_stdout () =
      let reader = Eio.Buf_read.of_flow stdout_r ~max_size:max_message_bytes in
      match
        while true do
          dispatch (Eio.Buf_read.line reader)
        done
      with
      | () -> ()
      | exception End_of_file -> ()
      | exception Eio.Buf_read.Buffer_limit_exceeded ->
        State.fail state "the server sent a message over 64MB"
    in
    let stderr_done, stderr_done_resolver = Promise.create () in
    Fiber.fork_daemon ~sw (fun () ->
      pump stderr_r ~on_data:(Output_tail.add stderr);
      Promise.resolve stderr_done_resolver ();
      `Stop_daemon);
    Fiber.fork_daemon ~sw (fun () ->
      read_stdout ();
      (* Once stdout is closed the connection is dead: reap the server and
         anything it left behind, which may hold stderr open. *)
      let status =
        match
          Eio.Time.with_timeout (Eio.Stdenv.clock env) 2. (fun () ->
            Ok (Eio.Process.await child))
        with
        | Ok status -> status
        | Error `Timeout ->
          kill_group ();
          Eio.Process.await child
      in
      kill_group ();
      Promise.await stderr_done;
      State.fail state (exit_reason status ~stderr);
      let reason = Option.value_exn state.failure in
      Hashtbl.iter pending ~f:(fun resolver ->
        Promise.resolve resolver (Error reason));
      Hashtbl.clear pending;
      Eio.Resource.close stdout_r;
      Eio.Resource.close stderr_r;
      `Stop_daemon);
    let request ~id json =
      match state.failure with
      | Some failure -> Error failure
      | None ->
        let promise, resolver = Promise.create () in
        Hashtbl.set pending ~key:id ~data:resolver;
        Exn.protect
          ~finally:(fun () -> Hashtbl.remove pending id)
          ~f:(fun () ->
            match send json with
            | Error e -> Error e
            | Ok () -> Promise.await promise)
    in
    let close () =
      State.fail state "the connection was closed";
      if !stdin_open
      then (
        stdin_open := false;
        Eio.Resource.close stdin_w;
        (match
           Eio.Time.with_timeout (Eio.Stdenv.clock env) 1. (fun () ->
             Ok (Eio.Process.await child))
         with
         | Ok _ | Error `Timeout -> ());
        kill_group ();
        ignore (Eio.Process.await child : Eio.Process.exit_status))
    in
    { Transport.send; request; stderr_tail = (fun () -> tail stderr); close }
  ;;
end

module Http = struct
  let snippet body =
    let body = String.strip body in
    if String.length body > 300 then String.prefix body 300 ^ "..." else body
  ;;

  let status_error ~(server : Mcp_config.Server.t) ~url ~status ~body =
    let message =
      match snippet body with
      | "" -> sprintf "HTTP %d from %s" status url
      | body -> sprintf "HTTP %d from %s: %s" status url body
    in
    match status with
    | 401 | 403 ->
      sprintf
        "%s; if it needs credentials, add an Authorization header to the \
         server's \"headers\" in %s"
        message
        server.source
    | _ -> message
  ;;

  let connect
        ~env
        ~sw
        ~(state : State.t)
        ~(server : Mcp_config.Server.t)
        ~url
        ~headers
    =
    let session_id = ref None in
    let initialized = ref false in
    let request_headers () =
      [ "Content-Type", "application/json"
      ; "Accept", "application/json, text/event-stream"
      ]
      @ headers
      @ (if !initialized
         then [ "MCP-Protocol-Version", protocol_version ]
         else [])
      @ Option.to_list
          (Option.map !session_id ~f:(fun id -> "Mcp-Session-Id", id))
    in
    (* Posts [json]; with [expect], returns the response to that request id,
       from a JSON body or an SSE stream. *)
    let rec post ?expect json =
      let response = ref None in
      let is_sse () =
        match
          Option.bind !response ~f:(fun r ->
            Http_client.Response.header r "content-type")
        with
        | Some content_type ->
          String.is_prefix
            (String.lowercase content_type)
            ~prefix:"text/event-stream"
        | None -> false
      in
      let body = Buffer.create 256 in
      let sse = Sse.create () in
      let found = ref None in
      let handle data =
        match Json.parse data with
        | Error _ -> ()
        | Ok json ->
          List.iter (Message.Incoming.of_json_many json) ~f:(function
            | Response { id; result } ->
              if Option.equal Int.equal id expect then found := Some result
            | Request { id; method_ } ->
              Fiber.fork ~sw (fun () ->
                ignore
                  (post (Message.answer ~id ~method_)
                   : (Json.t, string) Result.t))
            | Notification method_ -> State.on_notification state method_
            | Other -> ())
      in
      let on_chunk chunk =
        if is_sse ()
        then List.iter (Sse.feed sse chunk) ~f:(fun event -> handle event.data)
        else Buffer.add_string body chunk
      in
      match
        Http_client.post_stream
          ~env
          ~url
          ~headers:(request_headers ())
          ~body:(Json.to_string json)
          ~on_response:(fun r -> response := Some r)
          ~on_chunk
          ()
      with
      | Error e -> Error (sprintf "%s: %s" url (Http_client.Error.to_string e))
      | Ok r ->
        if Option.is_none !session_id
        then session_id := Http_client.Response.header r "mcp-session-id";
        if r.status >= 400
        then (
          if r.status = 404 && Option.is_some !session_id
          then State.fail state "the server ended the session (HTTP 404)";
          Error
            (status_error
               ~server
               ~url
               ~status:r.status
               ~body:(Buffer.contents body)))
        else (
          if is_sse ()
          then Option.iter (Sse.finish sse) ~f:(fun event -> handle event.data)
          else if Buffer.length body > 0
          then handle (Buffer.contents body);
          match expect, !found with
          | None, _ -> Ok `Null
          | Some _, Some result -> result
          | Some id, None ->
            Error
              (sprintf
                 "HTTP %d from %s had no response to request %d"
                 r.status
                 url
                 id))
    in
    let send json =
      match state.failure with
      | Some failure -> Error failure
      | None -> Result.map (post json) ~f:ignore
    in
    let request ~id json =
      match state.failure with
      | Some failure -> Error failure
      | None ->
        let result = post ~expect:id json in
        if
          Result.is_ok result
          && Option.equal
               String.equal
               (Option.bind (Json.member "method" json) ~f:Json.string)
               (Some "initialize")
        then initialized := true;
        result
    in
    { Transport.send
    ; request
    ; stderr_tail = (fun () -> "")
    ; close = (fun () -> State.fail state "the connection was closed")
    }
  ;;
end

type t =
  { state : State.t
  ; transport : Transport.t
  }

let next_id t =
  t.state.next_id <- t.state.next_id + 1;
  t.state.next_id
;;

let request t ~method_ ~params =
  let id = next_id t in
  t.transport.request ~id (Message.request ~id ~method_ ~params)
;;

let notify t ~method_ ~params =
  t.transport.send (Message.notification ~method_ ~params)
;;

let failure t = t.state.failure
let close t = t.transport.close ()

let connect
      ~env
      ~sw
      ?(timeout = Time_ns.Span.of_int_sec 60)
      (server : Mcp_config.Server.t)
  =
  let state = State.create () in
  let transport =
    match server.transport with
    | Stdio { command; args; env = extra_env } ->
      Stdio.connect ~env ~sw ~state ~server ~command ~args ~extra_env
    | Http { url; headers } ->
      Ok (Http.connect ~env ~sw ~state ~server ~url ~headers)
  in
  match transport with
  | Error e -> Or_error.error_string e
  | Ok transport ->
    let t = { state; transport } in
    let handshake () =
      let open Result.Let_syntax in
      let%bind (_ : Json.t) =
        request
          t
          ~method_:"initialize"
          ~params:
            (`Object
                [ "protocolVersion", `String protocol_version
                ; "capabilities", `Object []
                ; ( "clientInfo"
                  , `Object
                      [ "name", `String "prigh"
                      ; "version", `String Version.to_string
                      ] )
                ])
      in
      notify t ~method_:"notifications/initialized" ~params:(`Object [])
    in
    let with_stderr message =
      match transport.stderr_tail () with
      | "" -> message
      | stderr -> sprintf "%s; its stderr: %s" message stderr
    in
    (match
       Eio.Time.with_timeout
         (Eio.Stdenv.clock env)
         (Time_ns.Span.to_sec timeout)
         (fun () -> Ok (handshake ()))
     with
     | exception exn ->
       Eio.Cancel.protect (fun () -> close t);
       raise exn
     | Ok (Ok ()) -> Ok t
     | Ok (Error e) ->
       close t;
       Or_error.error_string e
     | Error `Timeout ->
       let message =
         with_stderr
           (sprintf
              "the server did not answer initialize within %s"
              (Time_ns.Span.to_string_hum timeout))
       in
       close t;
       Or_error.error_string message)
;;

let tools t =
  match t.state.tools with
  | Some tools -> Ok tools
  | None ->
    let generation = t.state.tools_generation in
    let rec pages ~cursor acc =
      let params =
        match cursor with
        | None -> `Object []
        | Some cursor -> `Object [ "cursor", `String cursor ]
      in
      match request t ~method_:"tools/list" ~params with
      | Error e -> Error e
      | Ok result ->
        let page =
          Option.bind (Json.member "tools" result) ~f:Json.list
          |> Option.value ~default:[]
          |> List.filter_map ~f:Tool.of_json
        in
        let acc = List.rev_append page acc in
        (match Json.member "nextCursor" result with
         | Some (`String cursor) when not (String.is_empty cursor) ->
           pages ~cursor:(Some cursor) acc
         | _ -> Ok (List.rev acc))
    in
    (match pages ~cursor:None [] with
     | Error e -> Or_error.error_string e
     | Ok tools ->
       if t.state.tools_generation = generation then t.state.tools <- Some tools;
       Ok tools)
;;

let tool_result json =
  let content =
    Option.bind (Json.member "content" json) ~f:Json.list
    |> Option.value ~default:[]
  in
  let texts, images =
    List.partition_map content ~f:(fun item ->
      let field name = Option.bind (Json.member name item) ~f:Json.string in
      match field "type", field "text", field "data", field "mimeType" with
      | Some "text", Some text, _, _ -> First text
      | Some "image", _, Some data, Some mime_type ->
        Second { Image.mime_type; data }
      | _ -> First (Json.to_string item))
  in
  let text =
    match texts, images, Json.member "structuredContent" json with
    | [], [], Some structured -> Json.to_string structured
    | _ -> String.concat ~sep:"\n" texts
  in
  let is_error =
    match Json.member "isError" json with
    | Some `True -> true
    | _ -> false
  in
  { Tool_result.text; is_error; images }
;;

let call t ~cancel ~tool ~arguments =
  let id = next_id t in
  let message =
    Message.request
      ~id
      ~method_:"tools/call"
      ~params:(`Object [ "name", `String tool; "arguments", arguments ])
  in
  match
    Cancellation.protect cancel ~f:(fun () -> t.transport.request ~id message)
  with
  | None ->
    ignore
      (notify
         t
         ~method_:"notifications/cancelled"
         ~params:
           (`Object
               [ "requestId", Message.id id
               ; "reason", `String "cancelled by the user"
               ])
       : (unit, string) Result.t);
    Tool_result.error "[cancelled]"
  | Some (Error e) -> Tool_result.error e
  | Some (Ok result) -> tool_result result
;;
