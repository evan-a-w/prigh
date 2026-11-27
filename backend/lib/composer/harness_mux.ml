open! Core
open! Import

module Protocol = struct
  type t =
    | Acp
    | Codex
  [@@deriving sexp_of, equal]
end

module Session = struct
  type t =
    { id : int
    ; lines : string option Eio.Stream.t
    ; ended : Line_transport.Exit.t Promise.t
    ; set_ended : Line_transport.Exit.t Promise.u
    ; mutable finished : bool
    ; mutable stopped : bool
    ; mutable acp_session_id : string option
    ; mutable codex_main_threads : string list
    ; codex_threads : String.Hash_set.t
    ; mutable release_mcp : unit -> unit
    }
end

module Pending_kind = struct
  type t =
    | Initialize of Json.t Or_error.t Promise.u
    | Acp_open of
        { session : Session.t
        ; orig_id : Json.t
        ; pre_registered : string option
        }
    | Codex_open of
        { session : Session.t
        ; orig_id : Json.t
        ; pre_registered : string option
        }
    | Codex_login of
        { session : Session.t
        ; orig_id : Json.t
        ; token : string
        }
    | Forward of
        { session : Session.t
        ; orig_id : Json.t
        }
    | Internal_ignore
end

module Server_req = struct
  type t =
    { session : Session.t
    ; id : Json.t
    ; method_ : string
    }
end

type t =
  { protocol : Protocol.t
  ; transport : Line_transport.t
  ; mutable next_wire_id : int
  ; mutable next_session_id : int
  ; mutable next_route_id : int
  ; mutable init_response : Json.t option
    (* full JSON-RPC response object from [initialize] *)
  ; mutable acp_close_session : bool
  ; mutable codex_logged_in_token : string option
  ; pending : Pending_kind.t Int.Table.t
  ; sessions : Session.t Int.Table.t
  ; by_route_id : Session.t String.Table.t
  ; server_reqs : Server_req.t String.Table.t
  ; mutable alive : bool
  ; mutable exit : Line_transport.Exit.t option
  ; on_empty : t -> unit
  ; on_drain : t -> unit
  ; on_exit : t -> unit
  }

let field name json = Json.member name json

let str name json =
  match field name json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let int name json = Option.bind (field name json) ~f:Json.int

let obj = function
  | Some (`Object _ as o) -> o
  | _ -> `Object []
;;

let list name json =
  match field name json with
  | Some (`Array l) -> l
  | _ -> []
;;

let present name json =
  match field name json with
  | None | Some `Null -> false
  | Some _ -> true
;;

let with_id json id =
  match json with
  | `Object fields ->
    `Object
      (("id", id)
       :: List.filter fields ~f:(fun (k, _) -> not (String.equal k "id")))
  | other -> other
;;

let req_key : Json.t -> string = function
  | `String s -> "s:" ^ s
  | `Number n -> "n:" ^ n
  | json -> "j:" ^ Json.to_string json
;;

let alloc_wire_id t =
  let id = t.next_wire_id in
  t.next_wire_id <- id + 1;
  id
;;

let write_physical t line =
  if t.alive then Line_transport.write_line t.transport line
;;

let push_session (session : Session.t) line =
  if not session.stopped then Eio.Stream.add session.lines (Some line)
;;

let send_acp_close t ~session_id =
  if t.alive && t.acp_close_session
  then (
    let wire_id = alloc_wire_id t in
    Hashtbl.set t.pending ~key:wire_id ~data:Internal_ignore;
    let line =
      Json.to_string
        (`Object
            [ "jsonrpc", `String "2.0"
            ; "id", `Number (Int.to_string wire_id)
            ; "method", `String "session/close"
            ; "params", `Object [ "sessionId", `String session_id ]
            ])
    in
    write_physical t line)
;;

let send_codex_unsubscribe t ~thread_id =
  if t.alive
  then (
    let wire_id = alloc_wire_id t in
    Hashtbl.set t.pending ~key:wire_id ~data:Internal_ignore;
    let line =
      Json.to_string
        (`Object
            [ "id", `Number (Int.to_string wire_id)
            ; "method", `String "thread/unsubscribe"
            ; "params", `Object [ "threadId", `String thread_id ]
            ])
    in
    write_physical t line)
;;

let cancel_reply_for_server_req t ~id ~method_ =
  match t.protocol with
  | Acp ->
    (match method_ with
     | "session/request_permission" ->
       Json.to_string
         (`Object
             [ "jsonrpc", `String "2.0"
             ; "id", id
             ; ( "result"
               , `Object
                   [ "outcome", `Object [ "outcome", `String "cancelled" ] ] )
             ])
     | _ ->
       Json.to_string
         (`Object
             [ "jsonrpc", `String "2.0"
             ; "id", id
             ; ( "error"
               , `Object
                   [ "code", `Number "-32601"
                   ; "message", `String (sprintf "method not found: %s" method_)
                   ] )
             ]))
  | Codex ->
    (match method_ with
     | "item/commandExecution/requestApproval"
     | "item/fileChange/requestApproval" ->
       Json.to_string
         (`Object
             [ "id", id; "result", `Object [ "decision", `String "cancel" ] ])
     | "item/permissions/requestApproval" ->
       Json.to_string
         (`Object
             [ "id", id
             ; ( "result"
               , `Object [ "permissions", `Object []; "scope", `String "turn" ]
               )
             ])
     | "mcpServer/elicitation/request" ->
       Json.to_string
         (`Object [ "id", id; "result", `Object [ "action", `String "cancel" ] ])
     | "item/tool/requestUserInput" ->
       Json.to_string
         (`Object [ "id", id; "result", `Object [ "answers", `Object [] ] ])
     | _ ->
       Json.to_string
         (`Object
             [ "id", id
             ; ( "error"
               , `Object
                   [ "code", `Number "-32602"
                   ; "message", `String "thread closed"
                   ] )
             ]))
;;

let is_auth_error t err =
  let code = int "code" err in
  let msg = Option.value (str "message" err) ~default:"" |> String.lowercase in
  match t.protocol with
  | Acp -> Option.equal Int.equal code (Some (-32000))
  | Codex ->
    Option.equal Int.equal code (Some (-32000))
    || String.is_substring msg ~substring:"authentication required"
    || String.is_substring msg ~substring:"unauthorized"
;;

let unregister_if_owned t ~key (session : Session.t) =
  match Hashtbl.find t.by_route_id key with
  | Some owner when owner.id = session.id -> Hashtbl.remove t.by_route_id key
  | Some _ | None -> ()
;;

let adopt_codex_subagents t (session : Session.t) params =
  let item = obj (field "item" params) in
  match str "type" item with
  | Some "collabAgentToolCall" ->
    if Option.equal String.equal (str "tool" item) (Some "spawnAgent")
    then
      List.iter (list "receiverThreadIds" item) ~f:(fun child ->
        Option.iter (Json.string child) ~f:(fun tid ->
          Hash_set.add session.codex_threads tid;
          Hashtbl.set t.by_route_id ~key:tid ~data:session))
  | Some "subAgentActivity" ->
    (match str "kind" item, str "agentThreadId" item with
     | Some "started", Some tid ->
       Hash_set.add session.codex_threads tid;
       Hashtbl.set t.by_route_id ~key:tid ~data:session
     | _ -> ())
  | _ -> ()
;;

let handle_server_line t line =
  if String.is_empty (String.strip line)
  then ()
  else (
    match Json.parse line with
    | Error _ ->
      Hashtbl.iter t.sessions ~f:(fun session -> push_session session line)
    | Ok json ->
      let method_ = str "method" json in
      let id = field "id" json in
      let params = obj (field "params" json) in
      (match method_, id with
       | None, Some id ->
         (match Json.int id with
          | None -> ()
          | Some wire_id ->
            (match Hashtbl.find_and_remove t.pending wire_id with
             | None | Some Internal_ignore -> ()
             | Some (Initialize resolve) -> Promise.resolve resolve (Ok json)
             | Some (Acp_open { session; orig_id; pre_registered }) ->
               (match field "error" json with
                | Some err ->
                  if is_auth_error t err then t.on_drain t;
                  Option.iter pre_registered ~f:(fun key ->
                    unregister_if_owned t ~key session);
                  push_session session (Json.to_string (with_id json orig_id))
                | None ->
                  let sid =
                    Option.first_some
                      (Option.bind (field "result" json) ~f:(str "sessionId"))
                      pre_registered
                  in
                  if session.stopped
                  then (
                    Option.iter pre_registered ~f:(fun key ->
                      unregister_if_owned t ~key session);
                    Option.iter sid ~f:(fun session_id ->
                      if
                        not
                          (Option.equal
                             String.equal
                             session.acp_session_id
                             (Some session_id))
                      then send_acp_close t ~session_id))
                  else (
                    Option.iter sid ~f:(fun sid ->
                      session.acp_session_id <- Some sid;
                      Hashtbl.set t.by_route_id ~key:sid ~data:session);
                    push_session session (Json.to_string (with_id json orig_id))))
             | Some (Codex_open { session; orig_id; pre_registered }) ->
               (match field "error" json with
                | Some err ->
                  if is_auth_error t err then t.on_drain t;
                  Option.iter pre_registered ~f:(fun key ->
                    unregister_if_owned t ~key session);
                  push_session session (Json.to_string (with_id json orig_id))
                | None ->
                  let tid =
                    Option.bind (field "result" json) ~f:(fun r ->
                      Option.bind (field "thread" r) ~f:(str "id"))
                  in
                  if session.stopped
                  then (
                    Option.iter pre_registered ~f:(fun key ->
                      unregister_if_owned t ~key session);
                    Option.iter tid ~f:(fun thread_id ->
                      send_codex_unsubscribe t ~thread_id))
                  else (
                    Option.iter tid ~f:(fun tid ->
                      Hash_set.add session.codex_threads tid;
                      session.codex_main_threads
                      <- tid :: session.codex_main_threads;
                      Hashtbl.set t.by_route_id ~key:tid ~data:session);
                    push_session session (Json.to_string (with_id json orig_id))))
             | Some (Codex_login { session; orig_id; token }) ->
               (match field "error" json with
                | None when not (String.is_empty token) ->
                  t.codex_logged_in_token <- Some token
                | Some err -> if is_auth_error t err then t.on_drain t
                | None -> ());
               push_session session (Json.to_string (with_id json orig_id))
             | Some (Forward { session; orig_id }) ->
               Option.iter (field "error" json) ~f:(fun err ->
                 if is_auth_error t err then t.on_drain t);
               push_session session (Json.to_string (with_id json orig_id))))
       | Some method_, Some id ->
         (match t.protocol with
          | Acp ->
            let target =
              Option.bind
                (str "sessionId" params)
                ~f:(Hashtbl.find t.by_route_id)
            in
            (match target with
             | Some session when not session.stopped ->
               Hashtbl.set
                 t.server_reqs
                 ~key:(req_key id)
                 ~data:{ session; id; method_ };
               push_session session line
             | Some _ | None ->
               write_physical t (cancel_reply_for_server_req t ~id ~method_))
          | Codex ->
            let target =
              if String.equal method_ "account/chatgptAuthTokens/refresh"
              then
                Hashtbl.data t.sessions |> List.find ~f:(fun s -> not s.stopped)
              else
                Option.bind
                  (str "threadId" params)
                  ~f:(Hashtbl.find t.by_route_id)
            in
            (match target with
             | Some session when not session.stopped ->
               Hashtbl.set
                 t.server_reqs
                 ~key:(req_key id)
                 ~data:{ session; id; method_ };
               push_session session line
             | Some _ | None ->
               write_physical t (cancel_reply_for_server_req t ~id ~method_)))
       | Some method_, None ->
         (match t.protocol with
          | Acp ->
            if String.equal method_ "$/cancel_request"
            then
              Option.iter (field "requestId" params) ~f:(fun r ->
                match Hashtbl.find_and_remove t.server_reqs (req_key r) with
                | Some sr when not sr.session.stopped ->
                  push_session sr.session line
                | Some _ | None -> ())
            else
              Option.iter (str "sessionId" params) ~f:(fun sid ->
                match Hashtbl.find t.by_route_id sid with
                | Some session when not session.stopped ->
                  push_session session line
                | Some _ | None -> ())
          | Codex ->
            if
              String.equal method_ "account/rateLimits/updated"
              || String.equal method_ "configWarning"
            then
              Hashtbl.iter t.sessions ~f:(fun session ->
                push_session session line)
            else if String.equal method_ "serverRequest/resolved"
            then
              Option.iter (field "requestId" params) ~f:(fun r ->
                match Hashtbl.find_and_remove t.server_reqs (req_key r) with
                | Some sr when not sr.session.stopped ->
                  push_session sr.session line
                | Some _ | None -> ())
            else
              Option.iter (str "threadId" params) ~f:(fun tid ->
                match Hashtbl.find t.by_route_id tid with
                | Some session when not session.stopped ->
                  if String.equal method_ "item/started"
                  then adopt_codex_subagents t session params;
                  push_session session line
                | Some _ | None -> ()))
       | None, None -> ()))
;;

let finish_session (session : Session.t) exit =
  if not session.stopped
  then (
    session.stopped <- true;
    session.release_mcp ();
    if not (Promise.is_resolved session.ended)
    then Promise.resolve session.set_ended exit;
    Eio.Stream.add session.lines None)
;;

let stop_session t (session : Session.t) =
  if not session.stopped
  then (
    session.stopped <- true;
    Hashtbl.remove t.sessions session.id;
    (match t.protocol with
     | Acp ->
       Option.iter session.acp_session_id ~f:(fun sid ->
         unregister_if_owned t ~key:sid session;
         send_acp_close t ~session_id:sid)
     | Codex ->
       Hash_set.iter session.codex_threads ~f:(fun tid ->
         unregister_if_owned t ~key:tid session);
       List.iter session.codex_main_threads ~f:(fun thread_id ->
         send_codex_unsubscribe t ~thread_id));
    let dangling =
      Hashtbl.fold t.server_reqs ~init:[] ~f:(fun ~key ~data:sr acc ->
        if sr.session.id = session.id then (key, sr) :: acc else acc)
    in
    List.iter dangling ~f:(fun (key, (sr : Server_req.t)) ->
      Hashtbl.remove t.server_reqs key;
      write_physical
        t
        (cancel_reply_for_server_req t ~id:sr.id ~method_:sr.method_));
    session.release_mcp ();
    let exit =
      Option.value
        t.exit
        ~default:{ Line_transport.Exit.code = None; stderr_tail = "" }
    in
    if not (Promise.is_resolved session.ended)
    then Promise.resolve session.set_ended exit;
    Eio.Stream.add session.lines None;
    if Hashtbl.is_empty t.sessions then t.on_empty t)
;;

let handle_client_line t (session : Session.t) line =
  if session.stopped || not t.alive
  then ()
  else (
    match Json.parse line with
    | Error _ -> write_physical t line
    | Ok json ->
      let method_ = str "method" json in
      let id = field "id" json in
      let params = obj (field "params" json) in
      (match method_, id with
       | Some "initialize", Some orig_id ->
         (match t.init_response with
          | Some cached ->
            push_session session (Json.to_string (with_id cached orig_id))
          | None ->
            let wire_id = alloc_wire_id t in
            Hashtbl.set
              t.pending
              ~key:wire_id
              ~data:(Forward { session; orig_id });
            write_physical
              t
              (Json.to_string (with_id json (`Number (Int.to_string wire_id)))))
       | Some "initialized", None when Protocol.equal t.protocol Codex -> ()
       | Some "account/login/start", Some orig_id
         when Protocol.equal t.protocol Codex ->
         let tok = str "accessToken" params in
         (match tok, t.codex_logged_in_token with
          | Some tok, Some prev when String.equal tok prev ->
            push_session
              session
              (Json.to_string (`Object [ "id", orig_id; "result", `Object [] ]))
          | _ ->
            let wire_id = alloc_wire_id t in
            Hashtbl.set
              t.pending
              ~key:wire_id
              ~data:
                (Codex_login
                   { session; orig_id; token = Option.value tok ~default:"" });
            write_physical
              t
              (Json.to_string (with_id json (`Number (Int.to_string wire_id)))))
       | ( Some (("session/new" | "session/load" | "session/resume") as m)
         , Some orig_id )
         when Protocol.equal t.protocol Acp ->
         let pre_registered =
           if String.equal m "session/new"
           then None
           else
             Option.filter (str "sessionId" params) ~f:(fun sid ->
               not (Hashtbl.mem t.by_route_id sid))
         in
         Option.iter pre_registered ~f:(fun sid ->
           session.acp_session_id <- Some sid;
           Hashtbl.set t.by_route_id ~key:sid ~data:session);
         let wire_id = alloc_wire_id t in
         Hashtbl.set
           t.pending
           ~key:wire_id
           ~data:(Acp_open { session; orig_id; pre_registered });
         write_physical
           t
           (Json.to_string (with_id json (`Number (Int.to_string wire_id))))
       | ( Some (("thread/start" | "thread/resume" | "thread/fork") as m)
         , Some orig_id )
         when Protocol.equal t.protocol Codex ->
         let pre_registered =
           if String.equal m "thread/resume"
           then
             Option.filter (str "threadId" params) ~f:(fun tid ->
               not (Hashtbl.mem t.by_route_id tid))
           else None
         in
         Option.iter pre_registered ~f:(fun tid ->
           Hash_set.add session.codex_threads tid;
           Hashtbl.set t.by_route_id ~key:tid ~data:session);
         let wire_id = alloc_wire_id t in
         Hashtbl.set
           t.pending
           ~key:wire_id
           ~data:(Codex_open { session; orig_id; pre_registered });
         write_physical
           t
           (Json.to_string (with_id json (`Number (Int.to_string wire_id))))
       | Some _, Some orig_id ->
         let wire_id = alloc_wire_id t in
         Hashtbl.set t.pending ~key:wire_id ~data:(Forward { session; orig_id });
         write_physical
           t
           (Json.to_string (with_id json (`Number (Int.to_string wire_id))))
       | Some _, None -> write_physical t line
       | None, Some id ->
         (match Hashtbl.find_and_remove t.server_reqs (req_key id) with
          | Some { method_ = "account/chatgptAuthTokens/refresh"; _ }
            when Protocol.equal t.protocol Codex ->
            Option.iter
              (Option.bind (field "result" json) ~f:(str "accessToken"))
              ~f:(fun tok ->
                if not (String.is_empty tok)
                then t.codex_logged_in_token <- Some tok)
          | Some _ | None -> ());
         write_physical t line
       | None, None -> write_physical t line))
;;

let lease t =
  let id = t.next_session_id in
  t.next_session_id <- id + 1;
  let ended, set_ended = Promise.create () in
  let session : Session.t =
    { id
    ; lines = Eio.Stream.create Int.max_value
    ; ended
    ; set_ended
    ; finished = false
    ; stopped = false
    ; acp_session_id = None
    ; codex_main_threads = []
    ; codex_threads = String.Hash_set.create ()
    ; release_mcp = ignore
    }
  in
  if not t.alive
  then (
    let exit =
      Option.value
        t.exit
        ~default:{ Line_transport.Exit.code = None; stderr_tail = "" }
    in
    session.stopped <- true;
    Promise.resolve set_ended exit;
    Eio.Stream.add session.lines None)
  else Hashtbl.set t.sessions ~key:id ~data:session;
  let mcp =
    match Line_transport.mcp t.transport with
    | None -> None
    | Some { bridge; serve; route = None } ->
      (* Unrouted legacy host: single-use session uses the base bridge. *)
      Some { Line_transport.Mcp.bridge; serve; route = None }
    | Some { bridge; serve = _; route = Some register } ->
      let seq = t.next_route_id in
      t.next_route_id <- seq + 1;
      let route_id = sprintf "r%d-%s" seq (Pkce.random_hex 8) in
      let routed_bridge =
        Injected_tools.Bridge.with_route bridge ~route:route_id
      in
      let serve injected =
        if not session.stopped
        then (
          let _, release = register route_id injected in
          session.release_mcp <- release)
      in
      Some { Line_transport.Mcp.bridge = routed_bridge; serve; route = None }
  in
  Line_transport.create
    ~read_line:(fun () ->
      if session.finished
      then None
      else (
        match Eio.Stream.take session.lines with
        | Some _ as line -> line
        | None ->
          session.finished <- true;
          None))
    ~write_line:(fun line -> handle_client_line t session line)
    ~stop:(fun () -> stop_session t session)
    ~ended:(fun () -> Promise.await session.ended)
    ~is_ended:(fun () -> Promise.is_resolved session.ended)
    ?mcp
    ?notice:(Line_transport.notice t.transport)
    ()
;;

let active_sessions t = Hashtbl.length t.sessions
let alive t = t.alive

let stop t =
  if t.alive
  then (
    t.alive <- false;
    let exit = { Line_transport.Exit.code = None; stderr_tail = "" } in
    t.exit <- Some exit;
    let sessions = Hashtbl.data t.sessions in
    Hashtbl.clear t.sessions;
    Hashtbl.clear t.by_route_id;
    Hashtbl.clear t.server_reqs;
    Hashtbl.iter t.pending ~f:(function
      | Initialize resolve ->
        Promise.resolve
          resolve
          (Or_error.error_string "the process was stopped")
      | _ -> ());
    Hashtbl.clear t.pending;
    List.iter sessions ~f:(fun s -> finish_session s exit);
    Line_transport.stop t.transport)
;;

let acp_close_capable result_json =
  let caps = obj (field "agentCapabilities" result_json) in
  let session = obj (field "sessionCapabilities" caps) in
  present "close" session
;;

let acp_protocol_v1 result_json =
  Option.equal Int.equal (int "protocolVersion" result_json) (Some 1)
;;

let create
      ~sw
      ~protocol
      ?(request_timeout = Time_float.Span.of_sec 60.)
      ?(on_empty = ignore)
      ?(on_drain = ignore)
      ?(on_exit = ignore)
      transport
  =
  let t =
    { protocol
    ; transport
    ; next_wire_id = 1
    ; next_session_id = 1
    ; next_route_id = 1
    ; init_response = None
    ; acp_close_session = false
    ; codex_logged_in_token = None
    ; pending = Int.Table.create ()
    ; sessions = Int.Table.create ()
    ; by_route_id = String.Table.create ()
    ; server_reqs = String.Table.create ()
    ; alive = true
    ; exit = None
    ; on_empty
    ; on_drain
    ; on_exit
    }
  in
  Fiber.fork ~sw (fun () ->
    let rec loop () =
      match Line_transport.read_line transport with
      | Some line ->
        handle_server_line t line;
        loop ()
      | None -> ()
    in
    loop ();
    let exit = Line_transport.ended transport in
    let was_alive = t.alive in
    t.alive <- false;
    t.exit <- Some exit;
    Hashtbl.iter t.pending ~f:(function
      | Initialize resolve ->
        Promise.resolve
          resolve
          (Or_error.error_string
             (Adapter.Driver.exit_message ~display_name:"the process" exit))
      | _ -> ());
    Hashtbl.clear t.pending;
    let sessions = Hashtbl.data t.sessions in
    Hashtbl.clear t.sessions;
    Hashtbl.clear t.by_route_id;
    Hashtbl.clear t.server_reqs;
    List.iter sessions ~f:(fun s -> finish_session s exit);
    if was_alive then t.on_exit t);
  let init_promise, init_resolve = Promise.create () in
  Hashtbl.set t.pending ~key:0 ~data:(Initialize init_resolve);
  let init_line =
    match protocol with
    | Acp ->
      Acp_client_codec.initialize
        (Acp_client_codec.create ())
        ~client_name:"prigh"
        ~client_version:Version.to_string
    | Codex ->
      Codex_codec.initialize
        (Codex_codec.create ())
        ~client_name:"prigh"
        ~client_version:Version.to_string
  in
  write_physical t init_line;
  let answer =
    Fiber.first
      (fun () -> Promise.await init_promise)
      (fun () ->
         Eio_unix.sleep (Time_float.Span.to_sec request_timeout);
         Hashtbl.remove t.pending 0;
         Or_error.errorf
           "no answer to initialize within %s"
           (Time_float.Span.to_string_hum request_timeout))
  in
  match answer with
  | Error _ when not t.alive ->
    (* Process exited before answering initialize: return single-use dead mux
       so the adapter formats its standard start_failed message with stderr. *)
    Ok (t, false)
  | Error e ->
    stop t;
    Error e
  | Ok resp ->
    t.init_response <- Some resp;
    let mcp_ok =
      match Line_transport.mcp transport with
      | Some { route = None; _ } -> false
      | Some { route = Some _; _ } | None -> true
    in
    let poolable =
      match field "error" resp, field "result" resp with
      | Some _, _ -> false
      | None, result ->
        let result = obj result in
        (match protocol with
         | Acp ->
           t.acp_close_session <- acp_close_capable result;
           mcp_ok && t.acp_close_session && acp_protocol_v1 result
         | Codex ->
           write_physical
             t
             (Json.to_string (`Object [ "method", `String "initialized" ]));
           mcp_ok)
    in
    Ok (t, poolable)
;;
