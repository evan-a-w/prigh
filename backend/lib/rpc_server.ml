open! Core
open! Import

let methods =
  [ "ping"
  ; "hello"
  ; "set_active_host"
  ; "tool_exec_output"
  ; "tool_exec_result"
  ; "prompt"
  ; "steer"
  ; "follow_up"
  ; "abort"
  ; "dequeue"
  ; "shell"
  ; "get_state"
  ; "get_messages"
  ; "get_entries"
  ; "set_model"
  ; "set_thinking"
  ; "list_models"
  ; "compact"
  ; "new_session"
  ; "switch_session"
  ; "list_sessions"
  ; "set_session_name"
  ; "delete_session"
  ; "export"
  ; "import"
  ; "fork"
  ; "clone"
  ; "rewind"
  ; "session_stats"
  ; "set_cwd"
  ; "list_paths"
  ; "list_dirs"
  ; "get_config"
  ; "set_config"
  ; "tool_confirm_respond"
  ; "auth_status"
  ; "login"
  ; "auth_respond"
  ; "auth_cancel"
  ; "logout"
  ]
;;

let param params name =
  match params with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let string_param params name =
  match param params name with
  | Some (`String s) -> Ok s
  | Some _ -> Or_error.errorf "param %S must be a string" name
  | None -> Or_error.errorf "missing param %S" name
;;

let string_param_opt params name =
  match param params name with
  | None | Some `Null -> Ok None
  | Some _ -> Or_error.map (string_param params name) ~f:Option.some
;;

let string_list_param params name =
  match param params name with
  | None -> Ok []
  | Some (`String s) -> Ok [ s ]
  | Some (`Array items) ->
    Or_error.all
      (List.map items ~f:(function
         | `String s -> Ok s
         | _ -> Or_error.errorf "param %S must contain only strings" name))
  | Some _ -> Or_error.errorf "param %S must be an array of strings" name
;;

let ok json = Ok json
let empty = ok (`Object [])
let unit_result r = Or_error.map r ~f:(fun () -> `Object [])

let provider_param params =
  Or_error.bind (string_param params "provider") ~f:(fun s ->
    match Provider_id.of_string s with
    | Some p -> Ok p
    | None ->
      Or_error.errorf
        "unknown provider %S (one of: %s)"
        s
        (String.concat
           ~sep:", "
           (List.map Provider_id.all ~f:Provider_id.to_string)))
;;

module Client = struct
  type t =
    { id : string
    ; seq : int
    ; mutable name : string
    ; mutable tools : bool
    ; mutable cwd : string option
    ; mutable agent : Agent.t
    ; mutable authed : bool
    ; send : Json.t -> unit
    }

  let id t = t.id
end

type t =
  { login : Login_manager.t
  ; token : string option (** required in [hello] before anything else *)
  ; sessions_dir : string
  ; cwd : string
  ; new_agent : ?session:Session.t -> cwd:string -> unit -> Agent.t
  ; default_agent : Agent.t option
  ; agents : Agent.t String.Table.t (** live sessions by session id *)
  ; clients : Client.t String.Table.t
  ; mutable client_seq : int
  ; execs : (string * Agent.t) String.Table.t
    (** in-flight remote executions by exec id: host client id and session *)
  }

let agent_of_client _t (client : Client.t) = client.agent
let session_id agent = Session.id (Agent.session agent)

let clients_of t agent =
  Hashtbl.data t.clients
  |> List.filter ~f:(fun (c : Client.t) -> phys_equal c.agent agent)
;;

let maybe_evict t agent =
  if
    (not (Option.exists t.default_agent ~f:(phys_equal agent)))
    && (not (Agent.is_running agent))
    && List.is_empty (clients_of t agent)
  then Hashtbl.remove t.agents (session_id agent)
;;

let host_of (client : Client.t) =
  { Agent.Host.id = client.id
  ; name = client.name
  ; cwd = Option.value client.cwd ~default:(Agent.state client.agent).cwd
  ; session_id = Some (session_id client.agent)
  ; session_name = Session.name (Agent.session client.agent)
  }
;;

let hosts t =
  Hashtbl.data t.clients
  |> List.filter ~f:(fun (c : Client.t) -> c.tools)
  |> List.sort ~compare:(fun (a : Client.t) b -> Int.compare a.seq b.seq)
  |> List.map ~f:host_of
;;

(* Snapshots the agents: setting hosts emits state changes, which may evict. *)
let publish_hosts t =
  let hosts = hosts t in
  List.iter (Hashtbl.data t.agents) ~f:(fun agent ->
    Agent.set_hosts agent hosts)
;;

(* Hosts are global: every client that advertised tools, whichever session
   it is attached to. Execs are keyed by id (not by the answering client's
   session) so a host can run tools for other sessions. *)
let route t agent (event : Agent.Event.t) =
  let json = Rpc_json.event event in
  (match event with
   | Tool_exec { host; exec_id; _ } ->
     Hashtbl.set t.execs ~key:exec_id ~data:(host, agent);
     Option.iter (Hashtbl.find t.clients host) ~f:(fun c -> c.send json)
   | Tool_exec_cancel { host; exec_id } ->
     Hashtbl.remove t.execs exec_id;
     Option.iter (Hashtbl.find t.clients host) ~f:(fun c -> c.send json)
   | _ -> List.iter (clients_of t agent) ~f:(fun c -> c.send json));
  match event with
  | State_changed { running; _ } ->
    if not running then maybe_evict t agent;
    (* A host's session name may have changed; [set_hosts] is a no-op when
       nothing did, so this does not loop. *)
    publish_hosts t
  | _ -> ()
;;

(* Hosts are set before subscribing: the state change would otherwise evict
   the agent, which has no client until [attach]. *)
let register t agent =
  Agent.set_hosts agent (hosts t);
  Hashtbl.set t.agents ~key:(session_id agent) ~data:agent;
  Agent.subscribe agent ~f:(route t agent);
  agent
;;

let create
      ~env:_
      ~sw:_
      ?token
      ~login
      ~sessions_dir
      ~cwd
      ~new_agent
      ?default_agent
      ()
  =
  let t =
    { login
    ; token
    ; sessions_dir
    ; cwd
    ; new_agent
    ; default_agent
    ; agents = String.Table.create ()
    ; clients = String.Table.create ()
    ; client_seq = 0
    ; execs = String.Table.create ()
    }
  in
  Option.iter default_agent ~f:(fun agent ->
    ignore (register t agent : Agent.t));
  Login_manager.subscribe login ~f:(fun event ->
    let json = Rpc_json.login_event event in
    Hashtbl.iter t.clients ~f:(fun c -> c.send json));
  t
;;

let attach t (client : Client.t) agent =
  let previous = client.agent in
  if not (phys_equal previous agent)
  then (
    client.agent <- agent;
    maybe_evict t previous);
  publish_hosts t;
  if client.tools then Agent.prefer_host agent client.id
;;

let fresh_agent t = register t (t.new_agent ~cwd:t.cwd ())

let connect t ~send =
  t.client_seq <- t.client_seq + 1;
  let agent =
    match t.default_agent with
    | Some agent -> agent
    | None -> fresh_agent t
  in
  let client =
    { Client.id = sprintf "client-%d" t.client_seq
    ; seq = t.client_seq
    ; name = sprintf "client-%d" t.client_seq
    ; tools = false
    ; cwd = None
    ; agent
    ; authed = Option.is_none t.token
    ; send
    }
  in
  Hashtbl.set t.clients ~key:client.id ~data:client;
  client
;;

let disconnect t (client : Client.t) =
  Hashtbl.remove t.clients client.id;
  Hashtbl.filter_inplace t.execs ~f:(fun (host, _) ->
    not (String.equal host client.id));
  if client.tools then publish_hosts t;
  maybe_evict t client.agent
;;

(* Finishing runs evict idle sessions, so iterate over a snapshot. *)
let shutdown t =
  let agents = Hashtbl.data t.agents in
  List.iter agents ~f:(fun agent -> ignore (Agent.abort agent : string list));
  Login_manager.cancel t.login;
  List.iter agents ~f:Agent.wait_idle;
  Login_manager.wait t.login
;;

(* Finds a live session by id or path, or loads it from disk. *)
let find_agent t key =
  let live =
    match Hashtbl.find t.agents key with
    | Some agent -> Some agent
    | None ->
      Hashtbl.data t.agents
      |> List.find ~f:(fun agent ->
        String.equal (Session.path (Agent.session agent)) key)
  in
  match live with
  | Some agent -> Ok agent
  | None ->
    let path =
      if Sys_unix.file_exists_exn key
      then Ok key
      else (
        match
          List.find (Session.list ~dir:t.sessions_dir) ~f:(fun s ->
            String.equal s.id key)
        with
        | Some summary -> Ok summary.path
        | None -> Or_error.errorf "no session %S" key)
    in
    Or_error.bind path ~f:(fun path ->
      Or_error.map (Session.load path) ~f:(fun session ->
        register t (t.new_agent ~session ~cwd:(Session.cwd session) ())))
;;

let new_agent_for t (client : Client.t) session =
  register t (t.new_agent ~session ~cwd:(Session.cwd session) ())
  |> attach t client
;;

let bool_param params name ~default =
  match param params name with
  | Some `True -> Ok true
  | Some `False -> Ok false
  | None -> Ok default
  | Some _ -> Or_error.errorf "param %S must be a boolean" name
;;

let hello t (client : Client.t) params =
  let authorised =
    match t.token with
    | None -> Ok ()
    | Some token ->
      (match param params "token" with
       | Some (`String given) when String.equal given token ->
         client.authed <- true;
         Ok ()
       | _ -> Or_error.error_string "unauthorised: bad or missing token")
  in
  Or_error.bind authorised ~f:(fun () ->
    Option.iter (param params "name") ~f:(function
      | `String name -> client.name <- name
      | _ -> ());
    Option.iter (param params "cwd") ~f:(function
      | `String cwd -> client.cwd <- Some cwd
      | _ -> ());
    Or_error.bind (bool_param params "tools" ~default:false) ~f:(fun tools ->
      client.tools <- tools;
      let agent =
        match param params "session" with
        | Some (`String key) -> find_agent t key
        | _ -> Ok client.agent
      in
      Or_error.map agent ~f:(fun agent ->
        attach t client agent;
        `Object
          [ "client_id", `String client.id
          ; "state", Rpc_json.state (Agent.state agent)
          ])))
;;

let list_sessions t =
  `Array
    (List.map (Session.list ~dir:t.sessions_dir) ~f:(fun summary ->
       let base = Rpc_json.session_summary summary in
       match Hashtbl.find t.agents summary.id, base with
       | Some agent, `Object fields ->
         `Object
           (fields
            @ [ "live", `True
              ; ("running", if Agent.is_running agent then `True else `False)
              ; ( "clients"
                , `Number (Int.to_string (List.length (clients_of t agent))) )
              ])
       | _, `Object fields -> `Object (fields @ [ "live", `False ])
       | _, other -> other))
;;

let exec_param t params =
  Or_error.bind (string_param params "exec_id") ~f:(fun exec_id ->
    match Hashtbl.find t.execs exec_id with
    | Some (_, agent) -> Ok (exec_id, agent)
    | None -> Or_error.errorf "no tool execution %S" exec_id)
;;

let dispatch_server t (client : Client.t) ~meth ~params
  : Json.t Or_error.t option
  =
  let agent = client.agent in
  match meth with
  | "hello" -> Some (hello t client params)
  | "set_active_host" ->
    Some
      (Or_error.bind (string_param params "host") ~f:(fun host ->
         Or_error.bind (string_param_opt params "cwd") ~f:(fun cwd ->
           unit_result (Agent.set_active_host agent host ~cwd))))
  | "tool_exec_output" ->
    Some
      (Or_error.bind (exec_param t params) ~f:(fun (exec_id, agent) ->
         Or_error.bind (string_param params "chunk") ~f:(fun chunk ->
           unit_result (Agent.tool_exec_output agent ~exec_id ~chunk))))
  | "tool_exec_result" ->
    Some
      (Or_error.bind (exec_param t params) ~f:(fun (exec_id, agent) ->
         Or_error.bind (string_param params "text") ~f:(fun text ->
           Or_error.bind
             (bool_param params "is_error" ~default:false)
             ~f:(fun is_error ->
               Hashtbl.remove t.execs exec_id;
               unit_result
                 (Agent.tool_exec_result agent ~exec_id ~text ~is_error)))))
  | "new_session" ->
    let cwd = (Agent.state agent).cwd in
    new_agent_for t client (Session.create ~dir:t.sessions_dir ~cwd ());
    Some empty
  | "switch_session" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         Or_error.map (find_agent t key) ~f:(fun target ->
           attach t client target;
           `Object [])))
  | "list_sessions" -> Some (ok (list_sessions t))
  | "delete_session" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun path ->
         if
           Hashtbl.data t.agents
           |> List.exists ~f:(fun a ->
             String.equal (Session.path (Agent.session a)) path)
         then Or_error.error_string "cannot delete a live session"
         else Or_error.try_with (fun () -> Core_unix.unlink path) |> unit_result))
  | "import" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun path ->
         Or_error.map
           (Session.import ~dir:t.sessions_dir path)
           ~f:(fun session ->
             new_agent_for t client session;
             `Object [ "path", `String (Session.path session) ])))
  | "fork" | "clone" ->
    let at =
      match meth, param params "at" with
      | "fork", Some (`String s) -> Some s
      | _ -> None
    in
    Some
      (Or_error.map
         (Session.fork ?at (Agent.session agent) ~dir:t.sessions_dir)
         ~f:(fun session ->
           new_agent_for t client session;
           `Object []))
  | _ -> None
;;

let dispatch agent login ~meth ~params : Json.t Or_error.t =
  match meth with
  | "ping" -> ok (`String "pong")
  | "prompt" ->
    Or_error.bind (string_param params "text") ~f:(fun text ->
      Or_error.bind
        (string_list_param params "attachments")
        ~f:(fun attachments ->
          unit_result (Agent.prompt ~attachments agent text)))
  | "steer" ->
    Or_error.bind (string_param params "text") ~f:(fun text ->
      Or_error.map
        (string_list_param params "attachments")
        ~f:(fun attachments ->
          Agent.steer ~attachments agent text;
          `Object []))
  | "follow_up" ->
    Or_error.bind (string_param params "text") ~f:(fun text ->
      Or_error.map
        (string_list_param params "attachments")
        ~f:(fun attachments ->
          Agent.follow_up ~attachments agent text;
          `Object []))
  | "abort" ->
    let restored = Agent.abort agent in
    ok
      (`Object
          [ "restored", `Array (List.map restored ~f:(fun s -> `String s)) ])
  | "dequeue" ->
    ok
      (match Agent.dequeue agent with
       | None -> `Null
       | Some (queued : Agent.Queued.t) ->
         `Object
           [ "text", `String queued.text
           ; ( "attachments"
             , `Array (List.map queued.attachments ~f:(fun a -> `String a)) )
           ])
  | "shell" ->
    Or_error.bind (string_param params "command") ~f:(fun command ->
      Or_error.bind
        (match param params "add_to_context" with
         | Some `True -> Ok true
         | Some `False -> Ok false
         | Some _ ->
           Or_error.error_string "param \"add_to_context\" must be a boolean"
         | None -> Ok false)
        ~f:(fun add_to_context ->
          Or_error.map
            (Agent.shell agent ~command ~add_to_context)
            ~f:(fun result ->
              `Object
                [ "text", `String result.text
                ; ("is_error", if result.is_error then `True else `False)
                ])))
  | "get_state" -> ok (Rpc_json.state (Agent.state agent))
  | "get_messages" ->
    ok (`Array (List.map (Agent.messages agent) ~f:Rpc_json.message))
  | "get_entries" ->
    (* [all] includes abandoned branches (for a tree view); the default is
       the active path. *)
    let session = Agent.session agent in
    let all =
      match param params "all" with
      | Some `True -> true
      | _ -> false
    in
    let entries =
      if all then Session.entries session else Session.active_path session
    in
    let head = Session.head session in
    ok
      (`Object
          [ "head", Option.value_map head ~default:`Null ~f:(fun h -> `String h)
          ; "entries", `Array (List.map entries ~f:Rpc_json.entry)
          ])
  | "set_model" ->
    Or_error.bind (string_param params "model") ~f:(fun id ->
      Or_error.map (Model.resolve id) ~f:(fun model ->
        Agent.set_model agent model;
        `Object []))
  | "set_thinking" ->
    Or_error.bind (string_param params "thinking") ~f:(fun s ->
      Or_error.map (Rpc_json.thinking_of_string s) ~f:(fun thinking ->
        Agent.set_thinking agent thinking;
        `Object []))
  | "list_models" -> ok (`Array (List.map Model.all ~f:Rpc_json.model))
  | "compact" ->
    Or_error.map (Agent.compact agent) ~f:(fun summary ->
      `Object [ "summary", `String summary ])
  | "set_session_name" ->
    Or_error.map (string_param params "name") ~f:(fun name ->
      Agent.set_session_name agent name;
      `Object [])
  | "export" ->
    Or_error.bind (string_param params "format") ~f:(fun format ->
      Or_error.bind (Session.Export_format.of_string format) ~f:(fun format ->
        let path =
          match param params "path" with
          | Some (`String s) -> Some s
          | _ -> None
        in
        Or_error.map (Agent.export agent ~format ?path ()) ~f:(fun path ->
          `Object [ "path", `String path ])))
  | "rewind" ->
    Or_error.bind (string_param params "to") ~f:(fun to_ ->
      unit_result (Agent.rewind agent ~to_))
  | "session_stats" -> ok (Rpc_json.session_stats (Agent.session_stats agent))
  | "set_cwd" ->
    Or_error.bind (string_param params "path") ~f:(fun path ->
      unit_result (Agent.set_cwd agent ~path))
  | "list_paths" ->
    let prefix =
      match param params "prefix" with
      | Some (`String s) -> s
      | _ -> ""
    in
    Agent.list_paths agent ~prefix
  | "list_dirs" ->
    let prefix =
      match param params "prefix" with
      | Some (`String s) -> s
      | _ -> ""
    in
    Or_error.bind (string_param_opt params "host") ~f:(fun host ->
      Agent.list_dirs ?host agent ~prefix)
  | "get_config" -> ok (Config.to_json (Agent.config agent))
  | "set_config" ->
    (match param params "config" with
     | None -> Or_error.error_string "missing param \"config\""
     | Some json ->
       Or_error.bind (Config.of_json json) ~f:(fun config ->
         Or_error.map (Agent.set_config agent config) ~f:(fun () ->
           Config.to_json (Agent.config agent))))
  | "tool_confirm_respond" ->
    Or_error.bind (string_param params "call_id") ~f:(fun call_id ->
      Or_error.bind
        (match param params "allow" with
         | Some `True -> Ok true
         | Some `False -> Ok false
         | Some _ -> Or_error.error_string "param \"allow\" must be a boolean"
         | None -> Or_error.error_string "missing param \"allow\"")
        ~f:(fun allow ->
          unit_result (Agent.respond_confirm agent ~call_id ~allow)))
  | "auth_status" ->
    Or_error.map (Login_manager.status login) ~f:(fun statuses ->
      `Array (List.map statuses ~f:Rpc_json.auth_status))
  | "login" ->
    Or_error.bind (provider_param params) ~f:(fun provider ->
      let method_ =
        match param params "method" with
        | Some (`String s) ->
          (match Provider_auth.Method.of_string s with
           | Some m -> Ok m
           | None -> Or_error.errorf "unknown login method %S" s)
        | _ -> Ok (List.hd_exn (Provider_auth.methods provider))
      in
      Or_error.bind method_ ~f:(fun method_ ->
        unit_result (Login_manager.start login provider method_)))
  | "auth_respond" ->
    Or_error.bind (string_param params "id") ~f:(fun id ->
      Or_error.bind (string_param params "value") ~f:(fun value ->
        unit_result (Login_manager.respond login ~id value)))
  | "auth_cancel" ->
    Login_manager.cancel login;
    empty
  | "logout" ->
    Or_error.bind (provider_param params) ~f:(fun provider ->
      unit_result (Login_manager.logout login provider))
  | _ -> Or_error.errorf "unknown method %S" meth
;;

let handle t client (request : Json.t) : Json.t =
  let id = Option.value (param request "id") ~default:`Null in
  let response =
    match param request "method" with
    | Some (`String meth) ->
      let params =
        Option.value (param request "params") ~default:(`Object [])
      in
      (match
         if (not client.Client.authed) && not (String.equal meth "hello")
         then
           Or_error.error_string "unauthorised: send hello with the token first"
         else (
           match dispatch_server t client ~meth ~params with
           | Some result -> result
           | None -> dispatch (agent_of_client t client) t.login ~meth ~params)
       with
       | result -> result
       | exception exn ->
         Or_error.error_s [%message "internal error" (exn : exn)])
    | _ -> Or_error.error_string "request must have a string \"method\""
  in
  match response with
  | Ok result ->
    `Object
      [ "type", `String "response"; "id", id; "ok", `True; "result", result ]
  | Error e ->
    `Object
      [ "type", `String "response"
      ; "id", id
      ; "ok", `False
      ; "error", `String (Error.to_string_hum e)
      ]
;;

let serve_lines t ~read_line ~write_line =
  Switch.run
  @@ fun sw ->
  let outbox : string option Eio.Stream.t = Eio.Stream.create 1024 in
  let send json = Eio.Stream.add outbox (Some (Json.to_string json)) in
  Fiber.fork ~sw (fun () ->
    let rec loop () =
      match Eio.Stream.take outbox with
      | None -> ()
      | Some line ->
        (match write_line line with
         | () -> loop ()
         | exception _ -> ())
    in
    loop ());
  let client = connect t ~send in
  let rec loop () =
    match read_line () with
    | None -> ()
    | Some line ->
      if not (String.is_empty (String.strip line))
      then (
        match Json.parse line with
        | Error e ->
          send
            (`Object
                [ "type", `String "response"
                ; "id", `Null
                ; "ok", `False
                ; "error", `String ("invalid JSON: " ^ Error.to_string_hum e)
                ])
        | Ok request ->
          (* Each request in its own fiber: a blocking method (shell, compact,
             a remote tool round trip) must not stall the reader. *)
          Fiber.fork ~sw (fun () -> send (handle t client request)));
      loop ()
  in
  loop ();
  disconnect t client;
  Eio.Stream.add outbox None
;;

let serve_connection t ~input ~output =
  let reader = Eio.Buf_read.of_flow input ~max_size:(64 * 1024 * 1024) in
  serve_lines
    t
    ~read_line:(fun () ->
      match Eio.Buf_read.line reader with
      | exception (End_of_file | Eio.Io _) -> None
      | line -> Some line)
    ~write_line:(fun line -> Eio.Flow.copy_string (line ^ "\n") output)
;;
