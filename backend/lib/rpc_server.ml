open! Core
open! Import

let methods =
  [ "ping"
  ; "hello"
  ; "list_users"
  ; "set_user"
  ; "set_active_host"
  ; "tool_exec_output"
  ; "tool_exec_result"
  ; "terminal_frame"
  ; "terminal_closed"
  ; "prompt"
  ; "steer"
  ; "follow_up"
  ; "abort"
  ; "dequeue"
  ; "remove_queued"
  ; "cancel_subagent"
  ; "list_subagents"
  ; "get_subagent"
  ; "get_pending"
  ; "kill_job"
  ; "job_output"
  ; "list_jobs"
  ; "list_skills"
  ; "list_mcp"
  ; "mcp_approve"
  ; "shell"
  ; "get_state"
  ; "get_messages"
  ; "get_messages_page"
  ; "get_tool_output"
  ; "get_entries"
  ; "set_model"
  ; "set_thinking"
  ; "list_models"
  ; "compact"
  ; "git_diff"
  ; "review"
  ; "init"
  ; "set_budget"
  ; "set_goal"
  ; "clear_goal"
  ; "resume_goal"
  ; "cancel_resume"
  ; "resume_now"
  ; "new_session"
  ; "switch_session"
  ; "list_sessions"
  ; "set_session_name"
  ; "delete_session"
  ; "delete_sessions"
  ; "delete_archived"
  ; "set_archived"
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
  ; "change_default"
  ; "tool_confirm_respond"
  ; "set_mode"
  ; "ask_user_respond"
  ; "btw"
  ; "btw_cancel"
  ; "auth_status"
  ; "login"
  ; "auth_respond"
  ; "auth_cancel"
  ; "logout"
  ]
  @ Files_rpc.methods
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

(* Images from clients are checked, and downscaled when too large. *)
let images_param ~env params =
  match param params "images" with
  | None | Some `Null -> Ok []
  | Some (`Array items) ->
    Or_error.all
      (List.map items ~f:(fun item ->
         match Json.member "mime_type" item, Json.member "data" item with
         | Some (`String mime_type), Some (`String data) ->
           Image.of_base64 ~env ~mime_type data
         | _ ->
           Or_error.error_string
             "param \"images\" must contain {mime_type, data} objects"))
  | Some _ -> Or_error.error_string "param \"images\" must be an array"
;;

(* From a tool host: already checked there. *)
let tool_images_param params =
  match param params "images" with
  | None | Some `Null -> Ok []
  | Some json ->
    Or_error.try_with (fun () -> [%of_jsonaf: Image.t list] json)
    |> Or_error.tag ~tag:"param \"images\""
;;

let ok json = Ok json
let empty = ok (`Object [])
let unit_result r = Or_error.map r ~f:(fun () -> `Object [])

let queued_json (queued : Agent.Queued.t) =
  `Object
    [ "text", `String queued.text
    ; "attachments", `Array (List.map queued.attachments ~f:(fun a -> `String a))
    ]
;;

let provider_of_string login s =
  let models = Login_manager.models login in
  match Provider_id.of_builtin_string s with
  | Some p -> Ok p
  | None ->
    (match Model_registry.find_provider models s with
     | Some p -> Ok (Custom_provider.provider_id p)
     | None ->
       Or_error.errorf
         "unknown provider %S (one of: %s; or custom to add an \
          OpenAI-compatible endpoint)"
         s
         (String.concat
            ~sep:", "
            (List.map Provider_id.builtins ~f:Provider_id.to_string
             @ List.map (Model_registry.providers models) ~f:(fun p -> p.name))))
;;

let provider_param login params =
  Or_error.bind (string_param params "provider") ~f:(provider_of_string login)
;;

(* What [list_sessions] shows of a live session that changes as it runs. *)
module Listed = struct
  type t =
    { running : bool
    ; background : int
    ; persisted : bool
    ; name : string option
    ; description : string option
    }
  [@@deriving equal]

  let of_agent agent =
    let session = Agent.session agent in
    { running = Agent.is_running agent
    ; background = Agent.running_background agent
    ; persisted = Session.persisted session
    ; name = Session.name session
    ; description = Session.description session
    }
  ;;
end

module Client = struct
  type t =
    { id : string
    ; seq : int
    ; mutable host_id : string
      (** what sessions know it by as a tool host: [hello]'s [host_id] or [id] *)
    ; mutable wanted_host_id : string option
      (** [hello]'s [host_id], also while a newer connection holds it *)
    ; mutable name : string
    ; mutable tools : bool
    ; mutable features : string list
      (** [hello]'s [features]: what the host supports beyond the basics
          ([exec_input]: [tool_exec_input]; [terminal_kill]: the
          [terminal_kill] event) *)
    ; mutable cwd : string option
    ; mutable agent : Agent.t
    ; mutable placeholder : Agent.t option
      (** the fresh session [connect] gave it, until [hello] (see
          [first_session]) *)
    ; mutable authed : bool
    ; signed_in : User_access.Signed_in.t option
      (** set when the router checked the credentials *)
    ; send : Json.t -> unit
    ; btws : Cancellation.t String.Table.t (** in-flight [btw] calls by id *)
    ; mutable told : String.Set.t (** model registry problems already sent *)
    }

  let id t = t.id
end

type t =
  { login : Login_manager.t
  ; token : string option (** required in [hello] before anything else *)
  ; namespace : string option
  ; backend_host : bool
  ; sessions_dir : string
  ; cwd : string
  ; new_agent : ?session:Session.t -> cwd:string -> unit -> Agent.t
  ; default_agent : Agent.t option
  ; agents : Agent.t String.Table.t (** live sessions by session id *)
  ; clients : Client.t String.Table.t
  ; mutable client_seq : int
  ; mutable btw_seq : int
  ; execs : (string * Agent.t) String.Table.t
    (** in-flight remote executions by exec id: host id and session *)
  ; mutable login_owner : string option
    (** the client running the current login flow *)
  ; terminals : Terminal_relay.t
  ; home : string option (** for the current [config.json] *)
  ; now : unit -> float
  ; listed : Listed.t String.Table.t
    (** what [list_sessions] last showed of each live session *)
  ; mutable on_session_deleted : (key:string -> unit) list
  ; loaded : Session.t File_cache.t
    (** sessions read from disk and not live: evicted ones, and those read
        for [get_messages_page] without opening them *)
  }

let agent_of_client _t (client : Client.t) = client.agent

(* Each client hears each model registry problem once. *)
let tell_problems (client : Client.t) problems =
  List.iter problems ~f:(fun problem ->
    if not (Set.mem client.told problem)
    then (
      client.told <- Set.add client.told problem;
      client.send (Rpc_json.event (Notice problem))))
;;

let session_id agent = Session.id (Agent.session agent)

let clients_of t agent =
  Hashtbl.data t.clients
  |> List.filter ~f:(fun (c : Client.t) -> phys_equal c.agent agent)
;;

let maybe_evict t agent =
  if
    (not (Option.exists t.default_agent ~f:(phys_equal agent)))
    && (not (Agent.is_running agent))
    && (not (Agent.has_running_background agent))
    && (not (Agent.resume_pending agent))
    && List.is_empty (clients_of t agent)
  then (
    Hashtbl.remove t.agents (session_id agent);
    Hashtbl.remove t.listed (session_id agent);
    let session = Agent.session agent in
    if Session.persisted session && not (Session.gone session)
    then File_cache.set t.loaded (Session.path session) session)
;;

(* Every client of the server, whichever session it is in: they refetch
   [list_sessions]. *)
let sessions_changed t =
  let json =
    `Object [ "type", `String "event"; "event", `String "sessions_changed" ]
  in
  Hashtbl.iter t.clients ~f:(fun (c : Client.t) -> c.send json)
;;

(* Only for changes: what is shown at [register] is the baseline. *)
let note_listed t agent =
  let id = session_id agent in
  match Hashtbl.find t.listed id with
  | Some before
    when Option.exists (Hashtbl.find t.agents id) ~f:(phys_equal agent) ->
    let listed = Listed.of_agent agent in
    if not (Listed.equal before listed)
    then (
      Hashtbl.set t.listed ~key:id ~data:listed;
      sessions_changed t)
  | Some _ | None -> ()
;;

(* The tool host clients are few: a scan is fine. *)
let host_client t host_id =
  List.find (Hashtbl.data t.clients) ~f:(fun (c : Client.t) ->
    c.tools && String.equal c.host_id host_id)
;;

let host_of (client : Client.t) =
  { Agent.Host.id = client.host_id
  ; name = client.name
  ; cwd = Option.value client.cwd ~default:(Agent.state client.agent).cwd
  ; session_id = Some (session_id client.agent)
  ; session_name = Session.name (Agent.session client.agent)
  ; exec_input = List.mem client.features "exec_input" ~equal:String.equal
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
  let json =
    Rpc_json.event
      ~now:(Eio.Time.now (Eio.Stdenv.clock (Agent.env agent)))
      event
  in
  (match event with
   | Tool_exec { host; exec_id; _ } ->
     Hashtbl.set t.execs ~key:exec_id ~data:(host, agent);
     Option.iter (host_client t host) ~f:(fun c -> c.send json)
   | Tool_exec_cancel { host; exec_id } ->
     Hashtbl.remove t.execs exec_id;
     Option.iter (host_client t host) ~f:(fun c -> c.send json)
   | Tool_exec_input { host; _ } ->
     Option.iter (host_client t host) ~f:(fun c -> c.send json)
   | _ -> List.iter (clients_of t agent) ~f:(fun c -> c.send json));
  (match event with
   | State_changed _ | Loop (Message_end _) -> note_listed t agent
   | _ -> ());
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
  Hashtbl.set t.listed ~key:(session_id agent) ~data:(Listed.of_agent agent);
  Agent.subscribe agent ~f:(route t agent);
  agent
;;

let create
      ~env
      ~sw:_
      ?token
      ?namespace
      ?(backend_host = true)
      ?home
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
    ; namespace
    ; backend_host
    ; sessions_dir
    ; cwd
    ; new_agent
    ; default_agent
    ; agents = String.Table.create ()
    ; clients = String.Table.create ()
    ; client_seq = 0
    ; btw_seq = 0
    ; execs = String.Table.create ()
    ; login_owner = None
    ; terminals = Terminal_relay.create ()
    ; loaded = File_cache.create ~max_bytes:(128 * 1024 * 1024) ()
    ; home
    ; now = (fun () -> Eio.Time.now (Eio.Stdenv.clock env))
    ; listed = String.Table.create ()
    ; on_session_deleted = []
    }
  in
  Option.iter default_agent ~f:(fun agent ->
    ignore (register t agent : Agent.t));
  (* The flow itself (URL, prompts) belongs to the client that started it:
     other frontends, possibly on other machines, must not open a browser or
     pop up a dialog. Everyone hears the outcome, to refresh their status. *)
  Login_manager.subscribe login ~f:(fun event ->
    let json = Rpc_json.login_event event in
    match event with
    | Auth_url _ | Prompt _ | Prompt_cancelled _ | Progress _ ->
      Option.iter
        (Option.bind t.login_owner ~f:(Hashtbl.find t.clients))
        ~f:(fun c -> c.send json)
    | Done _ | Failed _ | Logged_out _ ->
      Hashtbl.iter t.clients ~f:(fun c -> c.send json));
  Model_registry.subscribe (Login_manager.models login) ~f:(fun problem ->
    Hashtbl.iter t.clients ~f:(fun c -> tell_problems c [ problem ]));
  t
;;

let attach t (client : Client.t) agent =
  let previous = client.agent in
  if not (phys_equal previous agent)
  then (
    client.agent <- agent;
    maybe_evict t previous);
  publish_hosts t;
  if client.tools then Agent.prefer_host agent client.host_id
;;

(* A brand-new session made or opened for [client] starts on a host chosen
   from the client's context rather than the first host to connect: on the
   client itself when it is a tool host (in [cwd]), else where the client's
   session is (connected or not), in its cwd there. [None] (a client without
   tools on a session without a host): where any new session would be.
   Unpinned: the host was chosen by default. *)
let start_location (client : Client.t) ~cwd =
  if client.tools
  then
    Some
      ( { Session.Host.id = client.host_id; name = client.name; pinned = false }
      , cwd )
  else
    Option.map (Agent.location client.agent) ~f:(fun (host, cwd) ->
      { host with pinned = false }, cwd)
;;

(* The cwd [client]'s session last had on the client's own host. *)
let own_cwd (client : Client.t) =
  match
    List.find (Agent.hosts client.agent) ~f:(fun h ->
      String.equal h.id client.host_id)
  with
  | Some host -> host.cwd
  | None -> (host_of client).cwd
;;

(* A session that already ran on the chosen host goes back to its cwd there. *)
let with_recorded_cwd session start =
  Option.map start ~f:(fun ((host : Session.Host.t), cwd) ->
    ( host
    , Option.value (Map.find (Session.host_cwds session) host.id) ~default:cwd ))
;;

(* The start is recorded in the session before its agent exists, so the
   agent never has a moment without a host in which it would adopt the first
   one (see [Agent.set_hosts]). *)
let start_agent t session start =
  Option.iter start ~f:(fun ((host : Session.Host.t), cwd) ->
    if
      not
        (Option.equal Session.Host.equal (Session.host session) (Some host)
         && String.equal (Session.cwd session) cwd)
    then ignore (Session.set_cwd session ~host ~cwd () : Session.Entry.t));
  register t (t.new_agent ~session ~cwd:(Session.cwd session) ())
;;

(* A session loaded from disk keeps the host it recorded (waiting for it if
   need be); one without (or on the disabled backend) would adopt the first
   host to connect, so it starts like a new session for [client] instead. *)
let loaded_start t (client : Client.t) session =
  let has_host =
    match Session.host session with
    | None -> false
    | Some host ->
      t.backend_host || not (String.equal host.id Agent.Host.backend_id)
  in
  if t.backend_host || has_host
  then None
  else
    with_recorded_cwd session (start_location client ~cwd:(host_of client).cwd)
;;

let fresh_agent t = register t (t.new_agent ~cwd:t.cwd ())

(* Sessions waiting for a usage limit to reset when the backend last ran are
   loaded again, so that they resume on time with nobody attached. *)
let restore_resumes t =
  let now = t.now () in
  List.iter (Session.list ~dir:t.sessions_dir) ~f:(fun summary ->
    match summary.resume_at with
    | Some at when Float.( > ) at now && not (Hashtbl.mem t.agents summary.id)
      ->
      (match Session.load summary.path with
       | Ok session -> ignore (start_agent t session None : Agent.t)
       | Error error ->
         eprintf
           "prigh: session %s will not resume: %s\n%!"
           summary.id
           (Error.to_string_hum error))
    | Some _ | None -> ())
;;

let connect ?signed_in t ~send =
  t.client_seq <- t.client_seq + 1;
  let agent, placeholder =
    match t.default_agent with
    | Some agent -> agent, None
    | None ->
      let agent = fresh_agent t in
      agent, Some agent
  in
  let client =
    { Client.id = sprintf "client-%d" t.client_seq
    ; seq = t.client_seq
    ; host_id = sprintf "client-%d" t.client_seq
    ; wanted_host_id = None
    ; name = sprintf "client-%d" t.client_seq
    ; tools = false
    ; features = []
    ; cwd = None
    ; agent
    ; placeholder
    ; authed = Option.is_none t.token || Option.is_some signed_in
    ; signed_in
    ; send
    ; btws = String.Table.create ()
    ; told = String.Set.empty
    }
  in
  Hashtbl.set t.clients ~key:client.id ~data:client;
  client
;;

(* Fails what is in flight on [client] as the host it is now. *)
let release_host_id t (client : Client.t) ~text =
  let execs =
    Hashtbl.filter t.execs ~f:(fun (host, _) ->
      String.equal host client.host_id)
  in
  Hashtbl.iteri execs ~f:(fun ~key:exec_id ~data:(_, agent) ->
    Hashtbl.remove t.execs exec_id;
    ignore
      (Agent.tool_exec_result agent ~exec_id (Tool.Result.error text)
       : unit Or_error.t));
  Terminal_relay.host_gone t.terminals ~host:client.host_id
;;

let notice (client : Client.t) text = client.send (Rpc_json.event (Notice text))

(* Several connections may ask for one host id (two frontends on a machine
   share its id): when the one holding it goes, the newest of the others
   takes it back, so the host's sessions carry on there. *)
let hand_over_host_id t (gone : Client.t) =
  if not (String.equal gone.host_id gone.id)
  then
    Hashtbl.data t.clients
    |> List.filter ~f:(fun (c : Client.t) ->
      c.tools && Option.equal String.equal c.wanted_host_id (Some gone.host_id))
    |> List.max_elt ~compare:(fun (a : Client.t) b -> Int.compare a.seq b.seq)
    |> Option.iter ~f:(fun (next : Client.t) ->
      release_host_id t next ~text:"[tool host reconnected]";
      next.host_id <- gone.host_id;
      notice next (sprintf "this connection is tool host %S again" next.host_id))
;;

let disconnect t (client : Client.t) =
  Hashtbl.remove t.clients client.id;
  Hashtbl.iter client.btws ~f:Cancellation.cancel;
  release_host_id t client ~text:"[tool host disconnected]";
  hand_over_host_id t client;
  if client.tools then publish_hosts t;
  maybe_evict t client.agent
;;

(* Finishing runs evict idle sessions, so iterate over a snapshot. *)
let shutdown t =
  let agents = Hashtbl.data t.agents in
  List.iter agents ~f:(fun agent ->
    Agent.cancel_background ~discard:true agent;
    ignore (Agent.abort agent : string list));
  Login_manager.cancel t.login;
  List.iter agents ~f:Agent.wait_idle;
  Login_manager.wait t.login
;;

(* Without the backend host, clients have no business with the backend's
   files: only session files in the sessions directory. *)
let session_file t path =
  if t.backend_host
  then Ok path
  else (
    let real path =
      try Filename_unix.realpath path with
      | _ -> path
    in
    if String.is_prefix (real path) ~prefix:(real t.sessions_dir ^ "/")
    then Ok path
    else Or_error.errorf "%S is not in the sessions directory" path)
;;

let find_live t key =
  match Hashtbl.find t.agents key with
  | Some agent -> Some agent
  | None ->
    Hashtbl.data t.agents
    |> List.find ~f:(fun agent ->
      String.equal (Session.path (Agent.session agent)) key)
;;

(* The file of a saved session, by path or id. *)
let saved_path t key =
  if Sys_unix.file_exists_exn key
  then session_file t key
  else (
    match
      List.find (Session.list ~dir:t.sessions_dir) ~f:(fun s ->
        String.equal s.id key)
    with
    | Some summary -> Ok summary.path
    | None -> Or_error.errorf "no session %S" key)
;;

(* Finds a live session by id or path, or loads it from disk for [client]
   (taking it out of [t.loaded]: its agent owns it now). *)
let find_agent t (client : Client.t) key =
  match find_live t key with
  | Some agent -> Ok agent
  | None ->
    Or_error.bind (saved_path t key) ~f:(fun path ->
      let session = File_cache.find_or_read t.loaded path ~read:Session.load in
      File_cache.remove t.loaded path;
      Or_error.map session ~f:(fun session ->
        start_agent t session (loaded_start t client session)))
;;

(* A session's messages to read: the live session's, else from disk without
   opening it (kept in [t.loaded] for when it is opened). *)
let session_to_read t key =
  match find_live t key with
  | Some agent -> Ok (Agent.session agent)
  | None ->
    Or_error.bind (saved_path t key) ~f:(fun path ->
      File_cache.find_or_read t.loaded path ~read:Session.load)
;;

let int_param params name ~default =
  match param params name with
  | None | Some `Null -> Ok default
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some n -> Ok n
     | None -> Or_error.errorf "param %S must be an integer" name)
  | Some _ -> Or_error.errorf "param %S must be an integer" name
;;

let messages_page t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind limit = int_param params "limit" ~default:100 in
  let%bind before = string_param_opt params "before" in
  let%bind session =
    match%bind string_param_opt params "session" with
    | None -> Ok (Agent.session client.agent)
    | Some key -> session_to_read t key
  in
  let%map page =
    Message_page.create (Session.identified_messages session) ~limit ~before
  in
  Message_page.to_json page
;;

(* Without the backend host, the session [connect] gave a tool host may have
   adopted another host while the client was not yet known as one: it is
   replaced, before anyone used it, by one on the client in its cwd. (On the
   backend it is taken over instead, see [attach].) *)
let first_session t (client : Client.t) =
  match client.placeholder with
  | Some agent
    when client.tools
         && (not t.backend_host)
         && phys_equal agent client.agent
         && (not (Agent.is_running agent))
         && (not (Session.persisted (Agent.session agent)))
         && List.is_empty (Session.messages (Agent.session agent))
         && List.for_all (clients_of t agent) ~f:(phys_equal client) ->
    let cwd = (host_of client).cwd in
    start_agent
      t
      (Session.create ~dir:t.sessions_dir ~cwd ())
      (start_location client ~cwd)
  | Some _ | None -> client.agent
;;

let bool_param params name ~default =
  match param params name with
  | Some `True -> Ok true
  | Some `False -> Ok false
  | None -> Ok default
  | Some _ -> Or_error.errorf "param %S must be a boolean" name
;;

let unauthorised = User_access.unauthorised

let credentials_ok t ?user given =
  match t.token with
  | None -> true
  | Some token ->
    Option.exists given ~f:(String.equal token)
    &&
      (match user, t.namespace with
      | Some user, Some namespace -> String.equal user namespace
      | _ -> true)
;;

let namespace t = t.namespace

let terminal_key t ~session =
  match t.namespace with
  | None -> session
  | Some namespace -> namespace ^ ":" ^ session
;;

let on_session_deleted t ~f = t.on_session_deleted <- f :: t.on_session_deleted

let terminal_target t ~session =
  match Option.bind session ~f:(Hashtbl.find t.agents) with
  | None ->
    if t.backend_host
    then `Backend t.cwd
    else `Unavailable "no live session and the backend tool host is disabled"
  | Some agent ->
    let active = Agent.active_host agent in
    (match
       List.find (Agent.hosts agent) ~f:(fun h -> String.equal h.id active)
     with
     | Some host when String.equal host.id Agent.Host.backend_id ->
       `Backend host.cwd
     | Some host ->
       (match host_client t active with
        | Some _ -> `Host (active, host.cwd)
        | None -> `Unavailable (Agent.host_unavailable_message agent))
     | None -> `Unavailable (Agent.host_unavailable_message agent))
;;

let relay_terminal t ~host ~key ~cwd ~cols ~rows channel =
  let send_event json =
    Option.iter (host_client t host) ~f:(fun c -> c.send json)
  in
  if Option.is_some (host_client t host)
  then
    Terminal_relay.serve
      t.terminals
      ~host
      ~send_event
      ~key
      ~cwd
      ~cols
      ~rows
      channel
  else
    Terminal_channel.send_text
      channel
      (Json.to_string
         (`Object
             [ "type", `String "error"
             ; "message", `String "the tool host disconnected"
             ]))
;;

(* A tool host names itself with [host_id], the same on every connection of
   its machine, so that sessions on it resume when it reconnects or restarts.
   The newest connection owns the id: an older one still holding it is one
   the host gave up on (the backend may notice a dropped connection late) or
   another frontend on the same machine, so it is demoted to its client id,
   failing what was in flight on it, until the newer one goes
   ([hand_over_host_id]). *)
let claim_host_id t (client : Client.t) host_id =
  if String.equal host_id client.host_id
  then Ok ()
  else if
    String.is_empty host_id
    || String.equal host_id Agent.Host.backend_id
    || String.is_prefix host_id ~prefix:"client-"
  then Or_error.errorf "param \"host_id\": %S is reserved" host_id
  else (
    let text = "[tool host reconnected]" in
    release_host_id t client ~text;
    Hashtbl.iter t.clients ~f:(fun (other : Client.t) ->
      if String.equal other.host_id host_id
      then (
        release_host_id t other ~text;
        other.host_id <- other.id;
        notice
          other
          (sprintf
             "a newer connection is tool host %S now; this one is %S until it \
              disconnects"
             host_id
             other.id)));
    client.host_id <- host_id;
    client.wanted_host_id <- Some host_id;
    Ok ())
;;

let hello t (client : Client.t) params =
  let string name =
    match param params name with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let authorised =
    match client.signed_in with
    | Some _ -> Ok ()
    | None ->
      if not (credentials_ok t ?user:(string "user") (string "token"))
      then Or_error.error_string unauthorised
      else if
        Option.exists (string "as_user") ~f:(fun as_user ->
          (not (String.is_empty as_user))
          && not (Option.equal String.equal (Some as_user) t.namespace))
      then Or_error.error_string User_access.no_users
      else (
        client.authed <- true;
        Ok ())
  in
  Or_error.bind authorised ~f:(fun () ->
    Option.iter (param params "name") ~f:(function
      | `String name -> client.name <- name
      | _ -> ());
    Option.iter (param params "cwd") ~f:(function
      | `String cwd -> client.cwd <- Some cwd
      | _ -> ());
    Or_error.bind (bool_param params "tools" ~default:false) ~f:(fun tools ->
      let claimed =
        match string "host_id" with
        | Some host_id when tools -> claim_host_id t client host_id
        | _ -> Ok ()
      in
      Or_error.bind claimed ~f:(fun () ->
        client.tools <- tools;
        client.features
        <- (match param params "features" with
            | Some (`Array items) ->
              List.filter_map items ~f:(function
                | `String feature -> Some feature
                | _ -> None)
            | _ -> []);
        let agent =
          match param params "session" with
          | Some (`String key) -> find_agent t client key
          | _ -> Ok (first_session t client)
        in
        client.placeholder <- None;
        Or_error.map agent ~f:(fun agent ->
          attach t client agent;
          `Object
            [ "client_id", `String client.id
            ; "host_id", `String client.host_id
            ; ( "namespace"
              , Option.value_map t.namespace ~default:`Null ~f:(fun n ->
                  `String n) )
            ; ( "user"
              , Option.value_map client.signed_in ~default:`Null ~f:(fun s ->
                  `String s.user) )
            ; ( "superuser"
              , if Option.exists client.signed_in ~f:(fun s -> s.superuser)
                then `True
                else `False )
            ; "state", Rpc_json.state (Agent.state agent)
            ]))))
;;

(* The file is reread so that a change made from another session counts. *)
let archive_after_days t (client : Client.t) =
  let config =
    match Option.map t.home ~f:(fun home -> Config.load ~home) with
    | Some (Ok config) -> config
    | Some (Error _) | None -> Agent.config client.agent
  in
  config.archive_after_days
;;

(* A saved session as [list_sessions] shows it. *)
module Listing = struct
  type t =
    { summary : Session.Summary.t
    ; agent : Agent.t option
    ; active : bool (** a run, or background work *)
    ; background : int
    ; archived : bool
    }
end

let listings t client =
  let now = t.now () in
  let archive_after_days = archive_after_days t client in
  List.map (Session.list ~dir:t.sessions_dir) ~f:(fun summary ->
    let agent = Hashtbl.find t.agents summary.id in
    let background =
      Option.value_map agent ~default:0 ~f:Agent.running_background
    in
    let active = background > 0 || Option.exists agent ~f:Agent.is_running in
    { Listing.summary
    ; agent
    ; active
    ; background
    ; archived =
        Session.Summary.is_archived summary ~now ~archive_after_days ~active
    })
;;

(* What a client changing session needs first, saving it a [get_state]. *)
let attached_state (client : Client.t) =
  `Object [ "state", Rpc_json.state (Agent.state client.agent) ]
;;

let list_sessions t client =
  let bool b = if b then `True else `False in
  let int n = `Number (Int.to_string n) in
  `Array
    (List.map (listings t client) ~f:(fun (l : Listing.t) ->
       let live =
         match l.agent with
         | Some agent ->
           [ "live", `True; "clients", int (List.length (clients_of t agent)) ]
         | None -> [ "live", `False ]
       in
       match Rpc_json.session_summary l.summary with
       | `Object fields ->
         `Object
           (fields
            @ live
            @ [ "running", bool l.active
              ; "background", int l.background
              ; "archived", bool l.archived
              ])
       | other -> other))
;;

(* A saved session by its file (as the summaries give it) or its id. *)
let find_saved t key =
  Or_error.bind
    (if String.mem key '/' then session_file t key else Ok key)
    ~f:(fun key ->
      match
        List.find (Session.list ~dir:t.sessions_dir) ~f:(fun s ->
          String.equal s.path key || String.equal s.id key)
      with
      | Some summary -> Ok summary
      | None -> Or_error.errorf "no such session %S" key)
;;

let display (s : Session.Summary.t) =
  match s.name with
  | Some name -> sprintf "session %S" name
  | None -> "session " ^ s.id
;;

let is_active agent =
  Agent.is_running agent || Agent.has_running_background agent
;;

(* Why [client] may not delete [summary], if it may not. *)
let refusal t (client : Client.t) (summary : Session.Summary.t) =
  let name = display summary in
  let agent = Hashtbl.find t.agents summary.id in
  if String.equal (session_id client.agent) summary.id
  then
    Some
      (sprintf "%s is the session you are in: switch to another one first" name)
  else if
    Option.exists t.default_agent ~f:(fun a ->
      String.equal (session_id a) summary.id)
  then
    Some
      (sprintf
         "%s is the backend's default session (-session): restart the backend \
          without -session to delete it"
         name)
  else (
    match agent with
    | Some agent when is_active agent ->
      Some (sprintf "%s is running: wait for it or /abort it first" name)
    | Some agent when not (List.is_empty (clients_of t agent)) ->
      Some
        (sprintf
           "%s is open in %d other frontend(s): close it there first"
           name
           (List.length (clients_of t agent)))
    | Some _ | None -> None)
;;

module Deleted = struct
  type t =
    { path : string
    ; id : string
    ; bytes : int
    }

  let to_json t =
    `Object
      [ "path", `String t.path
      ; "id", `String t.id
      ; "bytes", `Number (Int.to_string t.bytes)
      ]
  ;;
end

(* Deletes the session's files and everything kept for it in memory. *)
let delete_saved t client (summary : Session.Summary.t) =
  match refusal t client summary with
  | Some refused -> Or_error.error_string refused
  | None ->
    Or_error.try_with (fun () ->
      let bytes = Session.disk_usage summary.path in
      Session.delete_files summary.path;
      let agent = Hashtbl.find t.agents summary.id in
      Hashtbl.remove t.agents summary.id;
      Hashtbl.remove t.listed summary.id;
      Option.iter agent ~f:(fun agent ->
        if Agent.resume_pending agent
        then ignore (Agent.cancel_resume agent : unit Or_error.t));
      let key = terminal_key t ~session:summary.id in
      List.iter (Terminal_relay.close_key t.terminals ~key) ~f:(fun host ->
        Option.iter (host_client t host) ~f:(fun (c : Client.t) ->
          if List.mem c.features "terminal_kill" ~equal:String.equal
          then c.send (Terminal_relay.kill_event ~host ~key)));
      List.iter t.on_session_deleted ~f:(fun f -> f ~key);
      { Deleted.path = summary.path; id = summary.id; bytes })
;;

(* One [sessions_changed] for the lot. *)
let delete_many t client keys =
  let results =
    List.map keys ~f:(fun key ->
      ( key
      , Or_error.bind (find_saved t key) ~f:(fun summary ->
          delete_saved t client summary) ))
  in
  let deleted = List.filter_map results ~f:(fun (_, r) -> Result.ok r) in
  if not (List.is_empty deleted) then sessions_changed t;
  `Object
    [ "deleted", `Array (List.map deleted ~f:Deleted.to_json)
    ; ( "refused"
      , `Array
          (List.filter_map results ~f:(fun (key, r) ->
             match r with
             | Ok _ -> None
             | Error e ->
               Some
                 (`Object
                     [ "path", `String key
                     ; "error", `String (Error.to_string_hum e)
                     ]))) )
    ]
;;

let set_archived t ~key ~archived =
  Or_error.bind (find_saved t key) ~f:(fun summary ->
    let agent = Hashtbl.find t.agents summary.id in
    if archived && Option.exists agent ~f:is_active
    then
      Or_error.errorf
        "%s is running: wait for it or /abort it before archiving"
        (display summary)
    else (
      let session =
        match agent with
        | Some agent -> Ok (Agent.session agent)
        | None -> Session.load summary.path
      in
      Or_error.map session ~f:(fun session ->
        ignore (Session.set_archived session ~archived : Session.Entry.t);
        sessions_changed t)))
;;

let exec_param t params =
  Or_error.bind (string_param params "exec_id") ~f:(fun exec_id ->
    match Hashtbl.find t.execs exec_id with
    | Some (_, agent) -> Ok (exec_id, agent)
    | None -> Or_error.errorf "no tool execution %S" exec_id)
;;

(* The answer streams to the asking client only and never enters the session. *)
let btw t (client : Client.t) params =
  Or_error.bind (string_param params "question") ~f:(fun question ->
    Or_error.bind (string_param_opt params "btw_id") ~f:(fun btw_id ->
      let btw_id =
        match btw_id with
        | Some id -> id
        | None ->
          t.btw_seq <- t.btw_seq + 1;
          sprintf "btw-%d" t.btw_seq
      in
      if String.is_empty (String.strip question)
      then Or_error.error_string "missing question"
      else if Hashtbl.mem client.btws btw_id
      then Or_error.errorf "btw %S is already running" btw_id
      else (
        let cancel = Cancellation.create () in
        Hashtbl.set client.btws ~key:btw_id ~data:cancel;
        let result =
          Exn.protect
            ~finally:(fun () -> Hashtbl.remove client.btws btw_id)
            ~f:(fun () ->
              Agent.btw client.agent ~question ~cancel ~on_delta:(fun delta ->
                client.send
                  (`Object
                      [ "type", `String "event"
                      ; "event", `String "btw_delta"
                      ; "btw_id", `String btw_id
                      ; "delta", `String delta
                      ])))
        in
        Or_error.map result ~f:(fun (reply, cost_usd) ->
          `Object
            [ "btw_id", `String btw_id
            ; "text", `String (Message.Assistant.text reply)
            ; "usage", Usage.jsonaf_of_t reply.usage
            ; "cost_usd", `Number (sprintf "%.15g" cost_usd)
            ]))))
;;

let switch_user (client : Client.t) params =
  match client.signed_in with
  | None -> Or_error.error_string User_access.no_users
  | Some signed_in ->
    Or_error.bind (string_param params "user") ~f:(fun user ->
      User_access.Signed_in.switch signed_in user)
;;

let request_params request =
  Option.value (param request "params") ~default:(`Object [])
;;

let dispatch_server t (client : Client.t) ~meth ~params
  : Json.t Or_error.t option
  =
  let agent = client.agent in
  match meth with
  | "hello" -> Some (hello t client params)
  | "list_users" ->
    Some
      (match client.signed_in with
       | None -> Or_error.error_string User_access.no_users
       | Some signed_in ->
         Or_error.map (User_access.Signed_in.users signed_in) ~f:(fun users ->
           `Array (List.map users ~f:(fun u -> `String u))))
  | "set_user" ->
    Some
      (Or_error.map (switch_user client params) ~f:(fun user ->
         `Object [ "user", `String user ]))
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
         let open Or_error.Let_syntax in
         let%bind text = string_param params "text" in
         let%bind is_error = bool_param params "is_error" ~default:false in
         let%bind images = tool_images_param params in
         Hashtbl.remove t.execs exec_id;
         unit_result
           (Agent.tool_exec_result
              agent
              ~exec_id
              { Tool_result.text; is_error; images })))
  | "btw" -> Some (btw t client params)
  | "btw_cancel" ->
    Some
      (Or_error.map (string_param params "btw_id") ~f:(fun btw_id ->
         let found = Hashtbl.find client.btws btw_id in
         Option.iter found ~f:Cancellation.cancel;
         `Object
           [ ("cancelled", if Option.is_some found then `True else `False) ]))
  | "terminal_frame" ->
    Some
      (unit_result
         (Terminal_relay.frame t.terminals ~host:client.host_id params))
  | "terminal_closed" ->
    Some
      (unit_result
         (Terminal_relay.closed t.terminals ~host:client.host_id params))
  | "new_session" ->
    let start = start_location client ~cwd:(own_cwd client) in
    let cwd =
      match start with
      | Some (_, cwd) -> cwd
      | None -> (Agent.state agent).cwd
    in
    start_agent t (Session.create ~dir:t.sessions_dir ~cwd ()) start
    |> attach t client;
    Some (Ok (attached_state client))
  | "switch_session" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         Or_error.map (find_agent t client key) ~f:(fun target ->
           attach t client target;
           attached_state client)))
  | "list_sessions" -> Some (ok (list_sessions t client))
  | "get_messages_page" -> Some (messages_page t client params)
  | "delete_session" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         Or_error.map
           (Or_error.bind (find_saved t key) ~f:(delete_saved t client))
           ~f:(fun (_ : Deleted.t) ->
             sessions_changed t;
             `Object [])))
  | "delete_sessions" ->
    Some
      (Or_error.map
         (string_list_param params "paths")
         ~f:(delete_many t client))
  | "delete_archived" ->
    Some
      (ok
         (delete_many
            t
            client
            (List.filter_map (listings t client) ~f:(fun (l : Listing.t) ->
               Option.some_if l.archived l.summary.path))))
  | "set_archived" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         let archived =
           match param params "archived" with
           | Some `True -> Ok true
           | Some `False -> Ok false
           | Some _ ->
             Or_error.error_string "param \"archived\" must be a boolean"
           | None -> Or_error.error_string "missing param \"archived\""
         in
         Or_error.bind archived ~f:(fun archived ->
           unit_result (set_archived t ~key ~archived))))
  | "import" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun path ->
         Or_error.bind (session_file t path) ~f:(fun path ->
           Or_error.map
             (Session.import ~dir:t.sessions_dir path)
             ~f:(fun session ->
               let start =
                 if client.tools
                 then
                   with_recorded_cwd
                     session
                     (start_location client ~cwd:(host_of client).cwd)
                 else loaded_start t client session
               in
               start_agent t session start |> attach t client;
               sessions_changed t;
               `Object [ "path", `String (Session.path session) ]))))
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
           (* A copy of the conversation stays where the user pinned it. *)
           let start =
             match Agent.location agent with
             | Some ((host, _) as location) when host.pinned -> Some location
             | Some _ | None -> start_location client ~cwd:(own_cwd client)
           in
           start_agent t session start |> attach t client;
           if Session.persisted session then sessions_changed t;
           attached_state client))
  | "login" ->
    (* The flow may emit its first event before it is started. *)
    let owned start =
      let previous = t.login_owner in
      t.login_owner <- Some client.id;
      let started = start () in
      if Result.is_error started then t.login_owner <- previous;
      unit_result started
    in
    Option.some
    @@
      (match param params "provider" with
      | Some (`String "custom") -> owned (Login_manager.start_custom t.login)
      | _ ->
        Or_error.bind (provider_param t.login params) ~f:(fun provider ->
          let method_ =
            match param params "method" with
            | Some (`String s) ->
              (match Provider_auth.Method.of_string s with
               | Some m -> Ok m
               | None -> Or_error.errorf "unknown login method %S" s)
            | _ -> Ok (List.hd_exn (Provider_auth.methods provider))
          in
          Or_error.bind method_ ~f:(fun method_ ->
            owned (fun () -> Login_manager.start t.login provider method_))))
  | "logout" ->
    (* A custom provider's logout asks the caller a question. *)
    Option.some
    @@ Or_error.bind (provider_param t.login params) ~f:(fun provider ->
      let previous = t.login_owner in
      t.login_owner <- Some client.id;
      let result = Login_manager.logout t.login provider in
      if Result.is_error result || not (Login_manager.in_progress t.login)
      then t.login_owner <- previous;
      unit_result result)
  | "list_models" ->
    let models = Login_manager.models t.login in
    Model_registry.reload models;
    tell_problems client (Model_registry.problems models);
    Some
      (ok (`Array (List.map (Model_registry.models models) ~f:Rpc_json.model)))
  | _ -> None
;;

let dispatch agent login ~meth ~params : Json.t Or_error.t =
  match meth with
  | "ping" -> ok (`String "pong")
  | ("prompt" | "steer" | "follow_up") as meth ->
    let open Or_error.Let_syntax in
    let%bind text = string_param params "text" in
    let%bind attachments = string_list_param params "attachments" in
    let%bind images = images_param ~env:(Agent.env agent) params in
    (match meth with
     | "prompt" -> unit_result (Agent.prompt ~attachments ~images agent text)
     | "steer" -> unit_result (Agent.steer ~attachments ~images agent text)
     | _ -> unit_result (Agent.follow_up ~attachments ~images agent text))
  | "list_skills" ->
    Or_error.map (Agent.skills agent) ~f:(fun skills ->
      `Object [ "skills", `Array (List.map skills ~f:[%jsonaf_of: Skill.t]) ])
  | "list_mcp" ->
    Or_error.bind
      (bool_param params "reconnect" ~default:false)
      ~f:(fun reconnect ->
        Or_error.map
          (Agent.mcp_servers ~reconnect agent)
          ~f:Mcp_tools.Listing.to_rpc_json)
  | "mcp_approve" ->
    let open Or_error.Let_syntax in
    let%bind source = string_param params "source" in
    let%bind server = string_param params "server" in
    Or_error.map
      (Agent.approve_mcp agent ~source ~server)
      ~f:Mcp_tools.Listing.to_rpc_json
  | "abort" ->
    let restored = Agent.abort agent in
    ok
      (`Object
          [ "restored", `Array (List.map restored ~f:(fun s -> `String s)) ])
  | "cancel_subagent" ->
    Or_error.bind (string_param params "agent_id") ~f:(fun agent_id ->
      unit_result (Agent.cancel_subagent agent ~agent_id))
  | "kill_job" ->
    Or_error.bind (string_param params "job_id") ~f:(fun job_id ->
      unit_result (Agent.kill_job agent ~job_id))
  | "job_output" ->
    Or_error.bind (string_param params "job_id") ~f:(fun job_id ->
      let lines =
        match param params "lines" with
        | Some (`Number n) -> Option.value (Int.of_string_opt n) ~default:200
        | _ -> 200
      in
      Or_error.map (Agent.job_output agent ~job_id ~lines) ~f:(fun text ->
        `Object [ "text", `String text ]))
  | "list_jobs" ->
    let now = Core_unix.gettimeofday () in
    ok (`Array (List.map (Agent.jobs agent) ~f:(Rpc_json.job ~now)))
  | "dequeue" ->
    ok
      (match Agent.dequeue agent with
       | None -> `Null
       | Some queued -> queued_json queued)
  | "remove_queued" ->
    let open Or_error.Let_syntax in
    let%bind queue =
      match%bind string_param params "queue" with
      | "steer" -> Ok `Steer
      | "follow_up" -> Ok `Follow_up
      | other ->
        Or_error.errorf
          "param \"queue\" is %S: use \"steer\" or \"follow_up\""
          other
    in
    let%bind index =
      match param params "index" with
      | Some (`Number n) ->
        (match Int.of_string_opt n with
         | Some i when i >= 0 -> Ok i
         | _ ->
           Or_error.error_string "param \"index\" must be a natural number")
      | Some _ | None ->
        Or_error.error_string "param \"index\" must be a natural number"
    in
    let%bind text = string_param_opt params "text" in
    Or_error.map (Agent.remove_queued agent ~queue ~index ~text) ~f:queued_json
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
          match bool_param params "background" ~default:false with
          | Error _ as e -> e
          | Ok true ->
            ok (`Object [ "job_id", `String (Agent.start_job agent ~command) ])
          | Ok false ->
            Or_error.map
              (Agent.shell agent ~command ~add_to_context)
              ~f:(fun result ->
                `Object
                  [ "text", `String result.text
                  ; ("is_error", if result.is_error then `True else `False)
                  ])))
  | "get_state" -> ok (Rpc_json.state (Agent.state agent))
  | "get_pending" ->
    let steer, follow_up = Agent.queued_texts agent in
    ok
      (Rpc_json.pending
         ~steer
         ~follow_up
         ~confirms:(Agent.pending_confirms agent)
         ~questions:(Agent.pending_questions agent))
  | "list_subagents" ->
    ok (`Array (List.map (Agent.subagents agent) ~f:Rpc_json.subagent_summary))
  | "get_subagent" ->
    Or_error.bind (string_param params "id") ~f:(fun id ->
      match Agent.subagent agent id, param params "elide" with
      | Some (summary, messages), Some `True ->
        ok
          (`Object
              [ "subagent", Rpc_json.subagent_summary summary
              ; "messages", `Array (List.map messages ~f:Message_page.message)
              ])
      | Some found, _ -> ok (Rpc_json.subagent found)
      | None, _ -> Or_error.errorf "unknown subagent %S" id)
  | "get_tool_output" ->
    let open Or_error.Let_syntax in
    let%bind call_id = string_param params "call_id" in
    let%bind agent_id = string_param_opt params "agent_id" in
    let%bind messages =
      match agent_id with
      | None -> Ok (Session.timed_messages (Agent.session agent))
      | Some id ->
        (match Agent.subagent agent id with
         | Some (_, messages) -> Ok messages
         | None -> Or_error.errorf "unknown subagent %S" id)
    in
    (match Message_page.find_output messages ~call_id with
     | Some r ->
       ok
         (`Object
             [ "text", `String r.text
             ; ("is_error", if r.is_error then `True else `False)
             ])
     | None -> Or_error.errorf "no output of tool call %S" call_id)
  | "get_messages" ->
    ok
      (`Array
          (List.map
             (Session.timed_messages (Agent.session agent))
             ~f:Rpc_json.timed_message))
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
      Or_error.map
        (Model_registry.resolve (Login_manager.models login) id)
        ~f:(fun model ->
          Agent.set_model agent model;
          `Object []))
  | "set_thinking" ->
    Or_error.bind (string_param params "thinking") ~f:(fun s ->
      Or_error.map (Rpc_json.thinking_of_string s) ~f:(fun thinking ->
        Agent.set_thinking agent thinking;
        `Object []))
  | "compact" ->
    Or_error.bind
      (string_param_opt params "instructions")
      ~f:(fun instructions ->
        let instructions =
          Option.filter instructions ~f:(fun s ->
            not (String.is_empty (String.strip s)))
        in
        Or_error.map (Agent.compact ?instructions agent) ~f:(fun summary ->
          `Object [ "summary", `String summary ]))
  | "git_diff" ->
    Or_error.bind (string_param_opt params "path") ~f:(fun path ->
      Or_error.map
        (Agent.git_diff ?path agent)
        ~f:[%jsonaf_of: Git_diff.Result.t])
  | "review" ->
    Or_error.bind (string_param_opt params "args") ~f:(fun args ->
      unit_result (Agent.review agent ~args:(Option.value args ~default:"")))
  | "init" -> unit_result (Agent.init agent)
  | "set_budget" ->
    (match param params "usd" with
     | None | Some `Null -> unit_result (Agent.set_budget agent None)
     | Some (`Number n) ->
       (match Float.of_string_opt n with
        | Some usd -> unit_result (Agent.set_budget agent (Some usd))
        | None -> Or_error.error_string "param \"usd\" must be a number")
     | Some _ ->
       Or_error.error_string "param \"usd\" must be a number of dollars or null")
  | "set_goal" ->
    Or_error.bind (string_param params "text") ~f:(fun text ->
      unit_result (Agent.set_goal agent text))
  | "clear_goal" -> unit_result (Agent.clear_goal agent)
  | "resume_goal" -> unit_result (Agent.resume_goal agent)
  | "cancel_resume" -> unit_result (Agent.cancel_resume agent)
  | "resume_now" -> unit_result (Agent.resume_now agent)
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
       (* Fields left out keep their values, so a frontend that does not know
          a field cannot reset it. *)
       let json =
         match Config.to_json (Agent.config agent), json with
         | `Object current, `Object given ->
           (* [confirm_tools] from an older frontend stands for
              [approval_mode]. *)
           let given_names =
             List.map given ~f:fst
             @
             if List.Assoc.mem given ~equal:String.equal "confirm_tools"
             then [ "approval_mode" ]
             else []
           in
           `Object
             (List.filter current ~f:(fun (k, _) ->
                not (List.mem given_names k ~equal:String.equal))
              @ given)
         | _, json -> json
       in
       Or_error.bind (Config.of_json json) ~f:(fun config ->
         Or_error.map (Agent.set_config agent config) ~f:(fun () ->
           Config.to_json (Agent.config agent))))
  | "change_default" ->
    Or_error.map (Agent.save_as_default agent) ~f:(fun () ->
      Config.to_json (Agent.config agent))
  | "tool_confirm_respond" ->
    let open Or_error.Let_syntax in
    let%bind call_id = string_param params "call_id" in
    let%bind agent_id = string_param_opt params "agent_id" in
    let%bind allow =
      match param params "allow" with
      | Some `True -> Ok true
      | Some `False -> Ok false
      | Some _ -> Or_error.error_string "param \"allow\" must be a boolean"
      | None -> Or_error.error_string "missing param \"allow\""
    in
    let%bind always = bool_param params "always" ~default:false in
    let%bind reason = string_param_opt params "reason" in
    let reason =
      Option.filter reason ~f:(fun r -> not (String.is_empty (String.strip r)))
    in
    let answer : Agent.Confirm_answer.t =
      match allow, always with
      | true, true -> Always
      | true, false -> Allow
      | false, _ -> Deny reason
    in
    unit_result (Agent.respond_confirm ?agent_id agent ~call_id answer)
  | "set_mode" ->
    let open Or_error.Let_syntax in
    let%bind approval =
      match%bind string_param_opt params "approval_mode" with
      | None -> Ok None
      | Some s -> Or_error.map (Approval_mode.of_string s) ~f:Option.some
    in
    let%bind plan =
      match param params "plan_mode" with
      | None | Some `Null -> Ok None
      | Some `True -> Ok (Some true)
      | Some `False -> Ok (Some false)
      | Some _ -> Or_error.error_string "param \"plan_mode\" must be a boolean"
    in
    Agent.set_mode ?approval ?plan agent;
    ok (Rpc_json.state (Agent.state agent))
  | "ask_user_respond" ->
    let open Or_error.Let_syntax in
    let%bind call_id = string_param params "call_id" in
    let%bind decline = bool_param params "decline" ~default:false in
    let%bind reply =
      if decline
      then Ok User_question.Reply.Declined
      else (
        match param params "answers" with
        | Some (`Array answers) ->
          Or_error.all
            (List.map answers ~f:(function
               | `Array items ->
                 Or_error.all
                   (List.map items ~f:(function
                      | `String s -> Ok s
                      | _ ->
                        Or_error.error_string
                          "param \"answers\" must be an array of arrays of \
                           strings"))
               | _ ->
                 Or_error.error_string
                   "param \"answers\" must be an array of arrays of strings"))
          |> Or_error.map ~f:(fun answers ->
            User_question.Reply.Answered answers)
        | _ ->
          Or_error.error_string
            "missing param \"answers\" (or \"decline\": true)")
    in
    unit_result (Agent.answer_question agent ~call_id reply)
  | "auth_status" ->
    Or_error.map (Login_manager.status login) ~f:(fun statuses ->
      `Array (List.map statuses ~f:Rpc_json.auth_status))
  | "auth_respond" ->
    Or_error.bind (string_param params "id") ~f:(fun id ->
      Or_error.bind (string_param params "value") ~f:(fun value ->
        unit_result (Login_manager.respond login ~id value)))
  | "auth_cancel" ->
    Login_manager.cancel login;
    empty
  | _ -> Or_error.errorf "unknown method %S" meth
;;

let handle t client (request : Json.t) : Json.t =
  let id = Option.value (param request "id") ~default:`Null in
  let response =
    match param request "method" with
    | Some (`String meth) ->
      let params = request_params request in
      (match
         if (not client.Client.authed) && not (String.equal meth "hello")
         then
           Or_error.error_string "unauthorised: send hello with the token first"
         else (
           match dispatch_server t client ~meth ~params with
           | Some result -> result
           | None ->
             (match
                Files_rpc.dispatch (agent_of_client t client) ~meth ~params
              with
              | Some result -> result
              | None ->
                dispatch (agent_of_client t client) t.login ~meth ~params))
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

let serve_lines ?signed_in t ~read_line ~write_line =
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
  let client = connect ?signed_in t ~send in
  let rec loop () =
    match read_line () with
    | None -> None
    | Some line ->
      if String.is_empty (String.strip line)
      then loop ()
      else (
        match Json.parse line with
        | Error e ->
          send
            (`Object
                [ "type", `String "response"
                ; "id", `Null
                ; "ok", `False
                ; "error", `String ("invalid JSON: " ^ Error.to_string_hum e)
                ]);
          loop ()
        | Ok request ->
          let switch =
            match param request "method" with
            | Some (`String "set_user") ->
              Result.ok (switch_user client (request_params request))
            | _ -> None
          in
          (match switch with
           | Some user ->
             (* Read no further: the router serves the rest of the
                connection as [user]. *)
             Some (Option.value (param request "id") ~default:`Null, user)
           | None ->
             (* Each request in its own fiber: a blocking method (shell,
                compact, a remote tool round trip) must not stall the
                reader. *)
             Fiber.fork ~sw (fun () -> send (handle t client request));
             loop ()))
  in
  let switch_to = loop () in
  disconnect t client;
  Eio.Stream.add outbox None;
  switch_to
;;
