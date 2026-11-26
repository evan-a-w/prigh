open! Core
open! Import

let methods =
  [ "ping"
  ; "resync"
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
  ; "steer_agent"
  ; "dequeue_agent"
  ; "remove_agent_queued"
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
  ; "mcp_signin"
  ; "mcp_signout"
  ; "shell"
  ; "get_state"
  ; "get_messages"
  ; "get_messages_page"
  ; "get_outline"
  ; "get_trim_points"
  ; "get_message_points"
  ; "get_trimmed"
  ; "get_tool_output"
  ; "get_entries"
  ; "set_model"
  ; "set_thinking"
  ; "list_models"
  ; "compact"
  ; "git_diff"
  ; "git_show"
  ; "git_refs"
  ; "list_worktrees"
  ; "create_worktree"
  ; "remove_worktree"
  ; "review"
  ; "init"
  ; "set_budget"
  ; "set_goal"
  ; "clear_goal"
  ; "resume_goal"
  ; "add_reminder"
  ; "remove_reminder"
  ; "clear_reminders"
  ; "cancel_resume"
  ; "resume_now"
  ; "downsize_images"
  ; "new_session"
  ; "set_adapter"
  ; "list_adapters"
  ; "set_adapter_login"
  ; "import_adapter_login"
  ; "keep_session"
  ; "switch_session"
  ; "list_sessions"
  ; "search_sessions"
  ; "set_session_name"
  ; "set_transcript_filter"
  ; "delete_session"
  ; "delete_sessions"
  ; "delete_archived"
  ; "set_archived"
  ; "set_pinned"
  ; "export"
  ; "import"
  ; "fork"
  ; "clone"
  ; "rewind"
  ; "trim"
  ; "trim_after"
  ; "untrim"
  ; "delete"
  ; "undelete"
  ; "session_stats"
  ; "trace"
  ; "set_cwd"
  ; "worktree_back"
  ; "list_paths"
  ; "list_files"
  ; "list_dirs"
  ; "get_config"
  ; "set_config"
  ; "change_default"
  ; "tool_confirm_respond"
  ; "set_mode"
  ; "forget_allowed_call"
  ; "ask_user_respond"
  ; "btw"
  ; "btw_cancel"
  ; "generate_text"
  ; "generate_cancel"
  ; "auth_status"
  ; "login"
  ; "auth_respond"
  ; "auth_cancel"
  ; "logout"
  ; "account_update"
  ; "account_share"
  ; "account_share_targets"
  ; "account_hide"
  ; "account_usage"
  ; "usage_limits"
  ; "forge_status"
  ; "forge_login"
  ; "forge_logout"
  ; "pr_list"
  ; "pr_view"
  ; "pr_defaults"
  ; "pr_create"
  ; "pr_checkout"
  ; "get_prefs"
  ; "set_prefs"
  ; "push_pref"
  ; "get_memory"
  ; "save_memory"
  ; "forget_memory"
  ; "set_memory_enabled"
  ; "get_user_info"
  ; "set_user_info"
  ; "get_custom_instructions"
  ; "save_custom_instruction"
  ; "remove_custom_instruction"
  ; "set_custom_instructions_enabled"
  ; "get_mcp_settings"
  ; "add_mcp_servers"
  ; "update_mcp_server"
  ; "remove_mcp_server"
  ; "set_mcp_server_enabled"
  ]
  @ Files_rpc.methods
  @ Review_rpc.methods
  @ Git_rpc.methods
  @ Nix_store_rpc.methods
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

let if_missing_param params =
  Or_error.bind
    (string_param_opt params "if_missing")
    ~f:Dir_change.If_missing.of_param
;;

let queue_param ?default params name =
  match param params name, default with
  | (None | Some `Null), Some kind -> Ok kind
  | _ ->
    Or_error.bind (string_param params name) ~f:(fun s ->
      Result.map_error (Steer_queue.Kind.of_string s) ~f:(fun _ ->
        Error.createf "param %S is %S: use \"steer\" or \"follow_up\"" name s))
;;

let index_param params =
  match param params "index" with
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some i when i >= 0 -> Ok i
     | _ -> Or_error.error_string "param \"index\" must be a natural number")
  | Some _ | None ->
    Or_error.error_string "param \"index\" must be a natural number"
;;

let unit_result r = Or_error.map r ~f:(fun () -> `Object [])

(* [hello]'s reply's [features]: what this backend takes beyond the basics
   ([pasted_text]: prompts' [pasted]). *)
let backend_features = [ "pasted_text" ]

(* A client's [hello] [features] with this takes [message_update]s as deltas
   (see [Stream_sync]); without it each carries the whole message so far. *)
let stream_deltas = "stream_deltas"
let strings l = `Array (List.map l ~f:(fun s -> `String s))

let pasted_field = function
  | [] -> []
  | pasted -> [ "pasted", strings pasted ]
;;

(* An answer through a component is dropped, not given back to edit: its
   text is empty, and [ui_action] says what it was. *)
let queued_json (queued : Agent.Queued.t) =
  match queued.ui_action with
  | Some action ->
    `Object
      [ "text", `String ""
      ; "attachments", `Array []
      ; ( "ui_action"
        , `Object
            [ "call_id", `String action.call_id
            ; "component", `String action.component
            ; "summary", `String queued.text
            ] )
      ]
  | None ->
    `Object
      ([ "text", `String queued.text
       ; "attachments", strings queued.attachments
       ]
       @ pasted_field queued.pasted)
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

(* An account named by [ref] (["<provider>/<id>"], as [auth_status] lists
   it) or by [provider] and [account] (an id or a label). *)
let account_param login params =
  let open Or_error.Let_syntax in
  let%bind provider, account =
    match%bind string_param_opt params "ref" with
    | Some r ->
      (match String.rsplit2 r ~on:'/' with
       | Some (p, a) ->
         let%map provider = provider_of_string login p in
         provider, a
       | None ->
         Or_error.errorf
           "account ref %S is not <provider>/<account> (as auth_status lists \
            it)"
           r)
    | None ->
      let%bind provider = provider_param login params in
      let%map account = string_param params "account" in
      provider, account
  in
  let%map account =
    Provider_accounts.find (Login_manager.store login) provider account
  in
  provider, account.id
;;

let optional_account_param login params =
  match param params "account", param params "ref" with
  | (None | Some `Null), (None | Some `Null) -> Ok None
  | _ -> Or_error.map (account_param login params) ~f:(fun (_, id) -> Some id)
;;

(* A model key, or [key@account] (an account's id or label); [account]
   instead of the suffix. A key with an [@] of its own still resolves. *)
let model_with_account ?member login key ~account =
  let open Or_error.Let_syntax in
  let models = Login_manager.models login in
  let%bind model, account =
    match Model_registry.resolve models key, account with
    | Ok model, _ -> Ok (model, account)
    | (Error _ as error), Some _ -> Or_error.map error ~f:(fun m -> m, account)
    | (Error _ as error), None ->
      (match String.rsplit2 key ~on:'@' with
       | Some (key, account) when not (String.is_empty account) ->
         let%map model = Model_registry.resolve models key in
         model, Some account
       | Some _ | None -> Or_error.map error ~f:(fun m -> m, None))
  in
  match account with
  | None -> Ok (model, None)
  | Some account ->
    let%map found =
      Login_choice.find
        ?member
        (Login_manager.store login)
        (model : Model.t).provider
        account
    in
    model, Some found.ref_
;;

(* Confirmations and questions waiting for the user. *)
let waiting agent =
  List.length (Agent.pending_confirms agent)
  + List.length (Agent.pending_questions agent)
;;

(* What [list_sessions] shows of a live session that changes as it runs. *)
module Listed = struct
  type t =
    { running : bool
    ; background : int
    ; waiting : int
    ; persisted : bool
    ; name : string option
    ; description : string option
    ; repo : Git_location.t option option
    ; worktrees : Session_worktree.t list
    ; home : Session.Home.t option
    }
  [@@deriving equal]

  let of_agent agent =
    let session = Agent.session agent in
    { running = Agent.is_running agent
    ; background = Agent.running_background agent
    ; waiting = waiting agent
    ; persisted = Session.persisted session
    ; name = Session.name session
    ; description = Session.description session
    ; repo = Session.repo session
    ; worktrees = Session.worktrees session
    ; home = Session.home session
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
    ; mutable kind : Client_kind.t
      (** [hello]'s [client]: its prompts are from this kind of client *)
    ; mutable tools : bool
    ; mutable computer : Computer_info.t option
      (** [hello]'s [computer], with the [computer] feature *)
    ; mutable features : string list
      (** [hello]'s [features]: what the host supports beyond the basics
          ([exec_input]: [tool_exec_input]; [terminal_kill]: the
          [terminal_kill] event; [git_location]: [$git_location]) *)
    ; mutable cwd : string option
    ; mutable home : string option (** [hello]'s [home]: a tool host's *)
    ; mutable prefer_host : string option
      (** [hello]'s [prefer_host], for a client without tools: the host id
          of the tool host on its machine *)
    ; mutable agent : Agent.t
    ; mutable placeholder : Agent.t option
      (** the fresh session [connect] gave it, until [hello] (see
          [first_session]) *)
    ; mutable authed : bool
    ; signed_in : User_access.Signed_in.t option
      (** set when the router checked the credentials *)
    ; send : Json.t -> unit
    ; send_shared : Json.t -> line:string Lazy.t -> unit
      (** [send], given what [Json.to_string] makes of it, shared with the
          other clients sent the same event *)
    ; streams : Stream_sync.t (** what it was sent of streamed messages *)
    ; close : string -> unit
      (** ends the connection (the reason goes to stderr) *)
    ; mutable tab : string option
      (** [hello]'s [tab]: the page instance it comes from *)
    ; btws : Cancellation.t String.Table.t (** in-flight [btw] calls by id *)
    ; generations : Cancellation.t String.Table.t
      (** in-flight [generate_text] calls by id *)
    ; mutable told : String.Set.t (** model registry problems already sent *)
    ; mutable heard : float
      (** when its last line came, by [heartbeat_clock]: a tool host that
          promised heartbeats ([features] has [heartbeat]) and is silent for
          [host_timeout] is given up on *)
    ; mutable silent : bool (** given up on for that *)
    }

  let id t = t.id
end

(* A live temporary session (see [end_temporary]). *)
module Temporary = struct
  type t =
    { key : string
      (** what reattaching to it takes ([hello]'s [temporary_key]): only
          the page that made it has it *)
    ; mutable away : unit ref option
      (** while it has no client: the grace period's token *)
    ; mutable written : Leftover.t list (** in its ledger *)
    }
end

(* An execution sent to a tool host, until its result. *)
module Exec = struct
  module Holder = struct
    type t =
      | Client of string (** the connection it was sent on *)
      | Away of unit ref
      (** that connection dropped: the host may come back with it still
          running; the token tells this absence from later ones *)
  end

  type t =
    { host : string
    ; agent : Agent.t
    ; mutable holder : Holder.t
    }
end

type t =
  { login : Login_manager.t
  ; member : Account_shares.Member.t option
    (** the user and the logins shared among users ([-tokens]) *)
  ; ledger : Account_ledger.t option (** what ran on the user's logins *)
  ; forges : Forges.t
  ; token : string option (** required in [hello] before anything else *)
  ; namespace : string option
  ; backend_host : bool
  ; sessions_dir : string
  ; cwd : string
  ; new_agent : ?session:Session.t -> cwd:string -> unit -> Agent.t
  ; default_agent : Agent.t option
  ; engines : Engines.t (** what this backend deploys *)
  ; adapter_health : Adapter_health.Cache.t
    (** whether the composer's CLIs are installed and logged in, by host *)
  ; login_seen : Adapter_health.Seen.Table.t
    (** what starting the ACP agents showed of their own logins, by host *)
  ; codex_sandbox : Codex_sandbox.Cache.t
    (** whether Codex's own sandbox can run, by host *)
  ; catalogs : Adapter_catalog.t
    (** the composer's CLIs' models and usage limits, by host *)
  ; spawn_external :
      (Agent.t
       -> host:string
       -> Adapter.Launch.t
       -> Line_transport.t Or_error.t)
        option
  ; catalog_keys : (string * string option) String.Table.t
    (** the host and login each composer session's models were last asked
        for, by session id: when either changes, its clients ask again *)
  ; agents : Agent.t String.Table.t (** live sessions by session id *)
  ; clients : Client.t String.Table.t
  ; mutable client_seq : int
  ; mutable btw_seq : int
  ; mutable generate_seq : int
  ; execs : Exec.t String.Table.t (** in-flight remote executions by id *)
  ; host_grace : float
  ; host_timeout : float
    (** how long a tool host that sends heartbeats may be silent *)
  ; heartbeat_clock : float Eio.Time.clock_ty Eio.Resource.t
    (** how long the executions of a host that dropped its connection wait
        for it to come back with them, and new calls for it to come back *)
  ; returning : float String.Table.t
    (** host ids whose connection dropped, by when they are given up on *)
  ; sleep : float -> unit
  ; sw : Switch.t
  ; mutable login_owner : string option
    (** the client running the current login flow *)
  ; terminals : Terminal_relay.t
  ; home : string option (** for the current [config.json] *)
  ; prefs : User_prefs.t option (** under [home] *)
  ; memory : Memory.t option (** under [home] *)
  ; user_info : User_info.t option (** under [home] *)
  ; custom_instructions : Custom_instructions.t option (** under [home] *)
  ; mcp_settings : Mcp_settings.t option (** under [home] *)
  ; mcp_auth : Mcp_auth.t option (** the user's MCP sign-ins *)
  ; now : unit -> float
  ; listed : Listed.t String.Table.t
    (** what [list_sessions] last showed of each live session *)
  ; mutable on_session_deleted : (key:string -> unit) list
  ; loaded : Session.t File_cache.t
    (** sessions read from disk and not live: evicted ones, and those read
        for [get_messages_page] without opening them *)
  ; loading : Session.t Or_error.t Promise.t String.Table.t
    (** session files being parsed, by path *)
  ; search : Session_search.t (** [search_sessions]' index *)
  ; served : unit Promise.t * unit Promise.u
    (** resolved once a request has been answered: {!start_background} waits
        for it *)
  ; repos : Git_location.t option String.Table.t
    (** where the cwds of saved sessions that recorded none are in git, read
        from the backend's files once (see [with_repo]) *)
  ; nix_store : Nix_store.t option
  ; usage_http : Subscription_usage.Http.t
  ; temporaries : Temporary.t String.Table.t (** by session id *)
  ; temporary_grace : float
  ; ledger_dir : string
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

(* After the response that opened the session, so that the client's reset
   for the new session does not clear it. *)
let tell_skipped (client : Client.t) =
  match Session.skipped (Agent.session client.agent) with
  | [] -> ()
  | skipped ->
    client.send
      (Rpc_json.event
         (Notice
            (sprintf
               "parts of this session could not be read and are left out:\n%s"
               (String.concat ~sep:"\n" skipped))))
;;

let clients_of t agent =
  Hashtbl.data t.clients
  |> List.filter ~f:(fun (c : Client.t) -> phys_equal c.agent agent)
;;

(* The tool host clients are few: a scan is fine. *)
let host_client t host_id =
  List.find (Hashtbl.data t.clients) ~f:(fun (c : Client.t) ->
    c.tools && String.equal c.host_id host_id)
;;

let terminal_key t ~session =
  match t.namespace with
  | None -> session
  | Some namespace -> namespace ^ ":" ^ session
;;

(* The ledger follows what the session made, so that a restart can delete
   it. *)
let write_ledger t agent =
  let id = session_id agent in
  Option.iter (Hashtbl.find t.temporaries id) ~f:(fun (temp : Temporary.t) ->
    let leftovers = Agent.leftovers agent in
    if not (List.equal Leftover.equal leftovers temp.written)
    then (
      temp.written <- leftovers;
      try Temporary_ledger.write ~dir:t.ledger_dir { id; leftovers } with
      | exn ->
        eprintf
          "prigh: the ledger of temporary session %s: %s\n%!"
          id
          (Exn.to_string exn)))
;;

let temporary_ended_event ~session ~messages ~kept =
  `Object
    [ "type", `String "event"
    ; "event", `String "temporary_ended"
    ; "session", `String session
    ; "messages", `Number (Int.to_string messages)
    ; ( "kept"
      , `Array
          (List.map kept ~f:(fun ((l : Leftover.t), why) ->
             `Object
               [ "path", `String l.path
               ; ( "kind"
                 , `String
                     (match l.kind with
                      | Upload -> "upload"
                      | Output -> "output"
                      | Worktree -> "worktree") )
               ; "why", `String why
               ])) )
    ]
;;

(* Its terminals go with it. *)
let forget_session t id =
  let key = terminal_key t ~session:id in
  List.iter (Terminal_relay.close_key t.terminals ~key) ~f:(fun host ->
    Option.iter (host_client t host) ~f:(fun (c : Client.t) ->
      if List.mem c.features "terminal_kill" ~equal:String.equal
      then c.send (Terminal_relay.kill_event ~host ~key)));
  List.iter t.on_session_deleted ~f:(fun f -> f ~key)
;;

(* Removes [leftovers] where their host is connected; the rest (and what a
   host failed at while going away) stay in the session's ledger, for when
   it connects. Answers what a connected host could not remove (worktrees
   with uncommitted changes). *)
let remove_leftovers t agent ~id leftovers =
  let reachable (l : Leftover.t) =
    if String.equal l.host Agent.Host.backend_id
    then t.backend_host
    else Option.is_some (host_client t l.host)
  in
  let now, later = List.partition_tf leftovers ~f:reachable in
  let failed = Agent.remove_leftovers agent now in
  let lost, kept =
    List.partition_tf failed ~f:(fun ((l : Leftover.t), _) -> not (reachable l))
  in
  (match later @ List.map lost ~f:fst with
   | [] -> Temporary_ledger.remove ~dir:t.ledger_dir ~id
   | leftovers -> Temporary_ledger.write ~dir:t.ledger_dir { id; leftovers });
  kept
;;

(* Leaving a temporary session ends it: nobody can reach it any more, what
   runs in it stops, and what it made goes (but worktrees with uncommitted
   changes, which [notify] hear of). What could not be deleted on a host
   that is gone stays in the ledger, for when it is back. *)
let end_temporary t agent ~notify =
  let id = session_id agent in
  if Option.exists (Hashtbl.find t.agents id) ~f:(phys_equal agent)
  then (
    Hashtbl.remove t.agents id;
    Hashtbl.remove t.listed id;
    Hashtbl.remove t.catalog_keys id;
    Hashtbl.remove t.temporaries id;
    Fiber.fork ~sw:t.sw (fun () ->
      Guarded.run ~what:"ending a temporary session" (fun () ->
        Agent.stop_everything agent;
        forget_session t id;
        let kept = remove_leftovers t agent ~id (Agent.leftovers agent) in
        let event =
          temporary_ended_event
            ~session:id
            ~messages:(List.length (Agent.messages agent))
            ~kept
        in
        List.iter notify ~f:(fun (c : Client.t) ->
          if Hashtbl.mem t.clients c.id then c.send event))))
;;

(* A temporary session without a client waits [temporary_grace] for one
   (a reload, a phone that killed the page) before it ends. *)
let await_return t agent =
  match Hashtbl.find t.temporaries (session_id agent) with
  | None -> ()
  | Some { away = Some _; _ } -> ()
  | Some temp ->
    let token = ref () in
    temp.away <- Some token;
    Fiber.fork_daemon ~sw:t.sw (fun () ->
      t.sleep t.temporary_grace;
      (match temp.away with
       | Some token' when phys_equal token token' ->
         temp.away <- None;
         if List.is_empty (clients_of t agent)
         then end_temporary t agent ~notify:[]
       | Some _ | None -> ());
      `Stop_daemon)
;;

(* Parsing a large session file takes a while (a second for 80 MB): it
   yields as it goes, so that every other client is served meanwhile, and
   those who want the same file meanwhile wait for the same parse, which
   finishes even if whoever started it goes (the next one finds it
   cached). *)
let load_session t path =
  match Hashtbl.find t.loading path with
  | Some loading -> Promise.await loading
  | None ->
    let loading, resolve = Promise.create () in
    Hashtbl.set t.loading ~key:path ~data:loading;
    let result =
      match
        Eio.Cancel.protect (fun () -> Session.load ~yield:Fiber.yield path)
      with
      | result -> result
      | exception exn -> Or_error.of_exn exn
    in
    Hashtbl.remove t.loading path;
    Promise.resolve resolve result;
    result
;;

(* The live sessions by their files' paths. *)
let live_sessions t =
  let live = String.Table.create () in
  Hashtbl.iter t.agents ~f:(fun agent ->
    let session = Agent.session agent in
    Hashtbl.set live ~key:(Session.path session) ~data:session);
  Hashtbl.find live
;;

(* The saved sessions; the live ones' files are summed up from memory, as
   they grow with every message. *)
let saved_sessions ?(yield = ignore) t =
  Session.list_with
    ~yield
    ~live:(live_sessions t)
    ~load:(load_session t)
    ~dir:t.sessions_dir
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

(* A saved session as [list_sessions] shows it to [client]. *)
let listing t ~now ~archive_after_days summary =
  let agent = Hashtbl.find t.agents summary.Session.Summary.id in
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
  }
;;

let listings t client =
  let now = t.now () in
  let archive_after_days = archive_after_days t client in
  List.map (saved_sessions t) ~f:(listing t ~now ~archive_after_days)
;;

(* Sessions from before repositories were recorded (or whose host could not
   tell) on the backend's own host: their cwd is looked up here, once. *)
let with_repo t (s : Session.Summary.t) =
  match s.repo with
  | Some _ -> s
  | None ->
    let on_backend =
      Option.value_map
        s.host
        ~default:true
        ~f:(String.equal Agent.Host.backend_id)
    in
    if t.backend_host && on_backend
    then (
      let repo =
        Hashtbl.find_or_add t.repos s.cwd ~default:(fun () ->
          Git_location.find ~cwd:s.cwd)
      in
      { s with repo = Some repo })
    else s
;;

let listing_json t (l : Listing.t) =
  let bool b = if b then `True else `False in
  let int n = `Number (Int.to_string n) in
  let live =
    match l.agent with
    | Some agent ->
      [ "live", `True
      ; "clients", int (List.length (clients_of t agent))
      ; "waiting", int (waiting agent)
      ]
    | None -> [ "live", `False ]
  in
  match Rpc_json.session_summary (with_repo t l.summary) with
  | `Object fields ->
    `Object
      (fields
       @ live
       @ [ "running", bool l.active
         ; "background", int l.background
         ; "archived", bool l.archived
         ])
  | other -> other
;;

let list_sessions t client =
  `Array (List.map (listings t client) ~f:(listing_json t))
;;

(* The clients that have signed in: the namespace's events (its sessions,
   settings, hosts and agent CLIs) are not for a connection that has not. *)
let iter_authed t ~f =
  Hashtbl.iter t.clients ~f:(fun (c : Client.t) -> if c.authed then f c)
;;

let broadcast t json = iter_authed t ~f:(fun (c : Client.t) -> c.send json)

(* Every client of the server, whichever session it is in. With [changed]
   (sessions whose entries changed, in memory as they are now) or
   [removed] (session ids), they get those sessions' [list_sessions]
   entries as they would see them and the ids of those no longer listed;
   without, they refetch [list_sessions]. *)
let sessions_changed ?(changed = []) ?(removed = []) t =
  let event fields =
    `Object
      ([ "type", `String "event"; "event", `String "sessions_changed" ] @ fields)
  in
  if List.is_empty changed && List.is_empty removed
  then broadcast t (event [])
  else (
    let live_now = live_sessions t in
    let live path =
      match
        List.find changed ~f:(fun s -> String.equal (Session.path s) path)
      with
      | Some session -> Some session
      | None -> live_now path
    in
    let summaries, gone =
      List.partition_map changed ~f:(fun session ->
        match
          Session.summary_with
            ~live
            ~load:(load_session t)
            (Session.path session)
        with
        | Some summary -> First summary
        | None -> Second (Session.id session))
    in
    let removed =
      ( "removed"
      , `Array
          (List.map
             (List.dedup_and_sort (removed @ gone) ~compare:String.compare)
             ~f:(fun id -> `String id)) )
    in
    let now = t.now () in
    iter_authed t ~f:(fun (c : Client.t) ->
      let archive_after_days = archive_after_days t c in
      c.send
        (event
           [ ( "changed"
             , `Array
                 (List.map summaries ~f:(fun summary ->
                    listing_json t (listing t ~now ~archive_after_days summary)))
             )
           ; removed
           ])))
;;

let maybe_evict t agent =
  if Agent.temporary agent
  then (if List.is_empty (clients_of t agent) then await_return t agent)
  else if
    (* a stale agent of a session reopened since is not the live one *)
    Option.exists
      (Hashtbl.find t.agents (session_id agent))
      ~f:(phys_equal agent)
    && (not (Option.exists t.default_agent ~f:(phys_equal agent)))
    && (not (Agent.is_running agent))
    && (not (Agent.has_running_background agent))
    && (not (Agent.resume_pending agent))
    && List.is_empty (clients_of t agent)
  then (
    Hashtbl.remove t.agents (session_id agent);
    Hashtbl.remove t.listed (session_id agent);
    Agent.stop_engine agent;
    let session = Agent.session agent in
    if Session.persisted session && not (Session.gone session)
    then (
      File_cache.set t.loaded (Session.path session) session;
      (* No longer live. *)
      sessions_changed t ~changed:[ session ]))
;;

(* Only for changes: what is shown at [register] is the baseline. *)
let note_listed t agent =
  let id = session_id agent in
  match Hashtbl.find t.listed id with
  | Some before
    when Option.exists (Hashtbl.find t.agents id) ~f:(phys_equal agent)
         && not (Agent.temporary agent) ->
    let listed = Listed.of_agent agent in
    if not (Listed.equal before listed)
    then (
      Hashtbl.set t.listed ~key:id ~data:listed;
      sessions_changed t ~changed:[ Agent.session agent ])
  | Some _ | None -> ()
;;

let host_of (client : Client.t) =
  { Agent.Host.id = client.host_id
  ; name = client.name
  ; cwd = Option.value client.cwd ~default:(Agent.state client.agent).cwd
  ; session_id = Some (session_id client.agent)
  ; session_name = Session.name (Agent.session client.agent)
  ; home = client.home
  ; exec_input = List.mem client.features "exec_input" ~equal:String.equal
  ; git_location = List.mem client.features "git_location" ~equal:String.equal
  ; computer = client.computer
  }
;;

let hosts t =
  Hashtbl.data t.clients
  |> List.filter ~f:(fun (c : Client.t) -> c.tools)
  |> List.sort ~compare:(fun (a : Client.t) b -> Int.compare a.seq b.seq)
  |> List.map ~f:host_of
;;

let returning t =
  Hashtbl.filter_keys t.returning ~f:(fun id ->
    Option.is_none (host_client t id))
  |> Hashtbl.to_alist
  |> String.Map.of_alist_exn
;;

(* Snapshots the agents: setting hosts emits state changes, which may evict. *)
let publish_hosts t =
  let hosts = hosts t in
  let returning = returning t in
  List.iter (Hashtbl.data t.agents) ~f:(fun agent ->
    Agent.set_hosts agent hosts ~returning)
;;

(* Why an execution failed when its host went away with it. *)
let host_disconnected =
  "[the tool host disconnected during this call, so its result is lost: run it \
   again if you still need it]"
;;

let host_restarted =
  "[the tool host restarted during this call, so its result is lost: run it \
   again if you still need it]"
;;

let host_silent t (client : Client.t) =
  sprintf
    "[the tool host %S stopped responding (no reply for %gs), so it was \
     disconnected and this call's result is lost: pick another host with \
     /host, or wait for it to reconnect and run it again]"
    client.name
    t.host_timeout
;;

(* The connection running an execution: the one it was sent on, which may
   have handed its host id to a newer one ([claim_host_id]). *)
let exec_client t ~host exec_id =
  match Hashtbl.find t.execs exec_id with
  | Some { holder = Client id; _ } -> Hashtbl.find t.clients id
  | Some { holder = Away _; _ } -> None
  | None -> host_client t host
;;

(* Hosts are global: every client that advertised tools, whichever session
   it is attached to. Execs are keyed by id (not by the answering client's
   session) so a host can run tools for other sessions. *)
(* An event for every client of [agent], serialised once for all. A
   [message_update] is sent as a delta to a client that has the stream so far,
   or with the whole message to one that has not (a client that does not
   take deltas: always). *)
let send_to_clients t agent ~now (event : Agent.Event.t) json =
  let line = lazy (Json.to_string json) in
  let clients = clients_of t agent in
  let update =
    match event with
    | Loop e -> Stream_sync.Update.of_event e
    | _ -> None
  in
  match update with
  | None -> List.iter clients ~f:(fun c -> c.send_shared json ~line)
  | Some update ->
    let resync = lazy (Rpc_json.event ~now ~resync:true event) in
    let resync_line = lazy (Json.to_string (force resync)) in
    List.iter clients ~f:(fun c ->
      let send : Stream_sync.Send.t =
        if List.mem c.features stream_deltas ~equal:String.equal
        then Stream_sync.send c.streams update
        else Resync
      in
      match send with
      | Skip -> ()
      | Delta -> c.send_shared json ~line
      | Resync -> c.send_shared (force resync) ~line:resync_line)
;;

(* The login of prigh's the user runs [adapter]'s CLI on (Settings →
   Engines), if one. *)
let adapter_login t ~adapter =
  match t.home, Cli_login.provider ~adapter with
  | Some home, Some provider ->
    (match Cli_login.find ~home ~adapter with
     | Own -> None
     | Prigh ref_ -> Some (provider, ref_))
  | _ -> None
;;

(* What {!Adapter_catalog} keeps [adapter]'s catalog under for the login it
   runs on: [None] for its own. *)
let catalog_login t ~adapter =
  Option.map (adapter_login t ~adapter) ~f:(fun (provider, ref_) ->
    Provider_id.to_string provider ^ "/" ^ Account_ref.to_string ref_)
;;

let login_label t provider ref_ =
  match
    Login_choice.find
      ?member:t.member
      (Login_manager.store t.login)
      provider
      (Account_ref.to_string ref_)
  with
  | Ok choice -> choice.label
  | Error _ -> Account_ref.to_string ref_
;;

(* What a composer session's CLI is handed: one of prigh's logins, whose
   problems (gone, unshared) the CLI's start reports. *)
let cli_credential t agent ~adapter =
  Option.map (adapter_login t ~adapter) ~f:(fun (provider, ref_) ->
    match
      Cli_credential.of_store
        ~env:(Agent.env agent)
        ?member:t.member
        ?ledger:t.ledger
        (Login_manager.store t.login)
        provider
        ref_
    with
    | Ok credential -> credential
    | Error e ->
      Cli_credential.create
        ~provider
        ~account:ref_
        ~label:(Account_ref.to_string ref_)
        ~fetch:(fun ?refused:_ ?valid_for_ms:_ () -> Error e)
        ())
;;

(* Where a composer session's harness is with listing its models while the
   session's model is a placeholder: being asked, or why it could not. *)
let harness_models t (s : Agent.State.t) =
  match s.engine with
  | { engine = Composer; adapter = Some ((Codex | Claude | Acp _) as a) }
    when External_model.is_placeholder s.model ->
    let adapter = Adapter_id.to_string a in
    let host = s.active_host in
    let login = catalog_login t ~adapter in
    (match
       ( Adapter_catalog.asking t.catalogs ~host ~login adapter
       , Adapter_catalog.failure t.catalogs ~host ~login adapter
       , Adapter_catalog.find t.catalogs ~host ~login adapter )
     with
     | true, _, _ -> Some Harness_models.Loading
     | false, Some why, _ ->
       let host_name =
         match
           List.find s.hosts ~f:(fun (h : Agent.Host.t) ->
             String.equal h.id host)
         with
         | Some h when not (String.is_empty h.name) -> h.name
         | _ ->
           (match s.active_host_name with
            | Some name when not (String.is_empty name) -> name
            | _ -> host)
       in
       Some
         (Harness_models.failed
            ~label:(Adapter_id.label a)
            ~host_name
            ~prigh_login:
              (Option.map (adapter_login t ~adapter) ~f:(fun (provider, _) ->
                 Provider_id.to_string provider))
            ~install:
              (Option.map
                 (Adapter_health.Cache.find t.adapter_health ~host a)
                 ~f:(fun (h : Adapter_health.Status.t) -> h.install))
            why)
     (* It listed none: it picks its own. *)
     | false, None, Some { models = []; _ } -> None
     | false, None, (Some { models = _ :: _; _ } | None) ->
       Some Harness_models.Loading)
  | _ -> None
;;

(* A session's state, with the label of prigh's login its CLI runs on and
   where its harness is with listing its models. *)
let state_json t (s : Agent.State.t) =
  let json = Rpc_json.state s in
  match s.engine, json with
  | { engine = Composer; adapter = Some a }, `Object fields ->
    `Object
      (fields
       @ (match adapter_login t ~adapter:(Adapter_id.to_string a) with
          | Some (provider, ref_) ->
            [ "adapter_login", `String (login_label t provider ref_) ]
          | None -> [])
       @
       match harness_models t s with
       | Some status -> [ "harness_models", Harness_models.to_json status ]
       | None -> [])
  | _ -> json
;;

(* Tells the session's clients its state when its harness's listing of its
   models moved on (asked, failed) while its model is a placeholder: the
   agent itself has nothing to say then. *)
let send_harness_models t agent =
  let s = Agent.state agent in
  if Option.is_some (harness_models t s)
  then (
    let json =
      `Object
        [ "type", `String "event"
        ; "event", `String "state"
        ; "state", state_json t s
        ]
    in
    List.iter (clients_of t agent) ~f:(fun (c : Client.t) -> c.send json))
;;

(* The session's model list changed: its clients ask for it again. *)
let models_changed t agent =
  Agent.models_changed agent;
  let json =
    `Object [ "type", `String "event"; "event", `String "models_changed" ]
  in
  List.iter (clients_of t agent) ~f:(fun (c : Client.t) -> c.send json);
  send_harness_models t agent
;;

(* A composer session's harness lists its models on its tool host, on the
   login it runs on: when the session moves to another host (as a new chat
   does when the host the user picked connects) or the harness to another
   login, its clients ask its models again, which asks them there. *)
let note_catalog_key t agent =
  match Agent.engine_choice agent with
  | { engine = Composer; adapter = Some ((Codex | Claude | Acp _) as a) } ->
    let key =
      Agent.active_host agent, catalog_login t ~adapter:(Adapter_id.to_string a)
    in
    let id = session_id agent in
    (match Hashtbl.find t.catalog_keys id with
     | Some old
       when [%equal: string * string option] old key
            || Agent.external_live agent -> ()
     | Some _ ->
       Hashtbl.set t.catalog_keys ~key:id ~data:key;
       models_changed t agent
     | None -> Hashtbl.set t.catalog_keys ~key:id ~data:key)
  | _ -> ()
;;

let route t agent (event : Agent.Event.t) =
  let now = Eio.Time.now (Eio.Stdenv.clock (Agent.env agent)) in
  let json = Rpc_json.event ~now event in
  let json =
    match event, json with
    | State_changed s, `Object fields ->
      `Object
        (List.map fields ~f:(fun (k, v) ->
           if String.equal k "state" then k, state_json t s else k, v))
    | _ -> json
  in
  (match event with
   | Tool_exec { host; exec_id; _ } ->
     (match host_client t host with
      | Some c ->
        Hashtbl.set
          t.execs
          ~key:exec_id
          ~data:{ Exec.host; agent; holder = Client c.id };
        c.send json
      | None ->
        ignore
          (Agent.tool_exec_result
             agent
             ~exec_id
             (Tool.Result.error (Agent.host_unavailable_message agent))
           : unit Or_error.t))
   | Tool_exec_cancel { host; exec_id } ->
     Option.iter (exec_client t ~host exec_id) ~f:(fun c -> c.send json);
     Hashtbl.remove t.execs exec_id
   | Tool_exec_input { host; exec_id; _ } ->
     Option.iter (exec_client t ~host exec_id) ~f:(fun c -> c.send json)
   | _ -> send_to_clients t agent ~now event json);
  (* A subagent's messages too: its tools may have made worktrees. *)
  let rec message_end : Agent_event.t -> bool = function
    | Message_end _ -> true
    | Subagent { event; _ } -> message_end event
    | _ -> false
  in
  (match event with
   | State_changed _
   | Loop (Tool_confirm _ | Tool_confirm_done _ | Ask_user _ | Ask_user_done _)
     -> note_listed t agent
   | Loop e when message_end e ->
     note_listed t agent;
     if Agent.temporary agent then write_ledger t agent
   | _ -> ());
  match event with
  | State_changed { running; _ } ->
    note_catalog_key t agent;
    if not running then maybe_evict t agent;
    (* A host's session name may have changed; [set_hosts] is a no-op when
       nothing did, so this does not loop. *)
    publish_hosts t
  | _ -> ()
;;

(* Why [agent]'s session cannot run here: its engine is not deployed, or it
   names one this prigh does not know. *)
let engine_unavailable t agent =
  match Session.engine (Agent.session agent) with
  | Ok choice -> Engines.unavailable t.engines choice
  | Error e ->
    Some
      (sprintf
         "this session was made by a newer prigh (%s): update prigh to run it; \
          it can still be read here"
         (Error.to_string_hum e))
;;

(* A probed status with what else is known of its login: the prigh login it
   runs on, what starting it showed, a check under way. *)
let with_evidence t (s : Adapter_health.Status.t) =
  let adapter = Adapter_id.to_string s.adapter in
  Adapter_health.Status.with_evidence
    ?no_sandbox:
      (match s.adapter with
       | Codex ->
         (match Codex_sandbox.Cache.find t.codex_sandbox ~host:s.host with
          | Some (Unavailable why) -> Some why
          | Some (Works | Unknown _) | None -> None)
       | Claude | Prigh | Acp _ -> None)
    s
    ~prigh_login:
      (Option.map (adapter_login t ~adapter) ~f:(fun (provider, ref_) ->
         login_label t provider ref_))
    ~seen:(Adapter_health.Seen.Table.find t.login_seen ~host:s.host s.adapter)
    ~checking:
      (Adapter_catalog.asking t.catalogs ~host:s.host ~login:None adapter)
;;

(* The statuses of the deployed adapters on [hosts], in that order. *)
let adapter_statuses t hosts =
  List.concat_map hosts ~f:(fun (host : Agent.Host.t) ->
    List.filter_map t.engines.adapters ~f:(fun adapter ->
      Adapter_health.Cache.find t.adapter_health ~host:host.id adapter
      |> Option.map ~f:(with_evidence t)))
;;

let adapters_json t statuses =
  `Array (List.map statuses ~f:(Adapter_health.Status.to_json ~now:(t.now ())))
;;

let adapters_changed t ~hosts =
  let json =
    `Object
      [ "type", `String "event"
      ; "event", `String "adapters_changed"
      ; "adapters", adapters_json t (adapter_statuses t hosts)
      ]
  in
  broadcast t json
;;

(* Notes what starting an ACP agent on its own login on [host] showed of
   that login: whether that changed what is known. *)
let note_seen t ~host (adapter : Adapter_id.t) (result : _ Or_error.t) =
  match adapter with
  | Codex | Claude | Prigh -> false
  | Acp _ ->
    let at = t.now () in
    let seen : Adapter_health.Seen.t =
      match result with
      | Ok _ -> Opened { at }
      | Error e when Acp_adapter.is_not_logged_in e -> Refused { at }
      | Error e -> Check_failed { at; why = Error.to_string_hum e }
    in
    let table = t.login_seen in
    let before = Adapter_health.Seen.Table.find table ~host adapter in
    Adapter_health.Seen.Table.record table ~host adapter seen;
    let after = Adapter_health.Seen.Table.find table ~host adapter in
    let kind : Adapter_health.Seen.t option -> int = function
      | Some (Opened _) -> 1
      | Some (Refused _) -> 2
      | Some (Check_failed _) -> 3
      | None -> 0
    in
    kind before <> kind after
;;

(* The session's active tool host by name, for messages. *)
let active_host_label agent =
  let id = Agent.active_host agent in
  match
    List.find (Agent.hosts agent) ~f:(fun (h : Agent.Host.t) ->
      String.equal h.id id)
  with
  | Some host when not (String.is_empty host.name) -> host.name
  | _ ->
    let name = Agent.active_host_name agent in
    if String.is_empty name then id else name
;;

(* The harness on the session's active tool host, in its cwd. *)
let host_spawn t agent launch =
  Host_transport.spawn
    ~sw:t.sw
    ~exec:(fun ~stdin ~cancel ~on_output ~name args ->
      Agent.host_call agent ~stdin ~cancel ~on_output ~name args)
    ~host:(active_host_label agent)
    launch
;;

(* [adapter]'s harness failed to start on the session's host: what was
   known of it there is checked again, and the frontends told. *)
let recheck_adapter t agent adapter =
  let host = Agent.active_host agent in
  Adapter_health.Cache.forget t.adapter_health ~host adapter;
  Codex_sandbox.Cache.forget_host t.codex_sandbox ~host;
  Fiber.fork ~sw:t.sw (fun () ->
    let statuses =
      Adapter_health.probe
        ~exec:(fun ~name arguments ->
          Agent.host_call_on
            agent
            ~host
            ~cwd:(Session.cwd (Agent.session agent))
            ~name
            arguments)
        [ adapter ]
        ~host
        ~host_name:(active_host_label agent)
        ~now:(t.now ())
    in
    Adapter_health.Cache.set t.adapter_health statuses;
    adapters_changed t ~hosts:(Agent.state agent).hosts)
;;

(* The harness [adapter] said its catalog on [host], on the login [login]:
   the sessions it runs there, on that login, show its models. *)
let catalog_changed t ~host ~login adapter =
  if [%equal: string option] login (catalog_login t ~adapter)
  then
    Hashtbl.iter t.agents ~f:(fun agent ->
      match Agent.engine_choice agent with
      | { engine = Composer; adapter = Some a }
        when String.equal (Adapter_id.to_string a) adapter
             && String.equal (Agent.active_host agent) host ->
        models_changed t agent
      | _ -> ())
;;

(* Why [adapter_id]'s harness, handed prigh's login [credential], did not
   say its catalog: a refused login is that login's problem, not the
   harness's own login on the host. *)
let catalog_login_error (adapter_id : Adapter_id.t) credential e =
  let label = Cli_credential.label credential in
  let provider = Provider_id.to_string (Cli_credential.provider credential) in
  let what_next =
    sprintf
      "/login %s logs in again, or Settings → Engines runs %s on its own login"
      provider
      (Adapter_id.label adapter_id)
  in
  let refused =
    Acp_adapter.is_not_logged_in e
    || String.is_substring (Error.to_string_hum e) ~substring:"refused prigh's"
  in
  match Cli_credential.last credential with
  | Some (Error why) ->
    Error.createf
      "prigh's %s login %S gave it no token (%s): %s"
      provider
      label
      (Error.to_string_hum why)
      what_next
  | (Some (Ok _) | None) when refused ->
    Error.createf
      "it was refused prigh's %s login %S, or could not use it: %s"
      provider
      label
      what_next
  | Some (Ok _) | None -> e
;;

(* Runs [adapter]'s harness on the tool host [host] (named [host_name]), in
   [cwd], only to ask its catalog ({!Adapter.S.catalog}), on prigh's login
   [login] (else its own: then what that shows of its own login is
   noted). *)
let fetch_catalog
      t
      agent
      ~host
      ~host_name
      ~cwd
      ~login
      adapter_id
      (module A : Adapter.S)
      ()
  =
  let login = force login in
  let start = { (Adapter.Start.default ~cwd) with login } in
  let launch = A.launch start in
  let result =
    let%bind.Or_error transport =
      match t.spawn_external with
      | Some spawn -> spawn agent ~host launch
      | None ->
        Host_transport.spawn
          ~sw:t.sw
          ~exec:(fun ~stdin ~cancel ~on_output ~name args ->
            Agent.host_call_on
              agent
              ~stdin
              ~cancel
              ~on_output
              ~host
              ~cwd
              ~name
              args)
          ~host:host_name
          launch
    in
    Switch.run (fun sw ->
      Exn.protect
        ~finally:(fun () -> Line_transport.stop transport)
        ~f:(fun () ->
          A.catalog
            ~sw
            ~request_timeout:(Time_float.Span.of_sec 30.)
            transport
            start))
  in
  match login with
  | None ->
    ignore (note_seen t ~host adapter_id result : bool);
    result
  | Some credential ->
    Result.map_error result ~f:(catalog_login_error adapter_id credential)
;;

(* Asks, in the background, the catalog of a Codex, Claude Code or ACP
   session's harness on its tool host, on the login it runs on, unless one
   younger than [max_age] seconds is kept. *)
let ensure_catalog ?force ?(max_age = 1800.) t agent =
  match Agent.engine_choice agent with
  | { engine = Composer
    ; adapter = Some ((Codex | Claude | Acp _) as adapter_id)
    } ->
    (match Composer_adapters.find t.engines adapter_id with
     | Error _ -> ()
     | Ok adapter ->
       let host = Agent.active_host agent in
       let name = Adapter_id.to_string adapter_id in
       let login = catalog_login t ~adapter:name in
       let was_asking = Adapter_catalog.asking t.catalogs ~host ~login name in
       Adapter_catalog.refresh
         ?force
         t.catalogs
         ~sw:t.sw
         ~host
         ~login
         name
         ~max_age
         ~fetch:
           (fetch_catalog
              t
              agent
              ~host
              ~host_name:(active_host_label agent)
              ~cwd:(Session.cwd (Agent.session agent))
              ~login:(lazy (cli_credential t agent ~adapter:name))
              adapter_id
              adapter)
         ~on_done:(fun () ->
           catalog_changed t ~host ~login name;
           match adapter_id with
           | Acp _ when Option.is_none login ->
             adapters_changed t ~hosts:(hosts t)
           | Acp _ | Codex | Claude | Prigh -> ());
       if
         (not was_asking) && Adapter_catalog.asking t.catalogs ~host ~login name
       then send_harness_models t agent)
  | _ -> ()
;;

(* The session's harness's catalog, as asked on its tool host on its
   login. *)
let find_catalog t agent (a : Adapter_id.t) =
  let adapter = Adapter_id.to_string a in
  Adapter_catalog.find
    t.catalogs
    ~host:(Agent.active_host agent)
    ~login:(catalog_login t ~adapter)
    adapter
;;

(* The session's harness listed no models when asked last (Goose): it
   picks its own, and is not asked again. *)
let lists_no_models t agent =
  match Agent.engine_choice agent with
  | { engine = Composer; adapter = Some a } ->
    (match find_catalog t agent a with
     | Some { models = []; _ } -> true
     | Some _ | None -> false)
  | _ -> false
;;

(* The models of a session an external harness runs: its own, never
   prigh's, even before it runs here. *)
let external_models t agent =
  match Agent.external_models agent, Agent.engine_choice agent with
  | Some models, _ -> Some models
  | None, { engine = Composer; adapter = Some ((Codex | Claude | Acp _) as a) }
    ->
    Some
      (match find_catalog t agent a with
       | Some { models = _ :: _ as models; _ } -> models
       | Some { models = []; _ } ->
         [ External_model.unlisted ~adapter:(Adapter_id.to_string a) ]
       | None -> [ External_model.pending ~adapter:(Adapter_id.to_string a) ])
  | None, _ -> None
;;

(* A composer session's harness login's usage, as the harness says (asked
   when it has not said lately). *)
let harness_usage t agent : Subscription_usage.Harness_usage.t list =
  match Agent.engine_choice agent with
  | { engine = Composer; adapter = Some ((Codex | Claude) as adapter_id) } ->
    let host = Agent.active_host agent in
    let name = Adapter_id.to_string adapter_id in
    ensure_catalog ~max_age:60. t agent;
    if Option.is_none (Agent.external_limits agent)
    then
      Adapter_catalog.await
        t.catalogs
        ~host
        ~login:(catalog_login t ~adapter:name)
        name;
    let label = Adapter_id.label adapter_id in
    let status : Subscription_usage.Status.t =
      match Agent.external_limits agent with
      | Some status -> status
      | None ->
        Failed
          (match adapter_login t ~adapter:name with
           | None ->
             sprintf
               "%s on %s did not say: check that it is installed and logged in \
                there (Settings → Engines)"
               label
               (active_host_label agent)
           | Some (provider, ref_) ->
             sprintf
               "%s on %s did not say, on prigh's %s login %S: check that it is \
                installed there and that the login works (Settings → Engines)"
               label
               (active_host_label agent)
               (Provider_id.to_string provider)
               (login_label t provider ref_))
    in
    [ { adapter = name; name = label; status } ]
  | _ -> []
;;

(* Why [adapter]'s own sandbox cannot run on [host], found out once per host
   and binary: Codex runs without it there. Not for a harness [spawn_external]
   runs, which is not the host's. *)
let no_sandbox t agent ~host ~host_name ~cwd (adapter : Adapter_id.t) =
  let installed () =
    match Adapter_health.Cache.find t.adapter_health ~host adapter with
    | Some { install = Installed { path; _ }; _ } -> `Path path
    | Some { install = Not_installed _; _ } -> `None
    | Some { install = Unknown _; _ } | None -> `Unknown
  in
  match adapter, t.spawn_external with
  | (Claude | Prigh | Acp _), _ | Codex, Some _ -> None
  | Codex, None ->
    (match installed () with
     | `None -> None
     | (`Path _ | `Unknown) as installed ->
       (match
          Codex_sandbox.Cache.status
            t.codex_sandbox
            ~host
            ~installed:
              (match installed with
               | `Path path -> Some path
               | `Unknown -> None)
            ~exec:(fun ~name arguments ->
              Agent.host_call_on agent ~host ~cwd ~name arguments)
        with
        | Unavailable why -> Some (sprintf "%s (%s)" host_name why)
        | Works | Unknown _ -> None))
;;

(* A composer session's runner, driving its adapter's harness on the
   session's tool host and continuing the thread the session recorded there
   (each process records the threads it starts). *)
let composer_runner ?handoff_from t agent (adapter_id : Adapter_id.t) =
  match adapter_id with
  | Prigh ->
    Ok (Prigh_adapter.runner ~harness:(fun () -> Agent.harness_runner agent))
  | _ ->
    let%map.Or_error adapter = Composer_adapters.find t.engines adapter_id in
    let session = Agent.session agent in
    let adapter_name = Adapter_id.to_string adapter_id in
    let start ~cwd =
      let host = Agent.active_host agent in
      let (module A : Adapter.S) = adapter in
      let thread =
        List.find
          (Session.threads session)
          ~f:(fun (thread : Session.Thread.t) ->
            String.equal thread.adapter adapter_name
            && String.equal thread.host host)
      in
      (* A forked session continues a fork of its parent's thread, where the
         agent forks one. *)
      let resume, fork, at =
        match thread with
        | None -> None, false, None
        | Some { fork_from = None; thread_id; _ } -> Some thread_id, false, None
        | Some { fork_from = Some at; thread_id; _ } ->
          if A.fork_native
          then Some thread_id, true, Some at
          else None, false, None
      in
      { (Adapter.Start.default ~cwd) with
        resume
      ; fork
      ; at
      ; developer_instructions = Agent.user_instructions agent
      ; login = cli_credential t agent ~adapter:adapter_name
      ; no_sandbox =
          no_sandbox
            t
            agent
            ~host
            ~host_name:(active_host_label agent)
            ~cwd
            adapter_id
      }
    in
    let on_thread thread_id =
      ignore
        (Session.record_thread
           session
           { adapter = adapter_name
           ; host = Agent.active_host agent
           ; thread_id
           ; cwd = Session.cwd session
           ; fork_from = None
           }
         : Session.Entry.t)
    in
    Composer_runner.runner
      (Composer_runner.create
         ~sw:t.sw
         ~adapter
         ~spawn:(fun launch ->
           let spawned =
             match t.spawn_external with
             | Some spawn -> spawn agent ~host:(Agent.active_host agent) launch
             | None -> host_spawn t agent launch
           in
           if Result.is_error spawned then recheck_adapter t agent adapter_id;
           spawned)
         ~start
         ~on_thread
         ~on_start:(fun (start : Adapter.Start.t) result ->
           (* On one of prigh's logins, it shows nothing of its own. *)
           if
             Option.is_none start.login
             && note_seen t ~host:(Agent.active_host agent) adapter_id result
           then adapters_changed t ~hosts:(hosts t))
         ~checkpoint:(fun args ->
           Agent.host_call agent ~name:Checkpoint.op args)
         ~host:(fun () -> Agent.active_host agent)
         ?handoff_from
         ~catalog:(fun () -> find_catalog t agent adapter_id)
         ~on_change:(fun () -> models_changed t agent)
         ())
;;

(* Gives a composer session (that can run here) its runner. *)
let attach_engine t agent =
  match Agent.engine_choice agent, (Agent.state agent).unavailable with
  | { engine = Composer; adapter = Some adapter }, None
    when not (Engine_id.equal (Agent.engine agent) Composer) ->
    (match composer_runner t agent adapter with
     | Ok runner -> Agent.set_runner agent (Some runner)
     | Error e -> Agent.set_unavailable agent (Some (Error.to_string_hum e)))
  | _ -> ()
;;

(* Hosts are set before subscribing: the state change would otherwise evict
   the agent, which has no client until [attach]. *)
let register t agent =
  Agent.set_unavailable agent (engine_unavailable t agent);
  attach_engine t agent;
  Agent.set_hosts agent (hosts t) ~returning:(returning t);
  Hashtbl.set t.agents ~key:(session_id agent) ~data:agent;
  Hashtbl.set t.listed ~key:(session_id agent) ~data:(Listed.of_agent agent);
  Agent.subscribe agent ~f:(route t agent);
  Agent.on_noted agent ~f:(fun () -> write_ledger t agent);
  Agent.set_unattended agent (fun () ->
    if List.exists (clients_of t agent) ~f:(fun c -> not c.tools)
    then None
    else
      Some
        "no prigh frontend (prigh-web, the TUI, an editor) is connected \
         to          this session");
  Agent.report_interrupted_subagents agent;
  agent
;;

let memory_json memory =
  let%map.Or_error notes = Memory.notes memory in
  [ ("enabled", if Memory.enabled memory then `True else `False)
  ; "notes", `Array (List.map notes ~f:Memory.Note.to_json)
  ]
;;

(* Every client hears of every change, the model's saves included, so that
   open settings follow. *)
let memory_changed (t : t) memory =
  match memory_json memory with
  | Error _ -> ()
  | Ok fields ->
    let json =
      `Object
        ([ "type", `String "event"; "event", `String "memory_changed" ] @ fields)
    in
    broadcast t json
;;

let custom_instructions_json ci =
  let%map.Or_error notes = Custom_instructions.notes ci in
  let int n = `Number (Int.to_string n) in
  [ ("enabled", if Custom_instructions.enabled ci then `True else `False)
  ; "notes", `Array (List.map notes ~f:Custom_instructions.Note.to_json)
  ; ( "limits"
    , `Object
        [ "text", int Custom_instructions.max_text
        ; "title", int Custom_instructions.max_title
        ; "count", int Custom_instructions.max_notes
        ] )
  ]
;;

let custom_instructions_changed (t : t) ci =
  match custom_instructions_json ci with
  | Error _ -> ()
  | Ok fields ->
    let json =
      `Object
        ([ "type", `String "event"
         ; "event", `String "custom_instructions_changed"
         ]
         @ fields)
    in
    broadcast t json
;;

let user_info_json user_info =
  let%map.Or_error form = User_info.get user_info in
  let max key = key, `Number (Int.to_string (User_info.max_field key)) in
  [ "form", User_info.Form.to_json form
  ; "text", `String (Option.value (User_info.Form.render form) ~default:"")
  ; ( "limits"
    , `Object
        [ max "name"
        ; max "background"
        ; max "anything_else"
        ; "total", `Number (Int.to_string User_info.max_total)
        ] )
  ]
;;

let user_info_changed (t : t) user_info =
  match user_info_json user_info with
  | Error _ -> ()
  | Ok fields ->
    let json =
      `Object
        ([ "type", `String "event"; "event", `String "user_info_changed" ]
         @ fields)
    in
    broadcast t json
;;

let mcp_listing_json agent listing =
  Mcp_tools.Listing.to_rpc_json
    ~signed_in:(fun url -> Agent.mcp_signed_in agent ~url)
    listing
;;

let mcp_settings_json entries =
  [ "servers", `Array (List.map entries ~f:Mcp_settings.Entry.to_json) ]
;;

let mcp_settings_changed (t : t) mcp_settings =
  (match Mcp_settings.list mcp_settings with
   | Error _ -> ()
   | Ok entries ->
     let json =
       `Object
         ([ "type", `String "event"; "event", `String "mcp_settings_changed" ]
          @ mcp_settings_json entries)
     in
     broadcast t json);
  Hashtbl.iter t.agents ~f:Agent.mcp_settings_changed
;;

(* A sign-in came back or a token went: the hosts reconnect when the agents
   list again, and clients ask for the list again. *)
let mcp_auth_changed (t : t) =
  let json =
    `Object [ "type", `String "event"; "event", `String "mcp_auth_changed" ]
  in
  broadcast t json;
  Hashtbl.iter t.agents ~f:Agent.mcp_settings_changed
;;

let send_accounts_changed t =
  let json = Rpc_json.login_event Accounts_changed in
  Hashtbl.iter t.clients ~f:(fun (c : Client.t) -> c.send json)
;;

(* Another user shared or stopped sharing a login with this one: sessions
   pinned to one no longer shared go back to the default login. *)
let shares_changed t =
  send_accounts_changed t;
  Option.iter t.member ~f:(fun { shares; user } ->
    Hashtbl.iter t.agents ~f:(fun agent ->
      let state = Agent.state agent in
      match state.account with
      | Some (Shared { owner; id } as ref_)
        when not
               (Account_shares.granted
                  shares
                  ~recipient:user
                  state.model.provider
                  ~owner
                  id) ->
        Agent.set_model agent state.model;
        Agent.notice
          agent
          (sprintf
             "%s stopped sharing the %s login %s with you: carrying on with \
              your default login (/account picks another)"
             owner
             (Provider_id.display_name state.model.provider)
             (Account_ref.to_string ref_))
      | _ -> ()))
;;

let create
      ~env
      ~sw
      ?token
      ?(host_grace = 60.)
      ?(temporary_grace = 120.)
      ?sleep
      ?(host_timeout = 30.)
      ?heartbeat_clock
      ?namespace
      ?member
      ?ledger
      ?(backend_host = true)
      ?home
      ?forges
      ?memory
      ?user_info
      ?custom_instructions
      ?mcp_settings
      ?mcp_auth
      ?nix_store
      ?usage_http
      ~login
      ~sessions_dir
      ~cwd
      ?(engines = Engines.default)
      ?spawn_external
      ~new_agent
      ?default_agent
      ()
  =
  let forges =
    match forges with
    | Some forges -> forges
    | None ->
      Forges.create
        ~http:(Forge_http.real ~env)
        ~sleep:(Eio.Time.sleep (Eio.Stdenv.clock env))
        ~listen:(Forge_login.Listen.real ~env)
        ?home
        ~store:(Login_manager.store login)
        ()
  in
  let t =
    { login
    ; member
    ; ledger
    ; forges
    ; token
    ; namespace
    ; backend_host
    ; sessions_dir
    ; cwd
    ; new_agent
    ; default_agent
    ; engines
    ; adapter_health = Adapter_health.Cache.create ~ttl:60.
    ; login_seen = Adapter_health.Seen.Table.create ()
    ; codex_sandbox = Codex_sandbox.Cache.create ()
    ; catalogs =
        Adapter_catalog.create
          ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
          ()
    ; spawn_external
    ; catalog_keys = String.Table.create ()
    ; agents = String.Table.create ()
    ; clients = String.Table.create ()
    ; client_seq = 0
    ; btw_seq = 0
    ; generate_seq = 0
    ; execs = String.Table.create ()
    ; host_grace
    ; host_timeout
    ; heartbeat_clock =
        Option.value
          heartbeat_clock
          ~default:
            (Eio.Stdenv.clock env :> float Eio.Time.clock_ty Eio.Resource.t)
    ; returning = String.Table.create ()
    ; sleep =
        Option.value_or_thunk sleep ~default:(fun () ->
          Eio.Time.sleep (Eio.Stdenv.clock env))
    ; sw
    ; login_owner = None
    ; terminals = Terminal_relay.create ()
    ; loaded =
        (* A parsed session takes about as much memory as its file. *)
        File_cache.create ~max_bytes:(64 * 1024 * 1024) ()
    ; home
    ; prefs =
        Option.map home ~f:(fun home ->
          User_prefs.create ~path:(User_prefs.path ~home))
    ; memory =
        (match memory with
         | Some _ -> memory
         | None -> Option.map home ~f:(fun home -> Memory.create ~home))
    ; user_info =
        (match user_info with
         | Some _ -> user_info
         | None -> Option.map home ~f:(fun home -> User_info.create ~home))
    ; custom_instructions =
        (match custom_instructions with
         | Some _ -> custom_instructions
         | None ->
           Option.map home ~f:(fun home -> Custom_instructions.create ~home))
    ; mcp_settings =
        (match mcp_settings with
         | Some _ -> mcp_settings
         | None -> Option.map home ~f:(fun home -> Mcp_settings.create ~home))
    ; mcp_auth
    ; now = (fun () -> Eio.Time.now (Eio.Stdenv.clock env))
    ; listed = String.Table.create ()
    ; on_session_deleted = []
    ; loading = String.Table.create ()
    ; search =
        Session_search.create ~file:(Session_search.file ~dir:sessions_dir) ()
    ; served = Promise.create ()
    ; repos = String.Table.create ()
    ; nix_store
    ; usage_http =
        Option.value_or_thunk usage_http ~default:(fun () ->
          Subscription_usage.Http.real ~env)
    ; temporaries = String.Table.create ()
    ; temporary_grace
    ; ledger_dir = Temporary_ledger.dir ~sessions_dir
    }
  in
  Option.iter default_agent ~f:(fun agent ->
    ignore (register t agent : Agent.t));
  (* The flow itself (URL, prompts) belongs to the client that started it:
     other frontends, possibly on other machines, must not open a browser or
     pop up a dialog. Everyone hears the outcome, to refresh their status. *)
  Option.iter t.memory ~f:(fun memory ->
    Memory.subscribe memory ~f:(fun () -> memory_changed t memory));
  Option.iter t.user_info ~f:(fun user_info ->
    User_info.subscribe user_info ~f:(fun () -> user_info_changed t user_info));
  Option.iter t.custom_instructions ~f:(fun ci ->
    Custom_instructions.subscribe ci ~f:(fun () ->
      custom_instructions_changed t ci));
  Option.iter t.mcp_settings ~f:(fun mcp_settings ->
    Mcp_settings.subscribe mcp_settings ~f:(fun () ->
      mcp_settings_changed t mcp_settings));
  Option.iter t.mcp_auth ~f:(fun auth ->
    Mcp_auth.subscribe auth ~f:(fun () -> mcp_auth_changed t));
  Login_manager.subscribe login ~f:(fun event ->
    let json = Rpc_json.login_event event in
    match event with
    | Auth_url _ | Prompt _ | Prompt_cancelled _ | Progress _ ->
      Option.iter
        (Option.bind t.login_owner ~f:(Hashtbl.find t.clients))
        ~f:(fun c -> c.send json)
    | Done _
    | Failed _
    | Logged_out _
    | Accounts_changed
    | Forge_done _
    | Forge_logged_out _ -> broadcast t json);
  (* Recipients see a login's new label, or that it is gone. *)
  Login_manager.subscribe login ~f:(function
    | Done _ | Logged_out _ | Accounts_changed ->
      Option.iter t.member ~f:(fun { shares; user } ->
        Account_shares.changed shares ~owner:user)
    | _ -> ());
  Option.iter t.member ~f:(fun { shares; user } ->
    Account_shares.subscribe shares ~user ~f:(fun () -> shares_changed t));
  Model_registry.subscribe (Login_manager.models login) ~f:(fun problem ->
    iter_authed t ~f:(fun c -> tell_problems c [ problem ]));
  t
;;

(* The tool host on the machine of a client without tools, while it is
   connected. *)
let preferred_host t (client : Client.t) =
  if client.tools
  then None
  else Option.bind client.prefer_host ~f:(host_client t)
;;

let attach t (client : Client.t) agent =
  let previous = client.agent in
  Option.iter
    (Hashtbl.find t.temporaries (session_id agent))
    ~f:(fun temp -> temp.away <- None);
  if not (phys_equal previous agent)
  then (
    client.agent <- agent;
    if Agent.temporary previous && List.is_empty (clients_of t previous)
    then end_temporary t previous ~notify:[ client ]
    else maybe_evict t previous);
  publish_hosts t;
  if client.tools
  then Agent.prefer_host agent client.host_id
  else (
    Option.iter (preferred_host t client) ~f:(fun host ->
      Agent.prefer_host agent host.host_id);
    Agent.chat_without_host agent);
  Agent.prewarm agent
;;

(* A brand-new session made or opened for [client] starts on a host chosen
   from the client's context rather than the first host to connect: on the
   client itself when it is a tool host (in [cwd]), else where the client's
   session is (connected or not), in its cwd there. [None] (a client without
   tools on a session without a host): where any new session would be.
   Unpinned: the host was chosen by default. *)
let start_location t (client : Client.t) ~cwd =
  if client.tools
  then
    Some
      ( { Session.Host.id = client.host_id; name = client.name; pinned = false }
      , cwd )
  else (
    match preferred_host t client with
    | Some host ->
      let cwd =
        match
          List.find (Agent.hosts client.agent) ~f:(fun h ->
            String.equal h.id host.host_id)
        with
        | Some h -> h.cwd
        | None -> (host_of host).cwd
      in
      Some
        ( { Session.Host.id = host.host_id; name = host.name; pinned = false }
        , cwd )
    | None ->
      Option.map (Agent.location client.agent) ~f:(fun (host, cwd) ->
        { host with pinned = false }, cwd))
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

let start_new_agent t session start =
  let agent = start_agent t session start in
  Agent.start_in_default_mode agent;
  agent
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
    with_recorded_cwd
      session
      (start_location t client ~cwd:(host_of client).cwd)
;;

let fresh_agent t =
  let agent = register t (t.new_agent ~cwd:t.cwd ()) in
  Agent.start_in_default_mode agent;
  agent
;;

(* Sessions waiting for a usage limit to reset when the backend last ran are
   loaded again, so that they resume on time with nobody attached. *)
let restore_resumes ?yield t =
  let now = t.now () in
  List.iter (saved_sessions ?yield t) ~f:(fun summary ->
    match summary.resume_at with
    | Some at when Float.( > ) at now && not (Hashtbl.mem t.agents summary.id)
      ->
      (match load_session t summary.path with
       | Ok session ->
         (* Someone may have opened it while it was read. *)
         if not (Hashtbl.mem t.agents summary.id)
         then ignore (start_agent t session None : Agent.t)
       | Error error ->
         eprintf
           "prigh: session %s will not resume: %s\n%!"
           summary.id
           (Error.to_string_hum error))
    | Some _ | None -> ())
;;

let connect ?signed_in ?(close = fun _ -> ()) ?send_shared t ~send =
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
    ; kind = Other
    ; tools = false
    ; features = []
    ; computer = None
    ; cwd = None
    ; home = None
    ; prefer_host = None
    ; agent
    ; placeholder
    ; authed = Option.is_none t.token || Option.is_some signed_in
    ; signed_in
    ; send
    ; send_shared =
        Option.value send_shared ~default:(fun json ~line:_ -> send json)
    ; streams = Stream_sync.create ()
    ; close
    ; tab = None
    ; btws = String.Table.create ()
    ; generations = String.Table.create ()
    ; told = String.Set.empty
    ; heard = Eio.Time.now t.heartbeat_clock
    ; silent = false
    }
  in
  Hashtbl.set t.clients ~key:client.id ~data:client;
  client
;;

(* Fails the executions on [host] that [f] selects. *)
let fail_execs t ~host ~text ~f =
  Hashtbl.filteri t.execs ~f:(fun ~key ~data:(e : Exec.t) ->
    String.equal e.host host && f key e)
  |> Hashtbl.iteri ~f:(fun ~key:exec_id ~data:(e : Exec.t) ->
    Hashtbl.remove t.execs exec_id;
    ignore
      (Agent.tool_exec_result e.agent ~exec_id (Tool.Result.error text)
       : unit Or_error.t))
;;

(* Fails what is in flight on [client] as the host it is now. *)
let release_host_id t (client : Client.t) ~text =
  fail_execs t ~host:client.host_id ~text ~f:(fun _ _ -> true);
  Terminal_relay.host_gone t.terminals ~host:client.host_id
;;

(* A host whose connection dropped may still run what it was given (a TUI
   reconnecting keeps its tool host process): its executions wait
   [host_grace] for it to come back with them (see [claim_host_id]), and so
   do the sessions' new calls for it (see [Agent.set_hosts]). *)
let await_host t (client : Client.t) =
  let token = ref () in
  Hashtbl.iter t.execs ~f:(fun (e : Exec.t) ->
    match e.holder with
    | Client id when String.equal id client.id -> e.holder <- Away token
    | Client _ | Away _ -> ());
  Fiber.fork_daemon ~sw:t.sw (fun () ->
    t.sleep t.host_grace;
    Hashtbl.filter t.execs ~f:(fun (e : Exec.t) ->
      match e.holder with
      | Away token' -> phys_equal token token'
      | Client _ -> false)
    |> Hashtbl.iteri ~f:(fun ~key:exec_id ~data:(e : Exec.t) ->
      Hashtbl.remove t.execs exec_id;
      ignore
        (Agent.tool_exec_result
           e.agent
           ~exec_id
           (Tool.Result.error host_disconnected)
         : unit Or_error.t));
    `Stop_daemon)
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
      release_host_id t next ~text:host_disconnected;
      Hashtbl.remove t.returning gone.host_id;
      next.host_id <- gone.host_id;
      notice next (sprintf "this connection is tool host %S again" next.host_id))
;;

let disconnect t (client : Client.t) =
  Hashtbl.remove t.clients client.id;
  Hashtbl.iter client.btws ~f:Cancellation.cancel;
  Hashtbl.iter client.generations ~f:Cancellation.cancel;
  (* A host that went silent is not waited for: whatever it was given fails
     now, and so do new calls until it is back. *)
  if client.tools && client.silent
  then release_host_id t client ~text:(host_silent t client)
  else (
    if client.tools then await_host t client;
    if client.tools && not (String.equal client.host_id client.id)
    then (
      Hashtbl.set
        t.returning
        ~key:client.host_id
        ~data:(t.now () +. t.host_grace);
      Terminal_relay.host_gone t.terminals ~host:client.host_id)
    else release_host_id t client ~text:host_disconnected);
  hand_over_host_id t client;
  if client.tools then publish_hosts t;
  maybe_evict t client.agent
;;

(* Finishing runs evict idle sessions, so iterate over a snapshot. *)
let shutdown t =
  let agents = Hashtbl.data t.agents in
  List.iter agents ~f:Agent.shutdown;
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

(* Temporary sessions are not found: only [hello] with their key reaches
   them. *)
let find_live t key =
  (match Hashtbl.find t.agents key with
   | Some agent -> Some agent
   | None ->
     Hashtbl.data t.agents
     |> List.find ~f:(fun agent ->
       String.equal (Session.path (Agent.session agent)) key))
  |> Option.filter ~f:(fun agent -> not (Agent.temporary agent))
;;

let find_temporary t ~session ~key =
  match Hashtbl.find t.temporaries session, Hashtbl.find t.agents session with
  | Some temp, Some agent when String.equal temp.key key -> Ok agent
  | _ ->
    Or_error.errorf
      "temporary session %S is gone: it was deleted when it was left"
      session
;;

(* The file of a saved session, by path, id or a unique prefix of its id
   (what the frontends show, and session links may hold). *)
let saved_path t key =
  if Sys_unix.file_exists_exn key
  then session_file t key
  else
    Or_error.map
      (Session.find_summary (saved_sessions t) key)
      ~f:(fun s -> s.path)
;;

(* Finds a live session by id or path, or loads it from disk for [client]
   (taking it out of [t.loaded]: its agent owns it now). *)
let find_agent t (client : Client.t) key =
  match find_live t key with
  | Some agent -> Ok agent
  | None ->
    Or_error.bind (saved_path t key) ~f:(fun path ->
      let session =
        File_cache.find_or_read t.loaded path ~read:(load_session t)
      in
      (* Another client may have opened it while it was being read. *)
      match find_live t path with
      | Some agent -> Ok agent
      | None ->
        File_cache.remove t.loaded path;
        Or_error.map session ~f:(fun session ->
          start_agent t session (loaded_start t client session)))
;;

(* The live session with this id, a temporary one too, else the saved one,
   loaded with no client: it starts where it last was. *)
let wake_session t ~session =
  let agent =
    match Hashtbl.find t.agents session with
    | Some agent -> Ok agent
    | None ->
      (match
         List.find (saved_sessions t) ~f:(fun s -> String.equal s.id session)
       with
       | None -> Or_error.errorf "no session %S" session
       | Some summary ->
         let loaded =
           File_cache.find_or_read t.loaded summary.path ~read:(load_session t)
         in
         (* A client may have opened it while it was being read. *)
         (match Hashtbl.find t.agents session with
          | Some agent -> Ok agent
          | None ->
            File_cache.remove t.loaded summary.path;
            Or_error.map loaded ~f:(fun session -> start_agent t session None)))
  in
  Or_error.map agent ~f:Agent.check_mail
;;

let serve_questions t questions =
  Session_questions.set_wake questions (wake_session t)
;;

(* A session's messages to read: the live session's, else from disk without
   opening it (kept in [t.loaded] for when it is opened). *)
let session_to_read ?(client : Client.t option) t key =
  let own =
    Option.map client ~f:(fun (c : Client.t) -> c.agent)
    |> Option.filter ~f:(fun agent ->
      String.equal (session_id agent) key
      || String.equal (Session.path (Agent.session agent)) key)
  in
  match Option.first_some own (find_live t key) with
  | Some agent -> Ok (Agent.session agent)
  | None ->
    Or_error.bind (saved_path t key) ~f:(fun path ->
      File_cache.find_or_read t.loaded path ~read:(load_session t))
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
  let%bind from = string_param_opt params "from" in
  let%bind session =
    match%bind string_param_opt params "session" with
    | None -> Ok (Agent.session client.agent)
    | Some key -> session_to_read ~client t key
  in
  let%map page =
    Message_page.create
      ?from
      (Session.identified_messages session)
      ~limit
      ~before
  in
  Message_page.to_json page
;;

let read_session t (client : Client.t) params =
  match%bind.Or_error string_param_opt params "session" with
  | None -> Ok (Agent.session client.agent)
  | Some key -> session_to_read ~client t key
;;

let outline t client params =
  Or_error.map (read_session t client params) ~f:(fun session ->
    Outline.to_json (Outline.create (Session.identified_messages session)))
;;

let trim_points t client params =
  Or_error.map (read_session t client params) ~f:(fun session ->
    Trim_point.to_json (Trim_point.create (Session.identified_messages session)))
;;

let message_points t client params =
  Or_error.map (read_session t client params) ~f:(fun session ->
    Message_point.to_json
      (Message_point.create (Session.identified_messages session)))
;;

(* Without [entry], what the trims before a message dropped. *)
let trimmed t client params =
  let%bind.Or_error session = read_session t client params in
  let%map.Or_error entry = string_param_opt params "entry" in
  let messages =
    match entry with
    | None -> Session.trimmed_messages session
    | Some entry -> Session.hidden_messages session ~entry
  in
  `Object
    [ ( "messages"
      , `Array (List.map messages ~f:(fun (_, m) -> Message_page.message m)) )
    ]
;;

(* Searches the whole session file, so that outputs from before a
   compaction or on an abandoned branch are found too. *)
let tool_output t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind call_id = string_param params "call_id" in
  let%bind agent_id = string_param_opt params "agent_id" in
  let%bind key = string_param_opt params "session" in
  let%bind messages =
    match agent_id with
    | None ->
      let%map session =
        match key with
        | None -> Ok (Agent.session client.agent)
        | Some key -> session_to_read ~client t key
      in
      List.filter_map (Session.entries session) ~f:(fun (e : Session.Entry.t) ->
        match e.payload with
        | Message message -> Some { Timed_message.message; at = e.at }
        | _ -> None)
    | Some id ->
      let%bind agent =
        match key with
        | None -> Ok client.agent
        | Some key ->
          (match
             if String.equal key (session_id client.agent)
             then Some client.agent
             else find_live t key
           with
           | Some agent -> Ok agent
           | None ->
             Or_error.errorf
               "session %S is not open: switch to it to read its subagents' \
                outputs"
               key)
      in
      (match Agent.subagent agent id with
       | Some (_, messages) -> Ok messages
       | None ->
         Or_error.errorf
           "unknown subagent %S: list_subagents shows this session's"
           id)
  in
  match Message_page.find_output messages ~call_id with
  | Some r ->
    Ok
      (`Object
          [ "text", `String r.text
          ; ("is_error", if r.is_error then `True else `False)
          ])
  | None ->
    Or_error.errorf
      "no output of tool call %S in %s: pass the \"session\" (and, for a \
       subagent's call, \"agent_id\") it belongs to"
      call_id
      (match agent_id with
       | None -> "this session"
       | Some id -> sprintf "subagent %S" id)
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
    start_new_agent
      t
      (Session.create ~dir:t.sessions_dir ~cwd ())
      (start_location t client ~cwd)
  | Some _ | None -> client.agent
;;

let bool_param params name ~default =
  match param params name with
  | Some `True -> Ok true
  | Some `False -> Ok false
  | None -> Ok default
  | Some _ -> Or_error.errorf "param %S must be a boolean" name
;;

let required_bool_param params name =
  match param params name with
  | Some _ -> bool_param params name ~default:false
  | None -> Or_error.errorf "missing param %S" name
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
   another frontend on the same machine, so it is demoted to its client id
   until the newer one goes ([hand_over_host_id]); new calls go to the newer
   one. The host says which executions it still runs ([running]: a TUI that
   reconnects keeps its tool host process): they carry on over the new
   connection, and those nobody waits for any more are cancelled. Of the
   others on the id, those of a connection still open carry on there (its
   results are taken), unless the newcomer took some of that connection's
   over (so it replaces that connection, which the backend has not noticed
   is dead); the rest fail. *)
let claim_host_id t (client : Client.t) host_id ~running =
  if String.equal host_id client.host_id
  then Ok ()
  else if
    String.is_empty host_id
    || String.equal host_id Agent.Host.backend_id
    || String.is_prefix host_id ~prefix:"client-"
  then Or_error.errorf "param \"host_id\": %S is reserved" host_id
  else (
    release_host_id t client ~text:host_restarted;
    let replaced =
      Set.to_list running
      |> List.filter_map ~f:(fun exec_id ->
        match Hashtbl.find t.execs exec_id with
        | Some { holder = Client id; host; _ } when String.equal host host_id ->
          Some id
        | Some _ | None -> None)
      |> String.Set.of_list
    in
    let held_elsewhere (e : Exec.t) =
      match e.holder with
      | Client id -> Hashtbl.mem t.clients id && not (Set.mem replaced id)
      | Away _ -> false
    in
    Hashtbl.iter t.clients ~f:(fun (other : Client.t) ->
      if String.equal other.host_id host_id
      then (
        Terminal_relay.host_gone t.terminals ~host:host_id;
        other.host_id <- other.id;
        notice
          other
          (sprintf
             "a newer connection is tool host %S now; this one is %S until it \
              disconnects"
             host_id
             other.id)));
    fail_execs t ~host:host_id ~text:host_restarted ~f:(fun exec_id e ->
      (not (Set.mem running exec_id)) && not (held_elsewhere e));
    Set.iter running ~f:(fun exec_id ->
      Hashtbl.find t.execs exec_id
      |> Option.iter ~f:(fun (e : Exec.t) ->
        if String.equal e.host host_id then e.holder <- Client client.id));
    Hashtbl.remove t.returning host_id;
    Set.iter running ~f:(fun exec_id ->
      if
        not
          (Option.exists (Hashtbl.find t.execs exec_id) ~f:(fun (e : Exec.t) ->
             String.equal e.host host_id))
      then
        client.send
          (Rpc_json.event (Tool_exec_cancel { host = host_id; exec_id })));
    client.host_id <- host_id;
    client.wanted_host_id <- Some host_id;
    Ok ())
;;

(* A page that reconnects (its network changed, it was suspended) may leave
   its older connection open here: what that one still waits for is for
   nobody, so it ends. *)
let take_over_tab t (client : Client.t) tab =
  Hashtbl.data t.clients
  |> List.iter ~f:(fun (c : Client.t) ->
    if (not (phys_equal c client)) && Option.equal String.equal c.tab (Some tab)
    then (
      c.tab <- None;
      c.close "a newer connection from the same page took over"));
  client.tab <- Some tab
;;

(* What temporary sessions made that the backend did not live to delete
   (it stopped while they were open): deleted on the hosts that are here,
   the rest when their host connects. Worktrees with uncommitted changes
   stay, said on stderr. *)
let clean_leftovers t =
  let ledgers =
    Temporary_ledger.load_all ~dir:t.ledger_dir
    |> List.filter ~f:(fun (l : Temporary_ledger.t) ->
      not (Hashtbl.mem t.temporaries l.id))
  in
  if not (List.is_empty ledgers)
  then (
    let agent =
      t.new_agent
        ~session:
          (Session.create ~dir:t.sessions_dir ~cwd:t.cwd ~temporary:true ())
        ~cwd:t.cwd
        ()
    in
    Agent.set_hosts agent (hosts t) ~returning:String.Map.empty;
    List.iter ledgers ~f:(fun (ledger : Temporary_ledger.t) ->
      List.iter
        (remove_leftovers t agent ~id:ledger.id ledger.leftovers)
        ~f:(fun ((l : Leftover.t), why) ->
          eprintf
            "prigh: temporary session %s left %s: %s\n%!"
            ledger.id
            l.path
            why));
    Agent.shutdown agent)
;;

let composer_deployed t =
  List.mem t.engines.engines Composer ~equal:Engine_id.equal
;;

(* A host that (re)connects may have had its CLIs installed or logged in
   meanwhile: its statuses are checked again when next asked for. *)
let host_connected t (client : Client.t) =
  if composer_deployed t
  then (
    Adapter_health.Cache.forget_host t.adapter_health ~host:client.host_id;
    Codex_sandbox.Cache.forget_host t.codex_sandbox ~host:client.host_id;
    adapters_changed t ~hosts:(Agent.state client.agent).hosts)
;;

(* A backend deploying more than the harness says what, so that frontends
   offer the choice; one with the harness alone says nothing, as before
   engines. *)
let engine_features t =
  if Engines.equal t.engines Engines.default then [] else [ "engines" ]
;;

let engines_json t =
  if Engines.equal t.engines Engines.default
  then []
  else
    [ "engines", strings (Engines.engine_strings t.engines)
    ; "adapters", strings (Engines.adapter_strings t.engines)
    ]
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
    Option.iter (string "tab") ~f:(take_over_tab t client);
    Option.iter (param params "name") ~f:(function
      | `String name -> client.name <- name
      | _ -> ());
    Option.iter (param params "cwd") ~f:(function
      | `String cwd -> client.cwd <- Some cwd
      | _ -> ());
    client.home <- string "home";
    (* A second hello that does not say keeps what the first said. *)
    Option.iter (string "client") ~f:(fun kind ->
      client.kind <- Client_kind.of_hello (Some kind));
    Or_error.bind (bool_param params "tools" ~default:false) ~f:(fun tools ->
      let claimed =
        match string "host_id" with
        | Some host_id when tools ->
          Or_error.bind
            (string_list_param params "running_execs")
            ~f:(fun running ->
              claim_host_id
                t
                client
                host_id
                ~running:(String.Set.of_list running))
        | _ -> Ok ()
      in
      Or_error.bind claimed ~f:(fun () ->
        client.tools <- tools;
        client.prefer_host
        <- Option.filter (string "prefer_host") ~f:(fun _ -> not tools);
        client.features
        <- (match param params "features" with
            | Some (`Array items) ->
              List.filter_map items ~f:(function
                | `String feature -> Some feature
                | _ -> None)
            | _ -> []);
        client.computer
        <- (match param params "computer" with
            | Some json
              when tools
                   && List.mem
                        client.features
                        Computer_info.feature
                        ~equal:String.equal ->
              Result.ok (Computer_info.of_json json)
            | _ -> None);
        let agent =
          match param params "session", string "temporary_key" with
          | Some (`String session), Some key -> find_temporary t ~session ~key
          | Some (`String key), None -> find_agent t client key
          | _ -> Ok (first_session t client)
        in
        client.placeholder <- None;
        Or_error.map agent ~f:(fun agent ->
          attach t client agent;
          if tools
          then
            (* The frontends on its machine that came before it. *)
            Hashtbl.iter t.clients ~f:(fun (c : Client.t) ->
              if Option.equal String.equal c.prefer_host (Some client.host_id)
              then Agent.prefer_host c.agent client.host_id);
          if tools
          then
            Fiber.fork ~sw:t.sw (fun () ->
              Guarded.run ~what:"cleaning up temporary sessions" (fun () ->
                clean_leftovers t));
          if tools then host_connected t client;
          `Object
            ([ "client_id", `String client.id
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
             ; "features", strings (backend_features @ engine_features t)
             ; "state", state_json t (Agent.state agent)
             ]
             @ engines_json t)))))
;;

(* The namespace's file as it is now, which another session may have
   changed. *)
let current_config t agent =
  match Option.map t.home ~f:(fun home -> Config.load ~home) with
  | Some (Ok config) -> config
  | Some (Error _) | None -> Agent.config agent
;;

(* Every session hears of a change one of them saved, and tells its clients:
   open settings follow, and a later [set_config] from any of them starts
   from it. *)
let config_saved t agent =
  let config = Agent.config agent in
  Hashtbl.iter t.agents ~f:(fun other ->
    if not (phys_equal other agent) then Agent.adopt_config other config)
;;

(* What a client changing session needs first, saving it a [get_state]. *)
let attached_state t (client : Client.t) =
  `Object [ "state", state_json t (Agent.state client.agent) ]
;;

(* Moves the attached session to another of the composer's adapters (the
   new one's harness takes over at the next turn, handed what it has not
   seen of the session), or back to prigh's harness engine ([harness]),
   which reads the session itself. Not while a run goes on. *)
let set_adapter t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let agent = client.agent in
  let%bind name = string_param params "adapter" in
  let%bind choice =
    if String.equal name "harness"
    then Engines.choose t.engines ~engine:(Some "harness") ~adapter:None
    else Engines.choose t.engines ~engine:(Some "composer") ~adapter:(Some name)
  in
  let previous = Agent.engine_choice agent in
  if Engine_choice.equal choice previous
  then Ok (attached_state t client)
  else if Agent.is_running agent
  then
    Or_error.errorf
      "%s is still running: wait for the run to end (or abort it with Esc), \
       then switch to %s"
      (Agent.engine_label agent)
      (Option.value_map
         choice.adapter
         ~default:(Engine_choice.label choice)
         ~f:Adapter_id.label)
  else (
    let%bind () =
      match Engines.unavailable t.engines choice with
      | None -> Ok ()
      | Some why -> Error (Error.of_string why)
    in
    match choice with
    | { engine = Harness; _ } ->
      Agent.set_engine agent choice;
      Agent.set_unavailable agent None;
      Agent.set_runner agent None;
      models_changed t agent;
      Ok (attached_state t client)
    | { engine = Composer; _ } ->
      let%bind adapter =
        Result.of_option
          choice.adapter
          ~error:(Error.of_string "the composer needs an adapter")
      in
      let%map runner = composer_runner t agent adapter ~handoff_from:previous in
      Agent.set_engine agent choice;
      Agent.set_unavailable agent None;
      Agent.set_runner agent (Some runner);
      Agent.prewarm agent;
      models_changed t agent;
      attached_state t client)
;;

(* A temporary session is not saved or listed, and ends when its client
   leaves it ([attach]), or [temporary_grace] after the last one went. Its
   ledger exists from the start, so that a restart knows to clean up. *)
(* Where [new_session] starts a session. A tool host's sessions start on
   it, where it is; another client's on the config's [default_host] while
   that is connected, else as [start_location] says. On the default host
   (the backend's without one), in [default_cwd] when there is one. *)
let new_session_start t (client : Client.t) =
  let start = start_location t client ~cwd:(own_cwd client) in
  if client.tools
  then start
  else (
    let config = current_config t client.agent in
    let hosts = Agent.hosts client.agent in
    let find id =
      List.find hosts ~f:(fun (h : Agent.Host.t) -> String.equal h.id id)
    in
    let start =
      match Option.bind config.default_host ~f:find with
      | Some h ->
        Some ({ Session.Host.id = h.id; name = h.name; pinned = false }, h.cwd)
      | None -> start
    in
    let on = Option.value config.default_host ~default:Agent.Host.backend_id in
    let default_dir (h : Agent.Host.t) dir =
      if String.equal h.id Agent.Host.backend_id
      then (
        let dir = Tool.expand_home dir in
        match Sys_unix.is_directory dir with
        | `Yes -> Some dir
        | `No | `Unknown -> None)
      else if String.is_prefix dir ~prefix:"~"
      then Option.map h.home ~f:(fun home -> Tool.expand_home ~home dir)
      else Some dir
    in
    match start, config.default_cwd with
    | Some (host, cwd), Some dir when String.equal host.id on ->
      let cwd =
        Option.bind (find on) ~f:(fun h -> default_dir h dir)
        |> Option.value ~default:cwd
      in
      Some (host, cwd)
    | _ -> start)
;;

let new_session
      ?(engine = Engine_choice.harness)
      t
      (client : Client.t)
      ~temporary
  =
  let start = new_session_start t client in
  let cwd =
    match start with
    | Some (_, cwd) -> cwd
    | None -> (Agent.state client.agent).cwd
  in
  let session = Session.create ~dir:t.sessions_dir ~cwd ~temporary () in
  let key = Option.some_if temporary (Pkce.random_hex 16) in
  Option.iter key ~f:(fun key ->
    Hashtbl.set
      t.temporaries
      ~key:(Session.id session)
      ~data:{ Temporary.key; away = None; written = [] };
    try
      Temporary_ledger.write
        ~dir:t.ledger_dir
        { id = Session.id session; leftovers = [] }
    with
    | exn ->
      eprintf
        "prigh: the ledger of a temporary session: %s\n%!"
        (Exn.to_string exn));
  let agent = start_new_agent t session start in
  (* Attached first: recording the engine changes the state, and a session
     no client has is evicted when its state changes. *)
  attach t client agent;
  Agent.set_engine agent engine;
  Agent.set_unavailable agent (engine_unavailable t agent);
  attach_engine t agent;
  match attached_state t client, key with
  | `Object fields, Some key ->
    `Object (fields @ [ "temporary_key", `String key ])
  | state, _ -> state
;;

(* A session for [search_sessions]' index: the live one's from memory. *)
let load_for_search t (s : Session.Summary.t) =
  match
    Option.filter (Hashtbl.find t.agents s.id) ~f:(fun agent ->
      String.equal (Session.path (Agent.session agent)) s.path)
  with
  | Some agent -> Some (Agent.session agent)
  | None -> Result.ok (load_session t s.path)
;;

let index_sessions t =
  Session_search.index
    t.search
    ~sessions:(fun () -> saved_sessions ~yield:Fiber.yield t)
    ~load:(load_for_search t)
    ~yield:Fiber.yield
;;

let indexed_sessions t = Session_search.indexed t.search

let start_background t =
  Fiber.fork_daemon ~sw:t.sw (fun () ->
    (* The server serves first: a client starting up waits for none of
       this, which only starts once a request is answered (or after a
       quiet second), and yields after each session. *)
    Fiber.first (fun () -> Promise.await (fst t.served)) (fun () -> t.sleep 1.);
    Fiber.yield ();
    restore_resumes ~yield:Fiber.yield t;
    (match index_sessions t with
     | () -> ()
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn ->
       eprintf
         "prigh: indexing the sessions for search failed (searches read them \
          instead): %s\n\
          %!"
         (Exn.to_string exn));
    `Stop_daemon)
;;

let search_hits t ~query ~limit =
  Session_search.search
    t.search
    ~sessions:(saved_sessions t)
    ~load:(load_for_search t)
    ~query
    ~limit
;;

(* The sessions [list_sessions] lists, by what was said in them. *)
let search_sessions t params =
  let open Or_error.Let_syntax in
  let%bind query = string_param params "query" in
  let%map limit = int_param params "limit" ~default:50 in
  let hits = search_hits t ~query ~limit in
  `Object
    [ "query", `String query
    ; "hits", `Array (List.map hits ~f:Session_search.Hit.to_json)
    ]
;;

(* A saved session by its file (as the summaries give it) or its id. *)
let find_saved t key =
  Or_error.bind
    (if String.mem key '/' then session_file t key else Ok key)
    ~f:(fun key ->
      match
        List.find (saved_sessions t) ~f:(fun s ->
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

(* Why [client] may not delete [summary], if it may not. [leaving]: as if
   [client] had moved to another session first. *)
let refusal
      ?(leaving = false)
      t
      (client : Client.t)
      (summary : Session.Summary.t)
  =
  let name = display summary in
  let agent = Hashtbl.find t.agents summary.id in
  let others agent =
    List.filter (clients_of t agent) ~f:(fun c -> not (phys_equal c client))
  in
  if (not leaving) && String.equal (session_id client.agent) summary.id
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
    | Some agent when not (List.is_empty (others agent)) ->
      Some
        (sprintf
           "%s is open in %d other frontend(s): close it there first"
           name
           (List.length (others agent)))
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
(* Where a session kept checkpoints of its files (the tool hosts and
   directories of its composer threads), for them to go with it. *)
let checkpointed_places (summary : Session.Summary.t) =
  match Session.load summary.path with
  | Error _ -> []
  | Ok session ->
    if
      List.exists (Session.external_turns ~all:true session) ~f:(fun run ->
        Option.is_some run.after)
    then
      List.map (Session.threads session) ~f:(fun (thread : Session.Thread.t) ->
        thread.host, thread.cwd)
      |> List.dedup_and_sort ~compare:[%compare: string * string]
    else []
;;

(* Best effort, in the background: a host away keeps them (hidden refs,
   which git's gc keeps too). *)
let drop_checkpoints t (client : Client.t) id places =
  if not (List.is_empty places)
  then
    Fiber.fork ~sw:t.sw (fun () ->
      List.iter places ~f:(fun (host, cwd) ->
        ignore
          (Agent.host_call_on
             client.agent
             ~host
             ~cwd
             ~name:Checkpoint.op
             (`Object
                 [ "action", `String "drop"
                 ; "prefix", `String (Checkpoint.session_prefix ~session:id)
                 ])
           : Tool.Result.t)))
;;

let delete_saved ?leaving t client (summary : Session.Summary.t) =
  match refusal ?leaving t client summary with
  | Some refused -> Or_error.error_string refused
  | None ->
    let checkpointed = checkpointed_places summary in
    Or_error.try_with (fun () ->
      let bytes = Session.disk_usage summary.path in
      Session.delete_files summary.path;
      let agent = Hashtbl.find t.agents summary.id in
      Hashtbl.remove t.agents summary.id;
      Hashtbl.remove t.listed summary.id;
      Hashtbl.remove t.catalog_keys summary.id;
      Option.iter agent ~f:(fun agent ->
        if Agent.resume_pending agent
        then ignore (Agent.cancel_resume agent : unit Or_error.t);
        (* Its external harness, idle, would run on its host for good. *)
        Agent.stop_engine agent);
      forget_session t summary.id;
      drop_checkpoints t client summary.id checkpointed;
      { Deleted.path = summary.path; id = summary.id; bytes })
;;

module Switch_to = struct
  type t =
    | Session of string
    | New_session

  let of_params params =
    let open Or_error.Let_syntax in
    let%bind session = string_param_opt params "switch_to" in
    let%map new_ = bool_param params "switch_to_new" ~default:false in
    match session, new_ with
    | Some key, _ -> Some (Session key)
    | None, true -> Some New_session
    | None, false -> None
  ;;
end

(* Deleting the session [client] is in moves it to [switch_to] first, in
   the same request so that nothing comes between, unless the session would
   be kept anyway: then the client stays. The reply then has the new
   session's state, as [switch_session]'s does. *)
let leave_for_deletion t (client : Client.t) ~switch_to keys =
  let current = session_id client.agent in
  let ids =
    List.filter_map keys ~f:(fun key ->
      Result.ok (find_saved t key)
      |> Option.map ~f:(fun (s : Session.Summary.t) -> s.id, s))
  in
  match List.Assoc.find ids current ~equal:String.equal, switch_to with
  | None, _ | _, None -> Ok None
  | Some summary, Some (switch_to : Switch_to.t) ->
    if Option.is_some (refusal ~leaving:true t client summary)
    then Ok None
    else (
      match switch_to with
      | New_session -> Ok (Some (new_session t client ~temporary:false))
      | Session key ->
        Or_error.bind (find_agent t client key) ~f:(fun target ->
          if List.Assoc.mem ids (session_id target) ~equal:String.equal
          then
            Or_error.error_string
              "switch_to is among the sessions to delete: name another one"
          else (
            attach t client target;
            Ok (Some (attached_state t client)))))
;;

(* One [sessions_changed] for the lot. *)
let delete_many ?switch_to t client keys =
  Or_error.map (leave_for_deletion t client ~switch_to keys) ~f:(fun state ->
    let results =
      List.map keys ~f:(fun key ->
        ( key
        , Or_error.bind (find_saved t key) ~f:(fun summary ->
            delete_saved ~leaving:(Option.is_some switch_to) t client summary) ))
    in
    let deleted = List.filter_map results ~f:(fun (_, r) -> Result.ok r) in
    if not (List.is_empty deleted)
    then
      sessions_changed
        t
        ~removed:(List.map deleted ~f:(fun (d : Deleted.t) -> d.id));
    let state =
      match state with
      | Some (`Object fields) -> fields
      | Some _ | None -> []
    in
    `Object
      (state
       @ [ "deleted", `Array (List.map deleted ~f:Deleted.to_json)
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
         ]))
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
        sessions_changed t ~changed:[ session ])))
;;

let set_pinned t ~key ~pinned =
  Or_error.bind (find_saved t key) ~f:(fun summary ->
    let session =
      match Hashtbl.find t.agents summary.id with
      | Some agent -> Ok (Agent.session agent)
      | None -> Session.load summary.path
    in
    Or_error.map session ~f:(fun session ->
      ignore (Session.set_pinned session ~pinned : Session.Entry.t);
      sessions_changed t ~changed:[ session ]))
;;

let exec_param t params =
  Or_error.bind (string_param params "exec_id") ~f:(fun exec_id ->
    match Hashtbl.find t.execs exec_id with
    | Some e -> Ok (exec_id, e.agent)
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

(* Like [btw]: the draft streams to the asking client only. *)
let generate_text t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind kind =
    Or_error.bind (string_param params "kind") ~f:Generate_text.Kind.of_string
  in
  let%bind gen_id = string_param_opt params "gen_id" in
  let%bind dir = string_param_opt params "dir" in
  let%bind base = string_param_opt params "base" in
  let%bind task = string_param_opt params "task" in
  let blank =
    Option.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
  in
  let gen_id =
    match gen_id with
    | Some id -> id
    | None ->
      t.generate_seq <- t.generate_seq + 1;
      sprintf "gen-%d" t.generate_seq
  in
  if Hashtbl.mem client.generations gen_id
  then Or_error.errorf "generation %S is already running" gen_id
  else (
    let cancel = Cancellation.create () in
    Hashtbl.set client.generations ~key:gen_id ~data:cancel;
    let%map result =
      Exn.protect
        ~finally:(fun () -> Hashtbl.remove client.generations gen_id)
        ~f:(fun () ->
          Generate_text.run
            client.agent
            kind
            { dir = blank dir; base = blank base; task = blank task }
            ~cancel
            ~on_delta:(fun delta ->
              client.send
                (`Object
                    [ "type", `String "event"
                    ; "event", `String "generate_delta"
                    ; "gen_id", `String gen_id
                    ; "delta", `String delta
                    ])))
    in
    match Generate_text.Result.to_json result with
    | `Object fields ->
      `Object
        (("gen_id", `String gen_id)
         :: ("kind", `String (Generate_text.Kind.to_string kind))
         :: fields)
    | json -> json)
;;

let switch_user (client : Client.t) params =
  match client.signed_in with
  | None -> Or_error.error_string User_access.no_users
  | Some signed_in ->
    Or_error.bind (string_param params "user") ~f:(fun user ->
      User_access.Signed_in.switch signed_in user)
;;

let number_param params name =
  match param params name with
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some n -> Ok n
     | None -> Or_error.errorf "param %S must be a whole number" name)
  | Some _ -> Or_error.errorf "param %S must be a number" name
  | None -> Or_error.errorf "missing param %S" name
;;

(* The repository is the client's session's, on its active tool host. *)
let session_git (client : Client.t) params =
  let%map.Or_error dir = string_param_opt params "dir" in
  let dir = Option.filter dir ~f:(fun d -> not (String.is_empty d)) in
  fun request ->
    Agent.host_call
      client.agent
      ~name:Git_forge.op
      (Git_forge.in_dir dir (Git_forge.Request.to_json request))
;;

let pull_requests t (client : Client.t) ~meth ~params =
  let open Or_error.Let_syntax in
  let%bind git = session_git client params in
  match meth with
  | "pr_list" ->
    let%bind state = string_param_opt params "state" in
    Forges.list_pulls t.forges ~git ~state:(Option.value state ~default:"open")
    >>| Forges.Pulls.to_json
  | "pr_view" ->
    let%bind number = number_param params "number" in
    Forges.view_pull t.forges ~git number >>| Forges.Pull.to_json
  | "pr_defaults" ->
    Forges.pull_defaults t.forges ~git >>| Forges.Defaults.to_json
  | _ ->
    let%bind title = string_param params "title" in
    let%bind body = string_param_opt params "body" in
    let%bind base = string_param_opt params "base" in
    let%bind head = string_param_opt params "head" in
    let%bind draft = bool_param params "draft" ~default:false in
    let%bind push = bool_param params "push" ~default:false in
    let blank =
      Option.filter ~f:(fun s -> not (String.is_empty (String.strip s)))
    in
    Forges.create_pull
      t.forges
      ~git
      ~title
      ~body:(Option.value body ~default:"")
      ~base:(blank base)
      ~head:(blank head)
      ~draft
      ~push
    >>| Forges.Pull.to_json
;;

let request_params request =
  Option.value (param request "params") ~default:(`Object [])
;;

(* ---- worktrees ------------------------------------------------------ *)

(* The sessions working in [worktree] on [host]: live ones where they are now,
   saved ones by their last recorded cwd (a session that recorded no host
   counts on any). *)
let worktree_sessions t client ~host (worktree : Worktree.t) =
  List.filter_map (listings t client) ~f:(fun (l : Listing.t) ->
    let on_host, cwd =
      match Option.bind l.agent ~f:Agent.location with
      | Some (h, cwd) -> Some h.id, cwd
      | None -> l.summary.host, l.summary.cwd
    in
    Option.some_if
      (Worktree.contains worktree ~dir:cwd
       && Option.value_map on_host ~default:true ~f:(String.equal host)
       && ((not l.archived) || Option.is_some l.agent))
      (`Object
          [ "id", `String l.summary.id
          ; "path", `String l.summary.path
          ; ( "name"
            , Option.value_map l.summary.name ~default:`Null ~f:(fun n ->
                `String n) )
          ; ("live", if Option.is_some l.agent then `True else `False)
          ; ("running", if l.active then `True else `False)
          ]))
;;

let worktree_json t (client : Client.t) (worktree : Worktree.t) =
  let agent = client.agent in
  let extra =
    [ ( "current"
      , if Worktree.contains worktree ~dir:(Agent.state agent).cwd
        then `True
        else `False )
    ; ( "sessions"
      , `Array
          (worktree_sessions t client ~host:(Agent.active_host agent) worktree)
      )
    ]
  in
  match [%jsonaf_of: Worktree.t] worktree with
  | `Object fields -> `Object (fields @ extra)
  | other -> other
;;

let list_worktrees t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind cwd = string_param_opt params "cwd" in
  Or_error.map
    (Agent.worktrees ?cwd client.agent)
    ~f:(fun (listing : Worktree.Listing.t) ->
      `Object
        [ "root", `String listing.root
        ; "project_dir", `String listing.project_dir
        ; ( "worktrees"
          , `Array (List.map listing.worktrees ~f:(worktree_json t client)) )
        ])
;;

(* The client's session records a worktree that matters to it. *)
let record_worktree t (client : Client.t) (w : Session_worktree.t) =
  let session = Agent.session client.agent in
  ignore (Session.add_worktree session w : Session.Entry.t);
  sessions_changed t ~changed:[ session ]
;;

let create_worktree t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind branch = string_param params "branch" in
  let%bind base = string_param_opt params "base" in
  let%bind cwd = string_param_opt params "cwd" in
  let%map created = Agent.create_worktree ?cwd client.agent ~branch ~base in
  record_worktree
    t
    client
    { path = created.worktree.path
    ; branch = created.worktree.branch
    ; root = created.root
    ; source = Created
    };
  `Object
    [ "worktree", worktree_json t client created.worktree
    ; ("new_branch", if created.new_branch then `True else `False)
    ; "root", `String created.root
    ; "cwd", `String created.cwd
    ]
;;

(* [/pr checkout]: the worktree is recorded in the repository the session
   works in. *)
let checkout_pull t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind git = session_git client params in
  let%bind number = number_param params "number" in
  let%map checkout =
    Forges.checkout_pull
      t.forges
      ~git
      ~worktrees_dir:(Agent.config client.agent).worktrees_dir
      number
  in
  let root =
    match Session.repo (Agent.session client.agent) with
    | Some (Some l) -> l.root
    | Some None | None -> (Agent.state client.agent).cwd
  in
  record_worktree
    t
    client
    { path = checkout.path
    ; branch = Some checkout.branch
    ; root
    ; source = Pull_request { number }
    };
  [%jsonaf_of: Git_forge.Checkout.t] checkout
;;

(* The sessions (saved or live) whose worktrees include [path] forget it. *)
let forget_worktree t client ~path =
  let changed =
    List.filter (listings t client) ~f:(fun (l : Listing.t) ->
      List.exists l.summary.worktrees ~f:(fun w -> String.equal w.path path))
    |> List.filter_map ~f:(fun (l : Listing.t) ->
      let session =
        match l.agent with
        | Some agent -> Ok (Agent.session agent)
        | None -> session_to_read t l.summary.path
      in
      Result.ok session
      |> Option.map ~f:(fun session ->
        ignore (Session.worktree_removed session ~path : Session.Entry.t);
        session))
  in
  if not (List.is_empty changed) then sessions_changed t ~changed
;;

let remove_worktree t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind path = string_param params "path" in
  let%bind force = bool_param params "force" ~default:false in
  let%bind repo_cwd = string_param_opt params "cwd" in
  let cwd = (Agent.state client.agent).cwd in
  let home =
    if String.is_prefix path ~prefix:"~"
    then Agent.active_host_home client.agent
    else None
  in
  let target = Tool.resolve ?home ~cwd path in
  let%bind () =
    if String.equal cwd target || String.is_prefix cwd ~prefix:(target ^ "/")
    then
      Or_error.errorf
        "this session works in %s: /cd out of it (e.g. to the main worktree) \
         first"
        target
    else Ok ()
  in
  let recorded =
    List.find_map (listings t client) ~f:(fun (l : Listing.t) ->
      List.find l.summary.worktrees ~f:(fun w -> String.equal w.path target))
  in
  match
    Agent.remove_worktree ?cwd:repo_cwd client.agent ~path ~force, recorded
  with
  | Ok removed, _ ->
    forget_worktree t client ~path:removed.path;
    Ok (`Object [ "worktree", worktree_json t client removed ])
  | Error e, Some w
    when String.is_substring
           (Error.to_string_hum e)
           ~substring:"not a worktree of" ->
    (* Removed outside prigh: only the record of it is left. *)
    forget_worktree t client ~path:w.path;
    Ok
      (`Object
          [ ( "worktree"
            , worktree_json
                t
                client
                { Worktree.path = w.path
                ; branch = w.branch
                ; head = None
                ; main = false
                ; managed = false
                ; locked = false
                ; prunable = true
                ; changes = None
                ; upstream = None
                ; ahead = 0
                ; behind = 0
                } )
          ])
  | (Error _ as error), _ -> error
;;

(* The other clients of the server: the user's other browsers and terminals
   follow at once. *)
let prefs_changed t (from : Client.t) changes =
  if not (List.is_empty changes)
  then (
    let json =
      `Object
        [ "type", `String "event"
        ; "event", `String "prefs_changed"
        ; "prefs", User_prefs.Change.to_json changes
        ]
    in
    iter_authed t ~f:(fun (c : Client.t) ->
      if not (String.equal c.id from.id) then c.send json))
;;

let prefs t client ~meth ~params =
  match t.prefs with
  | None -> Or_error.error_string "this backend keeps no preferences"
  | Some prefs ->
    let open Or_error.Let_syntax in
    (match meth with
     | "get_prefs" ->
       let%bind keys =
         match param params "keys" with
         | None | Some `Null -> Ok None
         | Some _ ->
           Or_error.map (string_list_param params "keys") ~f:Option.some
       in
       let%map fields = User_prefs.get prefs ?keys in
       `Object [ "prefs", `Object fields ]
     | "set_prefs" ->
       let%bind changes =
         match param params "prefs" with
         | Some (`Object fields) ->
           Ok
             (List.map fields ~f:(fun (key, value) ->
                ( key
                , match value with
                  | `Null -> None
                  | value -> Some value )))
         | _ ->
           Or_error.error_string
             "param \"prefs\" must be an object of keys to values (null \
              removes one)"
       in
       let%map changed = User_prefs.set prefs changes in
       prefs_changed t client changed;
       `Object [ "prefs", User_prefs.Change.to_json changed ]
     | _ ->
       let%bind key = string_param params "key" in
       let%bind value =
         match param params "value" with
         | Some value -> Ok value
         | None -> Or_error.error_string "missing param \"value\""
       in
       let%bind max = int_param params "max" ~default:500 in
       let%map list =
         User_prefs.push prefs ~key value ~max:(Int.min max 1000)
       in
       prefs_changed t client [ key, Some list ];
       `Object [ "value", list ])
;;

let memory t ~meth ~params =
  match t.memory with
  | None -> Or_error.error_string "this backend keeps no memory"
  | Some memory ->
    let open Or_error.Let_syntax in
    let now () =
      Time_float.of_span_since_epoch (Time_float.Span.of_sec (t.now ()))
    in
    (match meth with
     | "get_memory" ->
       let%map fields = memory_json memory in
       `Object fields
     | "save_memory" ->
       let%bind id = string_param_opt params "id" in
       let%bind title = string_param_opt params "title" in
       let%bind text = string_param params "text" in
       let%map note =
         match id with
         | Some id -> Memory.edit memory ~id ?title ~now:(now ()) text
         | None ->
           Or_error.map
             (Memory.save memory ?title ~now:(now ()) text)
             ~f:(function Added note | Replaced note -> note)
       in
       `Object [ "note", Memory.Note.to_json note ]
     | "forget_memory" ->
       let%bind id = string_param params "id" in
       let%map note = Memory.forget memory id in
       `Object [ "note", Memory.Note.to_json note ]
     | _ ->
       let%bind enabled =
         match param params "enabled" with
         | Some `True -> Ok true
         | Some `False -> Ok false
         | _ -> Or_error.error_string "param \"enabled\" must be a boolean"
       in
       let%map () = Memory.set_enabled memory enabled in
       `Object [ ("enabled", if enabled then `True else `False) ])
;;

let user_info t ~meth ~params =
  match t.user_info with
  | None -> Or_error.error_string "this backend keeps no user info"
  | Some user_info ->
    let open Or_error.Let_syntax in
    let%bind () =
      match meth with
      | "set_user_info" ->
        (match param params "form", string_param_opt params "field" with
         | Some form, _ ->
           let%bind form = User_info.Form.of_json form in
           User_info.set user_info form
         | None, Ok (Some field) ->
           let%bind value = string_param params "value" in
           let%bind form = User_info.get user_info in
           let%bind form = User_info.Form.set_field form field value in
           User_info.set user_info form
         | None, Error e -> Error e
         | None, Ok None ->
           Or_error.error_string
             "set_user_info needs a form, or a field and its value")
      | _ -> Ok ()
    in
    let%map fields = user_info_json user_info in
    `Object fields
;;

let custom_instructions t ~meth ~params =
  match t.custom_instructions with
  | None -> Or_error.error_string "this backend keeps no custom instructions"
  | Some ci ->
    let open Or_error.Let_syntax in
    let now () =
      Time_float.of_span_since_epoch (Time_float.Span.of_sec (t.now ()))
    in
    let note n = `Object [ "note", Custom_instructions.Note.to_json n ] in
    (match meth with
     | "get_custom_instructions" ->
       let%map fields = custom_instructions_json ci in
       `Object fields
     | "save_custom_instruction" ->
       let%bind id = string_param_opt params "id" in
       let%bind title = string_param_opt params "title" in
       let%bind text = string_param params "text" in
       let%map n =
         match id with
         | Some id ->
           Custom_instructions.edit
             ci
             ~id
             ~title:(Option.value title ~default:"")
             ~text
             ~now:(now ())
             ()
         | None -> Custom_instructions.add ci ?title ~now:(now ()) text
       in
       note n
     | "remove_custom_instruction" ->
       let%bind id = string_param params "id" in
       let%map n = Custom_instructions.remove ci id in
       note n
     | _ ->
       let%bind enabled =
         match param params "enabled" with
         | Some `True -> Ok true
         | Some `False -> Ok false
         | _ -> Or_error.error_string "param \"enabled\" must be a boolean"
       in
       let%map () = Custom_instructions.set_enabled ci enabled in
       `Object [ ("enabled", if enabled then `True else `False) ])
;;

let mcp_settings t ~meth ~params =
  match t.mcp_settings with
  | None ->
    Or_error.error_string "this backend keeps no MCP servers in settings"
  | Some settings ->
    let open Or_error.Let_syntax in
    let%map entries =
      match meth with
      | "add_mcp_servers" ->
        (match param params "servers", param params "text" with
         | Some (`Object servers), _ -> Mcp_settings.add settings servers
         | None, Some (`String text) ->
           Or_error.bind
             (Mcp_settings.parse text)
             ~f:(Mcp_settings.add settings)
         | _ ->
           Or_error.error_string
             "give \"servers\" (an mcpServers object) or \"text\" (pasted JSON)")
      | "update_mcp_server" ->
        let%bind name = string_param params "name" in
        let%bind rename = string_param_opt params "rename" in
        (match param params "config" with
         | Some config -> Mcp_settings.update settings ~name ?rename config
         | None -> Or_error.error_string "missing param \"config\"")
      | "remove_mcp_server" ->
        let%bind name = string_param params "name" in
        Mcp_settings.remove settings ~name
      | "set_mcp_server_enabled" ->
        let%bind name = string_param params "name" in
        let%bind enabled =
          match param params "enabled" with
          | Some `True -> Ok true
          | Some `False -> Ok false
          | _ -> Or_error.error_string "param \"enabled\" must be a boolean"
        in
        Mcp_settings.set_enabled settings ~name enabled
      | _ -> Mcp_settings.list settings
    in
    `Object (mcp_settings_json entries)
;;

let keep_session t (client : Client.t) =
  let agent = client.agent in
  if not (Agent.temporary agent)
  then
    Or_error.error_string "this session is not temporary: it is saved already"
  else (
    let id = session_id agent in
    Hashtbl.remove t.temporaries id;
    Temporary_ledger.remove ~dir:t.ledger_dir ~id;
    Agent.keep agent;
    Hashtbl.set t.listed ~key:id ~data:(Listed.of_agent agent);
    sessions_changed t ~changed:[ Agent.session agent ];
    Ok (attached_state t client))
;;

let set_config t agent params =
  match param params "config" with
  | None -> Or_error.error_string "missing param \"config\""
  | Some json ->
    (* Fields left out keep their values, so a frontend that does not know a
       field cannot reset it. *)
    let json =
      match Config.to_json (current_config t agent), json with
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
    let open Or_error.Let_syntax in
    let%bind config = Config.of_json json in
    let%bind defaults_only = bool_param params "defaults_only" ~default:false in
    let%map () =
      Agent.set_config ~session_mode:(not defaults_only) agent config
    in
    config_saved t agent;
    Config.to_json (Agent.config agent)
;;

(* Every deployed adapter on every connected host, the session's host
   first; statuses older than the cache's ttl (all of them with [refresh])
   are probed again, the hosts in parallel. *)
(* Which login each adapter that can take one of prigh's runs on, and the
   logins it could. *)
let adapter_logins_json t =
  `Array
    (List.filter_map Cli_login.adapters ~f:(fun adapter ->
       Option.map (Cli_login.provider ~adapter) ~f:(fun provider ->
         let choices =
           Login_choice.list
             ?member:t.member
             (Login_manager.store t.login)
             provider
           |> Result.ok
           |> Option.value ~default:[]
         in
         let login = adapter_login t ~adapter in
         `Object
           [ "adapter", `String adapter
           ; "provider", `String (Provider_id.to_string provider)
           ; ( "login"
             , Option.value_map login ~default:`Null ~f:(fun (_, r) ->
                 `String (Account_ref.to_string r)) )
           ; ( "label"
             , Option.value_map login ~default:`Null ~f:(fun (p, r) ->
                 `String (login_label t p r)) )
           ; ( "choices"
             , `Array
                 (List.map choices ~f:(fun (c : Login_choice.t) ->
                    `Object
                      [ "ref", `String (Account_ref.to_string c.ref_)
                      ; "label", `String c.label
                      ])) )
           ])))
;;

(* [login]: a login of prigh's (ref, id or label), or ["own"] / null for the
   CLI's own. The sessions on it show it, and take it at their next turn. *)
let set_adapter_login t params =
  let open Or_error.Let_syntax in
  let%bind adapter = string_param params "adapter" in
  let%bind login = string_param_opt params "login" in
  let%bind home =
    match t.home with
    | Some home -> Ok home
    | None -> Or_error.error_string "this backend keeps no settings"
  in
  let%bind provider =
    match Cli_login.provider ~adapter with
    | Some provider -> Ok provider
    | None ->
      Or_error.errorf
        "%s runs only on its own login (prigh can hand one of its logins to: \
         %s)"
        adapter
        (String.concat ~sep:", " Cli_login.adapters)
  in
  let%bind choice =
    match login with
    | None | Some "own" -> Ok Cli_login.Own
    | Some login ->
      let%map found =
        Login_choice.find
          ?member:t.member
          (Login_manager.store t.login)
          provider
          login
      in
      Cli_login.Prigh found.ref_
  in
  let%map () = Cli_login.save ~home ~adapter choice in
  Hashtbl.iter t.agents ~f:(fun agent ->
    match Agent.engine_choice agent with
    | { engine = Composer; adapter = Some a }
      when String.equal (Adapter_id.to_string a) adapter ->
      route t agent (State_changed (Agent.state agent))
    | _ -> ());
  `Object [ "adapter_logins", adapter_logins_json t ]
;;

(* Checks, in the background, the logins of the ACP agents on [host] that
   nothing has shown ({!Adapter_health.Status.checkable}) by asking their
   model lists, which starts them briefly on their own logins ([force], on
   Refresh: even when asking failed lately, and those found logged out, which
   may have logged in since). The adapters checked, with the host's id. *)
let check_logins t agent ~force (host : Agent.Host.t) =
  List.filter_map t.engines.adapters ~f:(fun adapter_id ->
    let checkable =
      Option.exists
        (Adapter_health.Cache.find t.adapter_health ~host:host.id adapter_id)
        ~f:(fun s ->
          let s = with_evidence t s in
          Adapter_health.Status.checkable s
          ||
          match force, s.adapter, s.install, s.auth with
          | true, Acp _, Installed _, Logged_out -> true
          | _ -> false)
    in
    match checkable, Composer_adapters.find t.engines adapter_id with
    | true, Ok adapter ->
      let name = Adapter_id.to_string adapter_id in
      Adapter_catalog.refresh
        ~force
        t.catalogs
        ~sw:t.sw
        ~host:host.id
        ~login:None
        name
        ~max_age:0.
        ~fetch:
          (fetch_catalog
             t
             agent
             ~host:host.id
             ~host_name:host.name
             ~cwd:host.cwd
             ~login:(lazy None)
             adapter_id
             adapter)
        ~on_done:(fun () ->
          catalog_changed t ~host:host.id ~login:None name;
          adapters_changed t ~hosts:(hosts t));
      Some (host.id, name)
    | _ -> None)
;;

let list_adapters t (client : Client.t) params =
  let open Or_error.Let_syntax in
  let%bind () =
    if composer_deployed t
    then Ok ()
    else
      Or_error.error_string
        "this backend does not run the composer engine, so it drives no agent \
         CLIs: start one with prigh serve -engines composer"
  in
  let%map refresh = bool_param params "refresh" ~default:false in
  let state = Agent.state client.agent in
  let hosts =
    let active, rest =
      List.partition_tf state.hosts ~f:(fun (h : Agent.Host.t) ->
        String.equal h.id state.active_host)
    in
    active @ rest
  in
  let cache = t.adapter_health in
  let now = t.now () in
  let changed =
    Fiber.List.map
      (fun (host : Agent.Host.t) ->
         let stale =
           List.filter t.engines.adapters ~f:(fun adapter ->
             refresh
             || not
                  (Adapter_health.Cache.fresh cache ~now ~host:host.id adapter))
         in
         if List.is_empty stale
         then false
         else (
           let statuses =
             Adapter_health.probe
               ~acp:(Engines.acp_agent t.engines)
               ~exec:(fun ~name arguments ->
                 Agent.host_call_on
                   client.agent
                   ~host:host.id
                   ~cwd:host.cwd
                   ~name
                   arguments)
               stale
               ~host:host.id
               ~host_name:host.name
               ~now
           in
           let differs (s : Adapter_health.Status.t) =
             match Adapter_health.Cache.find cache ~host:s.host s.adapter with
             | None -> true
             | Some old ->
               not
                 (Adapter_health.Status.equal
                    { old with checked_at = 0. }
                    { s with checked_at = 0. })
           in
           let changed = List.exists statuses ~f:differs in
           Adapter_health.Cache.set cache statuses;
           (* Settings → Engines says when Codex's sandbox cannot run. *)
           if
             List.exists statuses ~f:(fun s ->
               match s.adapter, s.install with
               | Codex, Installed _ -> true
               | _ -> false)
           then
             ignore
               (no_sandbox
                  t
                  client.agent
                  ~host:host.id
                  ~host_name:host.name
                  ~cwd:host.cwd
                  Codex
                : string option);
           changed))
      hosts
    |> List.exists ~f:Fn.id
  in
  if changed then adapters_changed t ~hosts:state.hosts;
  let checks =
    List.concat_map hosts ~f:(check_logins t client.agent ~force:refresh)
  in
  if refresh
  then
    List.iter checks ~f:(fun (host, name) ->
      Adapter_catalog.await t.catalogs ~host ~login:None name);
  `Object
    [ "adapters", adapters_json t (adapter_statuses t hosts)
    ; "adapter_logins", adapter_logins_json t
    ]
;;

let via_host_token (client : Client.t) =
  Option.exists client.signed_in ~f:(fun s ->
    s.User_access.Signed_in.via_host_token)
;;

(* An agent that has its tool host's token must not be able to log its
   user in or out, or give their logins away. *)
let check_manages_logins (client : Client.t) =
  if via_host_token client
  then
    Or_error.error_string
      "this connection signed in with a host token, which cannot change model \
       logins: sign in as the user (prigh-web, or the user's own token)"
  else Ok ()
;;

(* The tool host [host] names (id or name; default: the session's active
   one). *)
let import_host agent host =
  let state = Agent.state agent in
  let host = Option.value host ~default:state.active_host in
  match
    List.find state.hosts ~f:(fun (h : Agent.Host.t) ->
      String.equal h.id host || String.equal h.name host)
  with
  | Some h -> Ok h
  | None ->
    Or_error.errorf
      "no tool host %S is connected (connected: %s)"
      host
      (String.concat
         ~sep:", "
         (List.map state.hosts ~f:(fun (h : Agent.Host.t) -> h.name)))
;;

(* Moves a tool host's own CLI login into prigh ({!Cli_import}). Without
   [confirm] it only finds it, answering what the user consents to; with
   it, the login becomes prigh's and the adapter runs on it. *)
let import_adapter_login t (client : Client.t) agent params =
  let open Or_error.Let_syntax in
  let%bind () = check_manages_logins client in
  let%bind adapter = string_param params "adapter" in
  let%bind host = string_param_opt params "host" in
  let%bind confirm = bool_param params "confirm" ~default:false in
  let%bind home =
    match t.home with
    | Some home -> Ok home
    | None -> Or_error.error_string "this backend keeps no settings"
  in
  let%bind host = import_host agent host in
  let exec ~name arguments =
    Agent.host_call_on agent ~host:host.id ~cwd:host.cwd ~name arguments
  in
  let host_name = if String.is_empty host.name then host.id else host.name in
  let logged_in =
    match Adapter_id.of_string adapter with
    | Ok adapter_id ->
      (match
         Adapter_health.Seen.Table.find t.login_seen ~host:host.id adapter_id
       with
       | Some (Opened _) -> true
       | Some (Refused _ | Check_failed _) | None -> false)
    | Error _ -> false
  in
  let%bind found =
    Cli_import.find ~logged_in ~exec ~adapter ~host:host_name ()
  in
  let found_json =
    [ "adapter", `String adapter
    ; "provider", `String (Provider_id.to_string found.provider)
    ; "host", `String host_name
    ; "path", `String found.path
    ; "label", `String (Cli_import.Found.label found)
    ; "warning", `String (Cli_import.Found.warning found)
    ]
  in
  if not confirm
  then Ok (`Object (("confirmed", `False) :: found_json))
  else (
    let env = Agent.env agent in
    let store = Login_manager.store t.login in
    let%bind () =
      match found.client with
      | Some client -> Oauth_antigravity.Client.save store client
      | None -> Ok ()
    in
    let%bind id, note =
      Cli_import.take
        ~exec
        ~refresh:
          (Provider_auth.refresh_oauth
             ~env
             ~cancel:Cancellation.never
             ~store:(Login_manager.store t.login))
        (Login_manager.store t.login)
        found
    in
    let%map () = Cli_login.save ~home ~adapter (Prigh (Account_ref.Own id)) in
    send_accounts_changed t;
    Hashtbl.iter t.agents ~f:(fun agent ->
      match Agent.engine_choice agent with
      | { engine = Composer; adapter = Some a }
        when String.equal (Adapter_id.to_string a) adapter ->
        route t agent (State_changed (Agent.state agent))
      | _ -> ());
    `Object
      ((("confirmed", `True) :: found_json)
       @ [ "account", `String (Account_id.to_string id)
         ; "note", Option.value_map note ~default:`Null ~f:(fun n -> `String n)
         ; "adapter_logins", adapter_logins_json t
         ]))
;;

let member_of t =
  match t.member with
  | Some m -> Ok m
  | None ->
    Or_error.error_string
      "sharing logins needs users (prigh serve -tokens): this server has one"
;;

(* A login another user shares, named by its ref
   (["~<owner>/<provider>/<id>"], as [auth_status] lists it). *)
let shared_ref_param login params =
  let open Or_error.Let_syntax in
  match param params "ref" with
  | Some (`String r) when String.is_prefix r ~prefix:"~" ->
    (match String.split (String.drop_prefix r 1) ~on:'/' with
     | [ owner; p; id ] when not (String.is_empty owner) ->
       let%bind provider = provider_of_string login p in
       let%map id = Account_id.of_string id in
       Some (provider, owner, id)
     | _ ->
       Or_error.errorf
         "shared login ref %S is not ~OWNER/PROVIDER/ACCOUNT (as auth_status \
          lists it)"
         r)
  | _ -> Ok None
;;

let set_shared_pref t ~provider ~owner id ~f =
  let open Or_error.Let_syntax in
  let%bind { Account_shares.Member.shares; user } = member_of t in
  let%map () = Account_shares.set_pref shares ~user provider ~owner id ~f in
  send_accounts_changed t
;;

let account_methods t (client : Client.t) ~meth ~params =
  let open Or_error.Let_syntax in
  match meth with
  | "account_share" ->
    Some
      (let%bind () = check_manages_logins client in
       let%bind { Account_shares.Member.shares; user } = member_of t in
       let%bind provider, account = account_param t.login params in
       let%bind users =
         match param params "users" with
         | Some (`Array users) ->
           Or_error.all
             (List.map users ~f:(function
                | `String u -> Ok u
                | _ -> Or_error.error_string "param \"users\" must be names"))
         | _ ->
           Or_error.error_string
             "missing param \"users\" (the user names to share with; [] stops \
              sharing)"
       in
       let%map grants =
         Account_shares.share shares ~owner:user provider account ~users
       in
       send_accounts_changed t;
       `Object
         [ ( "shared_with"
           , `Array
               (List.map grants ~f:(fun (g : Account_meta.Grant.t) ->
                  `Object
                    [ "user", `String g.user
                    ; "since_ms", `Number (Int.to_string g.since_ms)
                    ])) )
         ])
  | "account_share_targets" ->
    Some
      (let%bind { Account_shares.Member.shares; user } = member_of t in
       let%map users = Account_shares.targets shares ~user in
       `Array (List.map users ~f:(fun u -> `String u)))
  | "account_hide" ->
    Some
      (let%bind () = check_manages_logins client in
       let%bind hidden = bool_param params "hidden" ~default:true in
       match%bind shared_ref_param t.login params with
       | None ->
         Or_error.error_string
           "account_hide takes a shared login's ref (~OWNER/PROVIDER/ACCOUNT); \
            your own logins are logged out instead"
       | Some (provider, owner, id) ->
         let%map () =
           set_shared_pref t ~provider ~owner id ~f:(fun p -> { p with hidden })
         in
         `Object [])
  | "account_usage" ->
    Some
      (let self =
         Option.value_map
           t.member
           ~default:Account_ledger.single_user
           ~f:(fun m -> m.user)
       in
       let%map rows =
         match%bind shared_ref_param t.login params with
         | Some (provider, owner, id) ->
           let%bind { Account_shares.Member.shares; user } = member_of t in
           Account_shares.usage shares ~user provider ~owner id
         | None ->
           let%map provider, id = account_param t.login params in
           (match t.member, t.ledger with
            | Some { shares; user }, _ ->
              Account_shares.usage shares ~user provider ~owner:user id
              |> Result.ok
              |> Option.value ~default:[]
            | None, Some ledger ->
              Account_ledger.rows
                ledger
                ~login:(Account_ledger.login provider id)
            | None, None -> [])
       in
       `Array
         (List.map rows ~f:(fun (r : Account_ledger.Row.t) ->
            match Account_ledger.Periods.to_json r.usage with
            | `Object periods ->
              `Object
                ((("user", `String r.user)
                  :: (if String.equal r.user self then [ "you", `True ] else [])
                 )
                 @ periods)
            | json -> json)))
  | _ -> None
;;

let dispatch_server t (client : Client.t) ~meth ~params
  : Json.t Or_error.t option
  =
  let agent = client.agent in
  match meth with
  | "hello" -> Some (hello t client params)
  | "account_share" | "account_share_targets" | "account_hide" | "account_usage"
    -> account_methods t client ~meth ~params
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
  | "list_adapters" -> Some (list_adapters t client params)
  | "set_adapter_login" -> Some (set_adapter_login t params)
  | "import_adapter_login" -> Some (import_adapter_login t client agent params)
  | "set_active_host" ->
    Some
      (Or_error.bind (string_param params "host") ~f:(fun host ->
         Or_error.bind (string_param_opt params "cwd") ~f:(fun cwd ->
           Or_error.bind (if_missing_param params) ~f:(fun if_missing ->
             Or_error.map
               (Agent.set_active_host agent host ~cwd ~if_missing)
               ~f:Dir_change.Outcome.to_json))))
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
  | "get_prefs" | "set_prefs" | "push_pref" ->
    Some (prefs t client ~meth ~params)
  | "get_memory" | "save_memory" | "forget_memory" | "set_memory_enabled" ->
    Some (memory t ~meth ~params)
  | "get_user_info" | "set_user_info" -> Some (user_info t ~meth ~params)
  | "get_custom_instructions"
  | "save_custom_instruction"
  | "remove_custom_instruction"
  | "set_custom_instructions_enabled" ->
    Some (custom_instructions t ~meth ~params)
  | "get_mcp_settings"
  | "add_mcp_servers"
  | "update_mcp_server"
  | "remove_mcp_server"
  | "set_mcp_server_enabled" -> Some (mcp_settings t ~meth ~params)
  | "nix_store_status" | "nix_store_gc" ->
    Nix_store_rpc.dispatch
      t.nix_store
      ~signed_in:client.signed_in
      ~send:client.send
      ~meth
      ~params
  | "btw" -> Some (btw t client params)
  | "generate_text" -> Some (generate_text t client params)
  | "generate_cancel" ->
    Some
      (Or_error.map (string_param params "gen_id") ~f:(fun gen_id ->
         let found = Hashtbl.find client.generations gen_id in
         Option.iter found ~f:Cancellation.cancel;
         `Object
           [ ("cancelled", if Option.is_some found then `True else `False) ]))
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
  | "set_config" -> Some (set_config t client.agent params)
  | "change_default" ->
    Some
      (Or_error.map (Agent.save_as_default client.agent) ~f:(fun () ->
         config_saved t client.agent;
         Config.to_json (Agent.config client.agent)))
  | "set_adapter" -> Some (set_adapter t client params)
  | "new_session" ->
    Some
      (let%bind.Or_error temporary =
         bool_param params "temporary" ~default:false
       in
       let%bind.Or_error engine = string_param_opt params "engine" in
       let%bind.Or_error adapter = string_param_opt params "adapter" in
       let%map.Or_error engine = Engines.choose t.engines ~engine ~adapter in
       new_session ~engine t client ~temporary)
  | "keep_session" -> Some (keep_session t client)
  | "switch_session" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         Or_error.map (find_agent t client key) ~f:(fun target ->
           attach t client target;
           attached_state t client)))
  | "list_sessions" -> Some (ok (list_sessions t client))
  | "search_sessions" -> Some (search_sessions t params)
  | "list_worktrees" -> Some (list_worktrees t client params)
  | "create_worktree" -> Some (create_worktree t client params)
  | "remove_worktree" -> Some (remove_worktree t client params)
  | "get_messages_page" -> Some (messages_page t client params)
  | "get_outline" -> Some (outline t client params)
  | "get_trim_points" -> Some (trim_points t client params)
  | "get_message_points" -> Some (message_points t client params)
  | "get_trimmed" -> Some (trimmed t client params)
  | "get_tool_output" -> Some (tool_output t client params)
  | "delete_session" ->
    Some
      (let open Or_error.Let_syntax in
       let%bind key = string_param params "path" in
       let%bind switch_to = Switch_to.of_params params in
       let%bind state = leave_for_deletion t client ~switch_to [ key ] in
       let%map (deleted : Deleted.t) =
         Or_error.bind
           (find_saved t key)
           ~f:(delete_saved ~leaving:(Option.is_some switch_to) t client)
       in
       sessions_changed t ~removed:[ deleted.id ];
       Option.value state ~default:(`Object []))
  | "delete_sessions" ->
    Some
      (let open Or_error.Let_syntax in
       let%bind keys = string_list_param params "paths" in
       let%bind switch_to = Switch_to.of_params params in
       delete_many ?switch_to t client keys)
  | "delete_archived" ->
    Some
      (Or_error.bind (Switch_to.of_params params) ~f:(fun switch_to ->
         delete_many
           ?switch_to
           t
           client
           (List.filter_map (listings t client) ~f:(fun (l : Listing.t) ->
              Option.some_if l.archived l.summary.path))))
  | "set_archived" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         Or_error.bind
           (required_bool_param params "archived")
           ~f:(fun archived -> unit_result (set_archived t ~key ~archived))))
  | "set_pinned" ->
    Some
      (Or_error.bind (string_param params "path") ~f:(fun key ->
         Or_error.bind (required_bool_param params "pinned") ~f:(fun pinned ->
           unit_result (set_pinned t ~key ~pinned))))
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
                     (start_location t client ~cwd:(host_of client).cwd)
                 else loaded_start t client session
               in
               start_agent t session start |> attach t client;
               sessions_changed t ~changed:[ session ];
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
             | Some _ | None -> start_location t client ~cwd:(own_cwd client)
           in
           start_agent t session start |> attach t client;
           if Session.persisted session
           then sessions_changed t ~changed:[ session ];
           attached_state t client))
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
    @@ Or_error.bind (check_manages_logins client) ~f:(fun () ->
      match param params "provider" with
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
          let open Or_error.Let_syntax in
          let%bind method_ = method_ in
          let%bind account =
            match%bind string_param_opt params "account" with
            | None -> Ok None
            | Some a -> Or_error.map (Account_id.of_string a) ~f:Option.some
          in
          let%bind label = string_param_opt params "label" in
          owned (fun () ->
            Login_manager.start t.login ?account ?label provider method_)))
  | "logout" ->
    (* A custom provider's logout asks the caller a question. *)
    Option.some
    @@ Or_error.bind (check_manages_logins client) ~f:(fun () ->
      match shared_ref_param t.login params with
      | Error e -> Error e
      | Ok (Some (provider, owner, id)) ->
        Or_error.map
          (set_shared_pref t ~provider ~owner id ~f:(fun p ->
             { p with hidden = true }))
          ~f:(fun () -> `Object [])
      | Ok None ->
        Or_error.bind (provider_param t.login params) ~f:(fun provider ->
          let previous = t.login_owner in
          t.login_owner <- Some client.id;
          let result =
            Or_error.bind
              (optional_account_param t.login params)
              ~f:(fun account -> Login_manager.logout t.login ?account provider)
          in
          if Result.is_error result || not (Login_manager.in_progress t.login)
          then t.login_owner <- previous;
          unit_result result))
  | "account_update" ->
    Some
      (let open Or_error.Let_syntax in
       let%bind () = check_manages_logins client in
       match%bind shared_ref_param t.login params with
       | Some (provider, owner, id) ->
         if
           List.exists [ "label"; "default"; "before" ] ~f:(fun name ->
             Option.is_some (param params name))
         then
           Or_error.errorf
             "%s shares this login with you: you can hide it or let it take \
              over at usage limits (fallback), not rename, reorder or make it \
              the default"
             owner
         else (
           let%bind fallback = bool_param params "fallback" ~default:false in
           set_shared_pref t ~provider ~owner id ~f:(fun p ->
             { p with fallback })
           |> unit_result)
       | None ->
         let%bind provider, account = account_param t.login params in
         let%bind label = string_param_opt params "label" in
         let%bind make_default = bool_param params "default" ~default:false in
         let%bind fallback =
           match param params "fallback" with
           | None | Some `Null -> Ok None
           | Some _ ->
             Or_error.map
               (bool_param params "fallback" ~default:false)
               ~f:Option.some
         in
         let%bind move_before =
           match param params "before" with
           | None -> Ok None
           | Some `Null -> Ok (Some None)
           | Some (`String r) ->
             let%bind before_provider, before =
               account_param t.login (`Object [ "ref", `String r ])
             in
             if Provider_id.equal before_provider provider
             then Ok (Some (Some before))
             else
               Or_error.errorf
                 "%S is another provider's account: accounts move among their \
                  provider's"
                 r
           | Some _ ->
             Or_error.error_string
               "param \"before\" must be an account ref or null"
         in
         Login_manager.update_account
           t.login
           provider
           account
           { label; make_default; fallback; move_before }
         |> unit_result)
  | "usage_limits" ->
    let env = Agent.env agent in
    let shared =
      match t.member with
      | None -> []
      | Some { shares; user } ->
        Account_shares.shared_with shares ~user
        |> List.filter ~f:(fun (s : Account_shares.Shared.t) ->
          not s.pref.hidden)
        |> List.map ~f:(fun (s : Account_shares.Shared.t) ->
          { Subscription_usage.Shared.owner = s.owner
          ; provider = s.provider
          ; id = s.id
          ; label = s.label
          ; resolve =
              (fun () ->
                Or_error.map
                  (Account_shares.resolve
                     shares
                     ~env
                     ~recipient:user
                     s.provider
                     ~owner:s.owner
                     s.id)
                  ~f:Option.some)
          })
    in
    let harnesses = harness_usage t agent in
    let usage =
      Subscription_usage.fetch
        ~env
        ~shared
        ~getenv:(Login_manager.getenv t.login)
        ~http:t.usage_http
        ~now:t.now
        (Login_manager.store t.login)
    in
    (* With a harness's own login shown, prigh's logins it lacks are noise. *)
    let usage =
      if List.is_empty harnesses
      then usage
      else
        { usage with
          providers =
            List.filter
              usage.providers
              ~f:(fun (p : Subscription_usage.Provider_usage.t) ->
                not
                  (Subscription_usage.Status.equal p.status Not_signed_in
                   && List.is_empty p.accounts))
        }
    in
    Some (ok (Subscription_usage.to_json ~harnesses usage))
  | "forge_status" ->
    Some
      (Or_error.map (Forges.accounts t.forges) ~f:(fun accounts ->
         `Object
           [ "accounts", `Array (List.map accounts ~f:Forges.Account.to_json) ]))
  | "forge_login" ->
    Option.some
    @@
    let open Or_error.Let_syntax in
    let%bind host =
      string_param_opt params "host" >>| Option.value ~default:"github.com"
    in
    let%bind kind =
      match%bind string_param_opt params "kind" with
      | None -> Ok None
      | Some k -> Forge_kind.of_string k >>| Option.some
    in
    let%bind method_ =
      match%bind string_param_opt params "method" with
      | None -> Ok None
      | Some m -> Forge_credential.Method.of_string m >>| Option.some
    in
    let%bind instance = Forges.instance t.forges ?kind host in
    let previous = t.login_owner in
    t.login_owner <- Some client.id;
    let started =
      Login_manager.start_forge
        t.login
        ~host:instance.host
        ~run:(fun interaction ->
          Forges.login t.forges instance ~method_ interaction)
    in
    if Result.is_error started then t.login_owner <- previous;
    unit_result started
  | "forge_logout" ->
    Option.some
    @@ Or_error.bind (string_param params "host") ~f:(fun host ->
      Or_error.bind (Forges.instance t.forges host) ~f:(fun instance ->
        Or_error.map (Forges.logout t.forges instance.host) ~f:(fun () ->
          Login_manager.forge_logged_out t.login instance.host;
          `Object [])))
  | "pr_checkout" -> Some (checkout_pull t client params)
  | "pr_list" | "pr_view" | "pr_defaults" | "pr_create" ->
    Some (pull_requests t client ~meth ~params)
  | "retry_models" ->
    (* The session's harness, which could not list its models, asked again
       now: its clients hear of it as for any asking. *)
    let agent = client.agent in
    Hashtbl.remove t.catalog_keys (session_id agent);
    ensure_catalog ~force:true t agent;
    Some (ok (`Object []))
  | "list_models" ->
    let agent = client.agent in
    (match external_models t agent with
     | Some models ->
       (* Only the harness's default until it says its models: asked now,
          unless its process is up (its session says them) or it listed
          none before. *)
       if
         List.length models <= 1
         && (not (Agent.external_live agent))
         && not (lists_no_models t agent)
       then ensure_catalog t agent;
       Some (ok (`Array (List.map models ~f:Rpc_json.model)))
     | None ->
       let models = Login_manager.models t.login in
       Model_registry.reload models;
       tell_problems client (Model_registry.problems models);
       Some
         (ok
            (`Array (List.map (Model_registry.models models) ~f:Rpc_json.model))))
  | _ -> None
;;

let ui_action_param params ~meth =
  match param params "ui_action" with
  | None | Some `Null -> Ok None
  | Some _ when String.equal meth "steer" ->
    Or_error.error_string
      "steer does not take ui_action: send it with prompt, or follow_up while \
       a run is in progress"
  | Some json ->
    Or_error.bind (Ui_action.of_json json) ~f:(fun action ->
      match param params "text" with
      | None | Some (`String "") -> Ok (Some action)
      | Some _ ->
        Or_error.error_string
          "a prompt with ui_action takes no text: send text \"\"")
;;

(* One of an external harness's models, by key ([codex/gpt-5]), id or
   name. *)
let resolve_external (models : Model.t list) query ~who =
  let q = String.lowercase query in
  match
    List.find models ~f:(fun m ->
      String.equal (Model.key m) query
      || String.equal m.id query
      || String.equal (String.lowercase m.name) q)
  with
  | Some m -> Ok m
  | None ->
    Or_error.errorf
      "%s has no model %S: pick one of %s (/model lists them)"
      who
      query
      (String.concat ~sep:", " (List.map models ~f:(fun m -> m.id)))
;;

let dispatch
      ?member
      ?ledger
      agent
      login
      ~external_models
      ~state_json
      ~(client : Client.t)
      ~meth
      ~params
  : Json.t Or_error.t
  =
  match meth with
  | "ping" -> ok (`String "pong")
  | "resync" ->
    Stream_sync.reset client.streams;
    ok (`Object [])
  | ("prompt" | "steer" | "follow_up") as meth ->
    let open Or_error.Let_syntax in
    let%bind ui_action = ui_action_param params ~meth in
    let%bind text =
      match ui_action with
      | Some _ -> Ok ""
      | None -> string_param params "text"
    in
    let%bind attachments = string_list_param params "attachments" in
    let%bind pasted = string_list_param params "pasted" in
    let%bind images = images_param ~env:(Agent.env agent) params in
    let client = client.kind in
    (match meth with
     | "prompt" ->
       unit_result
         (Agent.prompt
            ~attachments
            ~pasted
            ~images
            ~client
            ?ui_action
            agent
            text)
     | "steer" ->
       unit_result (Agent.steer ~attachments ~pasted ~images ~client agent text)
     | _ ->
       unit_result
         (Agent.follow_up
            ~attachments
            ~pasted
            ~images
            ~client
            ?ui_action
            agent
            text))
  | "list_skills" ->
    Or_error.map (Agent.skills agent) ~f:(fun skills ->
      `Object [ "skills", `Array (List.map skills ~f:[%jsonaf_of: Skill.t]) ])
  | "list_mcp" ->
    Or_error.bind
      (bool_param params "reconnect" ~default:false)
      ~f:(fun reconnect ->
        Or_error.map
          (Agent.mcp_servers ~reconnect agent)
          ~f:(mcp_listing_json agent))
  | "mcp_approve" ->
    let open Or_error.Let_syntax in
    let%bind source = string_param params "source" in
    let%bind server = string_param params "server" in
    Or_error.map
      (Agent.approve_mcp agent ~source ~server)
      ~f:(mcp_listing_json agent)
  | "mcp_signin" ->
    let open Or_error.Let_syntax in
    let%bind source = string_param params "source" in
    let%bind server = string_param params "server" in
    let%bind redirect_base = string_param_opt params "redirect_base" in
    let%map url = Agent.mcp_sign_in agent ~source ~server ~redirect_base in
    `Object [ "url", `String url ]
  | "mcp_signout" ->
    let open Or_error.Let_syntax in
    let%bind source = string_param params "source" in
    let%bind server = string_param params "server" in
    Or_error.map
      (Agent.mcp_sign_out agent ~source ~server)
      ~f:(mcp_listing_json agent)
  | "abort" ->
    let restored = Agent.abort_queued agent in
    ok
      (`Object
          (( "restored"
           , strings (List.map restored ~f:(fun (q : Agent.Queued.t) -> q.text))
           )
           ::
           (* Each restored message's, when any has some. *)
           (if
              List.for_all restored ~f:(fun (q : Agent.Queued.t) ->
                List.is_empty q.pasted)
            then []
            else
              [ ( "pasted"
                , `Array
                    (List.map restored ~f:(fun (q : Agent.Queued.t) ->
                       strings q.pasted)) )
              ])))
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
    let%bind queue = queue_param params "queue" in
    let%bind index = index_param params in
    let%bind text = string_param_opt params "text" in
    Or_error.map (Agent.remove_queued agent ~queue ~index ~text) ~f:queued_json
  | "steer_agent" ->
    let open Or_error.Let_syntax in
    let%bind agent_id = string_param params "agent_id" in
    let%bind text = string_param params "text" in
    let%bind kind = queue_param params "mode" ~default:Steer in
    unit_result (Agent.steer_subagent agent ~agent_id ~kind text)
  | "dequeue_agent" ->
    Or_error.bind (string_param params "agent_id") ~f:(fun agent_id ->
      Or_error.map (Agent.dequeue_subagent agent ~agent_id) ~f:(function
        | None -> `Null
        | Some text -> `Object [ "text", `String text ]))
  | "remove_agent_queued" ->
    let open Or_error.Let_syntax in
    let%bind agent_id = string_param params "agent_id" in
    let%bind queue = queue_param params "queue" in
    let%bind index = index_param params in
    let%bind text = string_param_opt params "text" in
    Or_error.map
      (Agent.remove_subagent_queued agent ~agent_id ~queue ~index ~text)
      ~f:(fun text -> `Object [ "text", `String text ])
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
  | "get_state" -> ok (state_json (Agent.state agent))
  | "get_pending" ->
    let steer, follow_up = Agent.queued_texts agent in
    ok
      (Rpc_json.pending
         ~steer
         ~follow_up
         ~agents:(Agent.subagent_queues agent)
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
    let open Or_error.Let_syntax in
    let%bind id = string_param params "model" in
    let%bind account = string_param_opt params "account" in
    (match external_models () with
     | Some models ->
       let who = Agent.engine_label agent in
       let%bind () =
         match account with
         | None -> Ok ()
         | Some _ ->
           Or_error.errorf
             "%s uses its own login, not prigh's: pick the model without @LOGIN"
             who
       in
       let%map model =
         match resolve_external models id ~who, String.rsplit2 id ~on:'@' with
         | Ok model, _ -> Ok model
         | Error _, Some (key, _)
           when Result.is_ok (resolve_external models key ~who) ->
           Or_error.errorf
             "%s uses its own login, not prigh's: pick %s without @LOGIN"
             who
             key
         | (Error _ as error), _ -> error
       in
       Agent.set_model agent model;
       `Object []
     | None ->
       let%map model, account = model_with_account ?member login id ~account in
       Agent.set_model ?account agent model;
       `Object [])
  | "set_thinking" ->
    Or_error.bind (string_param params "thinking") ~f:(fun s ->
      Or_error.bind (Rpc_json.thinking_of_string s) ~f:(fun thinking ->
        Or_error.map (Agent.set_thinking agent thinking) ~f:(fun () ->
          `Object [])))
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
    let open Or_error.Let_syntax in
    let%bind path = string_param_opt params "path" in
    let%bind base = string_param_opt params "base" in
    let%bind dir = string_param_opt params "dir" in
    let%bind rev = string_param_opt params "rev" in
    let%bind direct = bool_param params "direct" ~default:false in
    Or_error.map
      (Agent.git_diff ?path ?base ?dir ?rev ~direct ~default_base:true agent)
      ~f:[%jsonaf_of: Git_diff.Result.t]
  | "git_refs" ->
    Or_error.bind (string_param_opt params "dir") ~f:(fun dir ->
      Or_error.map (Agent.git_refs ?dir agent) ~f:[%jsonaf_of: Git_diff.Refs.t])
  | "git_show" ->
    let open Or_error.Let_syntax in
    let%bind dir = string_param_opt params "dir" in
    let%bind rev = string_param params "rev" in
    let%bind path = string_param params "path" in
    Or_error.map
      (Agent.git_show ?dir agent ~rev ~path)
      ~f:[%jsonaf_of: Git_diff.Shown.t]
  | "review" ->
    Or_error.bind (string_param_opt params "args") ~f:(fun args ->
      unit_result
        (Agent.review
           ~client:client.kind
           agent
           ~args:(Option.value args ~default:"")))
  | "init" -> unit_result (Agent.init ~client:client.kind agent)
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
  | "add_reminder" ->
    let open Or_error.Let_syntax in
    let%bind text = string_param params "text" in
    let%bind trigger =
      match string_param_opt params "on", param params "every_turns" with
      | Ok (Some "stop"), None ->
        let%map max_repeats =
          int_param params "max_repeats" ~default:Reminder.default_max_repeats
        in
        Reminder.Trigger.On_stop { max_repeats }
      | Ok None, Some _ ->
        let%map n = int_param params "every_turns" ~default:0 in
        Reminder.Trigger.Every_turns n
      | _ ->
        Or_error.error_string
          "add_reminder needs \"on\": \"stop\" (with an optional \
           \"max_repeats\") or \"every_turns\": N"
    in
    let%map reminder = Agent.add_reminder agent ~text trigger in
    Rpc_json.reminder reminder ~stops:0
  | "remove_reminder" ->
    Or_error.bind (string_param params "name") ~f:(fun name ->
      unit_result (Agent.remove_reminder agent name))
  | "clear_reminders" -> unit_result (Agent.clear_reminders agent)
  | "cancel_resume" -> unit_result (Agent.cancel_resume agent)
  | "resume_now" -> unit_result (Agent.resume_now agent)
  | "downsize_images" ->
    Or_error.map (Agent.downsize_images agent) ~f:(fun (limit, retried) ->
      `Object
        [ "max_bytes", `Number (Int.to_string (limit.max_base64_bytes * 3 / 4))
        ; "max_dimension", `Number (Int.to_string limit.max_dimension)
        ; "described", `String (Image.Limit.describe limit)
        ; ("retried", if retried then `True else `False)
        ])
  | "set_session_name" ->
    Or_error.map (string_param params "name") ~f:(fun name ->
      Agent.set_session_name agent name;
      `Object [])
  | "set_transcript_filter" ->
    Or_error.map (string_param_opt params "filter") ~f:(fun filter ->
      Agent.set_transcript_filter agent filter;
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
  | "trim" ->
    Or_error.bind (string_param params "before") ~f:(fun before ->
      Or_error.map (Agent.trim agent ~before) ~f:Rpc_json.trim_result)
  | "trim_after" ->
    Or_error.bind (string_param params "after") ~f:(fun after ->
      Or_error.map (Agent.trim_after agent ~after) ~f:Rpc_json.hidden)
  | "untrim" ->
    Or_error.bind (string_param_opt params "entry") ~f:(fun entry ->
      Or_error.map (Agent.untrim ?entry agent) ~f:Rpc_json.hidden)
  | "delete" ->
    Or_error.bind (string_param params "message") ~f:(fun message ->
      Or_error.map (Agent.delete_message agent ~message) ~f:Rpc_json.hidden)
  | "undelete" ->
    Or_error.bind (string_param_opt params "entry") ~f:(fun entry ->
      Or_error.map (Agent.undelete ?entry agent) ~f:Rpc_json.hidden)
  | "session_stats" -> ok (Rpc_json.session_stats (Agent.session_stats agent))
  | "trace" ->
    Or_error.map
      (bool_param params "perfetto" ~default:false)
      ~f:(fun perfetto ->
        Rpc_json.trace_report (Agent.trace_report agent) ~perfetto)
  | "set_cwd" ->
    Or_error.bind (string_param params "path") ~f:(fun path ->
      Or_error.bind (if_missing_param params) ~f:(fun if_missing ->
        Or_error.bind
          (bool_param params "worktree" ~default:false)
          ~f:(fun worktree ->
            Or_error.map
              (Agent.set_cwd agent ~path ~if_missing ~worktree)
              ~f:Dir_change.Outcome.to_json)))
  | "worktree_back" ->
    Or_error.bind (bool_param params "main" ~default:false) ~f:(fun main ->
      Or_error.map (Agent.switch_back agent ~main) ~f:Dir_change.Outcome.to_json)
  | "list_paths" ->
    let prefix =
      match param params "prefix" with
      | Some (`String s) -> s
      | _ -> ""
    in
    Agent.list_paths agent ~prefix
  | "list_files" -> Agent.list_files agent
  | "list_dirs" ->
    let prefix =
      match param params "prefix" with
      | Some (`String s) -> s
      | _ -> ""
    in
    Or_error.bind (string_param_opt params "host") ~f:(fun host ->
      Agent.list_dirs ?host agent ~prefix)
  | "get_config" -> ok (Config.to_json (Agent.config agent))
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
    let flag name =
      match param params name with
      | None | Some `Null -> Ok None
      | Some `True -> Ok (Some true)
      | Some `False -> Ok (Some false)
      | Some _ -> Or_error.errorf "param %S must be a boolean" name
    in
    let%bind plan = flag "plan_mode" in
    let%bind chat = flag "chat_mode" in
    Agent.set_mode ?approval ?plan ?chat agent;
    ok (state_json (Agent.state agent))
  | "forget_allowed_call" ->
    let open Or_error.Let_syntax in
    let%bind all = bool_param params "all" ~default:false in
    let%bind call = string_param_opt params "call" in
    let%bind which =
      match call, all with
      | Some line, false -> Ok (Some line)
      | None, true -> Ok None
      | Some _, true | None, false ->
        Or_error.error_string
          "give \"call\" (a line of the state's allowed_calls) or \"all\": true"
    in
    unit_result (Agent.forget_allowed_calls agent which)
  | "ask_user_respond" ->
    let open Or_error.Let_syntax in
    let%bind call_id = string_param params "call_id" in
    let%bind decline = bool_param params "decline" ~default:false in
    let%bind unavailable = string_param_opt params "unavailable" in
    let%bind reply =
      match unavailable with
      | Some why -> Ok (User_question.Reply.Unavailable why)
      | None when decline -> Ok User_question.Reply.Declined
      | None ->
        (match param params "answers" with
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
    let shared, shareable =
      match member with
      | None -> [], false
      | Some { shares; user } ->
        ( Account_shares.shared_with shares ~user
        , (not (via_host_token client))
          &&
            (match Account_shares.targets shares ~user with
            | Ok (_ :: _) -> true
            | Ok [] | Error _ -> false) )
    in
    let usage provider (ref_ : Account_ref.t) =
      match ref_, ledger, member with
      | Own id, Some ledger, _ ->
        Account_ledger.usage ledger ~login:(Account_ledger.login provider id)
      | Shared { owner; id }, _, Some { shares; user } ->
        (match Account_shares.usage shares ~user provider ~owner id with
         | Ok rows ->
           List.fold rows ~init:Account_ledger.Periods.zero ~f:(fun acc r ->
             Account_ledger.Periods.add acc r.usage)
         | Error _ -> Account_ledger.Periods.zero)
      | _ -> Account_ledger.Periods.zero
    in
    Or_error.map (Login_manager.status login) ~f:(fun statuses ->
      `Array
        (List.map statuses ~f:(Rpc_json.auth_status ~shared ~shareable ~usage)))
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
      (* Every client's list shows the sessions archived by the new age. *)
      let archive_before =
        Option.some_if
          (client.Client.authed && String.equal meth "set_config")
          (archive_after_days t client)
      in
      let result =
        match
          if (not client.Client.authed) && not (String.equal meth "hello")
          then
            Or_error.error_string
              "unauthorised: send hello with the token first"
          else (
            match
              Or_error.bind
                (Engine_gate.check client.agent ~meth ~params)
                ~f:(fun () -> Ok (dispatch_server t client ~meth ~params))
            with
            | Error e -> Error e
            | Ok (Some result) -> result
            | Ok None ->
              let agent = agent_of_client t client in
              let by =
                Option.map client.signed_in ~f:(fun s ->
                  s.User_access.Signed_in.user)
              in
              (match
                 Option.first_some
                   (Files_rpc.dispatch ?by agent ~meth ~params)
                   (Option.first_some
                      (Review_rpc.dispatch
                         ?by
                         ~client:client.kind
                         agent
                         ~meth
                         ~params)
                      (Git_rpc.dispatch agent ~meth ~params))
               with
               | Some result -> result
               | None ->
                 dispatch
                   ?member:t.member
                   ?ledger:t.ledger
                   agent
                   t.login
                   ~external_models:(fun () -> external_models t agent)
                   ~state_json:(state_json t)
                   ~client
                   ~meth
                   ~params))
        with
        | result -> result
        | exception exn ->
          Or_error.error_s [%message "internal error" (exn : exn)]
      in
      (match archive_before, result with
       | Some before, Ok _
         when not (Float.equal before (archive_after_days t client)) ->
         sessions_changed t
       | _ -> ());
      result
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

let respond t (client : Client.t) request =
  let before = client.agent in
  client.send (handle t client request);
  ignore (Promise.try_resolve (snd t.served) () : bool);
  if not (phys_equal before client.agent) then tell_skipped client
;;

exception Connection_closed

(* An exception in one request must not end the connection. *)
let respond_or_log t (client : Client.t) request =
  match respond t client request with
  | () -> ()
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    eprintf
      "prigh: request from %s failed: %s\n%!"
      client.name
      (Exn.to_string exn)
;;

let sends_heartbeats (client : Client.t) =
  client.tools && List.mem client.features "heartbeat" ~equal:String.equal
;;

(* A tool host gone without closing its connection (a network dropped, a
   machine asleep, a frozen process) would be sent tool calls nobody
   answers: one that sends heartbeats is given up on once it is silent for
   [host_timeout]. *)
let watch_heartbeats t (client : Client.t) ~give_up =
  let rec loop () =
    Eio.Time.sleep t.heartbeat_clock (Float.min 5. (t.host_timeout /. 3.));
    if
      sends_heartbeats client
      && Float.(
           Eio.Time.now t.heartbeat_clock -. client.heard >= t.host_timeout)
    then (
      client.silent <- true;
      give_up (sprintf "no reply for %gs" t.host_timeout))
    else loop ()
  in
  loop ()
;;

let serve_lines ?signed_in ?(finish_requests = false) t ~read_line ~write_line =
  Switch.run
  @@ fun sw ->
  let outbox = Outbox.create () in
  let coalescer = Update_coalescer.create outbox in
  let send json = Update_coalescer.send coalescer json in
  let send_shared json ~line = Update_coalescer.send coalescer json ~line in
  (* A failed outbox also gives up on a write stuck on the peer. *)
  Fiber.fork ~sw (fun () ->
    Fiber.first
      (fun () -> Outbox.run outbox ~write:write_line)
      (fun () -> ignore (Promise.await (Outbox.failed outbox) : string)));
  let client =
    connect ?signed_in t ~send ~send_shared ~close:(Outbox.fail outbox)
  in
  Fiber.fork_daemon ~sw (fun () ->
    watch_heartbeats t client ~give_up:(Outbox.fail outbox);
    `Stop_daemon);
  let rec loop requests =
    match read_line () with
    | None -> None
    | Some line ->
      client.heard <- Eio.Time.now t.heartbeat_clock;
      if String.is_empty (String.strip line)
      then loop requests
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
          loop requests
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
             Fiber.fork ~sw:requests (fun () -> respond_or_log t client request);
             loop requests))
  in
  (* Requests still running when the peer has gone are cancelled: nobody
     is left to answer (a [set_user] lets them finish, its answers still
     go out on this connection). *)
  let read () =
    let result = ref None in
    (match
       Switch.run (fun requests ->
         result := loop requests;
         if Option.is_none !result && not finish_requests
         then Switch.fail requests Connection_closed)
     with
     | () | (exception Connection_closed) -> ());
    !result
  in
  let switch_to =
    Fun.protect
      ~finally:(fun () ->
        Eio.Cancel.protect (fun () ->
          disconnect t client;
          Outbox.finish outbox))
      (fun () ->
         Fiber.first read (fun () ->
           let reason = Promise.await (Outbox.failed outbox) in
           eprintf
             "prigh: closing the connection of %s: %s\n%!"
             client.name
             reason;
           None))
  in
  switch_to
;;
