open! Core
open Prigh

let home () = Option.value (Sys.getenv "HOME") ~default:"."

let auth_file_path_flag =
  let%map_open.Command auth_file =
    flag
      "-auth-file"
      (optional string)
      ~doc:"PATH credential file (default: ~/.config/prigh/auth.json)"
  in
  Option.value_or_thunk auth_file ~default:Auth_store.default_path
;;

let auth_file_flag =
  let%map.Command path = auth_file_path_flag in
  Auth_store.create ~path
;;

let provider_arg =
  Command.Arg_type.create (fun s ->
    match Provider_id.of_string s with
    | Some p -> p
    | None ->
      eprintf
        "unknown provider %s; one of: %s\n"
        s
        (String.concat
           ~sep:", "
           (List.map Provider_id.all ~f:Provider_id.to_string));
      exit 2)
;;

(* With no explicit or configured default model, prefer a provider the user is
   logged in to. *)
let default_model ~getenv store =
  match Provider_auth.status ~getenv store with
  | Error _ -> Model.default
  | Ok statuses ->
    List.find_map
      [ Provider_id.Anthropic; Openai_codex; Openai; Deepseek ]
      ~f:(fun provider ->
        List.find statuses ~f:(fun s ->
          Provider_id.equal s.provider provider && Option.is_some s.configured))
    |> Option.value_map ~default:Model.default ~f:(fun s ->
      Model.default_for s.provider)
;;

module Setup = struct
  type t =
    { session : Session.t option (** [-session], if given *)
    ; cwd : string
    ; new_agent : ?session:Session.t -> cwd:string -> unit -> Agent.t
    ; world : Namespace.World.t
    }
end

let common_params =
  let%map_open.Command model =
    flag
      "-model"
      (optional string)
      ~doc:
        "ID model id or provider/id (default: the session's, else the one \
         saved by /change_default, else the first logged-in provider's)"
  and thinking =
    flag "-thinking" (optional string) ~doc:"LEVEL off|on|low|high|max"
  and session =
    flag
      "-session"
      (optional string)
      ~doc:"PATH continue an existing session file"
  and cwd =
    flag
      "-cwd"
      (optional string)
      ~doc:"DIR working directory (default: current)"
  and no_tools = flag "-no-tools" no_arg ~doc:" disable all tools"
  and faux =
    flag
      "-faux"
      no_arg
      ~doc:" use a scripted provider that echoes prompts (for testing)"
  and faux_script =
    flag
      "-faux-script"
      (optional string)
      ~doc:
        "PATH JSON array of scripted provider replies (implies -faux; loops \
         when exhausted)"
  and auth_file = auth_file_path_flag in
  fun ~env ~sw ?(backend_host = true) ?world () ->
    let world =
      match world with
      | Some world -> world
      | None -> Namespace.World.legacy ~home:(home ()) ~auth_file
    in
    let { Namespace.World.home; sessions_dir; store; getenv } = world in
    let cwd = Option.value cwd ~default:(Core_unix.getcwd ()) in
    let model =
      Option.map model ~f:(fun id ->
        match Model.resolve id with
        | Ok m -> m
        | Error e ->
          eprintf "%s\n" (Error.to_string_hum e);
          exit 2)
    in
    let thinking =
      Option.map thinking ~f:(fun s ->
        match Rpc_json.thinking_of_string s with
        | Ok t -> t
        | Error e ->
          eprintf "%s\n" (Error.to_string_hum e);
          exit 2)
    in
    let provider =
      match faux_script with
      | Some path ->
        let replies =
          match Faux_provider.of_script_file path with
          | Ok replies -> replies
          | Error e ->
            eprintf "cannot load faux script: %s\n" (Error.to_string_hum e);
            exit 2
        in
        Faux_provider.create
          ~loop:true
          ~delay_between_events:(fun () ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.02)
          replies
      | None ->
        if faux
        then
          Faux_provider.create
            (List.init 1000 ~f:(fun _ -> Faux_provider.Reply.text "faux reply"))
        else Provider_router.create ~env ~getenv ~store ()
    in
    let session =
      Option.map session ~f:(fun path ->
        match Session.load path with
        | Ok s -> s
        | Error e ->
          eprintf "cannot load session: %s\n" (Error.to_string_hum e);
          exit 2)
    in
    (* One agent per session; the subagent tool follows its own agent's
       model and thinking level. *)
    let new_agent ?session ~cwd () =
      let agent_ref = ref None in
      let current f default () =
        Option.value_map !agent_ref ~default ~f:(fun a -> f (Agent.state a))
      in
      let subagent =
        Tool_subagent.create
          ~provider
          ~current_model:(current (fun s -> s.model) Model.default)
          ~current_thinking:(current (fun s -> s.thinking) Thinking.Off)
          ~home
      in
      let agent =
        Agent.create
          ~env
          ~sw
          ~provider
          ~tools:
            (if no_tools
             then []
             else Tools.all @ (subagent :: Tool_subagent.control_tools))
          ~sessions_dir
          ~home
          ?session
          ?model
          ?thinking
          ~fallback_model:(default_model ~getenv store)
          ~auto_describe:(Option.is_none faux_script && not faux)
          ~backend_host
          ~cwd
          ()
      in
      agent_ref := Some agent;
      agent
    in
    { Setup.session; cwd; new_agent; world }
;;

let run_command =
  Command.basic
    ~summary:"Run a single prompt headlessly, streaming the reply to stdout"
    (let%map_open.Command make_agent = common_params
     and prompt = anon ("PROMPT" %: string)
     and show_thinking =
       flag "-show-thinking" no_arg ~doc:" print thinking to stderr"
     and quiet =
       flag "-quiet" no_arg ~doc:" do not print tool activity to stderr"
     in
     fun () ->
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let { Setup.session; cwd; new_agent; _ } = make_agent ~env ~sw () in
       let agent = new_agent ?session ~cwd () in
       let flush_out () = Out_channel.flush stdout in
       let note fmt =
         ksprintf (fun s -> if not quiet then eprintf "%s\n%!" s) fmt
       in
       let failed = ref None in
       let rec handle ~in_subagent (event : Agent.Event.t) =
         match event with
         | Loop (Subagent { event = inner; _ }) ->
           handle ~in_subagent:true (Loop inner)
         | Loop (Subagent_start { agent_id; task; tools; _ }) ->
           note
             "[subagent %s] %s (tools: %s)"
             agent_id
             (String.prefix (String.strip task) 100)
             (String.concat ~sep:", " tools)
         | Loop
             (Subagent_end
                { call_id = _; agent_id; usage; turns; cost_usd; result }) ->
           note
             "[subagent %s] %d turns, in=%d out=%d cost=$%.4f%s"
             agent_id
             turns
             usage.input
             usage.output
             cost_usd
             (if result.is_error then " (error)" else "")
         | Loop (Message_update { delta = Text_delta s; _ }) ->
           if in_subagent
           then eprintf "%s%!" s
           else (
             Out_channel.output_string stdout s;
             flush_out ())
         | Loop (Message_update { delta = Thinking_delta s; _ }) ->
           if show_thinking then eprintf "%s%!" s
         | Loop (Tool_start call) ->
           note "[tool] %s %s" call.name (String.prefix call.arguments 200)
         | Loop (Tool_output { chunk; _ }) -> eprintf "%s%!" chunk
         | Loop (Tool_end { result; _ }) ->
           note
             "[tool] %s%s"
             (if result.is_error then "error: " else "")
             (String.prefix (String.strip result.text) 200)
         | Loop (Message_end (Assistant a)) ->
           if not in_subagent
           then (
             (match a.stop_reason with
              | Error e -> failed := Some e
              | Aborted | End_turn | Tool_use | Length -> ());
             if not (String.is_empty (Message.Assistant.text a))
             then print_endline "")
         | Notice n -> eprintf "%s\n%!" n
         | _ -> ()
       in
       Agent.subscribe agent ~f:(handle ~in_subagent:false);
       Or_error.ok_exn (Agent.prompt agent prompt);
       Agent.wait_idle agent;
       let state = Agent.state agent in
       note
         "[%s] tokens: in=%d (cached %d) out=%d cost=$%.4f session=%s"
         state.model.id
         state.usage.input
         state.usage.cache_read
         state.usage.output
         state.cost_usd
         state.session_path;
       match !failed with
       | Some e ->
         eprintf "error: %s\n" e;
         exit 1
       | None -> ())
;;

let parse_listen_addr spec =
  match String.rsplit2 spec ~on:':' with
  | None -> Or_error.errorf "listen address must be HOST:PORT, got %S" spec
  | Some (host, port) ->
    (match Int.of_string_opt port with
     | None -> Or_error.errorf "bad port %S" port
     | Some port ->
       let host = if String.is_empty host then "0.0.0.0" else host in
       (match Core_unix.Inet_addr.of_string_or_getbyname host with
        | addr -> Ok (Eio_unix.Net.Ipaddr.of_unix addr, port)
        | exception _ -> Or_error.errorf "cannot resolve host %S" host))
;;

(* A frontend's built assets: the environment variable, or a directory
   relative to this executable. *)
let find_root ~env_var ~candidates () =
  match Sys.getenv env_var with
  | Some dir -> Some dir
  | None ->
    let exe = Core_unix.readlink "/proc/self/exe" in
    List.find candidates ~f:(fun rel ->
      match
        Sys_unix.is_directory (Filename.concat (Filename.dirname exe) rel)
      with
      | `Yes -> true
      | `No | `Unknown -> false)
    |> Option.map ~f:(fun rel ->
      Filename_unix.realpath (Filename.concat (Filename.dirname exe) rel))
;;

let find_web_root =
  find_root
    ~env_var:"PRIGH_WEB_ROOT"
    ~candidates:
      [ "../../../../tui/_build/default/web-bin/site"
      ; "../../tui/_build/default/web-bin/site"
      ; "../share/prigh/web"
      ]
;;

(* The pi web frontend (pi-web/, built with vite). *)
let find_pi_web_root =
  find_root
    ~env_var:"PRIGH_PI_WEB_ROOT"
    ~candidates:
      [ "../../../../pi-web/dist"
      ; "../../pi-web/dist"
      ; "../share/prigh/pi-web"
      ]
;;

let open_in_browser url =
  let prog =
    if Sys_unix.file_exists_exn "/usr/bin/open" then "open" else "xdg-open"
  in
  match Core_unix.fork () with
  | `In_the_child ->
    (try
       Core_unix.exec ~prog ~argv:[ prog; url ] ~use_path:true ()
       |> never_returns
     with
     | _ -> exit 1)
  | `In_the_parent _ -> ()
;;

let serve_command =
  Command.basic
    ~summary:
      "Serve the JSON-lines RPC protocol on stdin/stdout, a TCP port and/or a \
       web port (the browser frontend)"
    (let%map_open.Command make_agent = common_params
     and listen =
       flag
         "-listen"
         (optional string)
         ~doc:
           "HOST:PORT accept TCP clients (multiple frontends, other machines)"
     and stdio =
       flag
         "-stdio"
         no_arg
         ~doc:
           " also serve stdin/stdout when -listen is given (the default \
            without it)"
     and token =
       flag
         "-token"
         (optional string)
         ~doc:"SECRET clients must present it in hello (default: $PRIGH_TOKEN)"
     and tokens =
       flag
         "-tokens"
         (optional string)
         ~doc:
           "NAME=TOKEN,... separate namespaces (sessions, logins, config, tool \
            hosts) under ~/.prigh/namespaces/NAME, chosen by the hello token; \
            [default] uses the usual paths. Provider keys are not read from \
            the environment (default: $PRIGH_TOKENS)"
     and no_backend_host =
       flag
         "-no-backend-host"
         no_arg
         ~doc:
           " never run tools on the backend machine, only on connected tool \
            hosts (also: $PRIGH_NO_BACKEND_HOST=1)"
     and web =
       flag
         "-web"
         (optional string)
         ~doc:
           "HOST:PORT serve the browser frontend and accept WebSocket clients \
            (port 0 picks a free one)"
     and web_root =
       flag
         "-web-root"
         (optional string)
         ~doc:
           "DIR the built web frontend (default: $PRIGH_WEB_ROOT or the dune \
            build)"
     and pi_web =
       flag
         "-pi-web"
         (optional string)
         ~doc:
           "HOST:PORT serve the pi web frontend (pi-web/) and accept its \
            WebSocket clients, speaking pi's RPC protocol"
     and pi_web_root =
       flag
         "-pi-web-root"
         (optional string)
         ~doc:
           "DIR the built pi web frontend (default: $PRIGH_PI_WEB_ROOT or \
            pi-web/dist)"
     and open_browser =
       flag "-open" no_arg ~doc:" open the web frontend in a browser"
     in
     fun () ->
       if Option.is_some tokens && Option.is_some token
       then (
         eprintf "-tokens and -token are mutually exclusive\n";
         exit 2);
       let namespaces =
         match tokens with
         | Some spec -> Some spec
         | None ->
           Option.filter (Sys.getenv "PRIGH_TOKENS") ~f:(Fn.non String.is_empty)
       in
       let namespaces =
         Option.map namespaces ~f:(fun spec ->
           match Namespace.parse_spec spec with
           | Ok namespaces -> namespaces
           | Error e ->
             eprintf "%s\n" (Error.to_string_hum e);
             exit 2)
       in
       let token =
         match token, namespaces with
         | Some t, _ -> Some t
         | None, None -> Sys.getenv "PRIGH_TOKEN"
         | None, Some _ -> None
       in
       let backend_host =
         not
           (no_backend_host
            || Option.equal
                 String.equal
                 (Sys.getenv "PRIGH_NO_BACKEND_HOST")
                 (Some "1"))
       in
       let listen =
         Option.map listen ~f:(fun spec ->
           match parse_listen_addr spec with
           | Ok addr -> addr
           | Error e ->
             eprintf "%s\n" (Error.to_string_hum e);
             exit 2)
       in
       let web =
         Option.map web ~f:(fun spec ->
           match parse_listen_addr spec with
           | Ok addr -> addr
           | Error e ->
             eprintf "%s\n" (Error.to_string_hum e);
             exit 2)
       in
       let pi_web =
         Option.map pi_web ~f:(fun spec ->
           match parse_listen_addr spec with
           | Ok addr -> addr
           | Error e ->
             eprintf "%s\n" (Error.to_string_hum e);
             exit 2)
       in
       if open_browser && Option.is_none web && Option.is_none pi_web
       then (
         eprintf "-open needs -web or -pi-web\n";
         exit 2);
       let stdio =
         stdio
         || (Option.is_none listen
             && Option.is_none web
             && Option.is_none pi_web)
       in
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let create_server ?token ?namespace ?world () =
         let { Setup.session; cwd; new_agent; world } =
           make_agent ~env ~sw ~backend_host ?world ()
         in
         let login =
           Login_manager.create
             ~env
             ~sw
             ~getenv:world.getenv
             ~store:world.store
             ()
         in
         let default_agent =
           Option.map session ~f:(fun session ->
             if Option.is_some namespace
             then (
               eprintf "-session cannot be combined with -tokens\n";
               exit 2);
             new_agent ~session ~cwd:(Session.cwd session) ())
         in
         Rpc_server.create
           ~env
           ~sw
           ?token
           ?namespace
           ~backend_host
           ~login
           ~sessions_dir:world.sessions_dir
           ~cwd
           ~new_agent
           ?default_agent
           ()
       in
       let router =
         match namespaces with
         | None -> Rpc_router.single (create_server ?token ())
         | Some namespaces ->
           Rpc_router.namespaced
             namespaces
             ~home:(home ())
             ~legacy_auth_file:(Auth_store.default_path ())
             ~create_server:(fun namespace world ->
               create_server
                 ~token:namespace.token
                 ~namespace:namespace.name
                 ~world
                 ())
       in
       Option.iter listen ~f:(fun (addr, port) ->
         let socket =
           Eio.Net.listen
             ~sw
             ~backlog:16
             ~reuse_addr:true
             (Eio.Stdenv.net env)
             (`Tcp (addr, port))
         in
         eprintf
           "prigh: listening on %s\n%!"
           (Eio.Net.Sockaddr.pp Format.str_formatter (`Tcp (addr, port));
            Format.flush_str_formatter ());
         Eio.Fiber.fork ~sw (fun () ->
           while true do
             Eio.Net.accept_fork
               ~sw
               socket
               ~on_error:(fun exn ->
                 eprintf "prigh: connection failed: %s\n%!" (Exn.to_string exn))
               (fun flow _addr ->
                  Rpc_router.serve_connection router ~input:flow ~output:flow)
           done));
       let serve_web ~label ~root ~root_flag ~env_var ~websockets (addr, port) =
         let port =
           Web_server.listen
             ~env
             ~sw
             ~addr
             ~port
             ~root
             ~websockets
             ~on_lines:(Rpc_router.serve_lines router)
         in
         let host =
           match Format.asprintf "%a" Eio.Net.Ipaddr.pp addr with
           | "0.0.0.0" -> "127.0.0.1"
           | "::" -> "::1"
           | host -> host
         in
         let url = Web_server.browser_url ~host ~port in
         eprintf "prigh: %s on %s\n%!" label url;
         (match root with
          | Some root ->
            eprintf "prigh: serving %s assets from %s\n%!" label root
          | None ->
            eprintf
              "prigh: no %s assets found (%s or $%s); only WebSockets are served\n\
               %!"
              label
              root_flag
              env_var);
         url
       in
       (* Shared, so both UIs on one session see the same shell. *)
       let terminal =
         Web_server.serve_terminal router (Terminals.create ~env ~sw ())
       in
       let opened = ref false in
       let maybe_open url =
         if open_browser && not !opened
         then (
           opened := true;
           open_in_browser url)
       in
       Option.iter web ~f:(fun addr ->
         let root =
           match web_root with
           | Some dir -> Some dir
           | None -> find_web_root ()
         in
         serve_web
           ~label:"web ui"
           ~root
           ~root_flag:"-web-root"
           ~env_var:"PRIGH_WEB_ROOT"
           ~websockets:
             [ "/ws", Web_server.serve_rpc router; "/terminal", terminal ]
           addr
         |> maybe_open);
       Option.iter pi_web ~f:(fun addr ->
         let root =
           match pi_web_root with
           | Some dir -> Some dir
           | None -> find_pi_web_root ()
         in
         serve_web
           ~label:"pi-web"
           ~root
           ~root_flag:"-pi-web-root"
           ~env_var:"PRIGH_PI_WEB_ROOT"
           ~websockets:
             [ "/ws", Pi_rpc.serve_websocket router; "/terminal", terminal ]
           addr
         |> maybe_open);
       if stdio
       then (
         Rpc_router.serve_connection
           router
           ~input:(Eio.Stdenv.stdin env)
           ~output:(Eio.Stdenv.stdout env);
         (* The spawning frontend went away: stop everything. *)
         Rpc_router.shutdown router;
         exit 0)
       else
         (* The web listener is a daemon fiber; keep the switch alive. *)
         Eio.Fiber.await_cancel ())
;;

let tool_host_command =
  Command.basic
    ~summary:
      "Run tools and terminals on this machine for a backend: as a frontend's \
       worker (JSON lines on stdin/stdout), or with -connect as a tool host \
       connected to the backend over TCP"
    (let%map_open.Command connect =
       flag
         "-connect"
         (optional string)
         ~doc:
           "HOST:PORT connect to a backend's JSON-lines port (-listen or -web) \
            and serve it, reconnecting when the connection drops"
     and token =
       flag
         "-token"
         (optional string)
         ~doc:"SECRET the backend's token (default: $PRIGH_TOKEN)"
     and name =
       flag
         "-name"
         (optional string)
         ~doc:"NAME how the host is shown (default: the hostname)"
     and cwd =
       flag
         "-cwd"
         (optional string)
         ~doc:"DIR the host's directory (default: the current one)"
     in
     fun () ->
       match connect with
       | None ->
         Eio_main.run
         @@ fun env ->
         Tool_host.run
           ~env
           ~input:(Eio.Stdenv.stdin env)
           ~output:(Eio.Stdenv.stdout env)
           ()
       | Some spec ->
         let host, port =
           match String.rsplit2 spec ~on:':' with
           | Some (host, port)
             when (not (String.is_empty host))
                  && Option.is_some (Int.of_string_opt port) ->
             host, Int.of_string port
           | _ ->
             eprintf "-connect must be HOST:PORT, got %S\n" spec;
             exit 2
         in
         let cwd =
           match cwd with
           | Some dir -> Filename_unix.realpath dir
           | None -> Core_unix.getcwd ()
         in
         Eio_main.run
         @@ fun env ->
         Tool_host.connect
           ~env
           ~host
           ~port
           ~token:(Option.first_some token (Sys.getenv "PRIGH_TOKEN"))
           ~name:(Option.value name ~default:(Core_unix.gethostname ()))
           ~cwd
           ())
;;

let login_command =
  Command.basic
    ~summary:"Log in to a provider (anthropic, openai, openai-codex, deepseek)"
    (let%map_open.Command store = auth_file_flag
     and provider = anon ("PROVIDER" %: provider_arg)
     and method_ =
       flag
         "-method"
         (optional string)
         ~doc:"METHOD api_key or oauth (default: the provider's first method)"
     and no_browser =
       flag
         "-no-browser"
         no_arg
         ~doc:" print the login URL instead of opening it"
     in
     fun () ->
       let method_ =
         match method_ with
         | None -> List.hd_exn (Provider_auth.methods provider)
         | Some s ->
           (match Provider_auth.Method.of_string s with
            | Some m -> m
            | None ->
              eprintf "unknown method %s (api_key or oauth)\n" s;
              exit 2)
       in
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let interaction =
         Auth_terminal.create ~env ~sw ~open_urls:(not no_browser) ()
       in
       match Provider_auth.login ~env store provider method_ interaction with
       | Ok () ->
         eprintf
           "Logged in to %s (%s); saved to %s\n"
           (Provider_id.display_name provider)
           (Provider_auth.Method.label provider method_)
           (Auth_store.path store)
       | Error e ->
         eprintf "login failed: %s\n" (Error.to_string_hum e);
         exit 1)
;;

let logout_command =
  Command.basic
    ~summary:"Remove a provider's stored credential"
    (let%map_open.Command store = auth_file_flag
     and provider = anon ("PROVIDER" %: provider_arg) in
     fun () ->
       Eio_main.run
       @@ fun _env ->
       match Provider_auth.logout store provider with
       | Ok () ->
         eprintf "Logged out of %s\n" (Provider_id.display_name provider)
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let auth_command =
  Command.basic
    ~summary:"Show which providers are configured"
    (let%map_open.Command store = auth_file_flag in
     fun () ->
       match Provider_auth.status store with
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1
       | Ok statuses ->
         List.iter statuses ~f:(fun s ->
           printf
             "%-14s %-24s %s\n"
             (Provider_id.to_string s.provider)
             (Provider_id.display_name s.provider)
             (match s.configured with
              | None ->
                sprintf
                  "not configured (login: %s)"
                  (String.concat
                     ~sep:", "
                     (List.map s.methods ~f:Provider_auth.Method.to_string))
              | Some (_, source) -> source)))
;;

let sessions_dir () = Session.default_dir ~home:(home ())

let print_summary (s : Session.Summary.t) =
  printf
    "%s  %s  %3d msgs  %s  %s\n"
    s.created_at
    s.id
    s.message_count
    s.cwd
    (Option.value_map s.first_prompt ~default:"" ~f:(fun p ->
       String.prefix (String.strip p) 60))
;;

let sessions_list_command =
  Command.basic
    ~summary:"List saved sessions"
    (Command.Param.return (fun () ->
       List.iter (Session.list ~dir:(sessions_dir ())) ~f:print_summary))
;;

let delete_sessions summaries =
  List.iter summaries ~f:(fun s ->
    Session.delete s;
    print_summary s);
  eprintf "Deleted %d session(s)\n" (List.length summaries)
;;

let sessions_delete_command =
  Command.basic
    ~summary:"Delete sessions by id"
    (let%map_open.Command ids = anon (sequence ("ID" %: string)) in
     fun () ->
       let all = Session.list ~dir:(sessions_dir ()) in
       let found, missing =
         List.partition_map ids ~f:(fun id ->
           match
             List.find all ~f:(fun s ->
               String.equal s.id id || String.is_prefix s.id ~prefix:id)
           with
           | Some s -> First s
           | None -> Second id)
       in
       List.iter missing ~f:(eprintf "no session %s\n");
       delete_sessions found;
       if not (List.is_empty missing) then exit 1)
;;

let sessions_prune_command =
  Command.basic
    ~summary:
      "Delete sessions matching all given filters (no filter: empty sessions). \
       Don't prune a session a running server is using."
    (let%map_open.Command max_messages =
       flag
         "-max-messages"
         (optional int)
         ~doc:
           "N sessions with at most N messages (default 0 when no other filter)"
     and cwd =
       flag
         "-cwd"
         (optional string)
         ~doc:"DIR sessions started in this directory"
     and older_than_days =
       flag
         "-older-than"
         (optional float)
         ~doc:"DAYS sessions not updated for this many days"
     and prompt =
       flag
         "-prompt"
         (optional string)
         ~doc:"TEXT sessions whose first prompt contains TEXT"
     and dry_run =
       flag "-dry-run" no_arg ~doc:" only list what would be deleted"
     in
     fun () ->
       let max_messages =
         match max_messages, cwd, older_than_days, prompt with
         | None, None, None, None -> Some 0
         | _ -> max_messages
       in
       let filter =
         { Session.Filter.max_messages
         ; cwd = Option.map cwd ~f:Filename_unix.realpath
         ; older_than_days
         ; prompt
         }
       in
       let victims =
         List.filter
           (Session.list ~dir:(sessions_dir ()))
           ~f:(Session.Filter.matches filter ~now:(Time_float.now ()))
       in
       if dry_run
       then (
         List.iter victims ~f:print_summary;
         eprintf "Would delete %d session(s)\n" (List.length victims))
       else delete_sessions victims)
;;

let sessions_command =
  Command.group
    ~summary:"Saved sessions"
    [ "list", sessions_list_command
    ; "delete", sessions_delete_command
    ; "prune", sessions_prune_command
    ]
;;

let () =
  Random.self_init ();
  Command_unix.run
    ~version:Version.to_string
    (Command.group
       ~summary:"prigh backend"
       [ "run", run_command
       ; "serve", serve_command
       ; "tool-host", tool_host_command
       ; "sessions", sessions_command
       ; "login", login_command
       ; "logout", logout_command
       ; "auth", auth_command
       ])
;;
