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

(* A built-in provider or a custom one from config.json. *)
let provider_of_arg models s =
  match Provider_id.of_builtin_string s with
  | Some p -> p
  | None ->
    (match Model_registry.find_provider models s with
     | Some p -> Custom_provider.provider_id p
     | None ->
       eprintf
         "unknown provider %s; one of: %s (or custom, to add an \
          OpenAI-compatible endpoint)\n"
         s
         (String.concat
            ~sep:", "
            (List.map Provider_id.builtins ~f:Provider_id.to_string
             @ List.map (Model_registry.providers models) ~f:(fun p -> p.name)));
       exit 2)
;;

(* With no explicit or configured default model, prefer a provider the user is
   logged in to, then the first custom provider's first known model. *)
let default_model ~getenv ~models store =
  let custom () =
    List.find (Model_registry.models models) ~f:(fun m ->
      Provider_id.is_custom m.provider)
  in
  match Provider_auth.status ~getenv store with
  | Error _ -> Model.default
  | Ok statuses ->
    List.find_map
      [ Provider_id.Anthropic; Openai_codex; Openai; Deepseek ]
      ~f:(fun provider ->
        List.find statuses ~f:(fun s ->
          Provider_id.equal s.provider provider && Option.is_some s.configured))
    |> Option.bind ~f:(fun s -> Model.default_for s.provider)
    |> (function
     | Some m -> m
     | None -> Option.value (custom ()) ~default:Model.default)
;;

module Setup = struct
  type t =
    { session : Session.t option (** [-session], if given *)
    ; cwd : string
    ; new_agent : ?session:Session.t -> cwd:string -> unit -> Agent.t
    ; world : Namespace.World.t
    ; models : Model_registry.t
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
      ~doc:
        "ID|PATH continue a saved session: its id (or a unique prefix of it), \
         as prigh sessions list shows, or its file"
  and cwd =
    flag
      "-cwd"
      (optional string)
      ~doc:"DIR working directory (default: current)"
  and no_tools = flag "-no-tools" no_arg ~doc:" disable all tools"
  and max_budget_usd =
    flag
      "-max-budget-usd"
      (optional float)
      ~doc:
        "DOLLARS stop starting turns once a session has cost this much \
         (default: the config's max_budget_usd; /budget changes a session's)"
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
    Option.iter max_budget_usd ~f:(fun usd ->
      if Float.( <= ) usd 0.
      then (
        eprintf "-max-budget-usd %g: must be a positive number of dollars\n" usd;
        exit 2));
    let cwd_given = Option.is_some cwd in
    Option.iter cwd ~f:(fun dir ->
      if backend_host && not (Sys_unix.is_directory_exn dir)
      then (
        eprintf "-cwd %s: no such directory (mkdir -p it first)\n" dir;
        exit 2));
    let cwd = Option.value cwd ~default:(Core_unix.getcwd ()) in
    let models = Model_registry.create ~env ~sw ~home ~store ~getenv () in
    let model =
      Option.map model ~f:(fun id ->
        match Model_registry.resolve models id with
        | Ok m -> m
        | Error e ->
          eprintf
            "-model: %s\nprigh models lists them all\n"
            (Error.to_string_hum e);
          exit 2)
    in
    let thinking =
      Option.map thinking ~f:(fun s ->
        match Rpc_json.thinking_of_string s with
        | Ok t -> t
        | Error e ->
          eprintf "-thinking %s: %s\n" s (Error.to_string_hum e);
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
        else Provider_router.create ~env ~getenv ~models ~store ()
    in
    let session =
      Option.map session ~f:(fun key ->
        let path =
          if Sys_unix.file_exists_exn key
          then Ok key
          else if String.mem key '/'
          then Or_error.error_string "no such file"
          else
            Session.find_summary (Session.list ~dir:sessions_dir) key
            |> Or_error.map ~f:(fun (s : Session.Summary.t) -> s.path)
        in
        match Or_error.bind path ~f:Session.load with
        | Ok s -> s
        | Error e ->
          eprintf
            "-session %s: %s\n\
             prigh sessions list shows the saved sessions (in %s)\n"
            key
            (Error.to_string_hum e)
            sessions_dir;
          exit 2)
    in
    let mcp = if no_tools then None else Some (Mcp_hub.create ~env ~sw ()) in
    (* One agent per session; the subagent tool follows its own agent's
       model and thinking level. *)
    let new_agent ?session ~cwd () =
      let agent_ref = ref None in
      let current f default () =
        Option.value_map !agent_ref ~default ~f:(fun a -> f (Agent.state a))
      in
      let subagent =
        Tool_subagent.create
          ~models
          ~worktrees_dir:(fun () ->
            match !agent_ref with
            | Some agent -> (Agent.config agent).worktrees_dir
            | None -> Config.default.worktrees_dir)
          ~provider
          ~current_model:(current (fun s -> s.model) Model.default)
          ~current_thinking:(current (fun s -> s.thinking) Thinking.default)
          ~home
          ()
      in
      let agent =
        Agent.create
          ~env
          ~sw
          ~provider
          ~tools:
            (if no_tools
             then []
             else
               Tools.all
               @ (subagent :: Tool_subagent.control_tools)
               @ Tool_jobs.tools
               @ Tools.interaction)
          ~sessions_dir
          ~home
          ?session
          ?model
          ?thinking
          ~fallback_model:(default_model ~getenv ~models store)
          ~models
          ~auto_describe:(Option.is_none faux_script && not faux)
          ~backend_host
          ?mcp
          ~use_default_cwd:(not cwd_given)
          ?max_budget_usd
          ~cwd
          ()
      in
      agent_ref := Some agent;
      agent
    in
    { Setup.session; cwd; new_agent; world; models }
;;

(* Errors from the agent name the slash command; on the command line the
   subcommand does the same. *)
let cli_hints message =
  String.substr_replace_all message ~pattern:"/login " ~with_:"prigh login "
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
       let { Setup.session; cwd; new_agent; models; _ } =
         make_agent ~env ~sw ()
       in
       Model_registry.subscribe models ~f:(fun p ->
         eprintf "%s\n%!" (cli_hints p));
       List.iter (Model_registry.problems models) ~f:(fun p ->
         eprintf "%s\n%!" (cli_hints p));
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
         | Loop Agent_start -> if not in_subagent then failed := None
         | Loop (Compacted _) | Compacted _ ->
           note "[compacted the conversation to fit the context window]"
         | Notice n -> eprintf "%s\n%!" (cli_hints n)
         (* Nobody is there to answer: refuse rather than wait forever. *)
         | Loop (Tool_confirm { call_id; agent_id; name; summary; _ }) ->
           note
             "[tool] %s %s needs confirmation: denied (headless)"
             name
             summary;
           ignore
             (Agent.respond_confirm
                ?agent_id
                agent
                ~call_id
                (Deny
                   (Some
                      "prigh run is headless, so nobody can confirm it; the \
                       user can allow it with approval_mode or permissions in \
                       ~/.prigh/config.json"))
              : unit Or_error.t)
         | Loop (Ask_user { call_id; _ }) ->
           ignore
             (Agent.answer_question
                agent
                ~call_id
                (Unavailable
                   "prigh run is headless, so nobody can answer; decide \
                    yourself and say what you assumed")
              : unit Or_error.t)
         | _ -> ()
       in
       Agent.subscribe agent ~f:(handle ~in_subagent:false);
       Or_error.ok_exn (Agent.prompt agent prompt);
       Agent.wait_idle agent;
       (* A run stopped by a usage limit resumes when it resets. *)
       while Option.is_some (Agent.state agent).resume_at do
         Eio.Time.sleep (Eio.Stdenv.clock env) 1.;
         Agent.wait_idle agent
       done;
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
         eprintf "error: %s\n" (cli_hints e);
         exit 1
       | None -> ())
;;

let parse_listen_addr ~flag spec =
  let usage () =
    match Int.of_string_opt spec with
    | Some port ->
      sprintf
        "%s must be HOST:PORT, got %S; did you mean 127.0.0.1:%d (this machine \
         only) or 0.0.0.0:%d (all interfaces)?"
        flag
        spec
        port
        port
    | None ->
      sprintf "%s must be HOST:PORT such as 127.0.0.1:7790, got %S" flag spec
  in
  let addr =
    match String.rsplit2 spec ~on:':' with
    | None -> Error (usage ())
    | Some (host, port) ->
      (match Int.of_string_opt port with
       | Some port when port >= 0 && port < 65536 ->
         let host = if String.is_empty host then "0.0.0.0" else host in
         (match Core_unix.Inet_addr.of_string_or_getbyname host with
          | addr -> Ok (Eio_unix.Net.Ipaddr.of_unix addr, port)
          | exception _ ->
            Error (sprintf "%s %s: cannot resolve host %S" flag spec host))
       | Some _ | None ->
         Error
           (sprintf
              "%s %s: the port must be a number from 0 to 65535"
              flag
              spec))
  in
  match addr with
  | Ok addr -> addr
  | Error message ->
    eprintf "%s\n" message;
    exit 2
;;

(* Binding fails for reasons the user can fix: say which flag, and how. *)
let listen_or_exit ~flag (addr, port) f =
  match f () with
  | result -> result
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    eprintf
      "prigh: cannot listen on %s for %s: %s\n%s\n%!"
      (Format.asprintf "%a" Eio.Net.Sockaddr.pp (`Tcp (addr, port)))
      flag
      (Net_error.to_string_hum exn)
      (match Net_error.unix_error exn with
       | Some EADDRINUSE ->
         sprintf
           "Another program (perhaps another prigh serve) has that port: stop \
            it, or pass %s another port (0 picks a free one)."
           flag
       | Some EACCES ->
         sprintf "Ports below 1024 need root: pass %s a higher port." flag
       | _ -> sprintf "Check the address given to %s." flag);
    exit 2
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

(* prigh-web, the browser frontend (frontend/web/). *)
let find_web_root =
  find_root
    ~env_var:"PRIGH_WEB_ROOT"
    ~candidates:
      [ "../../../../frontend/_build/default/web/bin/site"
      ; "../../frontend/_build/default/web/bin/site"
      ; "../share/prigh/web"
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
            hosts) under ~/.prigh/namespaces/NAME, chosen by the hello token \
            (users log in with NAME and TOKEN as the password); [default] uses \
            the usual paths. Provider keys are not read from the environment \
            (default: $PRIGH_TOKENS)"
     and superusers =
       flag
         "-superusers"
         (optional string)
         ~doc:
           "NAME,... -tokens users who may act as any user (as_user in hello, \
            set_user; /setusr in the UIs) (default: $PRIGH_SUPERUSERS)"
     and host_tokens =
       flag
         "-host-tokens"
         (optional string)
         ~doc:
           "NAME=TOKEN,... further tokens signing in as -tokens users, never \
            as superusers: for tool hosts, which then need not hold the user's \
            own token (default: $PRIGH_HOST_TOKENS)"
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
           "HOST:PORT serve prigh-web (the browser frontend) and accept its \
            WebSocket clients (port 0 picks a free one)"
     and web_root =
       flag
         "-web-root"
         (optional string)
         ~doc:
           "DIR the built prigh-web site (default: $PRIGH_WEB_ROOT or the dune \
            build)"
     and open_browser =
       flag "-open" no_arg ~doc:" open the web frontend in a browser"
     and nix_gc_helper =
       flag
         "-nix-gc-helper"
         (optional string)
         ~doc:
           "DIR superusers' Nix garbage collection goes through the root \
            helper listening there (the container's; see DOCKER.md) instead of \
            running nix as the backend's user (default: $PRIGH_NIX_GC_HELPER)"
     in
     fun () ->
       if Option.is_some tokens && Option.is_some token
       then (
         eprintf "-tokens and -token are mutually exclusive\n";
         exit 2);
       let from_env value var =
         match value with
         | Some v -> Some v
         | None -> Option.filter (Sys.getenv var) ~f:(Fn.non String.is_empty)
       in
       let namespaces =
         Option.map (from_env tokens "PRIGH_TOKENS") ~f:(fun spec ->
           match Namespace.parse_spec spec with
           | Ok namespaces -> namespaces
           | Error e ->
             eprintf "%s\n" (Error.to_string_hum e);
             exit 2)
       in
       let superusers =
         Option.map
           (from_env superusers "PRIGH_SUPERUSERS")
           ~f:User_access.parse_names
       in
       let host_tokens =
         Option.map (from_env host_tokens "PRIGH_HOST_TOKENS") ~f:(fun spec ->
           match Namespace.parse_spec ~flag:"-host-tokens" spec with
           | Ok host_tokens -> host_tokens
           | Error e ->
             eprintf "%s\n" (Error.to_string_hum e);
             exit 2)
       in
       let access =
         match namespaces with
         | None ->
           if Option.is_some superusers || Option.is_some host_tokens
           then (
             eprintf "-superusers and -host-tokens need -tokens\n";
             exit 2);
           None
         | Some namespaces ->
           (match
              User_access.create
                namespaces
                ~host_tokens:(Option.value host_tokens ~default:[])
                ~superusers:(Option.value superusers ~default:[])
            with
            | Ok access -> Some access
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
       (* Tools the backend runs inherit its environment. *)
       List.iter
         [ "PRIGH_TOKEN"; "PRIGH_TOKENS"; "PRIGH_HOST_TOKENS" ]
         ~f:Core_unix.unsetenv;
       let backend_host =
         not
           (no_backend_host
            || Option.equal
                 String.equal
                 (Sys.getenv "PRIGH_NO_BACKEND_HOST")
                 (Some "1"))
       in
       let listen = Option.map listen ~f:(parse_listen_addr ~flag:"-listen") in
       let web = Option.map web ~f:(parse_listen_addr ~flag:"-web") in
       if open_browser && Option.is_none web
       then (
         eprintf "-open needs -web\n";
         exit 2);
       let stdio = stdio || (Option.is_none listen && Option.is_none web) in
       List.iter
         [ "-listen", listen; "-web", web ]
         ~f:(fun (flag, addr) ->
           Option.iter
             (Option.bind addr ~f:(fun (addr, _) ->
                Web_server.plaintext_warning ~flag addr))
             ~f:(eprintf "%s\n%!"));
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       let nix_store =
         Nix_store.create
           ~sw
           ~now:(fun () -> Eio.Time.now (Eio.Stdenv.clock env))
           (match from_env nix_gc_helper "PRIGH_NIX_GC_HELPER" with
            | Some dir -> Nix_store_ops.helper ~env ~dir
            | None -> Nix_store_ops.local ~env)
       in
       let create_server ?token ?namespace ?world () =
         let { Setup.session; cwd; new_agent; world; models } =
           make_agent ~env ~sw ~backend_host ?world ()
         in
         let login =
           Login_manager.create
             ~env
             ~sw
             ~getenv:world.getenv
             ~models
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
         let server =
           Rpc_server.create
             ~env
             ~sw
             ?token
             ?namespace
             ~backend_host
             ~home:world.home
             ~nix_store
             ~login
             ~sessions_dir:world.sessions_dir
             ~cwd
             ~new_agent
             ?default_agent
             ()
         in
         Rpc_server.restore_resumes server;
         server
       in
       let router =
         match access with
         | None -> Rpc_router.single (create_server ?token ())
         | Some access ->
           Rpc_router.namespaced
             access
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
           listen_or_exit ~flag:"-listen" (addr, port) (fun () ->
             Eio.Net.listen
               ~sw
               ~backlog:16
               ~reuse_addr:true
               (Eio.Stdenv.net env)
               (`Tcp (addr, port)))
         in
         eprintf
           "prigh: listening on %s\n%!"
           (Format.asprintf
              "%a"
              Eio.Net.Sockaddr.pp
              (Eio.Net.listening_addr socket));
         Eio.Fiber.fork ~sw (fun () ->
           Guarded.accept_loop
             ~sw
             ~clock:(Eio.Stdenv.clock env)
             ~on_error:(fun exn ->
               eprintf "prigh: connection failed: %s\n%!" (Exn.to_string exn))
             socket
             (fun flow _addr ->
                Web_server.keepalive flow;
                Rpc_router.serve_connection router ~input:flow ~output:flow)));
       Option.iter web ~f:(fun (addr, port) ->
         (* Any terminal of a session is the same shell, whichever tab opens it. *)
         let terminals = Terminals.create ~env ~sw () in
         List.iter (Rpc_router.servers router) ~f:(fun (_, server) ->
           Rpc_server.on_session_deleted server ~f:(Terminals.kill terminals));
         let root =
           match web_root with
           | Some dir -> Some dir
           | None -> find_web_root ()
         in
         let port =
           listen_or_exit ~flag:"-web" (addr, port) (fun () ->
             Web_server.listen
               ~env
               ~sw
               ~addr
               ~port
               ~root
               ~websockets:
                 [ "/ws", Web_server.serve_rpc router
                 ; "/terminal", Web_server.serve_terminal router terminals
                 ]
               ~on_lines:(Rpc_router.serve_lines router)
               ())
         in
         let host =
           match Format.asprintf "%a" Eio.Net.Ipaddr.pp addr with
           | "0.0.0.0" -> "127.0.0.1"
           | "::" -> "::1"
           | host -> host
         in
         let url = Web_server.browser_url ~host ~port in
         eprintf "prigh: prigh-web on %s\n%!" url;
         (match root with
          | Some root -> eprintf "prigh: serving prigh-web from %s\n%!" root
          | None ->
            eprintf
              "prigh: no prigh-web site found (-web-root or $PRIGH_WEB_ROOT); \
               only WebSockets are served\n\
               %!");
         if open_browser then open_in_browser url);
       if stdio
       then (
         Rpc_router.serve_connection
           ~finish_requests:true
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
           "HOST:PORT|URL connect to a backend's JSON-lines port (-listen or \
            -web), or to its prigh-web URL (wss://host/ws behind a TLS proxy), \
            and serve it, reconnecting when the connection drops"
     and token =
       flag
         "-token"
         (optional string)
         ~doc:"SECRET the backend's token (default: $PRIGH_TOKEN)"
     and user =
       flag
         "-user"
         (optional string)
         ~doc:
           "NAME the user (namespace) name to log in as, with -token as its \
            password (default: $PRIGH_USER)"
     and name =
       flag
         "-name"
         (optional string)
         ~doc:"NAME how the host is shown (default: the hostname)"
     and host_id =
       flag
         "-host-id"
         (optional string)
         ~doc:
           "ID what sessions know this host by, so they resume on it when it \
            reconnects or restarts (default: the machine's, kept in \
            ~/.prigh/host-id and shared with the TUI)"
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
         let backend =
           match Backend_address.parse spec with
           | Ok backend -> backend
           | Error e ->
             eprintf "-connect: %s\n" (Error.to_string_hum e);
             exit 2
         in
         let cwd =
           match cwd with
           | Some dir -> Filename_unix.realpath dir
           | None -> Core_unix.getcwd ()
         in
         let token = Option.first_some token (Sys.getenv "PRIGH_TOKEN") in
         let user = Option.first_some user (Sys.getenv "PRIGH_USER") in
         let host_id =
           Host_id.choose
             ?given:host_id
             ~home:(home ())
             ~warn:(eprintf "%s\n%!")
             ()
         in
         (* Not for the tools it runs. *)
         List.iter [ "PRIGH_TOKEN"; "PRIGH_USER" ] ~f:Core_unix.unsetenv;
         let user_given = Option.is_some user in
         let token_given = Option.is_some token in
         let refused =
           Eio_main.run
           @@ fun env ->
           Tool_host.connect
             ~env
             ~backend
             ~token
             ?user
             ~host_id
             ~name:(Option.value name ~default:(Core_unix.gethostname ()))
             ~cwd
             ()
         in
         eprintf
           "prigh tool-host: %s\n%s\n"
           (Error.to_string_hum refused)
           (match token_given, user_given with
            | false, _ ->
              "No token was given: pass the backend's with -token SECRET (or \
               $PRIGH_TOKEN)."
            | true, false ->
              "Check -token (or $PRIGH_TOKEN) against the backend's; if it \
               serves several users (-tokens), also give -user NAME (or \
               $PRIGH_USER)."
            | true, true ->
              "Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) \
               against the backend's -tokens NAME=TOKEN (or -host-tokens) \
               entries.");
         exit 1)
;;

let web_command =
  Command.basic
    ~summary:
      "Serve prigh-web on this machine for a backend elsewhere (no backend \
       runs here), signing the page in and running its tools here"
    (let%map_open.Command connect =
       flag
         "-connect"
         (optional string)
         ~doc:
           "HOST:PORT|URL the backend: its -web (or -listen) port, or its \
            prigh-web URL (wss://host/ws, https://host/prigh/) behind a TLS \
            proxy (default: $PRIGH_CONNECT, else the connection saved by \
            prigh-tui or an earlier prigh web; -connect is saved)"
     and (_ : bool) =
       flag
         "-remote"
         no_arg
         ~doc:
           " the saved connection (or $PRIGH_CONNECT), as without -connect \
            (what makes ./prigh -web run this)"
     and token =
       flag
         "-token"
         (optional string)
         ~doc:
           "SECRET the backend's token (default: $PRIGH_TOKEN, else the saved \
            one)"
     and user =
       flag
         "-user"
         (optional string)
         ~doc:"NAME the user to sign in as (default: $PRIGH_USER, else saved)"
     and tools =
       flag
         "-tools"
         (optional string)
         ~doc:
           "local|remote run sessions' tools on this machine (local, the \
            default: a tool host connects to the backend) or on the backend"
     and name =
       flag
         "-name"
         (optional string)
         ~doc:"NAME how this machine's tool host is shown (default: hostname)"
     and host_id =
       flag
         "-host-id"
         (optional string)
         ~doc:"ID the tool host's id (default: the machine's, ~/.prigh/host-id)"
     and cwd =
       flag
         "-cwd"
         (optional string)
         ~doc:"DIR the tool host's directory (default: the current one)"
     and listen =
       flag
         "-listen"
         (optional string)
         ~doc:
           "HOST:PORT serve the site there (default: $PRIGH_WEB_LISTEN, else \
            127.0.0.1:7790 or the next free port)"
     and web_root =
       flag
         "-web-root"
         (optional string)
         ~doc:
           "DIR the built prigh-web site (default: $PRIGH_WEB_ROOT or the dune \
            build)"
     and open_browser =
       flag "-open" no_arg ~doc:" open the page in a browser"
     in
     fun () ->
       let home = home () in
       let fail ?(code = 2) fmt =
         ksprintf
           (fun message ->
              eprintf "prigh web: %s\n%!" message;
              exit code)
           fmt
       in
       let saved_file = Saved_connection.file ~home in
       let saved =
         match Saved_connection.load ~home with
         | Ok saved -> saved
         | Error e ->
           fail
             "cannot read the saved connection: %s\n\
              Fix or delete %s, or pass -connect HOST:PORT."
             (Error.to_string_hum e)
             saved_file
       in
       let getenv var =
         Option.filter (Sys.getenv var) ~f:(Fn.non String.is_empty)
       in
       let env_settings =
         { Saved_connection.Settings.connect = getenv "PRIGH_CONNECT"
         ; token = getenv "PRIGH_TOKEN"
         ; user = getenv "PRIGH_USER"
         }
       in
       let { Saved_connection.Resolved.settings; save } =
         Saved_connection.resolve
           ~saved
           ~flags:
             { connect = Option.map connect ~f:Backend_address.normalize
             ; token
             ; user
             }
           ~env:
             { env_settings with
               connect =
                 Option.map env_settings.connect ~f:Backend_address.normalize
             }
           ~local:false
       in
       let address =
         match settings.connect with
         | Some address -> address
         | None ->
           fail
             "no backend to connect to: pass -connect HOST:PORT (or a \
              prigh-web URL such as wss://host/ws), which is saved for next \
              time.\n\
              For prigh-web with a backend on this machine, run prigh serve \
              -web 127.0.0.1:7790 (./prigh -web without -connect or -remote)."
       in
       let source : Web_connect.Check.Source.t =
         if Option.is_some connect
         then Flag
         else if Option.is_some env_settings.connect
         then Env
         else Saved saved_file
       in
       let remote =
         match Backend_address.parse address with
         | Ok remote -> remote
         | Error e ->
           fail
             "-connect: %s%s"
             (Error.to_string_hum e)
             (match source with
              | Flag -> ""
              | Env -> " (from $PRIGH_CONNECT)"
              | Saved file -> sprintf " (saved in %s)" file)
       in
       let tools =
         match Option.map tools ~f:Web_connect.Tools.of_string with
         | None -> Web_connect.Tools.Local
         | Some (Ok tools) -> tools
         | Some (Error e) -> fail "%s" (Error.to_string_hum e)
       in
       let root =
         match Option.first_some web_root (find_web_root ()) with
         | Some root when Sys_unix.is_directory_exn root -> root
         | Some root ->
           fail
             "no prigh-web site in %s: build it (cd frontend && dune build \
              ./web/bin/site) or pass -web-root DIR"
             root
         | None ->
           fail
             "no prigh-web site found: build it (cd frontend && dune build \
              ./web/bin/site), or pass -web-root DIR (or $PRIGH_WEB_ROOT)"
       in
       let listen : Web_connect.Listen.t =
         match listen, getenv "PRIGH_WEB_LISTEN" with
         | Some spec, _ ->
           let addr, port = parse_listen_addr ~flag:"-listen" spec in
           At (addr, port)
         | None, Some spec ->
           let addr, port = parse_listen_addr ~flag:"$PRIGH_WEB_LISTEN" spec in
           At (addr, port)
         | None, None -> From (Eio.Net.Ipaddr.V4.loopback, 7790)
       in
       let cwd =
         match cwd with
         | None -> Core_unix.getcwd ()
         | Some dir when Sys_unix.is_directory_exn dir ->
           Filename_unix.realpath dir
         | Some dir -> fail "-cwd %s: no such directory (mkdir -p it first)" dir
       in
       let host_id =
         Host_id.choose ?given:host_id ~home ~warn:(eprintf "%s\n%!") ()
       in
       let credentials =
         { Web_connect.Credentials.token = settings.token
         ; user = settings.user
         }
       in
       (* Not for the tools the tool host runs. *)
       List.iter
         [ "PRIGH_TOKEN"; "PRIGH_USER"; "PRIGH_CONNECT" ]
         ~f:Core_unix.unsetenv;
       Eio_main.run
       @@ fun env ->
       Eio.Switch.run
       @@ fun sw ->
       (match Web_connect.Check.run ~env remote credentials with
        | Ok () -> ()
        | Error error ->
          fail
            ~code:1
            "%s"
            (Web_connect.Check.explain error remote ~source credentials));
       Option.iter save ~f:(fun connection ->
         match Saved_connection.save ~home connection with
         | Ok () -> ()
         | Error e ->
           eprintf
             "prigh web: cannot save the connection: %s\n%!"
             (Error.to_string_hum e));
       let log line = eprintf "prigh web: %s\n%!" line in
       let listen_addr =
         match listen with
         | At (addr, port) | From (addr, port) -> addr, port
       in
       let started =
         listen_or_exit ~flag:"-listen" listen_addr (fun () ->
           Web_connect.start
             ~env
             ~sw
             ~log
             { remote
             ; credentials
             ; tools
             ; name = Option.value name ~default:(Core_unix.gethostname ())
             ; host_id
             ; cwd
             ; listen
             ; root
             })
       in
       log
         (sprintf
            "prigh-web for %s on %s"
            (Backend_address.to_string remote)
            started.url);
       (match started.tool_host with
        | `Connected ->
          log
            (sprintf
               "tools run on this machine, in %s (-tools remote: on the \
                backend)"
               cwd)
        | `Connecting ->
          log
            "the tool host is still connecting: tools run on this machine once \
             it is"
        | `Refused _ -> ()
        | `Off -> log "tools run on the backend (-tools remote)");
       log "the link signs the page in once; Ctrl+C stops";
       if open_browser then open_in_browser started.url;
       Eio.Fiber.await_cancel ())
;;

(* The CLI's view of the legacy (non-namespace) world. *)
let with_registry ~store f =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let models =
    Model_registry.create
      ~env
      ~sw
      ~auto_fetch:false
      ~home:(home ())
      ~store
      ~getenv:Sys.getenv
      ()
  in
  List.iter (Model_registry.problems models) ~f:(eprintf "%s\n%!");
  f ~env ~sw models
;;

let login_command =
  Command.basic
    ~summary:
      "Log in to a provider (anthropic, openai, openai-codex, deepseek, a \
       custom one), or add a custom OpenAI-compatible provider (custom)"
    (let%map_open.Command store = auth_file_flag
     and provider = anon ("PROVIDER" %: string)
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
       with_registry ~store
       @@ fun ~env ~sw models ->
       let interaction =
         Auth_terminal.create ~env ~sw ~open_urls:(not no_browser) ()
       in
       let custom ?name () =
         match
           Custom_login.login
             ~env
             ~models
             ~store
             ~getenv:Sys.getenv
             ?name
             interaction
         with
         | Ok p ->
           eprintf
             "Saved %s (%s, %s) to %s; its models are %s/<model id> (prigh \
              models lists them)\n"
             p.name
             p.base_url
             (Custom_provider.Api.to_string p.api)
             (Config.path ~home:(home ()))
             p.name
         | Error e ->
           eprintf "login failed: %s\n" (Error.to_string_hum e);
           exit 1
       in
       match provider with
       | "custom" -> custom ()
       | provider ->
         (match provider_of_arg models provider with
          | Custom name -> custom ~name ()
          | provider ->
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
            (match
               Provider_auth.login ~env store provider method_ interaction
             with
             | Ok () ->
               eprintf
                 "Logged in to %s (%s); saved to %s\n"
                 (Provider_id.display_name provider)
                 (Provider_auth.Method.label provider method_)
                 (Auth_store.path store)
             | Error e ->
               eprintf "login failed: %s\n" (Error.to_string_hum e);
               exit 1)))
;;

let logout_command =
  Command.basic
    ~summary:
      "Remove a provider's stored credential (and, for a custom one, maybe the \
       provider)"
    (let%map_open.Command store = auth_file_flag
     and provider = anon ("PROVIDER" %: string) in
     fun () ->
       with_registry ~store
       @@ fun ~env ~sw models ->
       let provider = provider_of_arg models provider in
       let result =
         match provider with
         | Custom name ->
           Or_error.map
             (Custom_login.logout
                ~models
                ~store
                name
                (Auth_terminal.create ~env ~sw ()))
             ~f:(function
               | Key_removed -> sprintf "Removed %s's API key" name
               | Provider_removed -> sprintf "Removed %s" name
               | Kept -> sprintf "Kept %s" name)
         | provider ->
           Or_error.map (Provider_auth.logout store provider) ~f:(fun () ->
             sprintf "Logged out of %s" (Provider_id.display_name provider))
       in
       match result with
       | Ok message -> eprintf "%s\n" message
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1)
;;

let auth_command =
  Command.basic
    ~summary:"Show which providers are configured"
    (let%map_open.Command store = auth_file_flag in
     fun () ->
       with_registry ~store
       @@ fun ~env:_ ~sw:_ models ->
       match
         Provider_auth.status ~custom:(Model_registry.providers models) store
       with
       | Error e ->
         eprintf "%s\n" (Error.to_string_hum e);
         exit 1
       | Ok statuses ->
         List.iter statuses ~f:(fun s ->
           printf
             "%-14s %-24s %s\n"
             (Provider_id.to_string s.provider)
             (match s.custom with
              | None -> Provider_id.display_name s.provider
              | Some c ->
                sprintf
                  "%s (%s)"
                  c.base_url
                  (Custom_provider.Api.to_string c.api))
             (match s.configured with
              | None ->
                sprintf
                  "not configured (login: %s)"
                  (String.concat
                     ~sep:", "
                     (List.map s.methods ~f:Provider_auth.Method.to_string))
              | Some (_, source) -> source)))
;;

let models_command =
  Command.basic
    ~summary:"List the models (custom providers' lists are fetched first)"
    (let%map_open.Command store = auth_file_flag
     and provider =
       flag
         "-provider"
         (optional string)
         ~doc:"NAME only this provider's models"
     in
     fun () ->
       with_registry ~store
       @@ fun ~env:_ ~sw:_ models ->
       Option.iter provider ~f:(fun p ->
         ignore (provider_of_arg models p : Provider_id.t));
       let before = Model_registry.problems models in
       Model_registry.refresh models ();
       List.iter (Model_registry.problems models) ~f:(fun p ->
         if not (List.mem before p ~equal:String.equal) then eprintf "%s\n%!" p);
       List.iter (Model_registry.models models) ~f:(fun (m : Model.t) ->
         if
           Option.for_all
             provider
             ~f:(String.equal (Provider_id.to_string m.provider))
         then printf "%-50s %s\n" (Model.key m) m.name))
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

(* Archived sessions come last under their own heading, as in the
   frontends' pickers. Without a server, no session counts as active. *)
let sessions_list_command =
  Command.basic
    ~summary:
      "List saved sessions, the archived ones (by hand, or idle for the \
       config's archive_after_days) last"
    (Command.Param.return (fun () ->
       let archive_after_days =
         match Config.load ~home:(home ()) with
         | Ok config -> config.archive_after_days
         | Error e ->
           eprintf
             "%s; using the default archive_after_days\n"
             (Error.to_string_hum e);
           Config.default.archive_after_days
       in
       let now = Core_unix.gettimeofday () in
       let archived, current =
         List.partition_tf
           (Session.list ~dir:(sessions_dir ()))
           ~f:(fun s ->
             Session.Summary.is_archived
               s
               ~now
               ~archive_after_days
               ~active:false)
       in
       List.iter current ~f:print_summary;
       if not (List.is_empty archived)
       then (
         printf "archived:\n";
         List.iter archived ~f:print_summary)))
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
           match Session.find_summary all id with
           | Ok s -> First s
           | Error e -> Second (Error.to_string_hum e))
       in
       List.iter missing ~f:(eprintf "%s\n");
       if not (List.is_empty missing)
       then (
         eprintf
           "Nothing deleted; prigh sessions list shows the saved sessions\n";
         exit 1);
       delete_sessions found)
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
       ; "web", web_command
       ; "sessions", sessions_command
       ; "login", login_command
       ; "logout", logout_command
       ; "auth", auth_command
       ; "models", models_command
       ])
;;
