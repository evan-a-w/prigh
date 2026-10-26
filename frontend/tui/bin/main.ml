open! Core
open! Async

let find_backend () =
  match Sys.getenv "PRIGH_BACKEND" with
  | Some path -> path
  | None ->
    let exe = Core_unix.readlink "/proc/self/exe" in
    let candidates =
      List.map
        [ "../../../../../backend/_build/default/bin/main.exe"
        ; "../../backend/_build/default/bin/main.exe"
        ]
        ~f:(fun rel -> Filename.concat (Filename.dirname exe) rel)
    in
    List.find candidates ~f:Sys_unix.file_exists_exn
    |> Option.value ~default:"prigh"
;;

(* The tmux subcommands' output, or exits when tmux cannot be run. *)
let run_tmux tmux args =
  match%bind Process.create ~prog:tmux ~args () with
  | Ok process -> Process.collect_output_and_wait process
  | Error e ->
    let reason =
      match Error.to_exn e with
      | Core_unix.Unix_error (e, _, _) -> Core_unix.Error.message e
      | _ -> Error.to_string_hum e
    in
    if String.equal tmux "tmux"
    then
      Core.eprintf
        "cannot run tmux (%s): install tmux, or set PRIGH_TMUX to its path\n"
        reason
    else
      Core.eprintf
        "cannot run $PRIGH_TMUX (%s: %s): set it to a tmux executable, or \
         unset it to use tmux from PATH\n"
        tmux
        reason;
    Core.exit 1
;;

let detach ?name ~dir command =
  let module D = Prigh_tui.Tmux_detach in
  let tmux = D.tmux ~prigh_tmux:(Sys.getenv "PRIGH_TMUX") in
  let%bind sessions =
    let%map output =
      run_tmux tmux [ "list-sessions"; "-F"; "#{session_name}" ]
    in
    (* Fails when no server runs: there are no sessions. *)
    match output.exit_status with
    | Ok () -> String.split_lines output.stdout
    | Error _ -> []
  in
  let t =
    D.create
      ~tmux
      ~exists:(List.mem sessions ~equal:String.equal)
      ?name
      ~environment:(Array.to_list (Core_unix.environment ()))
      ~dir
      ~command
      ()
  in
  let%map output = run_tmux tmux t.args in
  match output.exit_status with
  | Ok () ->
    print_endline
      (D.message t ~inside_tmux:(Option.is_some (Sys.getenv "TMUX")));
    Ok ()
  | Error _ ->
    Core.eprintf
      "tmux could not start the session %s: %s\n"
      t.session
      (String.strip output.stderr);
    Core.exit 1
;;

let command =
  Command.async_or_error
    ~summary:
      "prigh terminal UI (spawns the backend with `serve`, or connects to one)"
    (let%map_open.Command backend =
       flag
         "-backend"
         (optional string)
         ~doc:
           "PATH backend executable (default: $PRIGH_BACKEND or the dune build)"
     and connect =
       flag
         "-connect"
         (optional string)
         ~doc:
           "ADDRESS connect to a running backend instead of spawning one: \
            HOST:PORT of `prigh serve -listen`, or a prigh-web URL \
            (wss://host/ws, https://host/, ws://, http://), e.g. behind an \
            HTTPS proxy (default: $PRIGH_CONNECT, else the saved connection); \
            saved for next time with -token and -user"
     and headers =
       flag
         "-header"
         (listed string)
         ~doc:
           "'NAME: VALUE' send this header with a prigh-web URL's WebSocket \
            upgrade, e.g. a proxy's credentials (repeatable; default: \
            $PRIGH_CONNECT_HEADERS, one per line; not saved)"
     and token =
       flag
         "-token"
         (optional string)
         ~doc:
           "SECRET the backend's -token, or with -tokens the user's password \
            (default: $PRIGH_TOKEN, else the saved one)"
     and user =
       flag
         "-user"
         (optional string)
         ~doc:
           "NAME the user (namespace) name of a backend with -tokens (default: \
            $PRIGH_USER, else the saved one)"
     and local =
       flag
         "-local"
         no_arg
         ~doc:" spawn a backend even if a connection is saved (keeps it)"
     and forget =
       flag
         "-forget"
         no_arg
         ~doc:" delete the saved connection (~/.prigh/tui-connection.sexp)"
     and tools =
       flag
         "-tools"
         (optional string)
         ~doc:
           "local|remote where this session's tools run: on this machine \
            (local, the default with -connect) or on the backend (remote, the \
            default when spawning)"
     and name =
       flag
         "-name"
         (optional string)
         ~doc:"NAME how this frontend appears in /host (default: hostname)"
     and faux = flag "-faux" no_arg ~doc:" scripted provider, no API calls"
     and session =
       flag "-session" (optional string) ~doc:"PATH resume a session file"
     and model = flag "-model" (optional string) ~doc:"ID model id"
     and thinking =
       flag "-thinking" (optional string) ~doc:"LEVEL off|on|low|high|max"
     and cwd_flag =
       flag
         "-cwd"
         (optional string)
         ~doc:"DIR working directory (same as the DIR argument)"
     and auth_file =
       flag "-auth-file" (optional string) ~doc:"PATH credential store"
     and detached =
       flag
         "-detached"
         no_arg
         ~doc:
           " start in a new detached tmux session (prigh-DIR; $PRIGH_TMUX or \
            tmux) and return at once"
     and tmux_session =
       flag
         "-tmux-session"
         (optional string)
         ~doc:"NAME the tmux session's name with -detached (default: prigh-DIR)"
     and rest = flag "--" escape ~doc:" extra backend arguments"
     and cwd_anon = anon (maybe ("DIR" %: string)) in
     fun () ->
       let cwd =
         match cwd_flag, cwd_anon with
         | Some a, Some b when not (String.equal a b) ->
           Core.eprintf
             "the working directory is given twice (-cwd %s and %s): pass one\n"
             a
             b;
           Core.exit 2
         | cwd, None | None, cwd -> cwd
         | Some _, Some _ -> cwd_flag
       in
       let opt name v =
         Option.value_map v ~default:[] ~f:(fun v -> [ name; v ])
       in
       if Option.is_some tmux_session && not detached
       then (
         Core.eprintf
           "-tmux-session names the session of -detached: pass both\n";
         Core.exit 2);
       let cwd =
         Option.map cwd ~f:(fun d ->
           match Filename_unix.realpath d with
           | path when Sys_unix.is_directory_exn path -> path
           | (_ : string) | (exception Core_unix.Unix_error _) ->
             Core.eprintf "no such directory: %s\n" d;
             Core.exit 2)
       in
       (* The child gets the flags as given (it resolves and saves the
          connection itself), with paths made absolute: it starts in [cwd]. *)
       let detached_args =
         let absolute path =
           if Filename.is_relative path
           then Filename.concat (Core_unix.getcwd ()) path
           else path
         in
         let path_like v = if String.mem v '/' then absolute v else v in
         let switch name b = if b then [ name ] else [] in
         opt "-backend" (Option.map backend ~f:path_like)
         @ opt "-connect" connect
         @ List.concat_map headers ~f:(fun h -> [ "-header"; h ])
         @ opt "-token" token
         @ opt "-user" user
         @ switch "-local" local
         @ switch "-forget" forget
         @ opt "-tools" tools
         @ opt "-name" name
         @ switch "-faux" faux
         @ opt "-session" (Option.map session ~f:path_like)
         @ opt "-model" model
         @ opt "-thinking" thinking
         @ opt "-auth-file" (Option.map auth_file ~f:absolute)
         @ Option.value_map rest ~default:[] ~f:(fun rest -> "--" :: rest)
       in
       let backend = Option.value backend ~default:(find_backend ()) in
       let local = local || faux in
       if local && Option.is_some connect
       then (
         Core.eprintf "-connect cannot be used with -local or -faux: pass one\n";
         Core.exit 2);
       let home = Sys.getenv "HOME" |> Option.value ~default:"/" in
       let module S = Prigh_client_unix.Saved_connection in
       let or_exit what = function
         | Ok v -> v
         | Error e ->
           Core.eprintf
             "cannot %s the saved connection: %s\n\
              fix or delete it (prigh-tui -forget)\n"
             what
             (Error.to_string_hum e);
           Core.exit 1
       in
       (* With -detached the child deletes and saves. *)
       if forget && not detached then or_exit "delete" (S.forget ~home);
       (* With -local it is not used, only kept for /connect. *)
       let saved =
         if forget
         then None
         else if local
         then Option.join (Result.ok (S.load ~home))
         else or_exit "read" (S.load ~home)
       in
       let { S.Resolved.settings = { connect; token; user }; save } =
         S.resolve
           ~saved
           ~flags:
             { connect = Option.map connect ~f:Prigh_ui.Server_address.normalize
             ; token
             ; user
             }
           ~env:
             { connect =
                 Option.map
                   (Sys.getenv "PRIGH_CONNECT")
                   ~f:Prigh_ui.Server_address.normalize
             ; token = Sys.getenv "PRIGH_TOKEN"
             ; user = Sys.getenv "PRIGH_USER"
             }
           ~local
       in
       Option.iter connect ~f:(fun address ->
         match Prigh_ui.Server_address.parse address with
         | Ok _ -> ()
         | Error e ->
           Core.eprintf "-connect: %s\n" e;
           Core.exit 2);
       let headers =
         (match headers with
          | [] ->
            Option.value_map
              (Sys.getenv "PRIGH_CONNECT_HEADERS")
              ~default:[]
              ~f:(String.split_lines)
            |> List.filter ~f:(Fn.non (String.for_all ~f:Char.is_whitespace))
          | headers -> headers)
         |> List.map ~f:(fun header ->
           match Prigh_client_unix.Websocket_transport.parse_header header with
           | Ok header -> header
           | Error e ->
             Core.eprintf "-header: %s\n" e;
             Core.exit 2)
       in
       if not detached
       then Option.iter save ~f:(fun t -> or_exit "save" (S.save ~home t));
       let module Link = Prigh_tui.Backend_link in
       let tools =
         Option.map tools ~f:(fun tools ->
           match Link.Tools.of_string tools with
           | Ok tools -> tools
           | Error e ->
             Core.eprintf "%s\n" (Error.to_string_hum e);
             Core.exit 2)
       in
       if detached
       then (
         let dir = Option.value cwd ~default:(Core_unix.getcwd ()) in
         detach
           ?name:tmux_session
           ~dir
           (Sys_unix.executable_name :: "-cwd" :: dir :: detached_args))
       else (
         (* The hello cwd is where our tools run; for a spawned backend the
            serve arguments already carry it. *)
         let hello =
           [ ( "name"
             , `String (Option.value name ~default:(Core_unix.gethostname ())) )
           ; "cwd", `String (Option.value cwd ~default:(Core_unix.getcwd ()))
           ]
         in
         let spawn_args =
           [ "serve" ]
           @ (if faux then [ "-faux" ] else [])
           @ opt "-model" model
           @ opt "-thinking" thinking
           @ opt "-cwd" cwd
           @ opt "-auth-file" auth_file
           @ Option.value rest ~default:[]
         in
         Prigh_tui.Term_app.run
           (Link.create
              ~headers
              ~home
              ~backend
              ~spawn_args
              ~tools
              ~hello
              ~session
              ~target:
                (match connect with
                 | Some address -> Remote address
                 | None -> Spawn)
              ~token
              ~user
              ~saved:(Option.first_some save saved)
              ())))
;;

let () = Command_unix.run command
