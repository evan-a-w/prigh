open! Core
open! Async

let find_backend () =
  match Sys.getenv "PRIGH_BACKEND" with
  | Some path -> path
  | None ->
    let exe = Core_unix.readlink "/proc/self/exe" in
    let candidates =
      List.map
        [ "../../../../backend/_build/default/bin/main.exe"
        ; "../../backend/_build/default/bin/main.exe"
        ]
        ~f:(fun rel -> Filename.concat (Filename.dirname exe) rel)
    in
    List.find candidates ~f:Sys_unix.file_exists_exn
    |> Option.value ~default:"prigh"
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
           "HOST:PORT connect to a running `prigh serve -listen` instead of \
            spawning one (default: $PRIGH_CONNECT, else the saved connection); \
            saved for next time with -token and -user"
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
       if forget then or_exit "delete" (S.forget ~home);
       let saved =
         if forget || local then None else or_exit "read" (S.load ~home)
       in
       let { S.Resolved.settings = { connect; token; user }; save } =
         S.resolve
           ~saved
           ~flags:{ connect; token; user }
           ~env:
             { connect = Sys.getenv "PRIGH_CONNECT"
             ; token = Sys.getenv "PRIGH_TOKEN"
             ; user = Sys.getenv "PRIGH_USER"
             }
           ~local
       in
       let address =
         Option.map connect ~f:(fun addr ->
           let bad () =
             Core.eprintf "-connect must be HOST:PORT, got %s\n" addr;
             Core.exit 2
           in
           match String.rsplit2 addr ~on:':' with
           | Some (host, port) ->
             (match Int.of_string_opt port with
              | Some port ->
                (if String.is_empty host then "127.0.0.1" else host), port
              | None -> bad ())
           | None -> bad ())
       in
       Option.iter save ~f:(fun t -> or_exit "save" (S.save ~home t));
       let local_tools =
         match tools, connect with
         | Some "local", _ | None, Some _ -> Some backend
         | Some "remote", _ | None, None -> None
         | Some other, _ ->
           Core.eprintf "-tools must be local or remote, got %s\n" other;
           Core.exit 2
       in
       let cwd =
         Option.map cwd ~f:(fun d ->
           match Filename_unix.realpath d with
           | path when Sys_unix.is_directory_exn path -> path
           | (_ : string) | (exception Core_unix.Unix_error _) ->
             Core.eprintf "no such directory: %s\n" d;
             Core.exit 2)
       in
       (* The hello cwd is where our tools run; for a spawned backend the serve
          arguments already carry it. *)
       let hello_cwd = Option.value cwd ~default:(Core_unix.getcwd ()) in
       let hello =
         [ ( "name"
           , `String (Option.value name ~default:(Core_unix.gethostname ())) )
         ]
         @ [ "cwd", `String hello_cwd ]
         @ Option.value_map token ~default:[] ~f:(fun t ->
           [ "token", `String t ])
         @ Option.value_map user ~default:[] ~f:(fun u -> [ "user", `String u ])
         @ Option.value_map session ~default:[] ~f:(fun s ->
           [ "session", `String s ])
       in
       match address with
       | Some (host, port) ->
         Prigh_ui_term.Term_app.run
           ~connect:(fun () ->
             Prigh_client_unix.Tcp_transport.connect ~host ~port)
           ~hello
           ~local_tools
       | None ->
         let args =
           [ "serve" ]
           @ (if faux then [ "-faux" ] else [])
           @ opt "-session" session
           @ opt "-model" model
           @ opt "-thinking" thinking
           @ opt "-cwd" cwd
           @ opt "-auth-file" auth_file
           @ Option.value rest ~default:[]
         in
         Prigh_ui_term.Term_app.run
           ~connect:(fun () ->
             Prigh_client_unix.Stdio_transport.spawn ~prog:backend ~args ())
           ~hello
           ~local_tools)
;;

let () = Command_unix.run command
