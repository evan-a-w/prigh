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
            spawning one (default: $PRIGH_CONNECT)"
     and token =
       flag
         "-token"
         (optional string)
         ~doc:"SECRET the backend's -token (default: $PRIGH_TOKEN)"
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
     and cwd = flag "-cwd" (optional string) ~doc:"DIR working directory"
     and auth_file =
       flag "-auth-file" (optional string) ~doc:"PATH credential store"
     and rest = flag "--" escape ~doc:" extra backend arguments" in
     fun () ->
       let opt name v =
         Option.value_map v ~default:[] ~f:(fun v -> [ name; v ])
       in
       let backend = Option.value backend ~default:(find_backend ()) in
       let connect =
         match connect with
         | Some c -> Some c
         | None -> Sys.getenv "PRIGH_CONNECT"
       in
       let token =
         match token with
         | Some t -> Some t
         | None -> Sys.getenv "PRIGH_TOKEN"
       in
       let local_tools =
         match tools, connect with
         | Some "local", _ | None, Some _ -> Some backend
         | Some "remote", _ | None, None -> None
         | Some other, _ ->
           eprintf "-tools must be local or remote, got %s\n" other;
           Core.exit 2
       in
       let cwd =
         Option.map cwd ~f:(fun d ->
           if Filename.is_absolute d
           then d
           else Filename.concat (Core_unix.getcwd ()) d)
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
         @ Option.value_map session ~default:[] ~f:(fun s ->
           [ "session", `String s ])
       in
       match connect with
       | Some addr ->
         let host, port =
           match String.rsplit2 addr ~on:':' with
           | Some (host, port) ->
             (match Int.of_string_opt port with
              | Some port ->
                (if String.is_empty host then "127.0.0.1" else host), port
              | None ->
                eprintf "-connect must be HOST:PORT, got %s\n" addr;
                Core.exit 2)
           | None ->
             eprintf "-connect must be HOST:PORT, got %s\n" addr;
             Core.exit 2
         in
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
