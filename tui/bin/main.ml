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
    ~summary:"prigh terminal UI (spawns the backend with `serve`)"
    (let%map_open.Command backend =
       flag
         "-backend"
         (optional string)
         ~doc:
           "PATH backend executable (default: $PRIGH_BACKEND or the dune build)"
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
       let backend = Option.value backend ~default:(find_backend ()) in
       Prigh_ui_term.Term_app.run ~backend ~args)
;;

let () = Command_unix.run command
