open! Core
open! Prigh

(* Inline tests run in the library's build directory. *)
let fake_server =
  Filename.concat (Sys_unix.getcwd ()) "fake_mcp_server/fake_mcp_server.exe"
;;

let run f =
  Eio_main.run
  @@ fun env ->
  let dir = Filename_unix.realpath (Filename_unix.temp_dir "prigh-mcp" "") in
  Exn.protect
    ~f:(fun () -> Eio.Switch.run (fun sw -> f ~env ~sw ~dir))
    ~finally:(fun () ->
      ignore (Sys_unix.command (sprintf "rm -rf %s" (Filename.quote dir)) : int))
;;

let log_file ~dir = Filename.concat dir "server.log"

let log ~dir =
  match In_channel.read_lines (log_file ~dir) with
  | lines -> lines
  | exception _ -> []
;;

let stdio_server ?(args = []) ?(env = []) ?(name = "fake") ~dir () =
  { Mcp_config.Server.name
  ; source = Filename.concat dir ".mcp.json"
  ; project = false
  ; dir
  ; transport =
      Stdio
        { command = fake_server
        ; args
        ; env = ("FAKE_MCP_LOG", log_file ~dir) :: env
        }
  ; approval = ""
  }
;;

let mask ~dir s =
  String.substr_replace_all s ~pattern:dir ~with_:"$DIR"
  |> String.substr_replace_all ~pattern:fake_server ~with_:"$FAKE_SERVER"
  |> String.substr_replace_all ~pattern:Version.to_string ~with_:"$VERSION"
;;

let print_log ~dir =
  List.iter (log ~dir) ~f:(fun line -> print_endline (mask ~dir line))
;;

let rec mask_sexp ~dir : Sexp.t -> Sexp.t = function
  | Atom atom -> Atom (mask ~dir atom)
  | List items -> List (List.map items ~f:(mask_sexp ~dir))
;;

let print_s_masked ~dir sexp = print_s (mask_sexp ~dir sexp)

let print_result ~dir ({ text; is_error; images } : Tool_result.t) =
  print_endline (mask ~dir (if is_error then "ERROR: " ^ text else text));
  List.iter images ~f:(fun image -> print_s [%sexp "image", (image : Image.t)])
;;

(* Polls until [f] holds, so tests can wait for a server to act. *)
let wait_until ~env f =
  let rec loop n =
    if f ()
    then ()
    else if n = 0
    then failwith "timed out waiting"
    else (
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
      loop (n - 1))
  in
  loop 1000
;;
