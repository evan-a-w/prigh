open! Core
open! Import

let open_browser ~env ~sw url =
  let candidates =
    match Sys.os_type with
    | "Unix" ->
      (match Core_unix.Utsname.sysname (Core_unix.uname ()) with
       | "Darwin" -> [ "open", [ url ] ]
       | _ -> [ "xdg-open", [ url ] ])
    | _ -> [ "cmd", [ "/c"; "start"; ""; url ] ]
  in
  List.iter candidates ~f:(fun (prog, args) ->
    try
      let proc =
        Eio.Process.spawn
          ~sw
          (Eio.Stdenv.process_mgr env)
          ~stdout:(Eio.Flow.buffer_sink (Buffer.create 16))
          ~stderr:(Eio.Flow.buffer_sink (Buffer.create 16))
          (prog :: args)
      in
      Fiber.fork_daemon ~sw (fun () ->
        ignore (Eio.Process.await proc : Eio.Process.exit_status);
        `Stop_daemon)
    with
    | _ -> ())
;;

let with_echo_off ~f =
  match Core_unix.Terminal_io.tcgetattr Core_unix.stdin with
  | exception _ -> f ()
  | attrs ->
    let quiet = { attrs with Core_unix.Terminal_io.c_echo = false } in
    Core_unix.Terminal_io.tcsetattr quiet Core_unix.stdin ~mode:TCSANOW;
    Exn.protect ~f ~finally:(fun () ->
      Core_unix.Terminal_io.tcsetattr attrs Core_unix.stdin ~mode:TCSANOW)
;;

let create ~env ~sw ?(open_urls = true) () : Auth_interaction.t =
  let reader =
    lazy (Eio.Buf_read.of_flow (Eio.Stdenv.stdin env) ~max_size:(1024 * 1024))
  in
  let say fmt = ksprintf (fun s -> eprintf "%s\n%!" s) fmt in
  let read_line () =
    match Eio.Buf_read.line (force reader) with
    | exception End_of_file -> None
    | line -> Some line
  in
  let prompt (p : Auth_interaction.Prompt.t) =
    let answer =
      match p with
      | Secret { message } ->
        eprintf "%s: %!" message;
        let line = with_echo_off ~f:read_line in
        eprintf "\n%!";
        line
      | Manual_code { message; placeholder } ->
        say "%s" message;
        eprintf "(%s) > %!" placeholder;
        read_line ()
      | Select { message; options } ->
        say "%s" message;
        List.iteri options ~f:(fun i (_, label) -> say "  %d. %s" (i + 1) label);
        eprintf "> %!";
        Option.bind (read_line ()) ~f:(fun line ->
          let line = String.strip line in
          match Int.of_string_opt line with
          | Some n when n >= 1 && n <= List.length options ->
            Some (fst (List.nth_exn options (n - 1)))
          | _ -> Some line)
    in
    match answer with
    | None -> Auth_interaction.cancelled ()
    | Some a -> Ok a
  in
  let notify : Auth_interaction.Notice.t -> unit = function
    | Auth_url { url; instructions } ->
      say "Open this URL to log in:\n\n  %s\n\n%s" url instructions;
      if open_urls then open_browser ~env ~sw url
    | Progress message -> say "%s" message
  in
  { prompt; notify; cancel = Cancellation.create () }
;;
