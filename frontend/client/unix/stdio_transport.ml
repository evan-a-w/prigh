open! Core
open! Async
open Prigh_client

let spawn ?env ~prog ~args () =
  match%map Process.create ?env ~prog ~args () with
  | Error _ as e -> e
  | Ok process ->
    let stdin = Process.stdin process in
    let closed = Deferred.ignore_m (Process.wait process) in
    Ok
      { Transport.send_line =
          (fun line ->
            if not (Writer.is_closed stdin) then Writer.write_line stdin line)
      ; lines = Reader.lines (Process.stdout process)
      ; stderr_lines = Reader.lines (Process.stderr process)
      ; close =
          (fun () ->
            don't_wait_for (Writer.close stdin);
            upon
              (Clock.after (Time_float.Span.of_sec 2.))
              (fun () ->
                if not (Deferred.is_determined closed)
                then Signal_unix.send_i Signal.kill (`Pid (Process.pid process))))
      ; closed
      }
;;
