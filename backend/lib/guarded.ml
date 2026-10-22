open! Core
open! Import

let report ~what exn =
  eprintf "prigh: %s failed: %s\n%!" what (Exn.to_string exn)
;;

let run ~what ?(on_error = report ~what) f =
  match f () with
  | () -> ()
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn -> on_error exn
;;

let fork ~sw ~what ?on_error f =
  Fiber.fork ~sw (fun () -> run ~what ?on_error f)
;;

let accept_loop ~sw ~clock ?(retry_after = 0.1) ~on_error socket handle =
  let rec loop () =
    (match Eio.Net.accept_fork ~sw socket ~on_error handle with
     | () -> ()
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn ->
       on_error exn;
       Eio.Time.sleep clock retry_after);
    loop ()
  in
  loop ()
;;
