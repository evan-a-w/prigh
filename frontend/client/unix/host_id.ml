open! Core

let file ~home = home ^/ ".prigh" ^/ "host-id"

let generate () =
  let state = Random.State.make_self_init ~allow_in_tests:true () in
  "host-"
  ^ String.init 16 ~f:(fun _ -> "0123456789abcdef".[Random.State.int state 16])
;;

let read path =
  match In_channel.read_all path with
  | exception Sys_error _ -> None
  | contents ->
    let id = String.strip contents in
    if String.is_empty id then None else Some id
;;

(* Written to a temporary file and linked into place, so the file is never
   seen half-written and a process that loses the race reads the winner's. *)
let load_or_create ~home =
  let path = file ~home in
  match read path with
  | Some id -> Ok id
  | None ->
    Or_error.try_with (fun () ->
      Core_unix.mkdir_p (Filename.dirname path);
      let id = generate () in
      let tmp =
        sprintf "%s.%s.tmp" path (Pid.to_string (Core_unix.getpid ()))
      in
      Out_channel.write_all tmp ~data:(id ^ "\n");
      Exn.protect
        ~finally:(fun () ->
          try Core_unix.unlink tmp with
          | Core_unix.Unix_error _ -> ())
        ~f:(fun () ->
          match Core_unix.link ~target:tmp ~link_name:path () with
          | () -> id
          | exception Core_unix.Unix_error (EEXIST, _, _) ->
            (match read path with
             | Some id -> id
             | None ->
               Core_unix.rename ~src:tmp ~dst:path;
               id)))
;;

let choose ~home ~warn =
  match load_or_create ~home with
  | Ok id -> id
  | Error e ->
    warn
      (sprintf
         "cannot keep this machine's tool host id in %s (%s): sessions on it \
          will not follow it across restarts"
         (file ~home)
         (Error.to_string_hum e));
    generate ()
;;
