open! Core
open! Import

let replay_lines = 2000

module Socket = struct
  type t =
    | Name of string
    | Path of string

  let args = function
    | Name name -> [ "-L"; name ]
    | Path path -> [ "-S"; path ]
  ;;
end

module Viewer = struct
  type t =
    { on_output : string -> unit
    ; mutable receiving : bool
    ; mutable detached : bool
    }
end

type t =
  { name : string
  ; stdin : Eio_unix.sink_ty Eio.Resource.t
  ; write_mutex : Eio.Mutex.t
  ; replies : ((string list, string list) Result.t -> unit) Queue.t
  ; mutable viewers : Viewer.t list
  ; mutable alive : bool
  ; mutable stdin_closed : bool
  ; exited : unit Promise.t
  ; set_exited : unit Promise.u
  }

let name t = t.name
let is_alive t = t.alive
let exited t = t.exited
let viewers t = List.length t.viewers

(* Commands are one per line; tmux answers each with one block, in order. *)
let send t commands =
  Eio.Mutex.use_rw ~protect:true t.write_mutex (fun () ->
    if t.alive && not t.stdin_closed
    then (
      List.iter commands ~f:(fun (_, on_reply) ->
        Queue.enqueue t.replies on_reply);
      match
        Eio.Flow.copy_string
          (String.concat (List.map commands ~f:(fun (line, _) -> line ^ "\n")))
          t.stdin
      with
      | () -> ()
      | exception Eio.Io _ -> ()))
;;

let ignore_reply (_ : (string list, string list) Result.t) = ()
let clamp n = Int.clamp_exn n ~min:2 ~max:1000

let resize_command ~cols ~rows =
  sprintf "refresh-client -C %dx%d" (clamp cols) (clamp rows)
;;

let resize t ~cols ~rows = send t [ resize_command ~cols ~rows, ignore_reply ]

let input t data =
  send
    t
    (List.map (Tmux_control.send_keys ~target:t.name data) ~f:(fun line ->
       line, ignore_reply))
;;

let mode_format =
  "#{cursor_x} #{cursor_y} #{alternate_on} #{cursor_flag} \
   #{keypad_cursor_flag} #{mouse_standard_flag} #{mouse_button_flag} \
   #{mouse_any_flag} #{mouse_sgr_flag}"
;;

let replay ~modes ~screen ~saved =
  let rows lines = String.concat ~sep:"\r\n" lines in
  match List.map (String.split modes ~on:' ') ~f:Int.of_string_opt with
  | [ Some x
    ; Some y
    ; Some alternate
    ; Some cursor
    ; Some keypad
    ; Some mouse_standard
    ; Some mouse_button
    ; Some mouse_any
    ; Some mouse_sgr
    ] ->
    let set flag code = if flag = 1 then sprintf "\027[?%dh" code else "" in
    String.concat
      [ (if alternate = 1 then rows saved ^ "\027[?1049h\027[H" else "")
      ; rows screen
      ; "\027[0m"
      ; sprintf "\027[%d;%dH" (y + 1) (x + 1)
      ; (if cursor = 0 then "\027[?25l" else "")
      ; set keypad 1
      ; set mouse_standard 1000
      ; set mouse_button 1002
      ; set mouse_any 1003
      ; set mouse_sgr 1006
      ]
  | _ -> rows screen ^ "\027[0m"
;;

let reply_lines = function
  | Ok lines -> lines
  | Error _ -> []
;;

let attach t ~cols ~rows ~on_output =
  let viewer = { Viewer.on_output; receiving = false; detached = false } in
  t.viewers <- viewer :: t.viewers;
  let modes = ref "" in
  let screen = ref [] in
  let capture flags =
    sprintf "capture-pane -p -e %s-t %s -S -%d" flags t.name replay_lines
  in
  send
    t
    [ resize_command ~cols ~rows, ignore_reply
    ; ( sprintf "display-message -p -t %s '%s'" t.name mode_format
      , fun reply -> modes := String.concat (reply_lines reply) )
    ; (capture "", fun reply -> screen := reply_lines reply)
    ; ( capture "-a -q "
      , fun reply ->
          if not viewer.detached
          then (
            on_output
              (replay ~modes:!modes ~screen:!screen ~saved:(reply_lines reply));
            viewer.receiving <- true) )
    ];
  viewer
;;

let detach t (viewer : Viewer.t) =
  viewer.detached <- true;
  viewer.receiving <- false;
  t.viewers <- List.filter t.viewers ~f:(fun v -> not (phys_equal v viewer))
;;

let finish t =
  if t.alive
  then (
    t.alive <- false;
    Queue.clear t.replies;
    Promise.resolve t.set_exited ())
;;

let handle t (event : Tmux_control.Event.t) =
  match event with
  | Output data ->
    List.iter t.viewers ~f:(fun (v : Viewer.t) ->
      if v.receiving then v.on_output data)
  | Reply reply -> Option.iter (Queue.dequeue t.replies) ~f:(fun f -> f reply)
  | Exit -> finish t
;;

let close_stdin t =
  Eio.Mutex.use_rw ~protect:true t.write_mutex (fun () ->
    if not t.stdin_closed
    then (
      t.stdin_closed <- true;
      try Eio.Flow.close t.stdin with
      | Eio.Io _ -> ()))
;;

let kill t =
  send t [ sprintf "kill-session -t %s" t.name, ignore_reply ];
  close_stdin t;
  Promise.await t.exited
;;

let tmux_args ~socket args = Socket.args socket @ args

let start_session ~env ~tmux ~socket ~name ~cwd ~cols ~rows ~command =
  let args =
    tmux_args
      ~socket
      [ "-f"; "/dev/null"; "start-server"; ";"
      ; "set-option"; "-g"; "default-terminal"; "xterm-256color"; ";"
      ; "set-option"; "-g"; "status"; "off"; ";"
      ; "set-option"; "-g"; "history-limit"; "10000"; ";"
      ; "new-session"; "-d"; "-s"; name
      ; "-x"; Int.to_string (clamp cols); "-y"; Int.to_string (clamp rows)
      ; "-c"; cwd
      ] [@ocamlformat "disable"]
    @ command
  in
  match Process.run_collect ~env ~prog:tmux ~args () with
  | exception Eio.Io (Eio.Process.E (Executable_not_found _), _) ->
    Or_error.errorf "%s not found: install tmux or set PRIGH_TMUX" tmux
  | exception exn ->
    Or_error.errorf "cannot run %s: %s" tmux (Exn.to_string exn)
  | { exit; stderr; _ } ->
    if Process.Exit.is_success exit
    then Ok ()
    else Or_error.errorf "tmux new-session failed: %s" (String.strip stderr)
;;

let read_loop t stdout =
  let parser = Tmux_control.create () in
  let buf = Cstruct.create 65536 in
  (try
     while true do
       let n = Eio.Flow.single_read stdout buf in
       List.iter
         (Tmux_control.feed parser (Cstruct.to_string buf ~len:n))
         ~f:(handle t)
     done
   with
   | End_of_file | Eio.Io _ -> ());
  finish t;
  close_stdin t
;;

let create ~env ~sw ~tmux ~socket ~name ~cwd ~cols ~rows ?(command = []) () =
  Or_error.bind
    (start_session ~env ~tmux ~socket ~name ~cwd ~cols ~rows ~command)
    ~f:(fun () ->
      let started, set_started = Promise.create () in
      (* The terminal's own switch holds the pipes and the client, so they
         are released as soon as it ends. *)
      Fiber.fork ~sw (fun () ->
        Switch.run
        @@ fun sw ->
        let stdin_r, stdin_w = Eio_unix.pipe sw in
        let stdout_r, stdout_w = Eio_unix.pipe sw in
        match
          Eio.Process.spawn
            ~sw
            (Eio.Stdenv.process_mgr env)
            ~stdin:stdin_r
            ~stdout:stdout_w
            (tmux :: tmux_args ~socket [ "-C"; "attach-session"; "-t"; name ])
        with
        | exception exn ->
          Promise.resolve
            set_started
            (Or_error.errorf "cannot attach to tmux: %s" (Exn.to_string exn))
        | child ->
          Eio.Flow.close stdin_r;
          Eio.Flow.close stdout_w;
          let exited, set_exited = Promise.create () in
          let t =
            { name
            ; stdin = stdin_w
            ; write_mutex = Eio.Mutex.create ()
            ; replies = Queue.create ()
            ; viewers = []
            ; alive = true
            ; stdin_closed = false
            ; exited
            ; set_exited
            }
          in
          send
            t
            [ ( sprintf "set-option -t %s destroy-unattached on" name
              , ignore_reply )
            ];
          Promise.resolve set_started (Ok t);
          read_loop t stdout_r;
          ignore (Eio.Process.await child : Eio.Process.exit_status));
      Promise.await started)
;;

module For_testing = struct
  let replay = replay
  let mode_format = mode_format
end
