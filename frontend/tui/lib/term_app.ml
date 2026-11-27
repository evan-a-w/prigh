open! Core
open! Async
open Bonsai_term
open Bonsai.Let_syntax
module App = Prigh_ui.App
module Client = Prigh_client.Client
module Server_link = Prigh_ui.Server_link

module Result_ = struct
  type t =
    { model : App.Model.t
    ; view : View.t
    ; handler : Event.t -> unit Effect.t
    ; inject : App.Action.t -> unit Effect.t
    }
end

let find_on_path name =
  match Sys.getenv "PATH" with
  | None -> None
  | Some path ->
    List.find_map (String.split path ~on:':') ~f:(fun dir ->
      let candidate = Filename.concat dir name in
      let exists =
        match Sys_unix.file_exists candidate with
        | `Yes -> true
        | `No | `Unknown -> false
      in
      Option.some_if exists candidate)
;;

let is_macos () =
  match Sys_unix.file_exists "/System/Library/CoreServices" with
  | `Yes -> true
  | `No | `Unknown -> false
;;

let open_browser url =
  let candidates = if is_macos () then [ "open" ] else [ "xdg-open"; "open" ] in
  match List.find_map candidates ~f:find_on_path with
  | None -> ()
  | Some prog ->
    don't_wait_for
      (Process.run ~prog ~args:[ url ] ()
       |> Deferred.map ~f:(fun (_ : string Or_error.t) -> ()))
;;

(* Direct tty access for the sequences Notty does not manage (alt screen,
   suspend) and OSC 52. Writes go to the controlling terminal. *)
let tty_fd () =
  if Core_unix.isatty Core_unix.stdin then Core_unix.stdin else Core_unix.stdout
;;

let write_tty s = ignore (Core_unix.write_substring (tty_fd ()) ~buf:s : int)

(* Filled in once the app exists so terminal release / focus loss cannot leave
   bracketed paste stuck in [Collecting]. *)
let reset_paste = ref (fun () -> ())

let release_terminal () =
  !reset_paste ();
  write_tty "\027[?2004l";
  write_tty Term_focus.disable;
  write_tty "\027[?1049l";
  ignore (Core_unix.system "stty sane </dev/tty" : Core_unix.Exit_or_signal.t)
;;

(* Notty diffs each frame against the one it believes is on screen, so after the
   alt screen is re-entered we must repaint that frame ourselves. Filled in once
   the driver exists. *)
let repaint = ref (fun () -> ())
let last_cursor : (int * int) option ref = ref None

let reacquire_terminal () =
  ignore
    (Core_unix.system "stty -icanon -echo -isig -ixon -iexten </dev/tty"
     : Core_unix.Exit_or_signal.t);
  write_tty "\027[?1049h";
  write_tty "\027[?2004h";
  write_tty Term_focus.enable;
  !repaint ()
;;

let install_repaint driver =
  repaint
  := fun () ->
       match Driver.prev_view driver with
       | None -> ()
       | Some view ->
         let { Dimensions.width; height } = Driver.dimensions driver in
         let buf = Buffer.create 4096 in
         Buffer.add_string buf "\027[2J\027[H";
         Notty.Render.to_buffer
           buf
           Notty.Cap.ansi
           (0, 0)
           (width, height)
           (View.Private.notty_image view);
         (match !last_cursor with
          | Some (row, col) ->
            Buffer.add_string
              buf
              (sprintf "\027[%d;%dH\027[?25h" (row + 1) (col + 1))
          | None -> ());
         write_tty (Buffer.contents buf)
;;

(* Notty has no api to release and re-acquire the terminal, so we leave the alt
   screen, restore cooked mode and SIGTSTP ourselves; on SIGCONT we put the tty
   back into raw mode, re-enter the alt screen and repaint. *)
let suspend () =
  release_terminal ();
  (* Stop the whole foreground process group, like the tty's ^Z would. *)
  let pid = Core_unix.getpid () in
  let target =
    match Core_unix.getpgid pid with
    | Some group -> `Group group
    | None -> `Pid pid
  in
  Signal_unix.send_i Signal.tstp target;
  reacquire_terminal ()
;;

let history_path () =
  Filename.concat
    (Option.value (Sys.getenv "HOME") ~default:".")
    ".prigh/history"
;;

let load_history () =
  match In_channel.read_lines (history_path ()) with
  | exception _ -> Ok (`Array [])
  | lines ->
    let entries =
      List.filter_map lines ~f:(fun line ->
        match Prigh_protocol.Json.parse line with
        | Ok (`String s) -> Some s
        | _ -> None)
    in
    let kept = List.drop entries (Int.max 0 (List.length entries - 500)) in
    Ok (`Array (List.map kept ~f:(fun s -> `String s)))
;;

let append_history text =
  let path = history_path () in
  (try Core_unix.mkdir_p (Filename.dirname path) with
   | _ -> ());
  Out_channel.with_file path ~append:true ~f:(fun oc ->
    Out_channel.output_string oc (Prigh_protocol.Json.to_string (`String text));
    Out_channel.newline oc)
;;

(* Pause our stdin reader while an external editor owns [/dev/tty] so Async
   does not steal keystrokes while staying responsive for WS heartbeats. *)
let pause_input : unit Ivar.t ref = ref (Ivar.create ())
let resume_input : unit Ivar.t option ref = ref None

let pause_stdin () =
  let resume = Ivar.create () in
  resume_input := Some resume;
  Ivar.fill_if_empty !pause_input ()
;;

let resume_stdin () =
  pause_input := Ivar.create ();
  match !resume_input with
  | None -> ()
  | Some resume ->
    resume_input := None;
    Ivar.fill_if_empty resume ()
;;

let copy_to_clipboard text =
  write_tty (sprintf "\027]52;c;%s\007" (Base64.encode_string text));
  let attempts =
    [ "wl-copy", []; "xclip", [ "-selection"; "clipboard" ]; "pbcopy", [] ]
  in
  match
    List.find_map attempts ~f:(fun (prog, args) ->
      Option.map (find_on_path prog) ~f:(fun _ -> prog, args))
  with
  | None -> ()
  | Some (prog, args) ->
    don't_wait_for
      (Process.run ~prog ~args ~stdin:text ()
       |> Deferred.map ~f:(fun (_ : string Or_error.t) -> ()))
;;

let edit_externally text =
  let tmp = Filename_unix.temp_file "prigh-prompt" ".md" in
  Out_channel.write_all tmp ~data:text;
  let editor =
    match Sys.getenv "VISUAL" with
    | Some v when not (String.is_empty v) -> v
    | _ ->
      (match Sys.getenv "EDITOR" with
       | Some v when not (String.is_empty v) -> v
       | _ -> "vi")
  in
  let cmd =
    sprintf "%s %s </dev/tty >/dev/tty 2>&1" editor (Filename.quote tmp)
  in
  pause_stdin ();
  release_terminal ();
  let%map.Deferred status =
    Monitor.protect
      (fun () -> In_thread.run (fun () -> Core_unix.system cmd))
      ~finally:(fun () ->
        reacquire_terminal ();
        resume_stdin ();
        Deferred.return ())
  in
  let contents = In_channel.read_all tmp in
  (try Core_unix.unlink tmp with
   | _ -> ());
  match status with
  | Ok () -> Ok contents
  | Error _ ->
    Error ("editor failed: " ^ Core_unix.Exit_or_signal.to_string_hum status)
;;

let platform link ~exit ~quit_requested : Prigh_ui.Component.Platform.t =
  let client = Backend_link.client link in
  { rpc =
      (fun method_ params ->
        Effect.of_deferred_fun
          (fun () ->
            Deferred.map
              (Client.call client method_ params)
              ~f:(Result.map_error ~f:Error.to_string_hum))
          ())
  ; open_browser = (fun url -> Effect.of_sync_fun open_browser url)
  ; load_history = (fun () -> Effect.of_sync_fun load_history ())
  ; append_history = (fun text -> Effect.of_sync_fun append_history text)
  ; copy_to_clipboard = (fun text -> Effect.of_sync_fun copy_to_clipboard text)
  ; notify =
      (fun mode notification ->
        Effect.of_sync_fun
          (fun () ->
            write_tty
              (Term_notify.sequence
                 mode
                 ~term:(Sys.getenv "TERM")
                 ~tmux:(Option.is_some (Sys.getenv "TMUX"))
                 notification))
          ())
  ; suspend = Effect.of_sync_fun suspend ()
  ; edit_externally = (fun text -> Effect.of_deferred_fun edit_externally text)
  ; reconnect =
      (fun ~delay_ms ~session ~as_user ->
        Effect.of_deferred_fun
          (fun () -> Backend_link.reconnect link ~delay_ms ~session ~as_user)
          ())
  ; relink =
      (fun change ->
        Effect.of_deferred_fun (fun () -> Backend_link.relink link change) ())
  ; signin_code =
      (fun code -> Effect.of_sync_fun (Backend_link.signin_code link) code)
  ; sign_out =
      Effect.of_deferred_fun (fun () -> Backend_link.sign_out link) ()
  ; quit =
      (* Bonsai_term's [Driver.finished] never resolves when [exit] is scheduled
         from apply_action (the next frame sees the exit status before any event
         fills the ivar), so we track the request ourselves. *)
      Effect.Many
        [ exit (); Effect.of_sync_fun (Ivar.fill_if_empty quit_requested) () ]
  }
;;

(* Bracketed paste arrives as Paste `Start, key presses, Paste `End; the
   buffered text becomes one [Paste] intent so Enter inside a paste is a
   newline. The buffer is a plain ref: Bonsai_term hands the handler a whole
   batch of events at once, and [Bonsai.state] would only update after the
   frame, so every key in the batch would still see [Idle]. *)
module Paste = struct
  type t =
    | Idle
    | Collecting of Buffer.t
end

let app
  ?start_on_activate
  link
  ~exit
  ~quit_requested
  ~(dimensions : Dimensions.t Bonsai.t)
  (local_ graph)
  =
  let platform = Bonsai.return (platform link ~exit ~quit_requested) in
  let model, inject =
    Prigh_ui.Component.create ?start_on_activate platform graph
  in
  let home = Option.value (Sys.getenv "HOME") ~default:"." in
  (* The local zone's offset: [mktime] reads UTC's broken-down time as
     local. *)
  let utc_offset =
    let now = Core_unix.time () in
    let as_local, _ =
      Core_unix.mktime
        { (Core_unix.gmtime now) with
          tm_isdst = (Core_unix.localtime now).tm_isdst
        }
    in
    Time_ns.Span.of_sec (now -. as_local)
  in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject in
       Effect.Many
         [ inject (App.Action.Set_home home)
         ; inject (App.Action.Set_utc_offset utc_offset)
         ])
    graph;
  let paste = ref Paste.Idle in
  reset_paste := (fun () -> paste := Paste.Idle);
  Bonsai.Edge.on_change
    dimensions
    ~equal:Dimensions.equal
    ~callback:
      (let%arr inject in
       fun (d : Dimensions.t) ->
         inject (Resize { width = d.width; height = d.height }))
    graph;
  let screen =
    let%arr model in
    Prigh_ui.Render.screen model
  in
  let set_cursor = Effect.set_cursor graph in
  Bonsai.Edge.on_change
    (let%arr screen in
     screen.cursor)
    ~equal:[%equal: (int * int) option]
    ~callback:
      (let%arr set_cursor in
       fun cursor ->
         last_cursor := cursor;
         set_cursor
           (Option.map cursor ~f:(fun (row, col) ->
              { Cursor.position = { x = col; y = row }; kind = Default })))
    graph;
  let view =
    let%arr screen in
    View_of_content.screen screen
  in
  let handler =
    let%arr inject and model in
    fun (event : Event.t) ->
      match event, !paste with
      | Paste `Start, _ ->
        paste := Collecting (Buffer.create 256);
        Effect.Ignore
      | Paste `End, Collecting buf ->
        paste := Idle;
        inject (Intent (Paste (Buffer.contents buf)))
      | Paste `End, Idle -> Effect.Ignore
      | Key_press _, Collecting buf ->
        (match Key_of_event.key event with
         | Some ({ code = Char ("c" | "C"); ctrl = true; alt = false; _ } as key)
           ->
           paste := Idle;
           inject (Key key)
         | Some { code = Char c; ctrl = false; alt = false; _ } ->
           Buffer.add_string buf c;
           Effect.Ignore
         | Some { code = Enter; _ } ->
           Buffer.add_char buf '\n';
           Effect.Ignore
         | Some { code = Tab; _ } ->
           Buffer.add_char buf '\t';
           Effect.Ignore
         | _ -> Effect.Ignore)
      | Key_press _, Idle ->
        (match Key_of_event.key event with
         | Some key -> inject (Key key)
         | None -> Effect.Ignore)
      | Mouse { kind = Scroll `Up; _ }, _ -> inject (Intent Scroll_up)
      | Mouse { kind = Scroll `Down; _ }, _ -> inject (Intent Scroll_down)
      | Mouse { kind = Left; position = { x; y }; _ }, _ ->
        (match Prigh_ui.Render.click model ~row:y ~col:x with
         | Some intent -> inject (Intent intent)
         | None -> Effect.Ignore)
      | Mouse _, _ -> Effect.Ignore
  in
  let%arr model and view and handler and inject in
  { Result_.model; view; handler; inject }
;;

let action_of_incoming (incoming : Client.Incoming.t) : App.Action.t =
  match incoming with
  | Event e -> Event e
  | Protocol_error e -> Protocol_error e
  | Stderr line -> Stderr line
  | Closed -> Backend_closed
;;

let forward_incoming link driver =
  don't_wait_for
    (Pipe.iter_without_pushback
       (Client.incoming (Backend_link.client link))
       ~f:(fun incoming ->
         Option.iter
           (Backend_link.handle_incoming link incoming)
           ~f:(fun incoming ->
             Driver.send_incoming_event driver (action_of_incoming incoming))))
;;

(* The driver only exposes [View.t], so tests read the last [Result_.t] (and
   hence the model) through this ref. Reset on each [start_for_testing]. *)
let latest_result : Result_.t option ref = ref None

(* The test owns the frame loop ([Driver.compute_frame]); [start_with_driver]
   would run its own in the background and the two would fight over events. *)
module Test_terminal = struct
  type t =
    { mutable size : int * int
    ; mutable winch : unit Ivar.t
    }

  let create ~width ~height = { size = width, height; winch = Ivar.create () }

  let resize t ~width ~height =
    t.size <- width, height;
    let winch = t.winch in
    t.winch <- Ivar.create ();
    Ivar.fill_if_empty winch ()
  ;;
end

let with_test_driver ~link ~(terminal : Test_terminal.t) ~writer ~reader f =
  latest_result := None;
  let quit_requested = Ivar.create () in
  Bonsai_term.Private.For_testing.with_driver
    ~dispose:None
    ~nosig:None
    ~mouse:(Some No_mouse_events)
    ~bpaste:None
    ~reader:(Some reader)
    ~writer:(Some writer)
    ~time_source:None
    ~for_mocking:
      (Some
         (Notty_async.For_mocking.create
            ~capabilities:(fun _ -> Notty.Cap.ansi)
            ~dimensions:(fun _ -> Some terminal.size)
            ~wait_for_next_window_change:(fun () -> Ivar.read terminal.winch)
            ~is_a_tty:(fun _ -> Deferred.return true)))
    ~optimize:None
    ~target_frames_per_second:None
    ~get_view_and_handler:(fun (r : Result_.t) ->
      latest_result := Some r;
      ~view:r.view, ~handler:r.handler)
    ~handle_incoming:(fun (r : Result_.t) action -> r.inject action)
    (fun ~exit ~dimensions graph ->
      app link ~exit ~quit_requested ~dimensions graph)
    (fun driver ->
      forward_incoming link driver;
      f driver)
;;

(* Notty drops the terminal's focus reports, so it reads our stdin through a
   filter that takes them out (and, reading from a pipe, leaves the tty's
   modes to us: the same raw mode it would set, restored at exit). [None]
   when stdin is not a terminal. *)
let focus_reporting_input ~on_focus =
  if not (Core_unix.isatty Core_unix.stdin)
  then None
  else (
    let tty = Core_unix.stdin in
    let saved = Core_unix.Terminal_io.tcgetattr tty in
    Core_unix.Terminal_io.tcsetattr
      { saved with c_icanon = false; c_echo = false; c_isig = false; c_ixon = false }
      tty
      ~mode:TCSANOW;
    let restore () =
      write_tty Term_focus.disable;
      try Core_unix.Terminal_io.tcsetattr saved tty ~mode:TCSANOW with
      | _ -> ()
    in
    at_exit restore;
    let filtered_r, filtered_w = Pipe.create () in
    let focus_parser = Term_focus.create () in
    let stdin_fd = Fd.stdin () in
    let buf = Bytes.create 4096 in
    let flush_pending () =
      let pending = Term_focus.flush focus_parser in
      if not (String.is_empty pending)
      then Pipe.write_without_pushback_if_open filtered_w pending
    in
    let rec read_loop () =
      match !resume_input with
      | Some resume ->
        flush_pending ();
        let%bind.Deferred () = Ivar.read resume in
        read_loop ()
      | None ->
        let choices =
          [ Deferred.choice (Ivar.read !pause_input) (fun () -> `Paused)
          ; Deferred.choice (Fd.ready_to stdin_fd `Read) (fun r -> `Fd r)
          ]
          @
          if Term_focus.has_pending focus_parser
          then
            [ Deferred.choice
                (Clock.after (Time_float.Span.of_ms 25.))
                (fun () -> `Flush)
            ]
          else []
        in
        let%bind.Deferred ready = Deferred.choose choices in
        (match ready with
         | `Paused -> read_loop ()
         | `Flush ->
           flush_pending ();
           read_loop ()
         | `Fd (`Bad_fd | `Closed) ->
           flush_pending ();
           Deferred.return ()
         | `Fd `Ready ->
           if Option.is_some !resume_input
           then read_loop ()
           else (
             match Core_unix.read tty ~buf with
             | 0 ->
               flush_pending ();
               Deferred.return ()
             | n ->
               let chunk = Bytes.To_string.sub buf ~pos:0 ~len:n in
               let input, reports = Term_focus.feed focus_parser chunk in
               List.iter reports ~f:(fun focused -> !on_focus focused);
               if not (String.is_empty input)
               then Pipe.write_without_pushback_if_open filtered_w input;
               read_loop ()
             | exception
                 Core_unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
               read_loop ()
             | exception Core_unix.Unix_error _ ->
               flush_pending ();
               Deferred.return ()))
    in
    don't_wait_for
      (let%map.Deferred () = read_loop () in
       Pipe.close filtered_w);
    Some (Reader.of_pipe (Info.of_string "prigh-tui input") filtered_r, restore))
;;

let run link =
  match%bind.Deferred Backend_link.start link with
  | Unreachable e -> Deferred.Or_error.error_string e
  | Rejected e
    when not
           (Server_link.is_rejection e
            && Option.is_some (Backend_link.info link).address) ->
    let%bind.Deferred () = Backend_link.close link in
    Deferred.Or_error.error_string e
  | (Rejected _ | Connected _ | Signin_required _) as first ->
    let quit_requested = Ivar.create () in
    let on_focus = ref (fun (_ : bool) -> ()) in
    let%bind.Deferred input =
      match focus_reporting_input ~on_focus with
      | None -> Deferred.return None
      | Some (reader, restore) ->
        Deferred.map reader ~f:(fun reader -> Some (reader, restore))
    in
    let%bind.Deferred driver =
      (* Mouse reporting so the wheel scrolls the transcript (the terminal would
         otherwise turn it into arrow keys, i.e. prompt history); selecting text
         needs Shift. *)
      Bonsai_term.start_with_driver
        ?reader:(Option.map input ~f:fst)
        ~mouse:All_mouse_events_except_hover
        ~get_view_and_handler:(fun (r : Result_.t) ->
          ~view:r.view, ~handler:r.handler)
        ~handle_incoming:(fun (r : Result_.t) action -> r.inject action)
        (fun ~exit ~dimensions graph ->
          app
            ~start_on_activate:false
            link
            ~exit
            ~quit_requested
            ~dimensions
            graph)
    in
    (match driver with
     | Error _ as e -> Deferred.return e
     | Ok driver ->
       (* notty's raw mode leaves IEXTEN set, which makes the tty eat ^O; it
          cannot restore the flag either, so we do both around the driver. *)
       let tty =
         if Core_unix.isatty Core_unix.stdin
         then Core_unix.stdin
         else Core_unix.stdout
       in
       let had_iexten = Tty.set_iexten tty false in
       install_repaint driver;
       (on_focus
        := fun focused ->
             Driver.send_incoming_event driver (App.Action.Focus focused));
       if Option.is_some input then write_tty Term_focus.enable;
       let send = Driver.send_incoming_event driver in
       send (App.Action.Server_link (Backend_link.info link));
       (match first with
        | Connected reply ->
          Result.iter
            (Prigh_protocol.Hello_reply.of_json reply)
            ~f:(fun reply -> send (App.Action.Hello reply))
        | Rejected e -> send (App.Action.Rejected e)
        | Signin_required why -> send (App.Action.Signin_required why)
        | Unreachable _ -> ());
       send App.Action.Start;
       Backend_link.set_notice link (fun text ->
         send (App.Action.Event (Notice text)));
       Backend_link.set_code_prompt link (fun prompt ->
         send (App.Action.Signin_code_prompt prompt));
       forward_incoming link driver;
       let%bind.Deferred () =
         Deferred.any_unit
           [ Deferred.ignore_m (Driver.finished driver)
           ; Ivar.read quit_requested
           ]
       in
       (* Give the driver a frame to release the terminal before we exit. *)
       let%bind.Deferred () = Clock.after (Time_float.Span.of_sec 0.2) in
       ignore (Tty.set_iexten tty had_iexten : bool);
       Option.iter input ~f:(fun (_, restore) -> restore ());
       let%bind.Deferred () = Backend_link.close link in
       Deferred.return (Ok ()))
;;
