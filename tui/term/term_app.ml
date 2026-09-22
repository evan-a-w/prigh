open! Core
open! Async
open Bonsai_term
open Bonsai.Let_syntax
module App = Prigh_ui.App
module Client = Prigh_client.Client

module Result_ = struct
  type t =
    { model : App.Model.t
    ; view : View.t
    ; handler : Event.t -> unit Effect.t
    ; inject : App.Action.t -> unit Effect.t
    }
end

let open_browser url =
  don't_wait_for
    (Process.run ~prog:"xdg-open" ~args:[ url ] ()
     |> Deferred.map ~f:(fun (_ : string Or_error.t) -> ()))
;;

(* Direct tty access for the sequences Notty does not manage (alt screen,
   suspend) and OSC 52. Writes go to the controlling terminal. *)
let tty_fd () =
  if Core_unix.isatty Core_unix.stdin then Core_unix.stdin else Core_unix.stdout
;;

let write_tty s = ignore (Core_unix.write_substring (tty_fd ()) ~buf:s : int)

let release_terminal () =
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
  release_terminal ();
  let status =
    Core_unix.system
      (sprintf "%s %s </dev/tty >/dev/tty 2>&1" editor (Filename.quote tmp))
  in
  reacquire_terminal ();
  let contents = In_channel.read_all tmp in
  (try Core_unix.unlink tmp with
   | _ -> ());
  match status with
  | Ok () -> Ok contents
  | Error _ ->
    Error ("editor failed: " ^ Core_unix.Exit_or_signal.to_string_hum status)
;;

let platform client ~exit ~quit_requested : Prigh_ui.Component.Platform.t =
  { rpc =
      (fun method_ params ->
        Effect.of_deferred_fun
          (fun () ->
            Deferred.map
              (Client.call client method_ params)
              ~f:(Result.map_error ~f:Error.to_string_hum))
          ())
  ; list_paths =
      (fun ~prefix ->
        Effect.of_deferred_fun
          (fun () -> Deferred.map (Paths.list ~prefix) ~f:(fun json -> Ok json))
          ())
  ; open_browser = (fun url -> Effect.of_sync_fun open_browser url)
  ; load_history = (fun () -> Effect.of_sync_fun load_history ())
  ; append_history = (fun text -> Effect.of_sync_fun append_history text)
  ; copy_to_clipboard = (fun text -> Effect.of_sync_fun copy_to_clipboard text)
  ; suspend = Effect.of_sync_fun suspend ()
  ; edit_externally = (fun text -> Effect.of_sync_fun edit_externally text)
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
  client
  ~exit
  ~quit_requested
  ~(dimensions : Dimensions.t Bonsai.t)
  (local_ graph)
  =
  let platform = Bonsai.return (platform client ~exit ~quit_requested) in
  let model, inject = Prigh_ui.Component.create platform graph in
  let home = Option.value (Sys.getenv "HOME") ~default:"." in
  Bonsai.Edge.lifecycle
    ~on_activate:
      (let%arr inject in
       inject (App.Action.Set_home home))
    graph;
  let paste = ref Paste.Idle in
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
    let%arr inject in
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
         | Some { code = Char c; ctrl = false; alt = false; _ } ->
           Buffer.add_string buf c
         | Some { code = Enter; _ } -> Buffer.add_char buf '\n'
         | Some { code = Tab; _ } -> Buffer.add_char buf '\t'
         | _ -> ());
        Effect.Ignore
      | Key_press _, Idle ->
        (match Key_of_event.key event with
         | Some key -> inject (Key key)
         | None -> Effect.Ignore)
      | Mouse { kind = Scroll `Up; _ }, _ -> inject (Intent Page_up)
      | Mouse { kind = Scroll `Down; _ }, _ -> inject (Intent Page_down)
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

let with_test_driver ~client ~(terminal : Test_terminal.t) ~writer ~reader f =
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
      app client ~exit ~quit_requested ~dimensions graph)
    (fun driver ->
      don't_wait_for
        (Pipe.iter_without_pushback (Client.incoming client) ~f:(fun incoming ->
           Driver.send_incoming_event driver (action_of_incoming incoming)));
      f driver)
;;

let run ~transport ~hello ~local_tools =
  let client = Client.create transport in
  let hello =
    hello @ [ ("tools", if Option.is_some local_tools then `True else `False) ]
  in
  match%bind.Deferred Client.call client "hello" hello with
  | Error _ as e ->
    Client.close client;
    Deferred.return e
  | Ok reply ->
    let client_id =
      match Jsonaf.member "client_id" reply with
      | Some (`String id) -> Some id
      | _ -> None
    in
    let tool_host =
      Option.map local_tools ~f:(fun backend ->
        Prigh_client.Tool_host.create ~client ~backend)
    in
    let quit_requested = Ivar.create () in
    let%bind.Deferred driver =
      Bonsai_term.start_with_driver
        ~mouse:No_mouse_events
        ~get_view_and_handler:(fun (r : Result_.t) ->
          ~view:r.view, ~handler:r.handler)
        ~handle_incoming:(fun (r : Result_.t) action -> r.inject action)
        (fun ~exit ~dimensions graph ->
          app client ~exit ~quit_requested ~dimensions graph)
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
       Option.iter client_id ~f:(fun id ->
         Driver.send_incoming_event driver (App.Action.Set_client_id id));
       don't_wait_for
         (Pipe.iter_without_pushback
            (Client.incoming client)
            ~f:(fun incoming ->
              (match incoming, tool_host with
               | Event e, Some host -> Prigh_client.Tool_host.handle host e
               | _ -> ());
              Driver.send_incoming_event driver (action_of_incoming incoming)));
       let%bind.Deferred () =
         Deferred.any_unit
           [ Deferred.ignore_m (Driver.finished driver)
           ; Ivar.read quit_requested
           ]
       in
       (* Give the driver a frame to release the terminal before we exit. *)
       let%bind.Deferred () = Clock.after (Time_float.Span.of_sec 0.2) in
       ignore (Tty.set_iexten tty had_iexten : bool);
       Option.iter tool_host ~f:Prigh_client.Tool_host.close;
       Client.close client;
       let%bind.Deferred () =
         Deferred.any_unit
           [ Client.closed client; Clock.after (Time_float.Span.of_sec 3.) ]
       in
       Deferred.return (Ok ()))
;;
