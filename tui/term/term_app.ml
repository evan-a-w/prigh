open! Core
open! Async
open Bonsai_term
open Bonsai.Let_syntax
module App = Prigh_ui.App
module Client = Prigh_client.Client

module Result_ = struct
  type t =
    { view : View.t
    ; handler : Event.t -> unit Effect.t
    ; inject : App.Action.t -> unit Effect.t
    }
end

let open_browser url =
  don't_wait_for
    (Process.run ~prog:"xdg-open" ~args:[ url ] ()
     |> Deferred.map ~f:(fun (_ : string Or_error.t) -> ()))
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
  ; quit =
      (* Bonsai_term's [Driver.finished] never resolves when [exit] is scheduled
         from apply_action (the next frame sees the exit status before any event
         fills the ivar), so we track the request ourselves. *)
      Effect.Many
        [ exit (); Effect.of_sync_fun (Ivar.fill_if_empty quit_requested) () ]
  }
;;

(* Bracketed paste arrives as Paste `Start, key presses, Paste `End; the
   buffered text becomes one [Insert] so Enter inside a paste is a newline. *)
module Paste = struct
  type t =
    | Idle
    | Collecting of string
  [@@deriving sexp_of]
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
  let paste, set_paste =
    Bonsai.state Paste.Idle ~sexp_of_model:Paste.sexp_of_t graph
  in
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
         set_cursor
           (Option.map cursor ~f:(fun (row, col) ->
              { Cursor.position = { x = col; y = row }; kind = Default })))
    graph;
  let view =
    let%arr screen in
    View_of_content.screen screen
  in
  let handler =
    let%arr inject and paste and set_paste in
    fun (event : Event.t) ->
      match event, paste with
      | Paste `Start, _ -> set_paste (Collecting "")
      | Paste `End, Collecting text ->
        Effect.Many [ set_paste Idle; inject (Intent (Insert text)) ]
      | Paste `End, Idle -> Effect.Ignore
      | Key_press _, Collecting text ->
        (match Key_of_event.key event with
         | Some { code = Char c; ctrl = false; alt = false; _ } ->
           set_paste (Collecting (text ^ c))
         | Some { code = Enter; _ } -> set_paste (Collecting (text ^ "\n"))
         | Some { code = Tab; _ } -> set_paste (Collecting (text ^ "\t"))
         | _ -> Effect.Ignore)
      | Key_press _, Idle ->
        (match Key_of_event.key event with
         | Some key -> inject (Key key)
         | None -> Effect.Ignore)
      | Mouse { kind = Scroll `Up; _ }, _ -> inject (Intent Page_up)
      | Mouse { kind = Scroll `Down; _ }, _ -> inject (Intent Page_down)
      | Mouse _, _ -> Effect.Ignore
  in
  let%arr view and handler and inject in
  { Result_.view; handler; inject }
;;

let action_of_incoming (incoming : Client.Incoming.t) : App.Action.t =
  match incoming with
  | Event e -> Event e
  | Protocol_error e -> Protocol_error e
  | Stderr line -> Stderr line
  | Closed -> Backend_closed
;;

let run ~backend ~args =
  match%bind.Deferred
    Prigh_client.Stdio_transport.spawn ~prog:backend ~args ()
  with
  | Error _ as e -> Deferred.return e
  | Ok transport ->
    let client = Client.create transport in
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
       don't_wait_for
         (Pipe.iter_without_pushback
            (Client.incoming client)
            ~f:(fun incoming ->
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
       Client.close client;
       let%bind.Deferred () =
         Deferred.any_unit
           [ Client.closed client; Clock.after (Time_float.Span.of_sec 3.) ]
       in
       Deferred.return (Ok ()))
;;
