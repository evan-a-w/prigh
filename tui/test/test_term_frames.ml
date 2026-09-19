(* M9.2: rendered-frame snapshots through the real Bonsai_term driver.

   [Term_app.start_for_testing] mounts the app with an in-memory tty
   ([Notty_async.For_mocking]) so no fds are touched. Bonsai_term's
   [start_with_driver] also starts a background frame loop; the test therefore
   lets that loop paint and synchronises with [Driver.compute_frame]. The driver
   only exposes [View.t], so the current [Result_.t] (and its model) is read
   from [Term_app.latest_result]. Each frame is compared against the pure
   renderer to prove the terminal path cannot diverge. *)
open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
open Fixtures
module P = Prigh_protocol
module Transport = Prigh_client.Transport
module Term_app = Prigh_ui_term.Term_app
module Driver = Bonsai_term.Driver
module Ekey = Bonsai_term.Event.Key
module Mod = Bonsai_term.Event.Modifier

let serve backend =
  don't_wait_for
    (Pipe.iter_without_pushback
       (Transport.In_memory.Backend.requests backend)
       ~f:(fun line ->
         let json = Or_error.ok_exn (P.Json.parse line) in
         let id = Or_error.ok_exn (P.Json.int_field json "id") in
         let method_ = Or_error.ok_exn (P.Json.string_field json "method") in
         let result =
           match method_ with
           | "get_state" -> state_json ()
           | "get_messages" -> messages_json
           | "auth_status" -> auth_json
           | "get_config" -> config_json
           | "list_models" -> models_json
           | _ -> "null"
         in
         Transport.In_memory.Backend.send
           backend
           (sprintf
              {|{"type":"response","id":%d,"ok":true,"result":%s}|}
              id
              result)))
;;

module H = struct
  type t =
    { driver : (Term_app.Result_.t, unit, Prigh_ui.App.Action.t) Driver.t
    ; vt : Vt.t
    ; out : Core_unix.File_descr.t
    ; writer : Writer.t
    ; terminal : Term_app.Test_terminal.t
    }

  (* [with_test_driver] owns the driver for the duration of [f]. *)
  let run ~width ~height f =
    let transport, backend = Transport.In_memory.create () in
    let client = Prigh_client.Client.create transport in
    serve backend;
    let%bind `Reader in_r, `Writer _in_w =
      Async_unix.Unix.pipe (Info.of_string "prigh-test-in")
    in
    let out_r, out_w = Core_unix.pipe () in
    Core_unix.set_nonblock out_r;
    let reader = Reader.create in_r in
    let writer =
      Writer.create (Fd.create Fifo out_w (Info.of_string "prigh-test-out"))
    in
    let vt = Vt.create ~width ~height in
    let terminal = Term_app.Test_terminal.create ~width ~height in
    let%bind result =
      Term_app.with_test_driver ~client ~terminal ~reader ~writer (fun driver ->
        Driver.compute_first_frame driver;
        let%bind () = f { driver; vt; out = out_r; writer; terminal } in
        Deferred.Or_error.return ())
    in
    Or_error.ok_exn result;
    return ()
  ;;

  (* Drains whatever the frame wrote into the emulator; returns the byte count
     so callers can tell an idle frame from a repaint. *)
  let read_into t =
    let%map () = Writer.flushed t.writer in
    let buf = Bytes.create 65536 in
    let rec loop total =
      match Core_unix.read t.out ~buf with
      | 0 -> total
      | n ->
        let chunk = Bytes.To_string.sub buf ~pos:0 ~len:n in
        Vt.feed t.vt chunk;
        loop (total + n)
      | exception Core_unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> total
    in
    loop 0
  ;;

  (* One frame paints the current view and then handles the queued events (or
     the frame timer); the next frame paints their effect. *)
  let frame t =
    let%bind (`Frame_painted finished) = Driver.compute_frame t.driver in
    let%bind (`Frame_finished (_ : unit Bonsai_term.Private.Frame_outcome.t)) =
      finished
    in
    read_into t
  ;;

  (* Keeps cycling frames until one paints nothing, so RPC replies travelling
     through the in-memory transport have all been rendered. *)
  let paint t =
    let rec go n =
      let%bind wrote = frame t in
      if (wrote = 0 && n >= 1) || n >= 20 then return () else go (n + 1)
    in
    go 0
  ;;

  let send t event = Driver.send_event t.driver event
  let incoming t action = Driver.send_incoming_event t.driver action

  let resize t ~width ~height =
    Term_app.Test_terminal.resize t.terminal ~width ~height;
    Vt.resize t.vt ~width ~height
  ;;

  (* Print the emulated terminal, the pure renderer's plain screen and whether
     they agree up to trailing whitespace. *)
  let show t =
    let vt = Vt.to_plain t.vt in
    print_endline "=== Vt.to_plain ===";
    print_endline vt;
    match !Term_app.latest_result with
    | None -> print_endline "=== no result ==="
    | Some r ->
      let pure =
        Prigh_ui.Screen.to_plain
          ~show_cursor:true
          (Prigh_ui.Render.screen r.model)
      in
      let norm s =
        String.split_lines s
        |> List.map ~f:String.rstrip
        |> String.concat ~sep:"\n"
        |> String.rstrip
      in
      if String.equal (norm vt) (norm pure)
      then print_endline "same: true"
      else (
        print_endline "=== Screen.to_plain (DIFFERS) ===";
        print_endline pure;
        print_endline "same: false")
  ;;

  let key t key = send t (Bonsai_term.Event.Key_press { key; mods = [] })
  let ascii t c = key t (Ekey.ASCII c)

  let alt t key =
    send t (Bonsai_term.Event.Key_press { key; mods = [ Mod.Meta ] })
  ;;

  let paste_start t = send t (Bonsai_term.Event.Paste `Start)
  let paste_end t = send t (Bonsai_term.Event.Paste `End)
end

let%expect_test "startup frame matches the pure renderer" =
  H.run ~width:80 ~height:20 (fun h ->
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===














      session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
      > earlier question
      earlier answer
      ────────────────────────────────────────────────────────────────────────────────
      > ▏
      /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
      same: true
      |}];
    return ())
;;

let%expect_test "typing, streamed reply, Ctrl+O and Alt+Enter through the \
                 driver"
  =
  H.run ~width:80 ~height:20 (fun h ->
    let%bind () = H.paint h in
    List.iter (String.to_list "hello") ~f:(fun c -> H.ascii h c);
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===














      session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
      > earlier question
      earlier answer
      ────────────────────────────────────────────────────────────────────────────────
      > hello▏
      /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
      same: true
      |}];
    (* A streamed assistant reply arrives as an incoming action. *)
    H.incoming
      h
      (Prigh_ui.App.Action.Event
         (P.Event.Message_update
            { partial; delta = P.Delta.Text_delta "\nstreamed reply" }));
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===












      session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
      > earlier question
      earlier answer

      streamed reply
      ────────────────────────────────────────────────────────────────────────────────
      > hello▏
      /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
      same: true
      |}];
    (* Raw ^O (ASCII '\015') goes through Key_of_event and cycles verbosity. *)
    H.ascii h '\015';
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===











      session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
      > earlier question
      earlier answer
      view: verbose — everything is shown

      streamed reply
      ────────────────────────────────────────────────────────────────────────────────
      > hello▏
      /work  deepseek-flash  think:off  view:verbose  ctx:0% 1.5k  $0.01
      same: true
      |}];
    (* Alt+Enter is a raw Enter with the Meta modifier; it queues a follow-up. *)
    H.alt h Ekey.Enter;
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===











      session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
      > earlier question
      earlier answer
      view: verbose — everything is shown

      streamed reply
      ────────────────────────────────────────────────────────────────────────────────
      > ▏
      /work  deepseek-flash  think:off  view:verbose  ctx:0% 1.5k  $0.01
      same: true
      |}];
    return ())
;;

let%expect_test "bracketed paste produces the paste chip" =
  H.run ~width:80 ~height:20 (fun h ->
    let%bind () = H.paint h in
    H.paste_start h;
    List.iter (String.to_list "one\ntwo\nthree\nfour") ~f:(fun c ->
      match c with
      | '\n' -> H.key h Ekey.Enter
      | c -> H.ascii h c);
    H.paste_end h;
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===














      session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
      > earlier question
      earlier answer
      ────────────────────────────────────────────────────────────────────────────────
      > [4 lines pasted]▏
      /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
      same: true
      |}];
    return ())
;;

let%expect_test "resize 80 -> 40 keeps the frame matching" =
  H.run ~width:80 ~height:20 (fun h ->
    let%bind () = H.paint h in
    H.resize h ~width:40 ~height:20;
    let%bind () = H.paint h in
    H.show h;
    [%expect {|
      === Vt.to_plain ===












      session abc123 in /work. /help for
      commands, Esc aborts, Ctrl+C twice
      quits.
      > earlier question
      earlier answer
      ────────────────────────────────────────
      > ▏
      …deepseek-flash  ctx:0% 1.5k  $0.01
      same: true
      |}];
    return ())
;;

let%expect_test "styled frame: markdown link, diff and autocomplete" =
  H.run ~width:80 ~height:30 (fun h ->
    let%bind () = H.paint h in
    H.incoming
      h
      (Prigh_ui.App.Action.Event (P.Event.State (state ~running:true ())));
    H.incoming
      h
      (Prigh_ui.App.Action.Event
         (P.Event.Message_update
            { partial
            ; delta =
                P.Delta.Text_delta "See [the docs](https://example.com).\n"
            }));
    let call = tool_call "c1" in
    H.incoming h (Prigh_ui.App.Action.Event (P.Event.Tool_start call));
    H.incoming
      h
      (Prigh_ui.App.Action.Event
         (P.Event.Tool_end
            { call
            ; result =
                tool_result
                  ~id:"c1"
                  "--- a/x.c\n\
                   +++ b/x.c\n\
                   @@ -1,2 +1,2 @@\n\
                   -old\n\
                   +new\n\
                  \ context\n"
            }));
    let%bind () = H.paint h in
    H.ascii h '/';
    let%bind () = H.paint h in
    print_endline "=== Vt.to_styled ===";
    print_endline (Vt.to_styled h.vt);
    [%expect {|
      === Vt.to_styled ===








      [yellow]session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.[/]
      [green][bold]> [/][bold]earlier question[/]
      earlier [bold]answer[/]
      See the docs (https://example.com).
      [magenta]⚙ bash[/]
      [gray]  --- a/x.c[/]
      [gray]  +++ b/x.c[/]
      [cyan]  @@ -1,2 +1,2 @@[/]
      [red]  -old[/]
      [green]  +new[/]
      [gray]  … (1 more)[/]
      [gray]────────────────────────────────────────────────────────────────────────────────[/]
      [cyan][bold]> [/]/
      [invert]▸ [/][bold][invert]/help[/][gray][invert]                              show commands and keys[/]
        [bold]/hotkeys[/][gray]                           show keyboard shortcuts[/]
        [bold]/model[/][gray] [name|id|provider/id]       pick or switch the model[/]
        [bold]/scoped-models[/][gray]                     pick the models Ctrl+P cycles through[/]
        [bold]/login[/][gray] [provider] [api_key|oauth]  log in to a provider[/]
        [bold]/logout[/][gray] [provider]                 remove a provider's stored credential[/]
        [bold]/thinking[/][gray] [off|on|low|high|max]    pick or set the thinking level[/]
        [bold]/verbosity[/][gray] [quiet|normal|verbose]  set the transcript verbosity[/]
      …[gray]deepseek-flash[/]  [gray]think:off[/]  [green]ctx:0% 1.5k[/]  [gray]$0.01[/]  [gray]Tab/Enter accept · Esc close[/]
      |}];
    return ())
;;
