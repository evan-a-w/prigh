open! Core
open! Expect_test_helpers_core
open Prigh_ui
module P = Prigh_protocol

(* A scripted platform: every RPC is recorded and answered from a table, so the
   test exercises the Bonsai wiring (commands become effects whose results
   re-enter the state machine as actions). *)
let make_platform ~replies : Component.Platform.t =
  { rpc =
      (fun method_ params ->
        Bonsai.Effect.of_sync_fun
          (fun () ->
            printf
              "rpc %s %s\n"
              method_
              (Sexp.to_string [%sexp (params : (string * P.Json.t) list)]);
            match List.Assoc.find replies ~equal:String.equal method_ with
            | Some (Ok json) -> Ok (Or_error.ok_exn (P.Json.parse json))
            | Some (Error e) -> Error e
            | None -> Ok (`Object []))
          ())
  ; list_paths =
      (fun ~prefix ->
        Bonsai.Effect.of_sync_fun
          (fun () ->
            printf "list_paths %s\n" prefix;
            Ok (`Array []))
          ())
  ; open_browser =
      (fun url ->
        Bonsai.Effect.of_sync_fun (fun () -> printf "open browser %s\n" url) ())
  ; load_history =
      (fun () ->
        Bonsai.Effect.of_sync_fun
          (fun () ->
            print_endline "load history";
            Ok (`Array []))
          ())
  ; append_history =
      (fun text ->
        Bonsai.Effect.of_sync_fun
          (fun () -> printf "append history %s\n" text)
          ())
  ; copy_to_clipboard =
      (fun text ->
        Bonsai.Effect.of_sync_fun (fun () -> printf "copy %s\n" text) ())
  ; suspend = Bonsai.Effect.of_sync_fun (fun () -> print_endline "suspend") ()
  ; edit_externally =
      (fun text ->
        Bonsai.Effect.of_sync_fun
          (fun () ->
            printf "edit %s\n" text;
            Ok text)
          ())
  ; reconnect =
      (fun ~delay_ms ~session ->
        Bonsai.Effect.of_sync_fun
          (fun () ->
            printf
              "reconnect after %dms session=%s\n"
              delay_ms
              (Option.value session ~default:"-");
            Ok (`Object [ "client_id", `String "client-2" ]))
          ())
  ; quit = Bonsai.Effect.of_sync_fun (fun () -> print_endline "quit") ()
  }
;;

let state_json running =
  sprintf
    {|{"session_id":"abc","session_path":"/s","cwd":"/w","model":{"id":"m","provider":"deepseek","key":"deepseek/m","name":"M","context_window":1000,"max_output":10,"supports_thinking":false,"cost":{"input":1,"output":1,"cache_read":1}},"thinking":"off","running":%b,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0}|}
    running
;;

module Result_spec = struct
  type t = App.Model.t * (App.Action.t -> unit Bonsai.Effect.t)
  type incoming = App.Action.t

  let view ((model : App.Model.t), _) =
    sprintf
      "spinner=%d running=%b\n%s"
      model.spinner
      (App.Model.running model)
      (Screen.to_plain
         ~show_cursor:true
         (Render.screen { model with width = 50; height = 8 }))
  ;;

  let incoming (_, inject) action = inject action
end

let%expect_test "component: startup requests, key handling, rpc round trip, \
                 quit"
  =
  let replies =
    [ "get_state", Ok (state_json false)
    ; "get_messages", Ok "[]"
    ; "auth_status", Ok "[]"
    ; "get_config", Ok {|{"scoped_models":[],"confirm_tools":false}|}
    ; "list_models", Ok "[]"
    ; "set_thinking", Error "thinking must be one of: off, on, low, high, max"
    ]
  in
  let handle =
    Bonsai_test.Handle.create (module Result_spec) (fun graph ->
      let model, inject =
        Component.create (Bonsai.return (make_platform ~replies)) graph
      in
      Bonsai.both model inject)
  in
  Bonsai_test.Handle.recompute_view_until_stable handle;
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    rpc get_state ()
    rpc get_messages ()
    rpc auth_status ()
    rpc get_config ()
    rpc list_models ()
    load history
    spinner=0 running=false



    session abc in /w. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    ──────────────────────────────────────────────────
    > ▏
    /w  m  think:n/a  view:normal  ctx:0% 0  $0.00
    |}];
  Bonsai_test.Handle.do_actions
    handle
    (List.map (String.to_list "/thinking zzz") ~f:(fun c ->
       App.Action.Key (Key.char c)));
  Bonsai_test.Handle.do_actions handle [ Key (Key.plain Enter) ];
  Bonsai_test.Handle.recompute_view_until_stable handle;
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    rpc set_thinking ((thinking zzz))
    spinner=0 running=false


    session abc in /w. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    thinking must be one of: off, on, low, high, max
    ──────────────────────────────────────────────────
    > ▏
    /w  m  think:n/a  view:normal  ctx:0% 0  $0.00
    |}];
  (* The spinner ticks only while running. *)
  Bonsai_test.Handle.do_actions
    handle
    [ Event
        (State
           (Or_error.ok_exn
              (P.State.of_json
                 (Or_error.ok_exn (P.Json.parse (state_json true))))))
    ];
  Bonsai_test.Handle.recompute_view_until_stable handle;
  Bonsai_test.Handle.advance_clock_by handle (Time_ns.Span.of_ms 350.);
  Bonsai_test.Handle.recompute_view_until_stable handle;
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    spinner=2 running=true


    session abc in /w. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    thinking must be one of: off, on, low, high, max
    ──────────────────────────────────────────────────
    > ▏
    …m  ctx:0% 0  ⠹ working (Esc aborts; Enter steers)
    |}];
  Bonsai_test.Handle.do_actions handle [ Key (Key.plain Escape) ];
  Bonsai_test.Handle.recompute_view_until_stable handle;
  [%expect {| rpc abort () |}];
  Bonsai_test.Handle.do_actions handle [ Backend_closed ];
  Bonsai_test.Handle.recompute_view_until_stable handle;
  Bonsai_test.Handle.show handle;
  [%expect
    {|
    reconnect after 0ms session=/s
    rpc get_state ()
    rpc get_messages ()
    rpc auth_status ()
    rpc get_config ()
    rpc list_models ()
    spinner=2 running=false


    reconnected to the backend
    session abc in /w. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    ──────────────────────────────────────────────────
    > ▏
    /w  m  think:n/a  view:normal  ctx:0% 0  $0.00
    |}]
;;
