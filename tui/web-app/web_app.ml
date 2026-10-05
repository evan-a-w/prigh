open! Core
open! Async_kernel
open Js_of_ocaml
open Bonsai_web
open Bonsai.Let_syntax
module App = Prigh_ui.App
module Client = Prigh_client.Client

(* Connect to the page's origin unless an explicit URL query selects another
   backend. *)
module Settings = struct
  type t =
    { backend : string
    ; login : Login.t
    ; session : string option
    ; as_user : string option
    ; name : string
    }

  let backend_key = "prigh.backend"

  let choose_backend ~query ~same_origin =
    Option.filter query ~f:(Fn.non String.is_empty)
    |> Option.value ~default:same_origin
  ;;

  let load () =
    let non_empty s = Option.filter s ~f:(Fn.non String.is_empty) in
    Browser.remove_item backend_key;
    { backend =
        choose_backend
          ~query:(Browser.query_param "backend")
          ~same_origin:(Browser.same_origin_ws_url ())
    ; login = Login.load Login.Storage.browser
    ; session = non_empty (Browser.query_param "session")
    ; as_user = non_empty (Browser.query_param "as_user")
    ; name =
        Option.value (non_empty (Browser.query_param "name")) ~default:"browser"
    }
  ;;

  let save ~user ~password =
    Browser.remove_item backend_key;
    Login.save Login.Storage.browser ~user ~password
  ;;

  let hello t =
    [ "name", `String t.name; "tools", `False ]
    @ Login.hello_fields t.login
    @ Option.value_map t.session ~default:[] ~f:(fun s ->
      [ "session", `String s ])
  ;;
end

module History = struct
  (* FNV-1a, so the key is stable across builds and does not reveal the token. *)
  let token_hash token =
    String.fold token ~init:0x811c9dc5l ~f:(fun h c ->
      Int32.( * )
        (Int32.bit_xor h (Int32.of_int_exn (Char.to_int c)))
        0x01000193l)
    |> sprintf "%08lx"
  ;;

  let key ~token =
    match token with
    | None -> "prigh.history"
    | Some token -> "prigh.history." ^ token_hash token
  ;;

  let limit = 500

  let load ~key =
    match Browser.get_item key with
    | None -> `Array []
    | Some text ->
      (match Prigh_protocol.Json.parse text with
       | Ok (`Array items) -> `Array items
       | _ -> `Array [])
  ;;

  let append ~key text =
    let items =
      match load ~key with
      | `Array items -> items
      | _ -> []
    in
    let items = items @ [ `String text ] in
    let items = List.drop items (Int.max 0 (List.length items - limit)) in
    Browser.set_item key (Prigh_protocol.Json.to_string (`Array items))
  ;;
end

let hello_params hello ~session ~as_user =
  let set name value params =
    match value with
    | None -> params
    | Some v ->
      List.Assoc.remove params ~equal:String.equal name @ [ name, `String v ]
  in
  hello
  |> List.filter ~f:(fun (name, _) -> not (String.equal name "as_user"))
  |> set "session" session
  |> set "as_user" as_user
;;

let send_hello client hello ~session ~as_user =
  Deferred.map
    (Client.call client "hello" (hello_params hello ~session ~as_user))
    ~f:(Result.map_error ~f:Error.to_string_hum)
;;

let reconnect client ~hello ~delay_ms ~session ~as_user =
  let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_int_ms delay_ms) in
  match%bind.Deferred Client.connect client with
  | Error e -> Deferred.return (Error (Error.to_string_hum e))
  | Ok () -> send_hello client hello ~session ~as_user
;;

let platform client ~hello ~history_key ~schedule ~quit ~sign_out
  : Prigh_ui.Component.Platform.t
  =
  let unavailable what =
    schedule
      (App.Action.Stderr (sprintf "%s is not available in the browser" what))
  in
  { rpc =
      (fun method_ params ->
        Effect.of_deferred_fun
          (fun () ->
            Deferred.map
              (Client.call client method_ params)
              ~f:(Result.map_error ~f:Error.to_string_hum))
          ())
  ; open_browser = (fun url -> Effect.of_sync_fun Browser.open_url url)
  ; load_history =
      (fun () ->
        Effect.of_sync_fun (fun () -> Ok (History.load ~key:history_key)) ())
  ; append_history =
      (fun text -> Effect.of_sync_fun (History.append ~key:history_key) text)
  ; copy_to_clipboard =
      (fun text -> Effect.of_sync_fun Browser.copy_to_clipboard text)
  ; suspend = Effect.of_sync_fun (fun () -> unavailable "suspend (Ctrl+Z)") ()
  ; edit_externally =
      (fun _ ->
        Effect.of_sync_fun
          (fun () ->
            Error "the external editor (Ctrl+G) is not available in the browser")
          ())
  ; reconnect =
      (fun ~delay_ms ~session ~as_user ->
        Effect.of_deferred_fun
          (fun () -> reconnect client ~hello ~delay_ms ~session ~as_user)
          ())
  ; sign_out = Effect.of_sync_fun (fun () -> Ok (sign_out ())) ()
  ; quit = Effect.of_sync_fun quit ()
  }
;;

module For_testing = struct
  let choose_backend = Settings.choose_backend
  let href_with_backend = Browser.href_with_backend
  let terminal_url = Terminal_panel.url
  let without_query_param = Browser.without_query_param
  let with_query_param = Browser.with_query_param
  let history_key = History.key
end

module Result_ = struct
  type t =
    { view : Vdom.Node.t
    ; inject : App.Action.t -> unit Effect.t
    }

  type extra = unit
  type incoming = App.Action.t

  let view t = t.view
  let extra _ = ()
  let incoming t action = t.inject action
end

let keyboard_input () =
  Dom_html.getElementById_coerce "keyboard-input" Dom_html.CoerceTo.textarea
;;

let focus_keyboard_input () =
  Option.iter (keyboard_input ()) ~f:(fun input ->
    input##.value := Js.string "";
    input##focus)
;;

(* Set by [install_listeners]: refits the app's grid to [#screen-area]. *)
let relayout = ref Fn.id

let sign_out_button ~namespace ~on_click =
  Vdom.Node.button
    ~attrs:
      [ Vdom.Attr.class_ "sign-out"
      ; Vdom.Attr.title
          (match namespace with
           | Some user -> sprintf "sign out (%s)" user
           | None -> "sign out")
      ; Vdom.Attr.on_click (fun _ -> on_click)
      ]
    [ Vdom.Node.text "sign out" ]
;;

let app platform ~terminal_url ~signed_in ~sign_out (local_ graph) =
  let model, inject =
    Prigh_ui.Component.create ~start_on_activate:false platform graph
  in
  let session =
    let%arr model in
    Option.map model.Prigh_ui.App.Model.state ~f:(fun s ->
      s.Prigh_protocol.State.session_id)
  in
  (* The page's URL names its session, so a reload rejoins it. *)
  Bonsai.Edge.on_change
    ~equal:[%equal: string option]
    session
    ~callback:
      (Bonsai.return (fun session ->
         Effect.of_sync_fun
           (Option.iter ~f:(Browser.replace_query_param "session"))
           session))
    graph;
  let as_user =
    let%arr model in
    Prigh_ui.App.Model.acting_as model
  in
  (* Likewise for the user a superuser acts as. *)
  Bonsai.Edge.on_change
    ~equal:[%equal: string option]
    as_user
    ~callback:
      (Bonsai.return (fun as_user ->
         Effect.of_sync_fun
           (function
             | Some user -> Browser.replace_query_param "as_user" user
             | None -> Browser.remove_query_param "as_user")
           as_user))
    graph;
  let terminal_open, set_terminal_open = Bonsai.state false graph in
  let view =
    let%arr model
    and session
    and as_user
    and terminal_open
    and set_terminal_open in
    let set_open value =
      Effect.Many
        [ set_terminal_open value
        ; Effect.of_sync_fun
            (fun () ->
              Browser.after_render (fun () ->
                !relayout ();
                if not value then focus_keyboard_input ()))
            ()
        ]
    in
    (* Bonsai replaces the element it binds to, so keep an [#app] wrapper for
       the messages shown after the app stops. *)
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "app" ]
      [ Vdom.Node.div
          ~attrs:[ Vdom.Attr.id "screen-area" ]
          [ Prigh_ui_web.Dom_of_screen.screen (Prigh_ui.Render.screen model)
          ; Vdom.Node.div
              ~attrs:[ Vdom.Attr.class_ "page-buttons" ]
              [ (if signed_in || Option.is_some model.namespace
                 then
                   sign_out_button
                     ~namespace:model.namespace
                     ~on_click:(Effect.of_sync_fun sign_out ())
                 else Vdom.Node.none)
              ; (if terminal_open
                 then Vdom.Node.none
                 else Terminal_panel.open_button ~on_click:(set_open true))
              ]
          ]
      ; (if terminal_open
         then
           Terminal_panel.view
             ~url:(terminal_url ~session ~as_user)
             ~on_close:(set_open false)
         else Vdom.Node.none)
      ]
  in
  let%arr view and inject in
  { Result_.view; inject }
;;

let key_event (ev : Dom_html.keyboardEvent Js.t)
  : Prigh_ui_web.Key_of_dom.Event.t
  =
  let string_of_optdef v = Js.Optdef.case v (fun () -> "") Js.to_string in
  { key = string_of_optdef ev##.key
  ; code = string_of_optdef ev##.code
  ; ctrl = Js.to_bool ev##.ctrlKey
  ; alt = Js.to_bool ev##.altKey
  ; shift = Js.to_bool ev##.shiftKey
  ; meta = Js.to_bool ev##.metaKey
  }
;;

let event_target (ev : #Dom_html.event Js.t) = Dom_html.eventTarget ev

let within_link (ev : #Dom_html.event Js.t) =
  Js.Opt.test
    ((Js.Unsafe.coerce (event_target ev))##closest (Js.string "a")
     : Dom_html.element Js.t Js.opt)
;;

(* The connect form's own fields must keep their focus and receive keystrokes,
   so those events are left to the browser. The hidden keyboard input is the
   only textarea, so a textarea target is ours. *)
let form_control (ev : #Dom_html.event Js.t) =
  let target = event_target ev in
  Js.Opt.test (Dom_html.CoerceTo.input target)
  || Js.Opt.test (Dom_html.CoerceTo.button target)
  || Js.Opt.test (Dom_html.CoerceTo.select target)
;;

let in_terminal (ev : #Dom_html.event Js.t) =
  Terminal_panel.contains (event_target ev)
;;

let install_listeners ~schedule =
  let document = Dom_html.document in
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.keydown
       (Dom.handler (fun ev ->
          if form_control ev || in_terminal ev
          then Js._true
          else (
            match Prigh_ui_web.Key_of_dom.key (key_event ev) with
            | Some key ->
              schedule (App.Action.Key key);
              Dom.preventDefault ev;
              Js._false
            | None -> Js._true)))
       Js._false
     : Dom_html.event_listener_id);
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.paste
       (Dom.handler (fun ev ->
          if form_control ev || in_terminal ev
          then Js._true
          else (
            (match Js.Opt.to_option ev##.clipboardData with
             | Some data ->
               let text = Js.to_string (data##getData (Js.string "text")) in
               if not (String.is_empty text)
               then schedule (App.Action.Intent (Paste text))
             | None -> ());
            Dom.preventDefault ev;
            Js._false)))
       Js._false
     : Dom_html.event_listener_id);
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.wheel
       (Dom.handler (fun ev ->
          let dy = Js.to_float ev##.deltaY in
          if in_terminal ev
          then ()
          else if Float.(dy < 0.)
          then schedule (App.Action.Intent Scroll_up)
          else if Float.(dy > 0.)
          then schedule (App.Action.Intent Scroll_down);
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  let resize () =
    Browser.fit_root ();
    let width, height = Browser.grid_size () in
    schedule (App.Action.Resize { width; height })
  in
  relayout := resize;
  Browser.on_viewport_change resize;
  Option.iter (keyboard_input ()) ~f:(fun input ->
    (* Keys the [keydown] handler recognised were prevented from reaching the
       textarea, so any text that does arrive came from a virtual keyboard whose
       [keydown] was unidentified. *)
    ignore
      (Dom_html.addEventListener
         input
         Dom_html.Event.input
         (Dom.handler (fun _ ->
            let text = Js.to_string input##.value in
            input##.value := Js.string "";
            List.iter (Prigh_ui_web.Key_of_dom.keys_of_text text) ~f:(fun key ->
              schedule (App.Action.Key key));
            Js._true))
         Js._false
       : Dom_html.event_listener_id));
  (* Coming back to the page returns focus to wherever it was: the terminal or
     the app. *)
  let terminal_focused = ref false in
  ignore
    (Dom_html.addEventListener
       document
       (Dom_html.Event.make "focusin")
       (Dom.handler (fun ev ->
          terminal_focused := in_terminal ev;
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       Dom_html.Event.focus
       (Dom.handler (fun _ ->
          if not !terminal_focused then focus_keyboard_input ();
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  Option.iter (Dom_html.getElementById_opt "root") ~f:(fun root ->
    ignore
      (Dom_html.addEventListener
         root
         Dom_html.Event.mousedown
         (Dom.handler (fun ev ->
            if not (form_control ev || in_terminal ev)
            then focus_keyboard_input ();
            Js._true))
         Js._false
       : Dom_html.event_listener_id);
    (* A tap focuses the hidden input from [touchend], a direct user gesture
       that mobile browsers accept for opening the keyboard (the input must also
       be visible enough; see style.css). The tap's default action would then
       synthesize mouse events and a click on non-editable content, which blurs
       the input again and closes the keyboard, so it is cancelled. Swipes
       scroll instead. *)
    let gesture = ref None in
    let touch_point (ev : Dom_html.touchEvent Js.t) =
      Js.Optdef.to_option (ev##.changedTouches##item 0)
      |> Option.map ~f:(fun t ->
        Js.to_float t##.clientX, Js.to_float t##.clientY)
    in
    let touch event handler =
      ignore
        (Dom_html.addEventListener root event (Dom.handler handler) Js._false
         : Dom_html.event_listener_id)
    in
    touch Dom_html.Event.touchstart (fun ev ->
      gesture
      := if form_control ev || within_link ev || in_terminal ev
         then None
         else
           Option.map (touch_point ev) ~f:(fun (x, y) ->
             let step =
               Browser.cell_height () *. Float.of_int App.wheel_lines
             in
             Touch.start ~x ~y, step);
      Js._true);
    touch Dom_html.Event.touchmove (fun ev ->
      (match !gesture, touch_point ev with
       | Some (g, step), Some (x, y) ->
         let g, steps = Touch.move g ~x ~y ~step in
         gesture := Some (g, step);
         for _ = 1 to abs steps do
           schedule
             (App.Action.Intent (if steps > 0 then Scroll_down else Scroll_up))
         done;
         Dom.preventDefault ev
       | _ -> ());
      Js._true);
    touch Dom_html.Event.touchend (fun ev ->
      (match !gesture with
       | None -> ()
       | Some (g, _) ->
         gesture := None;
         Dom.preventDefault ev;
         (match Touch.finish g with
          | `Tap -> focus_keyboard_input ()
          | `Swipe -> ()));
      Js._true);
    touch Dom_html.Event.touchcancel (fun _ ->
      gesture := None;
      Js._true));
  resize ();
  focus_keyboard_input ()
;;

let connect_form ~backend ~(login : Login.t) ~error =
  let escape s =
    String.concat_map s ~f:(function
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | '"' -> "&quot;"
      | c -> String.of_char c)
  in
  let value = Option.value_map ~default:"" ~f:escape in
  Browser.set_app_html
    (sprintf
       {|<form class="connect" id="connect-form">
  <h1>prigh</h1>
  <p class="error">%s</p>
  <label>backend <input id="backend" value="%s" placeholder="ws://host:port/ws"></label>
  <label>User name <input id="user" value="%s" autocomplete="username" autocapitalize="off" spellcheck="false"></label>
  <label>Password <input id="password" type="password" value="%s" autocomplete="current-password"></label>
  <p class="caps-lock" id="caps-lock" style="display: none">Caps Lock is on</p>
  <button id="connect-submit" type="submit" disabled>connect</button>
</form>|}
       (escape error)
       (escape backend)
       (value login.user)
       (value login.password));
  Option.iter
    (Dom_html.getElementById_coerce "password" Dom_html.CoerceTo.input)
    ~f:(fun password ->
      let show_caps_lock on =
        Option.iter (Dom_html.getElementById_opt "caps-lock") ~f:(fun warning ->
          warning##.style##.display := Js.string (if on then "" else "none"))
      in
      let on_key event =
        ignore
          (Dom_html.addEventListener
             password
             event
             (Dom.handler (fun (ev : Dom_html.keyboardEvent Js.t) ->
                show_caps_lock
                  (Js.to_bool
                     (Js.Unsafe.meth_call
                        ev
                        "getModifierState"
                        [| Js.Unsafe.inject (Js.string "CapsLock") |]));
                Js._true))
             Js._false
           : Dom_html.event_listener_id)
      in
      on_key Dom_html.Event.keydown;
      on_key Dom_html.Event.keyup;
      ignore
        (Dom_html.addEventListener
           password
           Dom_html.Event.blur
           (Dom.handler (fun _ ->
              show_caps_lock false;
              Js._true))
           Js._false
         : Dom_html.event_listener_id));
  match
    Dom_html.getElementById_coerce "connect-form" Dom_html.CoerceTo.form
  with
  | None -> ()
  | Some form ->
    ignore
      (Dom_html.addEventListener
         form
         Dom_html.Event.submit
         (Dom.handler (fun ev ->
            Dom.preventDefault ev;
            let backend = String.strip (Browser.input_value "backend") in
            Settings.save
              ~user:(Browser.input_value "user")
              ~password:(Browser.input_value "password");
            Browser.reload_with_backend backend;
            Js._false))
         Js._false
       : Dom_html.event_listener_id);
    Option.iter
      (Dom_html.getElementById_coerce "connect-submit" Dom_html.CoerceTo.button)
      ~f:(fun button -> button##.disabled := Js._false)
;;

(* Reloading drops every socket (the app's and any terminal's) and listener; the
   next page load sees the note and shows the connect form. *)
let sign_out () =
  Login.forget Login.Storage.browser;
  Browser.remove_query_param "as_user";
  Browser.reload_without_query_param "session"
;;

let run_app (settings : Settings.t) =
  let hello = Settings.hello settings in
  let client =
    Client.create ~connect:(fun () ->
      Ws_transport.connect ~url:settings.backend)
  in
  don't_wait_for
    (match%bind.Deferred
       match%bind.Deferred Client.connect client with
       | Error e -> Deferred.return (Error (Error.to_string_hum e))
       | Ok () ->
         (match%bind.Deferred
            send_hello client hello ~session:None ~as_user:settings.as_user
          with
          | Error _ when Option.is_some settings.as_user ->
            (* No longer allowed to act as that user: be ourselves. *)
            Browser.remove_query_param "as_user";
            send_hello client hello ~session:None ~as_user:None
          | result -> Deferred.return result)
     with
     | Error error ->
       let%map.Deferred () = Client.close client in
       connect_form ~backend:settings.backend ~login:settings.login ~error
     | Ok reply ->
       let hello_reply =
         Prigh_protocol.Hello_reply.of_json reply |> Or_error.ok
       in
       let handle_ref = ref None in
       let schedule action =
         Option.iter !handle_ref ~f:(fun handle ->
           Start.Handle.schedule handle action)
       in
       let quit () =
         Option.iter !handle_ref ~f:Start.Handle.stop;
         don't_wait_for (Client.close client);
         Browser.set_app_html
           {|<div class="connect"><h1>prigh</h1><p>disconnected — reload to start again</p></div>|}
       in
       let platform =
         Bonsai.return
           (platform
              client
              ~hello
              ~history_key:(History.key ~token:settings.login.password)
              ~schedule
              ~quit
              ~sign_out)
       in
       let terminal_url ~session ~as_user =
         Terminal_panel.url
           ~backend:settings.backend
           ~user:settings.login.user
           ~as_user
           ~token:settings.login.password
           ~session
       in
       let handle =
         Start.start_and_get_handle
           (module Result_)
           ~bind_to_element_with_id:"app"
           (fun graph ->
              app
                platform
                ~terminal_url
                ~signed_in:(Option.is_some settings.login.password)
                ~sign_out
                graph)
       in
       handle_ref := Some handle;
       (* [schedule] queues before the first frame, so startup and input must
          not wait on [Handle.started]'s Async continuation. *)
       schedule App.Action.Start;
       Option.iter hello_reply ~f:(fun reply ->
         schedule (App.Action.Hello reply));
       install_listeners ~schedule;
       don't_wait_for
         (Pipe.iter_without_pushback
            (Client.incoming client)
            ~f:(fun incoming ->
              schedule
                (match incoming with
                 | Event e -> Event e
                 | Protocol_error e -> Protocol_error e
                 | Stderr line -> Stderr line
                 | Closed -> Backend_closed)));
       Deferred.unit)
;;

let run () =
  Async_js.init ();
  let settings = Settings.load () in
  if Login.take_signed_out Login.Storage.browser
  then connect_form ~backend:settings.backend ~login:settings.login ~error:""
  else run_app settings
;;
