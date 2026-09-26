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
    ; token : string option
    ; session : string option
    ; name : string
    }

  let backend_key = "prigh.backend"
  let token_key = "prigh.token"

  let choose_backend ~query ~same_origin =
    Option.filter query ~f:(Fn.non String.is_empty)
    |> Option.value ~default:same_origin
  ;;

  let load () =
    let first_some options = List.find_map options ~f:Fn.id in
    let non_empty s = Option.filter s ~f:(Fn.non String.is_empty) in
    Browser.remove_item backend_key;
    { backend =
        choose_backend
          ~query:(Browser.query_param "backend")
          ~same_origin:(Browser.same_origin_ws_url ())
    ; token =
        first_some
          [ non_empty (Browser.query_param "token")
          ; non_empty (Browser.get_item token_key)
          ]
    ; session = non_empty (Browser.query_param "session")
    ; name =
        Option.value (non_empty (Browser.query_param "name")) ~default:"browser"
    }
  ;;

  let save ~token =
    Browser.remove_item backend_key;
    if String.is_empty token
    then Browser.remove_item token_key
    else Browser.set_item token_key token
  ;;

  let hello t =
    [ "name", `String t.name; "tools", `False ]
    @ Option.value_map t.token ~default:[] ~f:(fun token ->
      [ "token", `String token ])
    @ Option.value_map t.session ~default:[] ~f:(fun s ->
      [ "session", `String s ])
  ;;
end

module History = struct
  let key = "prigh.history"
  let limit = 500

  let load () =
    match Browser.get_item key with
    | None -> `Array []
    | Some text ->
      (match Prigh_protocol.Json.parse text with
       | Ok (`Array items) -> `Array items
       | _ -> `Array [])
  ;;

  let append text =
    let items =
      match load () with
      | `Array items -> items
      | _ -> []
    in
    let items = items @ [ `String text ] in
    let items = List.drop items (Int.max 0 (List.length items - limit)) in
    Browser.set_item key (Prigh_protocol.Json.to_string (`Array items))
  ;;
end

let hello_params hello ~session =
  match session with
  | None -> hello
  | Some path ->
    List.Assoc.remove hello ~equal:String.equal "session"
    @ [ "session", `String path ]
;;

let send_hello client hello ~session =
  Deferred.map
    (Client.call client "hello" (hello_params hello ~session))
    ~f:(Result.map_error ~f:Error.to_string_hum)
;;

let reconnect client ~hello ~delay_ms ~session =
  let%bind.Deferred () = Clock_ns.after (Time_ns.Span.of_int_ms delay_ms) in
  match%bind.Deferred Client.connect client with
  | Error e -> Deferred.return (Error (Error.to_string_hum e))
  | Ok () -> send_hello client hello ~session
;;

let platform client ~hello ~schedule ~quit : Prigh_ui.Component.Platform.t =
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
      (fun () -> Effect.of_sync_fun (fun () -> Ok (History.load ())) ())
  ; append_history = (fun text -> Effect.of_sync_fun History.append text)
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
      (fun ~delay_ms ~session ->
        Effect.of_deferred_fun
          (fun () -> reconnect client ~hello ~delay_ms ~session)
          ())
  ; quit = Effect.of_sync_fun quit ()
  }
;;

module For_testing = struct
  let choose_backend = Settings.choose_backend
  let href_with_backend = Browser.href_with_backend
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

let app platform (local_ graph) =
  let model, inject = Prigh_ui.Component.create platform graph in
  let view =
    let%arr model in
    (* Bonsai replaces the element it binds to, so keep an [#app] wrapper for
       the messages shown after the app stops. *)
    Vdom.Node.div
      ~attrs:[ Vdom.Attr.id "app" ]
      [ Prigh_ui_web.Dom_of_screen.screen (Prigh_ui.Render.screen model) ]
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

let install_listeners ~schedule =
  let document = Dom_html.document in
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.keydown
       (Dom.handler (fun ev ->
          match Prigh_ui_web.Key_of_dom.key (key_event ev) with
          | Some key ->
            schedule (App.Action.Key key);
            Dom.preventDefault ev;
            Js._false
          | None -> Js._true))
       Js._false
     : Dom_html.event_listener_id);
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.paste
       (Dom.handler (fun ev ->
          (match Js.Opt.to_option ev##.clipboardData with
           | Some data ->
             let text = Js.to_string (data##getData (Js.string "text")) in
             if not (String.is_empty text)
             then schedule (App.Action.Intent (Paste text))
           | None -> ());
          Dom.preventDefault ev;
          Js._false))
       Js._false
     : Dom_html.event_listener_id);
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.wheel
       (Dom.handler (fun ev ->
          let dy = Js.to_float ev##.deltaY in
          if Float.(dy < 0.)
          then schedule (App.Action.Intent Scroll_up)
          else if Float.(dy > 0.)
          then schedule (App.Action.Intent Scroll_down);
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  let resize () =
    let width, height = Browser.grid_size () in
    schedule (App.Action.Resize { width; height })
  in
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       Dom_html.Event.resize
       (Dom.handler (fun _ ->
          resize ();
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  resize ()
;;

let connect_form ~backend ~token ~error =
  let escape s =
    String.concat_map s ~f:(function
      | '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | '"' -> "&quot;"
      | c -> String.of_char c)
  in
  Browser.set_app_html
    (sprintf
       {|<form class="connect" id="connect-form">
  <h1>prigh</h1>
  <p class="error">%s</p>
  <label>backend <input id="backend" value="%s" placeholder="ws://host:port/ws"></label>
  <label>token <input id="token" type="password" value="%s"></label>
  <button type="submit">connect</button>
</form>|}
       (escape error)
       (escape backend)
       (escape token));
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
            Settings.save ~token:(String.strip (Browser.input_value "token"));
            Browser.reload_with_backend backend;
            Js._false))
         Js._false
       : Dom_html.event_listener_id)
;;

let run () =
  Async_js.init ();
  let settings = Settings.load () in
  let hello = Settings.hello settings in
  let client =
    Client.create ~connect:(fun () ->
      Ws_transport.connect ~url:settings.backend)
  in
  don't_wait_for
    (match%bind.Deferred
       match%bind.Deferred Client.connect client with
       | Error e -> Deferred.return (Error (Error.to_string_hum e))
       | Ok () -> send_hello client hello ~session:None
     with
     | Error error ->
       let%map.Deferred () = Client.close client in
       connect_form
         ~backend:settings.backend
         ~token:(Option.value settings.token ~default:"")
         ~error
     | Ok reply ->
       let client_id =
         match Jsonaf.member "client_id" reply with
         | Some (`String id) -> Some id
         | _ -> None
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
       let platform = Bonsai.return (platform client ~hello ~schedule ~quit) in
       let handle =
         Start.start_and_get_handle
           (module Result_)
           ~bind_to_element_with_id:"app"
           (fun graph -> app platform graph)
       in
       handle_ref := Some handle;
       let%map.Deferred () = Start.Handle.started handle in
       Option.iter client_id ~f:(fun id ->
         schedule (App.Action.Set_client_id id));
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
                 | Closed -> Backend_closed))))
;;
