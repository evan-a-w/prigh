open! Core
open! Async_kernel
open Js_of_ocaml
open Bonsai_web
module App = Prigh_web.App
module Client = Prigh_client.Client
module Browser = Prigh_ui_web_app.Browser
module Login = Prigh_ui_web_app.Login
module Ws_transport = Prigh_ui_web_app.Ws_transport

module Settings = struct
  type t =
    { backend : string
    ; login : Login.t
    ; session : string option
    }

  let load () =
    let non_empty s = Option.filter s ~f:(Fn.non String.is_empty) in
    { backend =
        Option.value
          (non_empty (Browser.query_param "backend"))
          ~default:(Browser.same_origin_ws_url ())
    ; login = Login.load Login.Storage.browser
    ; session = non_empty (Browser.query_param "session")
    }
  ;;

  let hello t ~session =
    [ "name", `String "prigh-web"; "tools", `False ]
    @ Login.hello_fields t.login
    @ Option.value_map session ~default:[] ~f:(fun s ->
      [ "session", `String s ])
  ;;
end

(* A session the backend no longer has (e.g. never saved before it restarted)
   must not keep us out: start a new one instead. *)
let send_hello client settings ~session =
  let hello session =
    Deferred.map
      (Client.call client "hello" (Settings.hello settings ~session))
      ~f:(Result.map_error ~f:Error.to_string_hum)
  in
  match%bind.Deferred hello session with
  | Error _ when Option.is_some session -> hello None
  | result -> Deferred.return result
;;

let history_key = "prigh-web.history"

let load_history () =
  match Browser.get_item history_key with
  | None -> []
  | Some text ->
    (match Jsonaf.parse text with
     | Ok (`Array items) ->
       List.filter_map items ~f:(function
         | `String s -> Some s
         | _ -> None)
     | _ -> [])
;;

let save_history entries =
  Browser.set_item
    history_key
    (Jsonaf.to_string (`Array (List.map entries ~f:(fun s -> `String s))))
;;

let focus id =
  Browser.after_render (fun () ->
    Option.iter (Dom_html.getElementById_opt id) ~f:(fun el -> el##focus))
;;

(* Reloading drops the socket; the next page load sees the note and shows the
   sign-in form. *)
let sign_out () =
  Login.forget Login.Storage.browser;
  Browser.reload_without_query_param "session"
;;

let perform client settings ctx (command : App.Command.t) =
  let inject = Bonsai.Apply_action_context.inject ctx in
  let eff =
    match command with
    | Rpc { method_; params; tag } ->
      let%bind.Effect result =
        Effect.of_deferred_fun
          (fun () ->
             Deferred.map
               (Client.call client method_ params)
               ~f:(Result.map_error ~f:Error.to_string_hum))
          ()
      in
      inject (App.Action.Reply (tag, result))
    | Reconnect { generation; delay_ms; session } ->
      let%bind.Effect result =
        Effect.of_deferred_fun
          (fun () ->
             let%bind.Deferred () =
               Clock_ns.after (Time_ns.Span.of_int_ms delay_ms)
             in
             match%bind.Deferred Client.connect client with
             | Error e -> Deferred.return (Error (Error.to_string_hum e))
             | Ok () -> send_hello client settings ~session)
          ()
      in
      inject (App.Action.Reply (Reconnect generation, result))
    | Set_url_session session ->
      Effect.of_sync_fun (Browser.replace_query_param "session") session
    | Expire_toast { id; after_ms } ->
      let%bind.Effect () =
        Effect.of_deferred_fun
          (fun () -> Clock_ns.after (Time_ns.Span.of_int_ms after_ms))
          ()
      in
      inject (App.Action.Dismiss_toast id)
    | Focus id -> Effect.of_sync_fun focus id
    | Save_history entries -> Effect.of_sync_fun save_history entries
    | Sign_out -> Effect.of_sync_fun sign_out ()
  in
  Bonsai.Apply_action_context.schedule_event ctx eff
;;

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

(* The editor grows with its text, up to the CSS max-height. *)
let autosize () =
  Browser.after_render (fun () ->
    Option.iter
      (Dom_html.getElementById_coerce "editor" Dom_html.CoerceTo.textarea)
      ~f:(fun el ->
        el##.style##.height := Js.string "auto";
        el##.style##.height := Js.string (sprintf "%dpx" el##.scrollHeight)))
;;

let component client settings ~current (local_ graph) =
  let model, inject =
    Bonsai.state_machine
      ~sexp_of_action:App.Action.sexp_of_t
      ~default_model:App.init
      ~apply_action:(fun ctx model action ->
        let model', commands = App.update model action in
        current := model';
        if
          not
            (String.equal model.draft model'.draft
             && Bool.equal (App.Model.running model) (App.Model.running model')
             && Bool.equal model.narrow model'.narrow)
        then autosize ();
        List.iter commands ~f:(perform client settings ctx);
        model')
      graph
  in
  let open Bonsai.Let_syntax in
  let%arr model
  and inject in
  { Result_.view = Prigh_web.View.view model ~inject; inject }
;;

let supported_images = [ "image/png"; "image/jpeg"; "image/gif"; "image/webp" ]

(* Pasted or dropped image files become attachments; the backend checks and
   downscales them. *)
let read_images ~schedule (files : File.fileList Js.t) =
  for i = 0 to files##.length - 1 do
    Js.Opt.iter
      (files##item i)
      (fun file ->
         let mime_type = Js.to_string file##._type in
         if not (String.is_prefix mime_type ~prefix:"image/")
         then ()
         else if not (List.mem supported_images mime_type ~equal:String.equal)
         then
           schedule
             (App.Action.Show_toast
                { text =
                    sprintf
                      "%s is not supported: use PNG, JPEG, GIF or WebP"
                      mime_type
                ; error = true
                })
         else (
           let reader = new%js File.fileReader in
           reader##.onload
           := Dom.handler (fun _ ->
                (match
                   Js.Opt.to_option (File.CoerceTo.string reader##.result)
                 with
                 | Some url ->
                   let url = Js.to_string url in
                   (match String.lsplit2 url ~on:',' with
                    | Some (_, data) ->
                      schedule
                        (App.Action.Add_image
                           { mime_type
                           ; data
                           ; bytes = Prigh_protocol.Image.decoded_size data
                           })
                    | None -> ())
                 | None -> ());
                Js._true);
           reader##readAsDataURL file))
  done
;;

let key_target (ev : Dom_html.keyboardEvent Js.t) : Prigh_web.Keys.Target.t =
  match Js.Opt.to_option ev##.target with
  | None -> Page
  | Some el ->
    (match Dom_html.tagged el with
     | Textarea t when String.equal (Js.to_string t##.id) "editor" ->
       Editor { cursor = t##.selectionStart }
     | Textarea _ | Input _ | Select _ -> Field
     | _ -> Page)
;;

let key (ev : Dom_html.keyboardEvent Js.t) : Prigh_web.Keys.t =
  { key = Js.Optdef.case ev##.key (fun () -> "") Js.to_string
  ; shift = Js.to_bool ev##.shiftKey
  ; alt = Js.to_bool ev##.altKey
  ; ctrl = Js.to_bool ev##.ctrlKey
  ; meta = Js.to_bool ev##.metaKey
  ; target = key_target ev
  }
;;

(* Keeps the highlighted item of a picker or the completion popup in view. *)
let reveal_selected () =
  Browser.after_render (fun () ->
    List.iter
      [ ".picker-item.selected"; ".popup-item.selected" ]
      ~f:(fun selector ->
        Js.Opt.iter
          (Dom_html.document##querySelector (Js.string selector))
          (fun el ->
             (Js.Unsafe.coerce el)##scrollIntoView
               (Js.Unsafe.obj
                  [| "block", Js.Unsafe.inject (Js.string "nearest") |]))))
;;

let narrow () = Dom_html.window##.innerWidth < 760

let install_listeners ~schedule ~current =
  let document = Dom_html.document in
  let listen event handler =
    ignore
      (Dom_html.addEventListener document event (Dom.handler handler) Js._false
       : Dom_html.event_listener_id)
  in
  listen Dom_html.Event.keydown (fun ev ->
    (* An IME composing text owns its keys. *)
    if Js.to_bool (Js.Unsafe.coerce ev)##.isComposing
    then Js._true
    else (
      match Prigh_web.Keys.handle !current (key ev) with
      | None -> Js._true
      | Some action ->
        Dom.preventDefault ev;
        (* Bonsai applies it on the next frame: a key pressed before then must
           see its effect (e.g. Esc closing a dialog, then Ctrl+K). *)
        current := fst (App.update !current action);
        schedule action;
        reveal_selected ();
        Js._false));
  Browser.on_viewport_change (fun () ->
    schedule (App.Action.Set_narrow (narrow ())));
  listen Dom_html.Event.paste (fun (ev : Dom_html.clipboardEvent Js.t) ->
    Js.Opt.iter ev##.clipboardData (fun data ->
      if data##.files##.length > 0
      then (
        Dom.preventDefault ev;
        read_images ~schedule data##.files));
    Js._true);
  listen Dom_html.Event.dragover (fun ev ->
    Dom.preventDefault ev;
    Js._true);
  listen Dom_html.Event.drop (fun (ev : Dom_html.dragEvent Js.t) ->
    Dom.preventDefault ev;
    Js.Opt.iter ev##.dataTransfer (fun data ->
      read_images ~schedule data##.files);
    Js._true);
  Chat_listeners.install ();
  (* The chat follows new output unless the user has scrolled up. *)
  let stick = ref true in
  let chat () = Dom_html.getElementById_opt "chat" in
  let at_bottom (el : Dom_html.element Js.t) =
    Float.(
      of_int el##.scrollHeight
      -. Js.to_float el##.scrollTop
      -. of_int el##.clientHeight
      < 60.)
  in
  listen (Dom_html.Event.make "scroll") (fun _ ->
    Option.iter (chat ()) ~f:(fun el -> stick := at_bottom el);
    Js._true);
  let observer =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun _ _ ->
         if !stick
         then
           Option.iter (chat ()) ~f:(fun el ->
             el##.scrollTop := Js.float (Float.of_int el##.scrollHeight))))
  in
  let options = MutationObserver.empty_mutation_observer_init () in
  options##.childList := true;
  options##.subtree := true;
  options##.characterData := true;
  observer##observe (document :> Dom.node Js.t) options
;;

let escape s =
  String.concat_map s ~f:(function
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '&' -> "&amp;"
    | '"' -> "&quot;"
    | c -> String.of_char c)
;;

let sign_in_form ?(notice = false) (settings : Settings.t) ~error =
  let value = Option.value_map ~default:"" ~f:escape in
  Browser.set_app_html
    (sprintf
       {|<div class="signin-page"><form class="signin" id="signin-form">
  <div class="brand"><span class="logo">prigh</span><span class="brand-sub">web</span></div>
  <h1>Sign in</h1>
  <p class="signin-sub">to the prigh backend at %s</p>
  <p class="signin-error%s">%s</p>
  <label>User name <input id="user" name="user" value="%s" autofocus autocomplete="username" autocapitalize="off" spellcheck="false"></label>
  <label>Password <input id="password" name="password" type="password" value="%s" autocomplete="current-password"></label>
  <details><summary>Backend</summary><input id="backend" name="backend" value="%s"></details>
  <button class="btn primary" type="submit">Sign in</button>
</form></div>|}
       (escape settings.backend)
       (if notice then " notice" else "")
       (escape error)
       (value settings.login.user)
       (value settings.login.password)
       (escape settings.backend));
  Option.iter
    (Dom_html.getElementById_coerce "signin-form" Dom_html.CoerceTo.form)
    ~f:(fun form ->
      ignore
        (Dom_html.addEventListener
           form
           Dom_html.Event.submit
           (Dom.handler (fun ev ->
              Dom.preventDefault ev;
              Login.save
                Login.Storage.browser
                ~user:(Browser.input_value "user")
                ~password:(Browser.input_value "password");
              Browser.reload_with_backend
                (String.strip (Browser.input_value "backend"));
              Js._false))
           Js._false
         : Dom_html.event_listener_id))
;;

let run_app (settings : Settings.t) =
  let client =
    Client.create ~connect:(fun () ->
      Ws_transport.connect ~url:settings.backend)
  in
  don't_wait_for
    (match%bind.Deferred
       match%bind.Deferred Client.connect client with
       | Error e -> Deferred.return (Error (Error.to_string_hum e))
       | Ok () -> send_hello client settings ~session:settings.session
     with
     | Error error ->
       let%map.Deferred () = Client.close client in
       (* Nothing to be wrong about before the first sign-in. *)
       let error =
         match settings.login.password with
         | None when String.is_prefix error ~prefix:"unauthorised" -> ""
         | _ -> error
       in
       sign_in_form settings ~error
     | Ok reply ->
       let handle_ref = ref None in
       let current = ref App.init in
       let schedule action =
         Option.iter !handle_ref ~f:(fun handle ->
           Start.Handle.schedule handle action)
       in
       let handle =
         Start.start_and_get_handle
           (module Result_)
           ~bind_to_element_with_id:"app"
           (component client settings ~current)
       in
       handle_ref := Some handle;
       Option.iter
         (Prigh_protocol.Hello_reply.of_json reply |> Or_error.ok)
         ~f:(fun hello -> schedule (App.Action.Hello hello));
       if
         Option.is_some settings.login.password
         || Option.is_some settings.login.user
       then schedule App.Action.Saved_login;
       schedule (App.Action.Set_narrow (narrow ()));
       schedule (App.Action.Load_history (load_history ()));
       schedule App.Action.Start;
       let tick () = schedule (App.Action.Tick (Time_ns.now ())) in
       tick ();
       Clock_ns.every (Time_ns.Span.of_sec 30.) tick;
       install_listeners ~schedule ~current;
       don't_wait_for
         (Pipe.iter_without_pushback
            (Client.incoming client)
            ~f:(fun incoming ->
              schedule
                (match incoming with
                 | Event e -> Event e
                 | Protocol_error e -> Protocol_error e
                 | Stderr text -> Show_toast { text; error = true }
                 | Closed -> Backend_closed)));
       Deferred.unit)
;;

let run () =
  Async_js.init ();
  let settings = Settings.load () in
  if Login.take_signed_out Login.Storage.browser
  then sign_in_form settings ~notice:true ~error:"Signed out."
  else run_app settings
;;
