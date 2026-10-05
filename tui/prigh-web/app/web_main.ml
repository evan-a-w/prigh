open! Core
open! Async_kernel
open Js_of_ocaml
open Bonsai_web
module App = Prigh_web.App
module Client = Prigh_client.Client
module Browser = Prigh_ui_web_app.Browser
module Login = Prigh_ui_web_app.Login
module Ws_transport = Prigh_ui_web_app.Ws_transport
module Accounts = Prigh_web.Accounts

let storage =
  { Accounts.Storage.get = Browser.get_item
  ; set = Browser.set_item
  ; remove = Browser.remove_item
  }
;;

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

(* Loads the page for [backend] (and rejoins [session]). *)
let navigate ~backend ~session =
  let location = Dom_html.window##.location in
  let search =
    Option.value_map session ~default:"" ~f:(fun s ->
      Browser.with_query_param ~search:"" "session" s)
  in
  location##.href
  := Js.string
       (Browser.href_with_backend
          ~pathname:(Js.to_string location##.pathname)
          ~search
          ~backend)
;;

(* Reloading drops the socket; the next page load sees the note and shows the
   sign-in form, with the other saved accounts. *)
let sign_out (settings : Settings.t) =
  Option.iter
    (Accounts.current storage ~backend:settings.backend)
    ~f:(fun account -> Accounts.remove storage account);
  Login.forget Login.Storage.browser;
  Browser.reload_without_query_param "session"
;;

let switch_account (account : Accounts.Account.t) =
  Accounts.activate storage account;
  navigate ~backend:account.backend ~session:account.session
;;

(* The next page load shows the sign-in form instead of connecting. *)
let adding_key = "prigh-web.adding"

let add_account () =
  Browser.set_item adding_key "1";
  Browser.reload_without_query_param "session"
;;

let take_adding () =
  let adding = Option.is_some (Browser.get_item adding_key) in
  Browser.remove_item adding_key;
  adding
;;

let chat_element () = Dom_html.getElementById_opt "chat"

(* The chat follows new output unless the user has scrolled up. *)
let follow = ref true

let scroll_to_end () =
  Option.iter (chat_element ()) ~f:(fun el ->
    el##.scrollTop := Js.float (Float.of_int el##.scrollHeight))
;;

let follow_chat () =
  follow := true;
  Browser.after_render scroll_to_end
;;

let scroll_chat pages =
  if pages < 0 then follow := false;
  Option.iter (chat_element ()) ~f:(fun chat ->
    let by = Float.of_int (pages * chat##.clientHeight) *. 0.85 in
    chat##.scrollTop := Js.float (Js.to_float chat##.scrollTop +. by))
;;

(* The previous (or next) of the user's messages above (or below) the top of
   the transcript's view. *)
let jump_to_user_message direction =
  if direction < 0 then follow := false;
  Option.iter (chat_element ()) ~f:(fun chat ->
    let top = Js.to_float chat##getBoundingClientRect##.top in
    let offsets =
      Dom.list_of_nodeList (chat##querySelectorAll (Js.string ".msg.user"))
      |> List.map ~f:(fun (el : Dom_html.element Js.t) ->
        Js.to_float el##getBoundingClientRect##.top -. top)
    in
    let target =
      if direction < 0
      then
        List.filter offsets ~f:(fun o -> Float.(o < -8.))
        |> List.max_elt ~compare:Float.compare
      else
        List.filter offsets ~f:(fun o -> Float.(o > 8.))
        |> List.min_elt ~compare:Float.compare
    in
    Option.iter target ~f:(fun offset ->
      chat##.scrollTop := Js.float (Js.to_float chat##.scrollTop +. offset -. 8.)))
;;

let perform client settings ctx (model : App.Model.t) (command : App.Command.t) =
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
      Effect.of_sync_fun
        (fun () ->
           Browser.replace_query_param "session" session;
           (* Acting as another user, the session is not this account's. *)
           match
             ( model.account
             , Option.bind model.hello ~f:Prigh_protocol.Hello_reply.acting_as )
           with
           | Some account, None -> Accounts.set_session storage account session
           | _ -> ())
        ()
    | Expire_toast { id; after_ms } ->
      let%bind.Effect () =
        Effect.of_deferred_fun
          (fun () -> Clock_ns.after (Time_ns.Span.of_int_ms after_ms))
          ()
      in
      inject (App.Action.Dismiss_toast id)
    | Focus id -> Effect.of_sync_fun focus id
    | Save_history entries -> Effect.of_sync_fun save_history entries
    | Sign_out -> Effect.of_sync_fun sign_out settings
    | Reveal path -> Effect.of_sync_fun Agents_listeners.reveal path
    | Copy text -> Effect.of_sync_fun Browser.copy_to_clipboard text
    | Switch_account account -> Effect.of_sync_fun switch_account account
    | Add_account -> Effect.of_sync_fun add_account ()
    | Scroll_chat pages -> Effect.of_sync_fun scroll_chat pages
    | Jump_to_user_message direction ->
      Effect.of_sync_fun jump_to_user_message direction
    | Follow_chat -> Effect.of_sync_fun follow_chat ()
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

(* When the first of the tool confirmations on show appeared: an Enter typed
   just before must not answer it. *)
let confirm_shown = ref Time_ns.epoch

let component client settings ~current (local_ graph) =
  let model, inject =
    Bonsai.state_machine
      ~sexp_of_action:App.Action.sexp_of_t
      ~default_model:App.init
      ~apply_action:(fun ctx model action ->
        let model', commands = App.update model action in
        current := model';
        if List.is_empty model.confirms && not (List.is_empty model'.confirms)
        then confirm_shown := Time_ns.now ();
        if
          not
            (String.equal model.draft model'.draft
             && Bool.equal (App.Model.running model) (App.Model.running model')
             && Bool.equal model.narrow model'.narrow)
        then autosize ();
        List.iter commands ~f:(perform client settings ctx model');
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
       Editor
         { cursor =
             Prigh_web.Utf16.byte_offset
               (Js.to_string t##.value)
               ~utf16:t##.selectionStart
         }
     | Input i when String.equal (Js.to_string i##.id) "session-search" ->
       Session_search
     | Textarea _ | Input _ | Select _ -> Field
     | Button _ | A _ -> Control
     | _ ->
       (match
          Js.Opt.to_option (el##getAttribute (Js.string "data-session"))
        with
        | Some id -> Session (Js.to_string id)
        | None ->
          if
            String.equal
              (String.lowercase (Js.to_string el##.tagName))
              "summary"
          then Control
          else Page))
;;

(* Selected text in a field or the page (so Ctrl+X cuts or does nothing). *)
let has_selection (ev : Dom_html.keyboardEvent Js.t) =
  let in_field =
    match Js.Opt.to_option ev##.target with
    | None -> false
    | Some el ->
      (match Dom_html.tagged el with
       | Textarea t -> t##.selectionStart <> t##.selectionEnd
       | Input i -> i##.selectionStart <> i##.selectionEnd
       | _ -> false)
  in
  in_field
  || not
       (String.is_empty
          (Js.to_string
             (Js.Unsafe.meth_call
                (Js.Unsafe.meth_call Dom_html.window "getSelection" [||])
                "toString"
                [||])))
;;

let key (ev : Dom_html.keyboardEvent Js.t) : Prigh_web.Keys.t =
  { key = Js.Optdef.case ev##.key (fun () -> "") Js.to_string
  ; code = Js.Optdef.case ev##.code (fun () -> "") Js.to_string
  ; selection = has_selection ev
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
  let listen ?(capture = false) event handler =
    ignore
      (Dom_html.addEventListener
         document
         event
         (Dom.handler handler)
         (Js.bool capture)
       : Dom_html.event_listener_id)
  in
  listen Dom_html.Event.keydown (fun ev ->
    (* An IME composing text owns its keys. *)
    if Js.to_bool (Js.Unsafe.coerce ev)##.isComposing
    then Js._true
    else if
      (not (List.is_empty (!current : App.Model.t).confirms))
      && String.equal (key ev).key "Enter"
      && Time_ns.Span.(
           Time_ns.diff (Time_ns.now ()) !confirm_shown < of_int_ms 500)
    then (
      Dom.preventDefault ev;
      Js._false)
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
  Agents_listeners.install ~schedule;
  let at_bottom (el : Dom_html.element Js.t) =
    Float.(
      of_int el##.scrollHeight
      -. Js.to_float el##.scrollTop
      -. of_int el##.clientHeight
      < 60.)
  in
  (* Scrolling up stops following at once: a smooth scroll's first frames
     are still near the end, and new output must not pull it back. *)
  let last_top = ref 0. in
  let in_chat target =
    match chat_element (), Js.Opt.to_option target with
    | Some chat, Some target ->
      Js.to_bool
        (Js.Unsafe.meth_call chat "contains" [| Js.Unsafe.inject target |])
    | _ -> false
  in
  listen
    ~capture:true
    Dom_html.Event.wheel
    (fun (ev : Dom_html.mousewheelEvent Js.t) ->
       if
         in_chat ev##.target
         && Float.(Js.to_float (Js.Unsafe.coerce ev)##.deltaY < 0.)
       then follow := false;
       Js._true);
  (* Scroll events do not bubble: only capturing sees the chat's. *)
  listen ~capture:true (Dom_html.Event.make "scroll") (fun ev ->
    Option.iter (chat_element ()) ~f:(fun el ->
      let target = Js.Opt.to_option ev##.target in
      if
        Option.exists target ~f:(fun t ->
          Js.strict_equals (Js.Unsafe.coerce t) el)
      then (
        let top = Js.to_float el##.scrollTop in
        if Float.(top < !last_top -. 1.)
        then follow := false
        else if at_bottom el
        then follow := true;
        last_top := top));
    Js._true);
  let observer =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun _ _ -> if !follow then scroll_to_end ()))
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

let on_click id f =
  Option.iter (Dom_html.getElementById_opt id) ~f:(fun el ->
    el##.onclick
    := Dom_html.handler (fun ev ->
         Dom.preventDefault ev;
         f ();
         Js._false))
;;

(* [adding]: another account, keeping the one we are signed in as. *)
let rec sign_in_form
          ?(notice = false)
          ?(adding = false)
          (settings : Settings.t)
          ~error
  =
  let value = Option.value_map ~default:"" ~f:escape in
  let accounts = Accounts.load storage in
  let current = Accounts.current storage ~backend:settings.backend in
  let is_current account =
    Option.exists current ~f:(Accounts.Account.same account)
  in
  let saved =
    match accounts with
    | [] -> ""
    | accounts ->
      sprintf
        {|<div class="saved-accounts"><p class="saved-title">%s</p>%s</div><p class="saved-or">or sign in</p>|}
        (if adding then "Saved accounts" else "Continue as")
        (String.concat
           (List.mapi accounts ~f:(fun i account ->
              sprintf
                {|<div class="saved-account%s"><button class="saved-switch" type="button" id="account-%d"><span class="avatar">%s</span><span class="saved-who"><span class="saved-name">%s</span><span class="saved-host">%s</span></span>%s</button><button class="btn icon ghost saved-forget" type="button" id="forget-%d" title="Forget %s" aria-label="Forget %s">×</button></div>|}
                (if is_current account then " current" else "")
                i
                (escape
                   (String.prefix
                      (String.uppercase (Accounts.Account.name account))
                      1))
                (escape (Accounts.Account.name account))
                (escape (Accounts.Account.host account))
                (if is_current account && not adding
                 then {|<span class="saved-badge">last used</span>|}
                 else "")
                i
                (escape (Accounts.Account.name account))
                (escape (Accounts.Account.name account)))))
  in
  let back =
    match adding, current with
    | true, Some account ->
      sprintf
        {|<button class="btn ghost" type="button" id="signin-back">Back to %s</button>|}
        (escape (Accounts.Account.name account))
    | _ -> ""
  in
  let login : Login.t =
    if adding then { user = None; password = None } else settings.login
  in
  Browser.set_app_html
    (sprintf
       {|<div class="signin-page"><form class="signin" id="signin-form">
  <div class="brand"><span class="logo">prigh</span><span class="brand-sub">web</span></div>
  <h1>%s</h1>
  <p class="signin-sub">to the prigh backend at %s</p>
  <p class="signin-error%s">%s</p>
  %s
  <label>User name <input id="user" name="user" value="%s" autofocus autocomplete="username" autocapitalize="off" spellcheck="false"></label>
  <label>Password <input id="password" name="password" type="password" value="%s" autocomplete="current-password"></label>
  <details><summary>Backend</summary><input id="backend" name="backend" value="%s"></details>
  <button class="btn primary" type="submit">Sign in</button>
  %s
</form></div>|}
       (if adding then "Add an account" else "Sign in")
       (escape settings.backend)
       (if notice then " notice" else "")
       (escape error)
       saved
       (value login.user)
       (value login.password)
       (escape settings.backend)
       back);
  List.iteri accounts ~f:(fun i account ->
    on_click (sprintf "account-%d" i) (fun () -> switch_account account);
    on_click (sprintf "forget-%d" i) (fun () ->
      Accounts.remove storage account;
      sign_in_form ~notice ~adding settings ~error));
  Option.iter current ~f:(fun account ->
    on_click "signin-back" (fun () -> switch_account account));
  Option.iter
    (Dom_html.getElementById_coerce "signin-form" Dom_html.CoerceTo.form)
    ~f:(fun form ->
      ignore
        (Dom_html.addEventListener
           form
           Dom_html.Event.submit
           (Dom.handler (fun ev ->
              Dom.preventDefault ev;
              let field id =
                Option.filter
                  (Some (String.strip (Browser.input_value id)))
                  ~f:(Fn.non String.is_empty)
              in
              let backend =
                Option.value (field "backend") ~default:settings.backend
              in
              let account =
                { Accounts.Account.backend
                ; user = field "user"
                ; token = field "password"
                ; session = None
                }
              in
              (* Saved as an account once the backend accepts it. *)
              Accounts.activate storage account;
              navigate
                ~backend
                ~session:
                  (if adding || not (String.equal backend settings.backend)
                   then None
                   else settings.session);
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
       schedule
         (App.Action.Set_accounts
            { accounts = Accounts.remember storage ~backend:settings.backend
            ; current = Accounts.current storage ~backend:settings.backend
            });
       schedule (App.Action.Set_narrow (narrow ()));
       schedule (App.Action.Load_history (load_history ()));
       schedule App.Action.Start;
       let tick () = schedule (App.Action.Tick (Time_ns.now ())) in
       tick ();
       Clock_ns.every (Time_ns.Span.of_sec 30.) tick;
       Clock_ns.every (Time_ns.Span.of_sec 1.) (fun () ->
         if App.Model.ticking !current
         then schedule (App.Action.Clock (Time_ns.now ())));
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
  else if take_adding ()
  then sign_in_form settings ~adding:true ~error:""
  else run_app settings
;;
