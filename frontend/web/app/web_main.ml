open! Core
open! Async_kernel
open Js_of_ocaml
open Bonsai_web
module App = Prigh_web.App
module Client = Prigh_client.Client
module Accounts = Prigh_web.Accounts
module Reconnect_gate = Prigh_web.Reconnect_gate
module Signin_link = Prigh_web.Signin_link

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
    ; prefer_host : string option
    }

  let load () =
    let non_empty s = Option.filter s ~f:(Fn.non String.is_empty) in
    { backend =
        Option.value
          (non_empty (Browser.query_param "backend"))
          ~default:(Browser.same_origin_ws_url ())
    ; login = Login.load Login.Storage.browser
    ; session = non_empty (Browser.query_param "session")
    ; prefer_host = non_empty (Browser.get_item Signin_link.prefer_host_key)
    }
  ;;

  (* Whose preferences the browser keeps ([Pref_cache]). *)
  let owner t = t.backend ^ " " ^ Option.value t.login.user ~default:""

  (* This page, for as long as it is loaded: its newer connections take
     over from its older ones on the backend. *)
  let tab =
    String.init 16 ~f:(fun _ ->
      "0123456789abcdef".[Float.iround_down_exn (Js.to_float Js.math##random *. 16.)])
  ;;

  let hello t ~session =
    [ "name", `String "prigh-web"; "tools", `False; "tab", `String tab ]
    @ Login.hello_fields t.login
    @ Option.value_map t.prefer_host ~default:[] ~f:(fun host ->
      [ "prefer_host", `String host ])
    @ Option.value_map session ~default:[] ~f:(fun s ->
      [ "session", `String s ])
  ;;
end

let refused error = String.is_prefix error ~prefix:"unauthorised"

(* A session the backend no longer has (e.g. never saved before it restarted)
   must not keep us out: we join another, and say which one we lost. *)
let send_hello client settings ~session =
  let hello session =
    Deferred.map
      (Client.call client "hello" (Settings.hello settings ~session))
      ~f:(Result.map_error ~f:Error.to_string_hum)
  in
  match%bind.Deferred hello session with
  | Error error when Option.is_some session && not (refused error) ->
    Deferred.map (hello None) ~f:(Result.map ~f:(fun reply -> reply, session))
  | result -> Deferred.return (Result.map result ~f:(fun reply -> reply, None))
;;

let url_session () =
  Option.filter (Browser.query_param "session") ~f:(Fn.non String.is_empty)
;;

(* A socket on a flaky network can take minutes to fail: give up sooner and
   try again. The [hello] may take longer: the backend may be reading a large
   session for it. *)
let connect_timeout = Time_ns.Span.of_sec 10.

let hello_timeout = Time_ns.Span.of_sec 30.

(* Connects, then says hello, each within its time. *)
let connect_and_hello client settings ~session =
  match%bind.Deferred
    Clock_ns.with_timeout connect_timeout (Client.connect client)
  with
  | `Timeout -> Deferred.return (Error "timed out")
  | `Result (Error e) -> Deferred.return (Error (Error.to_string_hum e))
  | `Result (Ok ()) ->
    (match%map.Deferred
       Clock_ns.with_timeout hello_timeout (send_hello client settings ~session)
     with
     | `Timeout -> Error "timed out"
     | `Result result -> result)
;;

(* Attempts made stale by a newer one stay put when they are due. *)
let reconnect_gate = Reconnect_gate.create ()

let reconnect client settings ~generation ~session =
  let session = Option.first_some session (url_session ()) in
  match%map.Deferred connect_and_hello client settings ~session with
  | Error "timed out" ->
    Client.disconnect client;
    App.Reply_tag.Reconnect generation, Error "timed out"
  | Error e -> Reconnect generation, Error e
  | Ok (reply, None) -> Reconnect generation, Ok reply
  | Ok (reply, Some session) -> Session_gone { generation; session }, Ok reply
;;

(* After a suspension or a change of network, a socket can look open but
   carry nothing: one that does not answer is dropped (and so reconnected).
   One that is slow but alive is not: while something comes in or other
   answers are still on their way (a large reply on a slow network, a busy
   backend: ours queues behind them), it gets more time, up to a minute. *)
let probe client =
  if not (Client.is_connected client)
  then Deferred.unit
  else (
    let others = Client.waiting client in
    let answered = Client.call client "get_state" [] in
    let rec wait ~waited ~received =
      match%bind.Deferred
        Clock_ns.with_timeout (Time_ns.Span.of_sec 8.) answered
      with
      | `Result _ -> Deferred.unit
      | `Timeout ->
        let now_received = Client.received client in
        if (now_received > received || others > 0) && waited < 7
        then wait ~waited:(waited + 1) ~received:now_received
        else (
          Client.disconnect client;
          Deferred.unit)
    in
    wait ~waited:0 ~received:(Client.received client))
;;

(* The page that loads next shows the sign-in form with this. *)
let refused_key = "prigh-web.refused"

let auth_failed message =
  Browser.set_item refused_key message;
  Dom_html.window##.location##reload
;;

let focus id =
  Browser.after_render (fun () ->
    Option.iter (Dom_html.getElementById_opt id) ~f:(fun el -> el##focus))
;;

(* Keeps the highlighted item of a picker, the completion popup or a chip's
   menu in view. *)
let reveal_selected () =
  Browser.after_render (fun () ->
    List.iter
      [ ".picker-item.selected"
      ; ".popup-item.selected"
      ; ".popover-item.selected"
      ]
      ~f:(fun selector ->
        Js.Opt.iter
          (Dom_html.document##querySelector (Js.string selector))
          (fun el ->
             (Js.Unsafe.coerce el)##scrollIntoView
               (Js.Unsafe.obj
                  [| "block", Js.Unsafe.inject (Js.string "nearest") |]))))
;;

(* A chip's menu opens upward from its chip, standing clear of the composer's
   box (raised by [--popover-lift]); below the chip only when there is not
   room enough above and more below. The height it may take goes to style.css
   as [--popover-room]. *)
let place_popover () =
  Browser.after_render (fun () ->
    Option.iter (Dom_html.getElementById_opt "chip-popover") ~f:(fun el ->
      Js.Opt.iter el##.parentNode (fun anchor ->
        let anchor : Dom_html.element Js.t = Js.Unsafe.coerce anchor in
        let chip = anchor##getBoundingClientRect in
        let box =
          match
            Js.Opt.to_option
              (Js.Unsafe.meth_call
                 anchor
                 "closest"
                 [| Js.Unsafe.inject (Js.string ".composer-box") |]
               : Dom_html.element Js.t Js.opt)
          with
          | Some box -> box##getBoundingClientRect
          | None -> chip
        in
        let margin = 8. in
        let lift = Js.to_float chip##.top -. Js.to_float box##.top in
        let above = Js.to_float box##.top -. margin in
        let below =
          Float.of_int Dom_html.window##.innerHeight
          -. Js.to_float chip##.bottom
          -. margin
        in
        let wanted = Float.of_int el##.scrollHeight in
        let down = Float.(above < wanted && below > above) in
        let set name value =
          el##.style##setProperty
            (Js.string name)
            (Js.string (sprintf "%.0fpx" value))
            Js.undefined
          |> ignore
        in
        el##setAttribute
          (Js.string "data-placement")
          (Js.string (if down then "below" else "above"));
        set "--popover-lift" lift;
        set "--popover-room" (if down then below else above);
        (* Within the composer's box: a chip far right opens leftward. *)
        set "--popover-shift" 0.;
        let menu = el##getBoundingClientRect in
        let right =
          Float.min
            (Js.to_float box##.right)
            (Float.of_int Dom_html.window##.innerWidth -. margin)
        in
        let over = Js.to_float menu##.right -. right in
        if Float.(over > 0.)
        then (
          let room = Js.to_float menu##.left -. Js.to_float box##.left in
          set "--popover-shift" (-.Float.min over room));
        (* The current choice in the middle of a long list. *)
        Js.Opt.iter
          (el##querySelector (Js.string ".popover-item.selected"))
          (fun item ->
             (Js.Unsafe.coerce item)##scrollIntoView
               (Js.Unsafe.obj
                  [| "block", Js.Unsafe.inject (Js.string "center") |])))))
;;

(* At once, so that a key typed before the next frame lands after the
   insert, and again once the page has rendered the draft, unless the text has
   changed since. *)
let set_caret ~draft byte =
  let apply ~put_value =
    Option.iter (Dom_html.getElementById_opt "editor") ~f:(fun el ->
      match Dom_html.tagged el with
      | Textarea t ->
        let same = String.equal (Js.to_string t##.value) draft in
        if put_value && not same then t##.value := Js.string draft;
        if same || put_value
        then (
          let n = Prigh_web.Utf16.units draft ~byte in
          t##focus;
          t##setSelectionRange n n)
      | _ -> ())
  in
  apply ~put_value:true;
  Browser.after_render (fun () -> apply ~put_value:false)
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

(* Whether the chat follows new output: until the user scrolls up, again once
   they scroll back to the end, press the jump button or send something. *)
let following = ref true

(* What the page last heard of [following] ([Chat_scrolled]): it follows
   again whenever it scrolls to the end itself ([Scroll_to_bottom]). *)
let reported = ref true

(* Where we last put the chat's scroll position, to tell the user's scrolls
   from ours. *)
let last_set = ref None

let at_end (el : Dom_html.element Js.t) =
  Float.(
    of_int el##.scrollHeight -. Js.to_float el##.scrollTop -. of_int el##.clientHeight
    < 40.)
;;

let follow_to_end (el : Dom_html.element Js.t) =
  el##.scrollTop := Js.float (Float.of_int el##.scrollHeight);
  last_set := Some (Js.to_float el##.scrollTop)
;;

let scroll_to_bottom () =
  following := true;
  Option.iter (chat_element ()) ~f:follow_to_end
;;

(* The user scrolled the chat themselves (wheel, touch, scrollbar, keys). *)
let user_scrolled = ref false

(* Where the chat was before the page reloaded, [from_end] its end. Not ours
   as far as the scroll listener knows, so it stops following (and shows the
   jump button). Older pages load until it is reached; what is above may
   still grow (fonts, images): it is put back while it does, until the user
   scrolls. *)
let restore_scroll from_end =
  following := false;
  user_scrolled := false;
  let rec put tries =
    Option.iter (chat_element ()) ~f:(fun chat ->
      if not (!following || !user_scrolled)
      then (
        let target = Float.of_int chat##.scrollHeight -. from_end in
        let tries =
          if Float.(target < 0.) && Paging_listeners.more ()
          then (
            chat##.scrollTop := Js.float 0.;
            Paging_listeners.load_older ();
            tries)
          else if Float.(target < 0.) && Paging_listeners.loading ()
          then tries
          else (
            if Float.(abs (Js.to_float chat##.scrollTop -. target) > 2.)
            then chat##.scrollTop := Js.float (Float.max 0. target);
            (* Reached: a little longer, while what is above settles. *)
            Int.min tries 6)
        in
        if tries > 0
        then
          ignore
            (Dom_html.setTimeout (fun () -> put (tries - 1)) 150.
             : Dom_html.timeout_id_safe)))
  in
  (* 9 s: time for a few older pages. *)
  put 60
;;

let scroll_chat pages =
  user_scrolled := true;
  if pages < 0 then following := false;
  Option.iter (chat_element ()) ~f:(fun chat ->
    let by = Float.of_int (pages * chat##.clientHeight) *. 0.85 in
    chat##.scrollTop := Js.float (Js.to_float chat##.scrollTop +. by))
;;

(* The previous (or next) of the user's messages above (or below) the top of
   the transcript's view; [false] when there is none loaded. *)
let jump_to_user_message direction =
  user_scrolled := true;
  if direction < 0 then following := false;
  Option.value_map (chat_element ()) ~default:false ~f:(fun chat ->
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
      chat##.scrollTop := Js.float (Js.to_float chat##.scrollTop +. offset -. 8.));
    Option.is_some target)
;;

(* A landmark jumped to ([Jump]): at the top of the view, marked for a
   moment. *)
let scroll_to ?(mark = true) find =
  Browser.after_render (fun () ->
    Option.iter (chat_element ()) ~f:(fun chat ->
      Option.iter (find chat) ~f:(fun (el : Dom_html.element Js.t) ->
        user_scrolled := true;
        following := false;
        let offset =
          Js.to_float el##getBoundingClientRect##.top
          -. Js.to_float chat##getBoundingClientRect##.top
        in
        chat##.scrollTop := Js.float (Js.to_float chat##.scrollTop +. offset -. 12.);
        if mark then Jump_listeners.arrived el)))
;;

let scroll_to_entry n = scroll_to (fun chat -> Jump_listeners.entry chat n)

let scroll_to_trimmed () =
  scroll_to (fun chat ->
    Js.Opt.to_option
      (chat##querySelector
         (Js.string (":scope > ." ^ Prigh_web.Trim_view.divider_class))))
;;

let scroll_to_hidden entry =
  scroll_to (fun chat ->
    Js.Opt.to_option
      (chat##querySelector
         (Js.string
            (sprintf
               ".trim-divider[%s=\"%s\"]"
               Prigh_web.Chat_view.hidden_attribute
               entry))))
;;

(* Where a deleted message was: the one before it at the top, unmarked. *)
let scroll_to_message n =
  scroll_to ~mark:false (fun chat ->
    Js.Opt.to_option
      (chat##querySelector
         (Js.string
            (sprintf
               ":scope > .entries > [%s=\"%d\"]"
               Prigh_web.Chat_view.message_attribute
               n))))
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
      let%bind.Effect reply =
        Effect.of_deferred_fun
          (fun () ->
             Reconnect_gate.schedule reconnect_gate ~generation;
             (* Pages that lost the backend together do not all come back
                at the same moment. *)
             let jittered =
               Float.of_int delay_ms *. (0.8 +. (0.4 *. Js.to_float Js.math##random))
             in
             let%bind.Deferred () =
               Clock_ns.after (Time_ns.Span.of_ms jittered)
             in
             if Reconnect_gate.due reconnect_gate ~generation
             then
               Deferred.map
                 (reconnect client settings ~generation ~session)
                 ~f:Option.some
             else Deferred.return None)
          ()
      in
      (match reply with
       | Some (tag, result) -> inject (App.Action.Reply (tag, result))
       | None -> Effect.Ignore)
    | Probe_connection -> Effect.of_deferred_fun probe client
    | Drop_connection -> Effect.of_sync_fun Client.disconnect client
    | Reload_page { keep_session } ->
      Effect.of_sync_fun
        (fun () ->
           if keep_session
           then Dom_html.window##.location##reload
           else Browser.reload_without_query_param "session")
        ()
    | Auth_failed message -> Effect.of_sync_fun auth_failed message
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
    | Clear_text_selection -> Effect.of_sync_fun Selection_listeners.clear ()
    | Place_popover -> Effect.of_sync_fun place_popover ()
    | Place_menu anchor -> Effect.of_sync_fun Menu_listeners.place anchor
    | Open_tab session ->
      Effect.of_sync_fun
        (fun () ->
           let location = Dom_html.window##.location in
           let search =
             Browser.with_query_param
               ~search:(Js.to_string location##.search)
               "session"
               session
           in
           ignore
             (Dom_html.window##open_
                (Js.string (Js.to_string location##.pathname ^ search))
                (Js.string "_blank")
                Js.null
              : Dom_html.window Js.t Js.opt))
        ()
    | Set_caret byte -> Effect.of_sync_fun (set_caret ~draft:model.draft) byte
    | Sign_out -> Effect.of_sync_fun sign_out settings
    | Reveal path -> Effect.of_sync_fun Agents_listeners.reveal path
    | Copy text -> Effect.of_sync_fun Browser.copy_to_clipboard text
    | Switch_account account -> Effect.of_sync_fun switch_account account
    | Add_account -> Effect.of_sync_fun add_account ()
    | Scroll_chat pages -> Effect.of_sync_fun scroll_chat pages
    | Scroll_to_bottom ->
      Effect.of_sync_fun
        (fun () ->
           reported := true;
           Browser.after_render (fun () ->
             match Tab_memory.take_scroll () with
             | Some from_end -> restore_scroll from_end
             | None -> scroll_to_bottom ()))
        ()
    | Jump_to_user_message direction ->
      let%bind.Effect found = Effect.of_sync_fun jump_to_user_message direction in
      (* None above: the previous one is not loaded. *)
      if found
         || direction > 0
         || not (Paging_listeners.more () || Paging_listeners.loading ())
      then Effect.Ignore
      else inject App.Action.Jump_older
    | Scroll_to_entry n -> Effect.of_sync_fun scroll_to_entry n
    | Scroll_to_trimmed -> Effect.of_sync_fun scroll_to_trimmed ()
    | Scroll_to_hidden entry -> Effect.of_sync_fun scroll_to_hidden entry
    | Scroll_to_message n -> Effect.of_sync_fun scroll_to_message n
    | Focus_terminal -> Effect.of_sync_fun Terminal_widget.focus ()
    | Notify notification ->
      Effect.of_sync_fun Page_notifications.show notification
    | Request_notification_permission ->
      let%bind.Effect permission =
        Effect.of_deferred_fun Page_notifications.request_permission ()
      in
      inject (App.Action.Notification_permission permission)
    | Set_title title -> Effect.of_sync_fun Page_notifications.set_title title
    | Files page -> Effect.of_sync_fun Files_widget.perform page
    | Cache_prefs changes ->
      Effect.of_sync_fun
        (Pref_cache.save Login.Storage.browser ~owner:(Settings.owner settings))
        changes
    | Apply_theme theme -> Effect.of_sync_fun Page_theme.apply theme
    | Keep_scroll_position ->
      Effect.of_sync_fun Paging_listeners.keep_position ()
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

let terminal
      (settings : Settings.t)
      ~inject
      (target : Prigh_web.Terminal.Target.t)
  =
  let key = Prigh_web.Terminal.Target.key target in
  Terminal_widget.view
    ~url:
      (Terminal_panel.url
         ~backend:settings.backend
         ~user:settings.login.user
         ~as_user:target.as_user
         ~token:settings.login.password
         ~session:(Some target.session)
         ~dir:target.dir)
    ~key
    ~on_status:(fun status ->
      inject (App.Action.Terminal_status { key; status }))
;;

let component client settings ~current (local_ graph) =
  let model, inject =
    Bonsai.state_machine
      ~sexp_of_action:App.Action.sexp_of_t
      ~default_model:App.init
      ~apply_action:(fun ctx model action ->
        let model', commands = App.update model action in
        current := model';
        if not (String.equal model.draft model'.draft)
        then Tab_memory.save_draft model'.draft;
        (* A confirmation or question just showed (also one over another). *)
        if
          (App.Model.waiting model' && not (App.Model.waiting model))
          || (List.is_empty model.confirms
              && not (List.is_empty model'.confirms))
        then confirm_shown := Time_ns.now ();
        (* The text, the placeholder or the editor's width changed. *)
        let layout (m : App.Model.t) =
          ( App.Model.running m
          , m.narrow
          , m.agents.open_
          , App.Model.docked m
          , m.review.open_
          , Option.is_some m.state )
        in
        if
          not
            (String.equal model.draft model'.draft
             && [%equal: bool * bool * bool * bool * bool * bool]
                  (layout model)
                  (layout model')
             && App.Connection.equal model.connection model'.connection)
        then autosize ();
        List.iter commands ~f:(perform client settings ctx model');
        model')
      graph
  in
  (* After Bonsai has patched the DOM and before the browser paints it: what
     measures or scrolls the page (the editor's height, the chat's scroll
     position, focus) lands in the frame that shows the change. A frame
     later, the editor showed new lines scrolled out of view at each
     newline and a cleared draft at its old height. *)
  Bonsai.Edge.after_display
    (Bonsai.return (Effect.of_sync_fun Browser.rendered ()))
    graph;
  let open Bonsai.Let_syntax in
  let%arr model
  and inject in
  { Result_.view =
      Prigh_web.View.view model ~inject ~terminal:(terminal settings ~inject)
  ; inject
  }
;;

let supported_images = [ "image/png"; "image/jpeg"; "image/gif"; "image/webp" ]

(* Pasted or dropped image files become images in the prompt (the backend
   checks and downscales them); other files are uploaded to the tool host and
   attached by path. *)
let read_images ~schedule (files : File.fileList Js.t) =
  let others = ref [] in
  for i = 0 to files##.length - 1 do
    Js.Opt.iter
      (files##item i)
      (fun file ->
         let mime_type = Js.to_string file##._type in
         if not (List.mem supported_images mime_type ~equal:String.equal)
         then others := file :: !others
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
  done;
  Files_widget.choose Chat (List.rev !others)
;;

let resize_handle (el : Dom_html.element Js.t) =
  Js.Opt.to_option (el##getAttribute (Js.string "data-resize"))
  |> Option.bind ~f:(fun name ->
    Prigh_web.Resize.Handle.of_name (Js.to_string name))
;;

let key_target (ev : Dom_html.keyboardEvent Js.t) : Prigh_web.Keys.Target.t =
  match Js.Opt.to_option ev##.target with
  | None -> Page
  | Some el when Terminal_widget.contains el -> Terminal
  | Some el when Option.is_some (resize_handle el) ->
    Resize (Option.value_exn (resize_handle el))
  | Some el
    when Files_widget.in_panel el
         &&
         match Dom_html.tagged el with
         | Input _ | Textarea _ | Select _ | Button _ | A _ -> false
         | _ -> true -> Files
  | Some el
    when (match Dom_html.getElementById_opt "review-page" with
          | None -> false
          | Some page ->
            Js.to_bool ((Js.Unsafe.coerce page)##contains el : bool Js.t))
         &&
         match Dom_html.tagged el with
         | Input _ | Textarea _ | Select _ | Button _ | A _ -> false
         | _ -> true -> Review
  | Some el
    when Js.Opt.test (el##getAttribute (Js.string "data-worktree")) ->
    let attr name =
      Js.to_string
        (Js.Opt.get (el##getAttribute (Js.string name)) (fun () -> Js.string ""))
    in
    Worktree { session = attr "data-owner"; path = attr "data-worktree" }
  | Some el
    when Js.Opt.test (el##getAttribute (Js.string "tabindex"))
         && Js.Opt.test
              (el##getAttribute
                 (Js.string Prigh_web.Chat_view.message_attribute)) ->
    (match
       Int.of_string_opt
         (Js.to_string
            (Js.Opt.get
               (el##getAttribute
                  (Js.string Prigh_web.Chat_view.message_attribute))
               (fun () -> Js.string "")))
     with
     | Some n -> Message n
     | None -> Page)
  | Some el
    when Js.Opt.test (el##getAttribute (Js.string "data-session")) ->
    Session
      (Js.to_string
         (Js.Opt.get (el##getAttribute (Js.string "data-session")) (fun () ->
            Js.string "")))
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
     | Textarea _ -> Multiline
     | Input _ | Select _ -> Field
     | Button _ | A _ -> Control
     | _ ->
       if String.equal (String.lowercase (Js.to_string el##.tagName)) "summary"
       then Control
       else Page)
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

(* Tab and Shift+Tab go round the open dialog (the page behind is inert),
   not out to the browser's toolbar. The page moves the focus itself, so the
   order does not depend on where each browser starts sequential navigation;
   a scrolling body is a stop, as browsers make it one. *)
let wrap_focus (ev : Dom_html.keyboardEvent Js.t) =
  let k = key ev in
  if (not (String.equal k.key "Tab")) || k.alt || k.ctrl || k.meta
  then false
  else (
    match
      List.last
        (Dom.list_of_nodeList
           (Dom_html.document##querySelectorAll (Js.string ".modal")))
    with
    | None -> false
    | Some modal ->
      let stops =
        Dom.list_of_nodeList
          (modal##querySelectorAll
             (Js.string
                "button:not(:disabled), input:not(:disabled), \
                 textarea:not(:disabled), select:not(:disabled), a[href], \
                 summary, [tabindex]:not([tabindex='-1']), .modal-body"))
        |> List.filter ~f:(fun (el : Dom_html.element Js.t) ->
          if Js.to_bool (el##.classList##contains (Js.string "modal-body"))
          then el##.scrollHeight > el##.clientHeight
          else true)
        |> Array.of_list
      in
      let n = Array.length stops in
      if n = 0
      then false
      else (
        let active : Dom_html.element Js.t option =
          Js.Opt.to_option (Js.Unsafe.coerce Dom_html.document)##.activeElement
        in
        let index =
          Option.bind active ~f:(fun a ->
            Array.findi stops ~f:(fun _ el ->
              phys_equal (Js.Unsafe.coerce a) el)
            |> Option.map ~f:fst)
        in
        let target =
          match index, k.shift with
          | Some i, false -> (i + 1) % n
          | Some i, true -> (i - 1 + n) % n
          | None, false -> 0
          | None, true -> n - 1
        in
        stops.(target)##focus;
        true))
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
  let in_terminal (ev : #Dom_html.event Js.t) =
    Js.Opt.case ev##.target (fun () -> false) Terminal_widget.contains
  in
  (* Before xterm.js, which keeps the keys it handles to itself: only Ctrl+`
     is ours there. *)
  listen ~capture:true Dom_html.Event.keydown (fun ev ->
    match
      if in_terminal ev then Prigh_web.Keys.handle !current (key ev) else None
    with
    | Some action ->
      Dom.preventDefault ev;
      Dom_html.stopPropagation ev;
      current := fst (App.update !current action);
      schedule action;
      Js._false
    | None -> Js._true);
  listen Dom_html.Event.keydown (fun ev ->
    (* An IME composing text owns its keys. *)
    if Js.to_bool (Js.Unsafe.coerce ev)##.isComposing || in_terminal ev
    then Js._true
    else if
      App.Model.waiting !current
      && String.equal (key ev).key "Enter"
      && Time_ns.Span.(
           Time_ns.diff (Time_ns.now ()) !confirm_shown < of_int_ms 350)
    then (
      Dom.preventDefault ev;
      Js._false)
    else (
      match Prigh_web.Keys.handle !current (key ev) with
      | None ->
        if wrap_focus ev
        then (
          Dom.preventDefault ev;
          Js._false)
        else Js._true
      | Some action ->
        Dom.preventDefault ev;
        (* Bonsai applies it on the next frame: a key pressed before then must
           see its effect (e.g. Esc closing a dialog, then Ctrl+K). *)
        current := fst (App.update !current action);
        schedule action;
        reveal_selected ();
        Js._false));
  Browser.on_viewport_change (fun () ->
    autosize ();
    schedule (App.Action.Set_narrow (narrow ()));
    schedule (Resize_widget.viewport ()));
  listen Dom_html.Event.paste (fun (ev : Dom_html.clipboardEvent Js.t) ->
    Js.Opt.iter ev##.clipboardData (fun data ->
      if data##.files##.length > 0 && not (in_terminal ev)
      then (
        Dom.preventDefault ev;
        read_images ~schedule data##.files));
    Js._true);
  listen Dom_html.Event.dragover (fun ev ->
    Dom.preventDefault ev;
    Js._true);
  listen Dom_html.Event.drop (fun (ev : Dom_html.dragEvent Js.t) ->
    Dom.preventDefault ev;
    Files_widget.drag_end ();
    if not (in_terminal ev)
    then
      Js.Opt.iter ev##.dataTransfer (fun data ->
        if Js.Opt.case ev##.target (fun () -> false) Files_widget.in_panel
        then Files_widget.choose Here (Files_widget.files_of data##.files)
        else read_images ~schedule data##.files);
    Js._true);
  Chat_listeners.install ~schedule;
  Paging_listeners.install ~schedule;
  Agents_listeners.install ~schedule;
  Jump_listeners.install ();
  Selection_listeners.install ~schedule;
  Menu_listeners.install ();
  Review_margin.install ();
  (* The chat follows new output unless the user has scrolled up. The page
     hears of a change of mind ([Chat_scrolled]) to show the jump button. *)
  let set_following value =
    following := value;
    if not (Bool.equal value !reported)
    then (
      reported := value;
      schedule (App.Action.Chat_scrolled { at_bottom = value }))
  in
  let in_chat (ev : #Dom_html.event Js.t) =
    match chat_element () with
    | None -> false
    | Some chat ->
      Js.Opt.case
        ev##.target
        (fun () -> false)
        (fun target ->
           Js.to_bool
             ((Js.Unsafe.coerce chat)##contains target : bool Js.t))
  in
  (* The scroll events miss what changed the chat's height since the last
     one (an output folded, a message added): leaving the page has the last
     word. *)
  let save_scroll () =
    Option.iter (chat_element ()) ~f:Tab_memory.save_scroll
  in
  (* Following the output scrolls at every chunk: sessionStorage is written
     at most every 300ms (and when the page goes). *)
  let save_pending = ref false in
  let save_scroll_soon () =
    if not !save_pending
    then (
      save_pending := true;
      ignore
        (Dom_html.setTimeout
           (fun () ->
              save_pending := false;
              save_scroll ())
           300.
         : Dom_html.timeout_id_safe))
  in
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       (Dom_html.Event.make "pagehide")
       (Dom.handler (fun _ ->
          save_scroll ();
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  listen (Dom_html.Event.make "visibilitychange") (fun _ ->
    if Js.to_bool (Js.Unsafe.get Dom_html.document "hidden")
    then save_scroll ();
    Js._true);
  List.iter
    [ Dom_html.Event.make "wheel"
    ; Dom_html.Event.make "touchstart"
    ; Dom_html.Event.mousedown
    ]
    ~f:(fun event ->
      listen event (fun ev ->
        if in_chat ev then user_scrolled := true;
        Js._true));
  (* Intent first: a wheel or swipe up stops following at once, before the
     scroll it causes lands, so output arriving meanwhile cannot undo it. *)
  listen
    (Dom_html.Event.make "wheel")
    (fun (ev : Dom_html.mouseScrollEvent Js.t) ->
       if in_chat ev && Float.(Js.to_float (Js.Unsafe.coerce ev)##.deltaY < 0.)
       then set_following false;
       Js._true);
  let touch_y = ref None in
  listen Dom_html.Event.touchstart (fun (ev : Dom_html.touchEvent Js.t) ->
    touch_y
    := Js.Optdef.to_option (ev##.touches##item 0)
       |> Option.map ~f:(fun t -> Js.to_float t##.clientY);
    Js._true);
  listen Dom_html.Event.touchmove (fun (ev : Dom_html.touchEvent Js.t) ->
    (match !touch_y, Js.Optdef.to_option (ev##.touches##item 0) with
     | Some y0, Some t when in_chat ev && Float.(Js.to_float t##.clientY > y0 +. 4.)
       -> set_following false
     | _ -> ());
    Js._true);
  (* Scroll events do not bubble: only capturing sees the chat's. Scrolling
     back to the end follows again; scrolling up any other way (scrollbar,
     keys) stops. *)
  listen ~capture:true (Dom_html.Event.make "scroll") (fun ev ->
    (match chat_element () with
     | Some el when phys_equal (Dom_html.eventTarget ev :> Dom.node Js.t) (el :> Dom.node Js.t) ->
       save_scroll_soon ();
       let top = Js.to_float el##.scrollTop in
       let ours =
         Option.value_map !last_set ~default:false ~f:(fun s ->
           Float.(abs (top -. s) < 2.))
       in
       if at_end el
       then set_following true
       else if not ours
       then set_following false
     | _ -> ());
    Js._true);
  let observer =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun records _ ->
         if !following && not (Terminal_widget.only_inside records)
         then Option.iter (chat_element ()) ~f:follow_to_end))
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

(* [getTimezoneOffset] is UTC minus local time, in minutes. *)
let utc_offset () =
  let minutes = (new%js Js.date_now)##getTimezoneOffset in
  Time_ns.Span.of_min (Float.of_int (-minutes))
;;

let run_app (settings : Settings.t) =
  let client =
    Client.create ~connect:(fun () ->
      Ws_transport.connect ~url:settings.backend)
  in
  (* A page that was in a session before (reloaded, or brought back by the
     browser) on a network that is down starts offline and reconnects. *)
  let known () =
    Option.is_some settings.session
    || List.exists (Accounts.load storage) ~f:(fun a ->
      String.equal a.backend settings.backend)
  in
  don't_wait_for
    (match%bind.Deferred
       match%map.Deferred
         connect_and_hello client settings ~session:settings.session
       with
       | Error "timed out" ->
         Client.disconnect client;
         Error "timed out"
       | result -> result
     with
     | Error error when refused error || not (known ()) ->
       let%map.Deferred () = Client.close client in
       (* Nothing to be wrong about before the first sign-in. *)
       let error =
         match settings.login.password with
         | None when String.is_prefix error ~prefix:"unauthorised" -> ""
         | _ -> error
       in
       sign_in_form settings ~error
     | result ->
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
       Option.iter (Result.ok result) ~f:(fun (reply, _) ->
         Option.iter
           (Prigh_protocol.Hello_reply.of_json reply |> Or_error.ok)
           ~f:(fun hello -> schedule (App.Action.Hello hello)));
       if
         Option.is_some settings.login.password
         || Option.is_some settings.login.user
       then schedule App.Action.Saved_login;
       schedule
         (App.Action.Set_accounts
            { accounts =
                (if Result.is_ok result
                 then Accounts.remember storage ~backend:settings.backend
                 else Accounts.load storage)
            ; current = Accounts.current storage ~backend:settings.backend
            });
       schedule (App.Action.Set_narrow (narrow ()));
       schedule
         (App.Action.Set_notification_permission
            (Page_notifications.permission ()));
       (let prefs, legacy =
          Pref_cache.load Login.Storage.browser ~owner:(Settings.owner settings)
        in
        schedule (App.Action.Restore_prefs { prefs; legacy }));
       Page_notifications.set_title "prigh";
       Page_notifications.on_visibility_change (fun hidden ->
         schedule (App.Action.Page_hidden hidden));
       Option.iter (Tab_memory.saved_draft ()) ~f:(fun draft ->
         schedule (App.Action.Set_draft draft));
       (match result with
        | Ok (_, None) -> schedule App.Action.Start
        | Ok (_, Some lost) ->
          schedule App.Action.Start;
          schedule
            (App.Action.Show_toast
               { text =
                   sprintf
                     "Session %s is no longer on the backend (it restarted \
                      before saving it): this is another one."
                     lost
               ; error = true
               })
        | Error _ -> schedule App.Action.Backend_closed);
       let on_window event action =
         ignore
           (Dom_html.addEventListener
              Dom_html.window
              (Dom_html.Event.make event)
              (Dom.handler (fun _ ->
                 schedule action;
                 Js._true))
              Js._false
            : Dom_html.event_listener_id)
       in
       on_window "online" (App.Action.Network true);
       on_window "offline" (App.Action.Network false);
       (* The offset changes with daylight saving time. *)
       let tick () =
         schedule (App.Action.Tick (Time_ns.now ()));
         let utc_offset = utc_offset () in
         if not (Time_ns.Span.equal utc_offset !current.utc_offset)
         then schedule (App.Action.Set_utc_offset utc_offset)
       in
       tick ();
       Clock_ns.every (Time_ns.Span.of_sec 30.) tick;
       Clock_ns.every (Time_ns.Span.of_sec 1.) (fun () ->
         if App.Model.ticking !current
         then schedule (App.Action.Clock (Time_ns.now ())));
       install_listeners ~schedule ~current;
       Files_widget.install ~schedule ~backend:settings.backend;
       Resize_widget.install ~schedule;
       don't_wait_for
         (Pipe.iter_without_pushback
            (Client.incoming client)
            ~f:(fun incoming ->
              schedule
                (match incoming with
                 | Event e ->
                   Event (Prigh_web.Chat.received e ~at:(Time_ns.now ()))
                 | Protocol_error e -> Protocol_error e
                 | Stderr text -> Show_toast { text; error = true }
                 | Closed -> Backend_closed)));
       Deferred.unit)
;;

let start () =
  let settings = Settings.load () in
  let refused_message = Browser.get_item refused_key in
  Browser.remove_item refused_key;
  match refused_message with
  | Some message -> sign_in_form settings ~notice:true ~error:message
  | None when Login.take_signed_out Login.Storage.browser ->
    sign_in_form settings ~notice:true ~error:"Signed out."
  | None when take_adding () -> sign_in_form settings ~adding:true ~error:""
  | None -> run_app settings
;;

(* Opened by [prigh web]: signed in with what it was given, which replaces
   any sign-out or refusal noted before. A code already used changes
   nothing. *)
let sign_in_from_link code ~f =
  Browser.post ~url:Signin_link.path ~body:code ~f:(fun reply ->
    Option.iter
      (Option.bind reply ~f:Signin_link.Credentials.of_json)
      ~f:(fun (c : Signin_link.Credentials.t) ->
        (match c.prefer_host with
         | Some host -> Browser.set_item Signin_link.prefer_host_key host
         | None -> Browser.remove_item Signin_link.prefer_host_key);
        Accounts.activate
          storage
          { Accounts.Account.backend = (Settings.load ()).backend
          ; user = c.user
          ; token = c.token
          ; session = None
          };
        ignore (Login.take_signed_out Login.Storage.browser : bool);
        ignore (take_adding () : bool);
        Browser.remove_item refused_key);
    f ())
;;

let run () =
  Async_js.init ();
  match Signin_link.code_of_hash (Browser.hash ()) with
  | Some code ->
    Browser.clear_hash ();
    sign_in_from_link code ~f:start
  | None -> start ()
;;
