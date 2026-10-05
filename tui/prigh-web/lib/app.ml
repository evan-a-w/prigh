open! Core
open! Import

module Auth_purpose = struct
  type t =
    | Refresh
    | Login_picker
    | Logout_picker
    | Show
  [@@deriving sexp_of, equal]
end

module Reply_tag = struct
  type t =
    | Ignore
    | Show_error
    | State
    | Messages
    | Sessions
    | Models
    | Auth_status of Auth_purpose.t
    | Paths of string
    | Restored
    | Dequeued
    | Deleted of string
    | Refresh_sessions
    | Login_started
    | Notice of string
    | Reconnect of int
  [@@deriving sexp_of, equal]
end

module Command = struct
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * Json.t) list
        ; tag : Reply_tag.t
        }
    | Reconnect of
        { generation : int
        ; delay_ms : int
        ; session : string option
        }
    | Set_url_session of string
    | Expire_toast of
        { id : int
        ; after_ms : int
        }
    | Focus of string
    | Save_history of string list
    | Sign_out
  [@@deriving sexp_of, equal]
end

module Connection = struct
  type t =
    | Connected
    | Reconnecting of
        { attempt : int
        ; generation : int
        }
  [@@deriving sexp_of, equal]

  let delay_ms ~attempt =
    if attempt <= 0
    then 0
    else Int.min 10_000 (250 * (1 lsl Int.min 10 (attempt - 1)))
  ;;
end

module Toast = struct
  type t =
    { id : int
    ; text : string
    ; error : bool
    }
  [@@deriving sexp_of, equal]

  let lifetime_ms = 4000
end

module Confirm = struct
  type t =
    { call_id : string
    ; name : string
    ; summary : string
    }
  [@@deriving sexp_of, equal]
end

module Action = struct
  type t =
    | Start
    | Hello of Hello_reply.t
    | Event of Event.t
    | Protocol_error of string
    | Backend_closed
    | Reply of Reply_tag.t * (Json.t, string) Result.t
    | Tick of Time_ns.t
    | Set_narrow of bool
    | Load_history of string list
    | Set_draft of string
    | Edit of
        { text : string
        ; cursor : int
        }
    | Send
    | Send_follow_up
    | Abort
    | History_older
    | History_newer
    | Complete_move of int
    | Complete_accept of { run : bool }
    | Complete_choose of int
    | Complete_close
    | New_session
    | Switch_session of string
    | Ask_delete of string
    | Set_session_query of string
    | Open_sessions
    | Set_model of string
    | Set_thinking of string
    | Open_model_picker
    | Open_thinking_picker
    | Open_help
    | Open_rename
    | Open_agents
    | Picker_query of string
    | Picker_move of int
    | Picker_accept
    | Picker_choose of string
    | Dialog_input of string
    | Dialog_move of int
    | Dialog_accept
    | Close_dialog
    | Login_choose of int
    | Start_login of string
    | Logout of string
    | Cancel_subagent of string
    | Kill_job of string
    | Dequeue
    | Toggle_sidebar
    | Respond_confirm of
        { call_id : string
        ; allow : bool
        }
    | Add_image of Image.t
    | Remove_image of int
    | Show_toast of
        { text : string
        ; error : bool
        }
    | Dismiss_toast of int
    | Sign_out
  [@@deriving sexp_of]
end

module Model = struct
  type t =
    { connection : Connection.t
    ; generation : int
    ; hello : Hello_reply.t option
    ; state : State.t option
    ; chat : Chat.t
    ; sessions : Session_summary.t list
    ; models : Llm.t list
    ; auth : Auth_status.t list
    ; now : Time_ns.t option
    ; narrow : bool
    ; draft : string
    ; cursor : int
    ; completion : Completion.t option
    ; history : History.t
    ; images : Image.t list
    ; queue : int * int
    ; confirms : Confirm.t list
    ; dialog : Dialog.t option
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    ; session_query : string
    }
  [@@deriving sexp_of]

  let running t =
    match t.state with
    | Some s -> s.running
    | None -> false
  ;;

  let popup t =
    Option.filter t.completion ~f:(fun c ->
      not (List.is_empty (Completion.items c)))
  ;;
end

let thinking_levels = Prigh_ui.Commands.thinking_levels

let init =
  { Model.connection = Connected
  ; generation = 0
  ; hello = None
  ; state = None
  ; chat = Chat.empty
  ; sessions = []
  ; models = []
  ; auth = []
  ; now = None
  ; narrow = false
  ; draft = ""
  ; cursor = 0
  ; completion = None
  ; history = History.empty
  ; images = []
  ; queue = 0, 0
  ; confirms = []
  ; dialog = None
  ; toasts = []
  ; next_toast = 0
  ; sidebar_open = true
  ; session_query = ""
  }
;;

let rpc ?(tag = Reply_tag.Show_error) method_ params =
  Command.Rpc { method_; params; tag }
;;

let str s = `String s
let max_toasts = 4

let toast (m : Model.t) ?(error = false) text =
  let id = m.next_toast in
  let toasts = m.toasts @ [ { Toast.id; text; error } ] in
  ( { m with
      toasts = List.drop toasts (List.length toasts - max_toasts)
    ; next_toast = id + 1
    }
  , if error
    then []
    else [ Command.Expire_toast { id; after_ms = Toast.lifetime_ms } ] )
;;

let error m text = toast m ~error:true text
let focus_editor = Command.Focus "editor"

let startup =
  [ rpc "get_state" [] ~tag:State
  ; rpc "list_models" [] ~tag:Models
  ; rpc "auth_status" [] ~tag:(Auth_status Refresh)
  ]
;;

let decode (m : Model.t) json of_json ~f =
  match of_json json with
  | Ok value -> f value
  | Error e -> error m (Error.to_string_hum e)
;;

let decode_list of_json json =
  match json with
  | `Array items -> Or_error.all (List.map items ~f:of_json)
  | _ -> Or_error.error_string "expected an array"
;;

let strings json =
  decode_list
    (function
      | `String s -> Ok s
      | _ -> Or_error.error_string "expected a string")
    json
;;

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let close_dialog (m : Model.t) = { m with dialog = None }, [ focus_editor ]

(* A new session (switched, new, forked) starts from its own messages. *)
let set_state (m : Model.t) (state : State.t) =
  let changed =
    match m.state with
    | Some old -> not (String.equal old.session_id state.session_id)
    | None -> true
  in
  let m = { m with state = Some state } in
  if changed
  then
    ( { m with
        chat = Chat.empty
      ; confirms = []
      ; queue = 0, 0
      ; dialog = Option.filter m.dialog ~f:(Fn.non Dialog.per_session)
      }
    , [ Command.Set_url_session state.session_id
      ; rpc "get_messages" [] ~tag:Messages
      ; rpc "list_sessions" [] ~tag:Sessions
      ] )
  else m, []
;;

let logged_in (m : Model.t) provider =
  List.exists m.auth ~f:(fun s ->
    String.equal s.provider provider && Option.is_some s.configured)
;;

let compact_tokens n =
  if n >= 1_000_000
  then
    sprintf
      "%gM"
      (Float.round_decimal ~decimal_digits:1 (Float.of_int n /. 1e6))
  else if n >= 1000
  then sprintf "%dk" (n / 1000)
  else Int.to_string n
;;

let model_picker ?(query = "") (m : Model.t) =
  let current = Option.map m.state ~f:(fun s -> s.model.key) in
  let known = not (List.is_empty m.auth) in
  let usable (model : Llm.t) = (not known) || logged_in m model.provider in
  let models =
    List.stable_sort m.models ~compare:(fun a b ->
      Bool.compare (usable b) (usable a))
  in
  let items =
    List.map models ~f:(fun model ->
      Picker.Item.create
        ~id:model.key
        ~detail:
          (sprintf
             "%s · %s ctx%s"
             model.provider
             (compact_tokens model.context_window)
             (if usable model then "" else " · not logged in"))
        ~search:(model.name ^ " " ^ model.provider ^ " " ^ model.key)
        ~marked:(Option.equal String.equal current (Some model.key))
        ~dimmed:(not (usable model))
        model.name)
  in
  Dialog.Picker
    { kind = Models; picker = Picker.create ~query ~title:"Switch model" items }
;;

let thinking_picker (m : Model.t) =
  let current = Option.value_map m.state ~default:"" ~f:(fun s -> s.thinking) in
  let detail = function
    | "off" -> "answer straight away"
    | "low" -> "a little"
    | "on" -> "the provider's default budget"
    | "high" -> "more"
    | "max" -> "as much as the model allows"
    | _ -> ""
  in
  Dialog.Picker
    { kind = Thinking
    ; picker =
        Picker.create
          ~title:"Thinking level"
          (List.map thinking_levels ~f:(fun level ->
             Picker.Item.create
               ~id:level
               ~detail:(detail level)
               ~marked:(String.equal level current)
               level))
    }
;;

let login_picker (statuses : Auth_status.t list) =
  let items =
    List.concat_map statuses ~f:(fun s ->
      List.map s.methods ~f:(fun meth ->
        let configured =
          match s.configured with
          | Some c when String.equal c.method_ meth.method_ ->
            sprintf " · logged in via %s" c.source
          | _ -> ""
        in
        Picker.Item.create
          ~id:(s.provider ^ " " ^ meth.method_)
          ~detail:(meth.label ^ configured)
          ~search:(s.name ^ " " ^ s.provider ^ " " ^ meth.method_)
          ~marked:(not (String.is_empty configured))
          s.name))
  in
  Dialog.Picker
    { kind = Login; picker = Picker.create ~title:"Log in to" items }
;;

let logout_picker (statuses : Auth_status.t list) =
  List.filter_map statuses ~f:(fun s ->
    Option.map s.configured ~f:(fun c ->
      Picker.Item.create
        ~id:s.provider
        ~detail:(sprintf "%s via %s" c.method_ c.source)
        ~search:(s.name ^ " " ^ s.provider)
        s.name))
  |> function
  | [] -> None
  | items ->
    Some
      (Dialog.Picker
         { kind = Logout; picker = Picker.create ~title:"Log out of" items })
;;

let open_dialog (m : Model.t) ?(focus = "dialog") dialog =
  { m with dialog = Some dialog; completion = None }, [ Command.Focus focus ]
;;

let open_picker m dialog = open_dialog m ~focus:"picker-input" dialog

let start_login (m : Model.t) ~provider ~method_ =
  let m, cmds = open_dialog m (Login (Login_flow.start provider)) in
  ( m
  , cmds
    @ [ rpc
          "login"
          (("provider", str provider)
           :: Option.value_map method_ ~default:[] ~f:(fun m ->
             [ "method", str m ]))
          ~tag:Login_started
      ] )
;;

let set_thinking (m : Model.t) level =
  match m.state with
  | Some state when not state.model.supports_thinking ->
    error
      m
      (sprintf
         "%s has no thinking levels: switch to a model that thinks with /model"
         state.model.name)
  | _ -> m, [ rpc "set_thinking" [ "thinking", str level ] ]
;;

let set_model (m : Model.t) key = m, [ rpc "set_model" [ "model", str key ] ]

let set_draft (m : Model.t) ?cursor draft =
  { m with draft; cursor = Option.value cursor ~default:(String.length draft) }
;;

let refresh_completion (m : Model.t) =
  match
    Completion.compute
      ~text:m.draft
      ~cursor:m.cursor
      ~models:m.models
      ~auth:m.auth
      ~current_model:(Option.map m.state ~f:(fun s -> s.model.key))
  with
  | None -> { m with completion = None }, []
  | Some c ->
    (match m.completion with
     | Some old when Completion.same old c -> m, []
     | _ ->
       ( { m with completion = Some c }
       , (match Completion.request c with
          | None -> []
          | Some (method_, prefix) ->
            [ rpc method_ [ "prefix", str prefix ] ~tag:(Paths prefix) ]) ))
;;

let edit m ?cursor text = refresh_completion (set_draft m ?cursor text)

let open_sessions (m : Model.t) =
  { m with sidebar_open = true }, [ Command.Focus "session-search" ]
;;

let open_rename (m : Model.t) =
  let name =
    Option.value_map m.state ~default:"" ~f:(fun s ->
      Option.value s.session_name ~default:"")
  in
  open_dialog m ~focus:"dialog-input" (Rename name)
;;

let run_command (m : Model.t) ({ name; rest } : Slash.Parsed.t) =
  let none = "" in
  match name, rest with
  | "help", _ -> open_dialog m Help
  | "new", _ -> m, [ rpc "new_session" [] ]
  | "model", "" -> open_picker m (model_picker m)
  | "model", query ->
    (match Prigh_ui.Model_match.resolve m.models query with
     | Found model -> set_model m model.key
     | Ambiguous _ -> open_picker m (model_picker ~query m)
     | Not_found suggestions ->
       error
         m
         (sprintf
            "No model matches %S.%s Ctrl+L lists them all."
            query
            (match suggestions with
             | [] -> none
             | l ->
               " Did you mean "
               ^ String.concat
                   ~sep:", "
                   (List.map l ~f:(fun (s : Llm.t) -> s.name))
               ^ "?")))
  | "thinking", "" -> open_picker m (thinking_picker m)
  | "thinking", level ->
    if List.mem thinking_levels level ~equal:String.equal
    then set_thinking m level
    else
      error
        m
        (sprintf
           "Unknown thinking level %S: use %s."
           level
           (String.concat ~sep:", " thinking_levels))
  | "compact", _ ->
    let m, cmds = toast m "Compacting the conversation…" in
    m, cmds @ [ rpc "compact" [] ~tag:(Notice "Compacted the conversation") ]
  | "name", "" -> open_rename m
  | "name", name ->
    m, [ rpc "set_session_name" [ "name", str name ] ~tag:Refresh_sessions ]
  | "sessions", _ -> open_sessions m
  | "clone", _ -> m, [ rpc "clone" [] ]
  | "cd", "" -> error m "Usage: /cd <path> (Tab completes directories)"
  | "cd", path ->
    ( m
    , [ rpc
          "set_cwd"
          [ "path", str path ]
          ~tag:(Notice ("Working directory: " ^ path))
      ] )
  | "abort", _ -> m, [ rpc "abort" [] ~tag:Restored ]
  | "agents", _ -> open_dialog m Agents
  | "login", "" -> m, [ rpc "auth_status" [] ~tag:(Auth_status Login_picker) ]
  | "login", args ->
    (match
       String.split args ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
     with
     | provider :: method_ :: _ ->
       start_login m ~provider ~method_:(Some method_)
     | _ -> start_login m ~provider:args ~method_:None)
  | "logout", "" -> m, [ rpc "auth_status" [] ~tag:(Auth_status Logout_picker) ]
  | "logout", provider -> m, [ rpc "logout" [ "provider", str provider ] ]
  | "auth", _ -> m, [ rpc "auth_status" [] ~tag:(Auth_status Show) ]
  | "signout", _ -> m, [ Command.Sign_out ]
  | name, _ ->
    error
      m
      (match Slash.closest name with
       | Some spec ->
         sprintf
           "Unknown command /%s. Did you mean /%s? (/help lists them)"
           name
           spec.name
       | None -> sprintf "Unknown command /%s: /help lists the commands." name)
;;

let reply (m : Model.t) (tag : Reply_tag.t) result =
  match tag, result with
  | Reconnect generation, _ when generation <> m.generation -> m, []
  | Reconnect _, Error _ ->
    (match m.connection with
     | Connected -> m, []
     | Reconnecting { attempt; generation } ->
       let attempt = attempt + 1 in
       ( { m with connection = Reconnecting { attempt; generation } }
       , [ Command.Reconnect
             { generation
             ; delay_ms = Connection.delay_ms ~attempt
             ; session = Option.map m.state ~f:(fun s -> s.session_id)
             }
         ] ))
  | Reconnect _, Ok json ->
    let m = { m with connection = Connected } in
    let m =
      match Hello_reply.of_json json with
      | Ok hello -> { m with hello = Some hello }
      | Error _ -> m
    in
    (* The session may have moved on while we were away: start over. *)
    let m, cmds =
      toast { m with state = None; chat = Chat.empty } "Reconnected"
    in
    m, cmds @ startup
  | (Ignore | Paths _ | Auth_status Refresh), Error _ -> m, []
  | Deleted title, Error e ->
    error
      m
      (sprintf
         "Couldn't delete %S: %s%s"
         title
         e
         (if String.is_substring e ~substring:"live"
          then
            ". Switch to another session and close other tabs using it first."
          else ""))
  | Login_started, Error e ->
    (match m.dialog with
     | Some (Login flow) ->
       { m with dialog = Some (Login { flow with failed = Some e }) }, []
     | _ -> error m e)
  | _, Error e -> error m e
  | (Ignore | Show_error | Login_started), Ok _ -> m, []
  | Notice text, Ok _ -> toast m text
  | State, Ok json -> decode m json State.of_json ~f:(set_state m)
  | Messages, Ok json ->
    decode m json (decode_list Message.of_json) ~f:(fun messages ->
      { m with chat = Chat.of_messages messages }, [])
  | Sessions, Ok json ->
    decode m json (decode_list Session_summary.of_json) ~f:(fun sessions ->
      { m with sessions }, [])
  | Refresh_sessions, Ok _ -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  | Models, Ok json ->
    decode m json (decode_list Llm.of_json) ~f:(fun models ->
      { m with models }, [])
  | Auth_status purpose, Ok json ->
    decode m json (decode_list Auth_status.of_json) ~f:(fun auth ->
      let m = { m with auth } in
      match purpose with
      | Refresh -> m, []
      | Show -> open_dialog m (Auth auth)
      | Login_picker -> open_picker m (login_picker auth)
      | Logout_picker ->
        (match logout_picker auth with
         | Some dialog -> open_picker m dialog
         | None -> toast m "No provider is logged in: /login logs in to one."))
  | Paths prefix, Ok json ->
    (match m.completion, strings json with
     | Some c, Ok paths ->
       { m with completion = Some (Completion.set_results c ~prefix paths) }, []
     | _ -> m, [])
  | Restored, Ok json ->
    (match Option.map (field json "restored") ~f:strings with
     | Some (Ok (_ :: _ as texts)) ->
       let draft =
         String.concat
           ~sep:"\n\n"
           (texts @ List.filter [ m.draft ] ~f:(Fn.non String.is_empty))
       in
       let m = set_draft m draft in
       let m, cmds =
         toast
           m
           (sprintf
              "Stopped; %d queued message%s back in the editor"
              (List.length texts)
              (if List.length texts = 1 then "" else "s"))
       in
       m, cmds @ [ focus_editor ]
     | _ -> m, [])
  | Dequeued, Ok json ->
    (match field json "text" with
     | Some (`String text) ->
       let draft =
         if String.is_empty m.draft then text else text ^ "\n\n" ^ m.draft
       in
       set_draft m draft, [ focus_editor ]
     | _ -> toast m "Nothing is queued")
  | Deleted title, Ok _ ->
    let m, cmds = toast m (sprintf "Deleted %S" title) in
    m, cmds @ [ rpc "list_sessions" [] ~tag:Sessions ]
;;

let auth_event (m : Model.t) (e : Auth_event.t) =
  match e, m.dialog with
  | Done { provider; method_ }, dialog ->
    let m =
      match dialog with
      | Some (Login _) -> { m with dialog = None }
      | _ -> m
    in
    let m, cmds = toast m (sprintf "Logged in to %s (%s)" provider method_) in
    ( m
    , cmds
      @ [ rpc "auth_status" [] ~tag:(Auth_status Refresh)
        ; rpc "list_models" [] ~tag:Models
        ] )
  | Logged_out provider, _ ->
    let m, cmds = toast m (sprintf "Logged out of %s" provider) in
    m, cmds @ [ rpc "auth_status" [] ~tag:(Auth_status Refresh) ]
  | _, Some (Login flow) ->
    { m with dialog = Some (Login (Login_flow.apply flow e)) }, []
  | Failed { provider; error = e }, _ ->
    error m (sprintf "Login to %s failed: %s. /login tries again." provider e)
  | (Auth_url _ | Prompt _), _ ->
    open_dialog m (Login (Login_flow.apply (Login_flow.start "") e))
  | (Prompt_cancelled _ | Progress _), _ -> m, []
;;

let event (m : Model.t) (event : Event.t) =
  let m = { m with chat = Chat.apply m.chat event } in
  match event with
  | State state -> set_state m state
  | Notice text -> toast m text
  | Queue_update { steer; follow_up } -> { m with queue = steer, follow_up }, []
  | Tool_confirm { call_id; name; summary } ->
    ( { m with confirms = m.confirms @ [ { call_id; name; summary } ] }
    , [ Command.Focus "confirm" ] )
  | Tool_end { call; _ } ->
    ( { m with
        confirms =
          List.filter m.confirms ~f:(fun c ->
            not (String.equal c.call_id call.id))
      }
    , [] )
  | Agent_end _ -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  | Auth e -> auth_event m e
  | _ -> m, []
;;

let image_json (image : Image.t) =
  `Object [ "mime_type", str image.mime_type; "data", str image.data ]
;;

let send (m : Model.t) ~follow_up =
  let text = String.strip m.draft in
  let sent (m : Model.t) =
    let history = History.add m.history text in
    ( { (set_draft m "") with history; completion = None }
    , [ Command.Save_history (History.to_list history) ] )
  in
  match Slash.parse text with
  | Some parsed when List.is_empty m.images ->
    let m, save = sent m in
    let m, cmds = run_command m parsed in
    m, save @ cmds
  | _ ->
    if String.is_empty text && List.is_empty m.images
    then m, []
    else (
      let method_ =
        if follow_up
        then "follow_up"
        else if Model.running m
        then "steer"
        else "prompt"
      in
      let images =
        match m.images with
        | [] -> []
        | images -> [ "images", `Array (List.map images ~f:image_json) ]
      in
      let m, save =
        if String.is_empty text
        then { m with completion = None }, []
        else sent m
      in
      ( { m with images = [] }
      , save @ [ rpc method_ (("text", str text) :: images) ] ))
;;

let accept_completion (m : Model.t) ~run =
  match Model.popup m with
  | None -> m, []
  | Some c ->
    let draft, cursor = Completion.accept c ~text:m.draft in
    let runs =
      run
      &&
      match Completion.source c, Completion.selected_item c with
      | Command, Some item ->
        (match Slash.find item.id with
         | Some spec -> String.is_empty spec.args
         | None -> false)
      | Argument Directory, _ -> false
      | Argument _, _ -> true
      | (Command | Path), _ -> false
    in
    if runs
    then send (set_draft m (String.strip draft)) ~follow_up:false
    else edit m ~cursor draft
;;

let picker_accept (m : Model.t) ~kind (item : Picker.Item.t) =
  let m = { m with dialog = None } in
  let m, cmds =
    match (kind : Dialog.Picker_kind.t) with
    | Models -> set_model m item.id
    | Thinking -> set_thinking m item.id
    | Login ->
      (match String.lsplit2 item.id ~on:' ' with
       | Some (provider, method_) ->
         start_login m ~provider ~method_:(Some method_)
       | None -> start_login m ~provider:item.id ~method_:None)
    | Logout -> m, [ rpc "logout" [ "provider", str item.id ] ]
  in
  m, (if Option.is_none m.dialog then [ focus_editor ] else []) @ cmds
;;

let with_picker (m : Model.t) ~f =
  match m.dialog with
  | Some (Picker { kind; picker }) ->
    { m with dialog = Some (Picker { kind; picker = f picker }) }, []
  | _ -> m, []
;;

let respond_login (m : Model.t) (flow : Login_flow.t) =
  match Login_flow.answer flow with
  | None -> m, []
  | Some (id, value) ->
    ( { m with dialog = Some (Login { flow with prompt = None; input = "" }) }
    , [ rpc "auth_respond" [ "id", str id; "value", str value ] ] )
;;

let dialog_accept (m : Model.t) =
  match m.dialog with
  | None -> m, []
  | Some (Picker { kind; picker }) ->
    (match Picker.selected_item picker with
     | Some item -> picker_accept m ~kind item
     | None -> m, [])
  | Some (Rename name) ->
    let name = String.strip name in
    if String.is_empty name
    then error m "Type a name for the session (Esc keeps the current one)."
    else (
      let m, cmds = close_dialog m in
      ( m
      , cmds
        @ [ rpc "set_session_name" [ "name", str name ] ~tag:Refresh_sessions ]
      ))
  | Some (Delete { path; title }) ->
    let m, cmds = close_dialog m in
    m, cmds @ [ rpc "delete_session" [ "path", str path ] ~tag:(Deleted title) ]
  | Some (Login flow) ->
    (match flow.failed, flow.prompt with
     | None, Some _ -> respond_login m flow
     | Some _, _ -> close_dialog m
     | None, None -> m, [])
  | Some (Help | Auth _ | Agents) -> close_dialog m
;;

let update (m : Model.t) (action : Action.t) =
  match action with
  | Start -> m, startup
  | Hello hello -> { m with hello = Some hello }, []
  | Event e -> event m e
  | Protocol_error e -> error m ("Protocol error: " ^ e)
  | Backend_closed ->
    (match m.connection with
     | Reconnecting _ -> m, []
     | Connected ->
       let generation = m.generation + 1 in
       ( { m with
           connection = Reconnecting { attempt = 0; generation }
         ; generation
         }
       , [ Command.Reconnect
             { generation
             ; delay_ms = 0
             ; session = Option.map m.state ~f:(fun s -> s.session_id)
             }
         ] ))
  | Reply (tag, result) -> reply m tag result
  | Tick now ->
    let refresh =
      match m.connection, m.now with
      | Connected, Some _ -> [ rpc "list_sessions" [] ~tag:Sessions ]
      | _ -> []
    in
    { m with now = Some now }, refresh
  | Set_narrow narrow ->
    if Bool.equal narrow m.narrow
    then m, []
    else { m with narrow; sidebar_open = not narrow }, []
  | Load_history entries -> { m with history = History.of_list entries }, []
  | Set_draft draft -> edit m draft
  | Edit { text; cursor } -> edit m ~cursor text
  | Send -> send m ~follow_up:false
  | Send_follow_up -> send m ~follow_up:true
  | Abort -> m, [ rpc "abort" [] ~tag:Restored ]
  | History_older ->
    (match History.older m.history ~draft:m.draft with
     | None -> m, []
     | Some (history, text) -> edit { m with history } ~cursor:0 text)
  | History_newer ->
    (match History.newer m.history with
     | None -> m, []
     | Some (history, text) -> edit { m with history } text)
  | Complete_move delta ->
    ( { m with
        completion =
          Option.map m.completion ~f:(fun c -> Completion.move c delta)
      }
    , [] )
  | Complete_accept { run } -> accept_completion m ~run
  | Complete_choose i ->
    (match m.completion with
     | None -> m, []
     | Some c ->
       let c = Completion.move c (i - Completion.selected c) in
       accept_completion { m with completion = Some c } ~run:true)
  | Complete_close -> { m with completion = None }, []
  | New_session ->
    ( { m with sidebar_open = m.sidebar_open && not m.narrow }
    , [ rpc "new_session" [] ] )
  | Switch_session path ->
    let m = { m with sidebar_open = m.sidebar_open && not m.narrow } in
    let current = Option.map m.state ~f:(fun s -> s.session_path) in
    if Option.equal String.equal current (Some path)
    then m, []
    else m, [ rpc "switch_session" [ "path", str path ] ]
  | Ask_delete path ->
    (match List.find m.sessions ~f:(fun s -> String.equal s.path path) with
     | None -> m, []
     | Some s -> open_dialog m (Delete { path; title = Session_list.title s }))
  | Set_session_query session_query -> { m with session_query }, []
  | Open_sessions -> open_sessions m
  | Set_model key -> set_model m key
  | Set_thinking level -> set_thinking m level
  | Open_model_picker -> open_picker m (model_picker m)
  | Open_thinking_picker -> open_picker m (thinking_picker m)
  | Open_help -> open_dialog m Help
  | Open_rename -> open_rename m
  | Open_agents -> open_dialog m Agents
  | Picker_query query -> with_picker m ~f:(fun p -> Picker.set_query p query)
  | Picker_move delta -> with_picker m ~f:(fun p -> Picker.move p delta)
  | Picker_accept -> dialog_accept m
  | Picker_choose id ->
    (match m.dialog with
     | Some (Picker { kind; picker }) ->
       (match
          List.find (Picker.visible picker) ~f:(fun i -> String.equal i.id id)
        with
        | Some item -> picker_accept m ~kind item
        | None -> m, [])
     | _ -> m, [])
  | Dialog_input text ->
    (match m.dialog with
     | Some (Rename _) -> { m with dialog = Some (Rename text) }, []
     | Some (Login flow) ->
       { m with dialog = Some (Login { flow with input = text }) }, []
     | Some (Picker _) -> with_picker m ~f:(fun p -> Picker.set_query p text)
     | _ -> m, [])
  | Dialog_move delta ->
    (match m.dialog with
     | Some (Login flow) ->
       { m with dialog = Some (Login (Login_flow.move flow delta)) }, []
     | Some (Picker _) -> with_picker m ~f:(fun p -> Picker.move p delta)
     | _ -> m, [])
  | Dialog_accept -> dialog_accept m
  | Close_dialog ->
    (match m.dialog with
     | Some (Login { failed = None; _ }) ->
       let m, cmds = close_dialog m in
       m, cmds @ [ rpc "auth_cancel" [] ]
     | Some _ -> close_dialog m
     | None -> m, [])
  | Login_choose i ->
    (match m.dialog with
     | Some (Login flow) ->
       let flow = Login_flow.move flow (i - flow.selected) in
       respond_login m flow
     | _ -> m, [])
  | Start_login provider -> start_login m ~provider ~method_:None
  | Logout provider -> m, [ rpc "logout" [ "provider", str provider ] ]
  | Cancel_subagent id -> m, [ rpc "cancel_subagent" [ "agent_id", str id ] ]
  | Kill_job id -> m, [ rpc "kill_job" [ "job_id", str id ] ]
  | Dequeue -> m, [ rpc "dequeue" [] ~tag:Dequeued ]
  | Toggle_sidebar -> { m with sidebar_open = not m.sidebar_open }, []
  | Respond_confirm { call_id; allow } ->
    ( { m with
        confirms =
          List.filter m.confirms ~f:(fun c ->
            not (String.equal c.call_id call_id))
      }
    , [ rpc
          "tool_confirm_respond"
          [ "call_id", str call_id; "allow", Json.bool allow ]
      ; focus_editor
      ] )
  | Add_image image -> { m with images = m.images @ [ image ] }, []
  | Remove_image i ->
    { m with images = List.filteri m.images ~f:(fun j _ -> j <> i) }, []
  | Show_toast { text; error } -> toast m ~error text
  | Dismiss_toast id ->
    { m with toasts = List.filter m.toasts ~f:(fun t -> t.id <> id) }, []
  | Sign_out -> m, [ Command.Sign_out ]
;;
