open! Core
open! Import

module Reply_tag = struct
  type t =
    | Ignore
    | Show_error
    | State
    | Messages
    | Sessions
    | Models
    | Reload_state
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
    | Set_draft of string
    | Send
    | Send_follow_up
    | Abort
    | New_session
    | Switch_session of string
    | Set_model of string
    | Set_thinking of string
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
    ; draft : string
    ; images : Image.t list
    ; queue : int * int
    ; confirms : Confirm.t list
    ; toasts : Toast.t list
    ; next_toast : int
    ; sidebar_open : bool
    }
  [@@deriving sexp_of]

  let running t =
    match t.state with
    | Some s -> s.running
    | None -> false
  ;;
end

let thinking_levels = [ "off"; "low"; "on"; "high"; "max" ]

let init =
  { Model.connection = Connected
  ; generation = 0
  ; hello = None
  ; state = None
  ; chat = Chat.empty
  ; sessions = []
  ; models = []
  ; draft = ""
  ; images = []
  ; queue = 0, 0
  ; confirms = []
  ; toasts = []
  ; next_toast = 0
  ; sidebar_open = true
  }
;;

let rpc ?(tag = Reply_tag.Show_error) method_ params =
  Command.Rpc { method_; params; tag }
;;

let toast (m : Model.t) ?(error = false) text =
  { m with
    toasts = m.toasts @ [ { id = m.next_toast; text; error } ]
  ; next_toast = m.next_toast + 1
  }
;;

(* The first [state] asks for the session's messages and the session list. *)
let startup =
  [ rpc "get_state" [] ~tag:State; rpc "list_models" [] ~tag:Models ]
;;

let decode (m : Model.t) json of_json ~f =
  match of_json json with
  | Ok value -> f value
  | Error e -> toast m ~error:true (Error.to_string_hum e), []
;;

let decode_list json of_json =
  match json with
  | `Array items -> Or_error.all (List.map items ~f:of_json)
  | _ -> Or_error.error_string "expected an array"
;;

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
    ( { m with chat = Chat.empty; confirms = []; queue = 0, 0 }
    , [ Command.Set_url_session state.session_id
      ; rpc "get_messages" [] ~tag:Messages
      ; rpc "list_sessions" [] ~tag:Sessions
      ] )
  else m, []
;;

let reply (m : Model.t) (tag : Reply_tag.t) result =
  match tag, result with
  | Reconnect generation, _ when generation <> m.generation -> m, []
  | Reconnect _, Error error ->
    (match m.connection with
     | Connected -> m, []
     | Reconnecting { attempt; generation } ->
       let attempt = attempt + 1 in
       let m = { m with connection = Reconnecting { attempt; generation } } in
       ( toast m ~error:true (sprintf "reconnecting: %s" error)
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
    { m with state = None; chat = Chat.empty }, startup
  | _, Error error ->
    (match tag with
     | Ignore -> m, []
     | _ -> toast m ~error:true error, [])
  | (Ignore | Show_error), Ok _ -> m, []
  | State, Ok json -> decode m json State.of_json ~f:(set_state m)
  | Reload_state, Ok _ -> m, [ rpc "get_state" [] ~tag:State ]
  | Messages, Ok json ->
    decode
      m
      json
      (fun j -> decode_list j Message.of_json)
      ~f:(fun messages -> { m with chat = Chat.of_messages messages }, [])
  | Sessions, Ok json ->
    decode
      m
      json
      (fun j -> decode_list j Session_summary.of_json)
      ~f:(fun sessions -> { m with sessions }, [])
  | Models, Ok json ->
    decode
      m
      json
      (fun j -> decode_list j Llm.of_json)
      ~f:(fun models -> { m with models }, [])
;;

let event (m : Model.t) (event : Event.t) =
  let m = { m with chat = Chat.apply m.chat event } in
  match event with
  | State state -> set_state m state
  | Notice text -> toast m text, []
  | Queue_update { steer; follow_up } -> { m with queue = steer, follow_up }, []
  | Tool_confirm { call_id; name; summary } ->
    { m with confirms = m.confirms @ [ { call_id; name; summary } ] }, []
  | Tool_end { call; _ } ->
    ( { m with
        confirms =
          List.filter m.confirms ~f:(fun c ->
            not (String.equal c.call_id call.id))
      }
    , [] )
  | Agent_end _ -> m, [ rpc "list_sessions" [] ~tag:Sessions ]
  | _ -> m, []
;;

let image_json (image : Image.t) =
  `Object [ "mime_type", `String image.mime_type; "data", `String image.data ]
;;

let send (m : Model.t) ~follow_up =
  let text = String.strip m.draft in
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
    ( { m with draft = ""; images = [] }
    , [ rpc method_ (("text", `String text) :: images) ] ))
;;

let update (m : Model.t) (action : Action.t) =
  match action with
  | Start -> m, startup
  | Hello hello -> { m with hello = Some hello }, []
  | Event e -> event m e
  | Protocol_error e -> toast m ~error:true ("protocol error: " ^ e), []
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
  | Set_draft draft -> { m with draft }, []
  | Send -> send m ~follow_up:false
  | Send_follow_up -> send m ~follow_up:true
  | Abort -> m, [ rpc "abort" [] ]
  | New_session -> m, [ rpc "new_session" [] ~tag:Reload_state ]
  | Switch_session path ->
    m, [ rpc "switch_session" [ "path", `String path ] ~tag:Reload_state ]
  | Set_model key -> m, [ rpc "set_model" [ "model", `String key ] ]
  | Set_thinking level ->
    m, [ rpc "set_thinking" [ "thinking", `String level ] ]
  | Toggle_sidebar -> { m with sidebar_open = not m.sidebar_open }, []
  | Respond_confirm { call_id; allow } ->
    ( { m with
        confirms =
          List.filter m.confirms ~f:(fun c ->
            not (String.equal c.call_id call_id))
      }
    , [ rpc
          "tool_confirm_respond"
          [ "call_id", `String call_id; "allow", Json.bool allow ]
      ] )
  | Add_image image -> { m with images = m.images @ [ image ] }, []
  | Remove_image i ->
    { m with images = List.filteri m.images ~f:(fun j _ -> j <> i) }, []
  | Show_toast { text; error } -> toast m ~error text, []
  | Dismiss_toast id ->
    { m with toasts = List.filter m.toasts ~f:(fun t -> t.id <> id) }, []
;;
