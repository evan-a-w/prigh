open! Core
open! Import

module Auth_method = struct
  type t =
    { id : string
    ; name : string
    ; description : string option
    ; terminal : bool
    }
  [@@deriving sexp_of, equal]
end

module Agent_capabilities = struct
  type t =
    { load_session : bool
    ; resume_session : bool
    ; close_session : bool
    ; images : bool
    ; audio : bool
    ; embedded_context : bool
    ; mcp_http : bool
    ; mcp_sse : bool
    }
  [@@deriving sexp_of, equal]
end

module Agent_info = struct
  type t =
    { name : string
    ; title : string option
    ; version : string option
    }
  [@@deriving sexp_of, equal]
end

module Mode = struct
  type t =
    { id : string
    ; name : string
    ; description : string option
    }
  [@@deriving sexp_of, equal]
end

module Config_option = struct
  module Choice = struct
    type t =
      { value : string
      ; name : string
      ; description : string option
      ; group : string option
      }
    [@@deriving sexp_of, equal]
  end

  module Value = struct
    type t =
      | Select of
          { current : string
          ; choices : Choice.t list
          }
      | Boolean of bool
    [@@deriving sexp_of, equal]
  end

  type t =
    { id : string
    ; name : string
    ; description : string option
    ; category : string option
    ; value : Value.t
    }
  [@@deriving sexp_of, equal]
end

module Session_config = struct
  type t =
    { current_mode : string option
    ; modes : Mode.t list
    ; config_options : Config_option.t list
    }
  [@@deriving sexp_of, equal]

  let empty = { current_mode = None; modes = []; config_options = [] }

  let option t ~category =
    List.find t.config_options ~f:(fun (o : Config_option.t) ->
      Option.equal String.equal o.category (Some category))
  ;;
end

let session_model_option = "prigh:session-model"

module Command = struct
  type t =
    { name : string
    ; description : string
    ; input_hint : string option
    }
  [@@deriving sexp_of, equal]
end

module Mcp_server = struct
  type t =
    | Stdio of
        { name : string
        ; command : string
        ; args : string list
        ; env : (string * string) list
        }
    | Http of
        { name : string
        ; url : string
        ; headers : (string * string) list
        }
  [@@deriving sexp_of]

  let to_json = function
    | Stdio { name; command; args; env } ->
      `Object
        [ "name", `String name
        ; "command", `String command
        ; "args", `Array (List.map args ~f:(fun a -> `String a))
        ; ( "env"
          , `Array
              (List.map env ~f:(fun (name, value) ->
                 `Object [ "name", `String name; "value", `String value ])) )
        ]
    | Http { name; url; headers } ->
      `Object
        [ "type", `String "http"
        ; "name", `String name
        ; "url", `String url
        ; ( "headers"
          , `Array
              (List.map headers ~f:(fun (name, value) ->
                 `Object [ "name", `String name; "value", `String value ])) )
        ]
  ;;
end

module Input = struct
  type t =
    | Text of string
    | Image of Image.t
end

module Request_kind = struct
  type t =
    | Initialize
    | New_session
    | Load_session
    | Resume_session
    | Close_session
    | Prompt
    | Set_mode
    | Set_config_option
  [@@deriving sexp_of, equal]
end

module Response = struct
  type t =
    | Initialized of
        { protocol_version : int
        ; agent : Agent_info.t option
        ; capabilities : Agent_capabilities.t
        ; auth_methods : Auth_method.t list
        }
    | Session_opened of
        { kind : Request_kind.t
        ; session_id : string
        ; config : Session_config.t
        }
    | Session_closed
    | Mode_set of string
    | Config_set of Config_option.t list
    | Failed of
        { kind : Request_kind.t
        ; code : int
        ; message : string
        }
  [@@deriving sexp_of]
end

module Output = struct
  type t =
    | Event of Adapter_event.t
    | Send of string
    | Response of Response.t
    | Config_changed of Session_config.t
    | Commands of Command.t list
    | Title of string option
  [@@deriving sexp_of]
end

(* JSON reading, lenient where the spec says to default on error. *)
let field name json = Json.member name json

let str name json =
  match field name json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let flag name json =
  match field name json with
  | Some `True -> true
  | _ -> false
;;

let present name json =
  match field name json with
  | None | Some `Null -> false
  | Some _ -> true
;;

let items name json =
  match field name json with
  | Some (`Array l) -> l
  | _ -> []
;;

let obj = function
  | Some (`Object _ as o) -> o
  | _ -> `Object []
;;

module Pending = struct
  type t =
    | Initialize
    | New_session
    | Load_session of string
    | Resume_session of string
    | Close_session
    | Prompt
    | Steer (* a message for the running turn *)
    | Set_mode of string
    | Set_config_option of string * Config_option.Value.t
      (* the option, the value asked for: an answer without the options
         means it took it *)
    | Set_model of string

  let kind : t -> Request_kind.t = function
    | Initialize -> Initialize
    | New_session -> New_session
    | Load_session _ -> Load_session
    | Resume_session _ -> Resume_session
    | Close_session -> Close_session
    | Prompt | Steer -> Prompt
    | Set_mode _ -> Set_mode
    | Set_config_option _ | Set_model _ -> Set_config_option
  ;;
end

module Tool = struct
  type t =
    { acp_kind : string
    ; kind : Adapter_event.Tool_kind.t
    ; native_name : string
    ; title : string
    ; input : Json.t
    ; mutable output : string
    ; mutable changes : Adapter_event.Change.t list
    ; mutable images : Image.t list
    ; mutable running : bool (* said to be [in_progress] *)
    ; mutable ended : bool
    ; mutable backgrounded : bool
      (* still running across [end_turn Done]: settled in the prior turn so
         its recorder closes cleanly, kept in [t.tools] so [has_open_work]
         stays true and a later update re-opens it in the next turn *)
    }
end

module Permission = struct
  module Kind = struct
    type t =
      | Options of (string * string) list (** option id, kind *)
      | Plan (** Grok's [x.ai/exit_plan_mode] *)
  end

  type t =
    { id : Json.t
    ; kind : Kind.t
    }
end

module Open_item = struct
  type t =
    { item : string
    ; message_id : string option
    ; text : Buffer.t
    }
end

module Turn = struct
  type t =
    { id : string
    ; mutable cancelled : bool
    ; mutable autonomous : bool
    ; mutable saw_text : bool
    ; mutable text : Open_item.t option
    ; mutable thought : Open_item.t option
    ; mutable items : int
    ; mutable prompts : int (* session/prompt requests unanswered *)
    ; mutable usage : Usage.t option
      (* the counts its answers gave so far: whether an agent counts each
         prompt or the whole turn is unknown, so the turn reports the most
         of each *)
    }
end

type t =
  { mutable next_id : int
  ; pending : Pending.t Int.Table.t
  ; mutable capabilities : Agent_capabilities.t option
  ; mutable session_id : string option
  ; mutable loading : bool
  ; mutable config : Session_config.t
  ; mutable models : Config_option.t option
    (* the session model state of the unstable API, as an option *)
  ; mutable turn : Turn.t option
  ; mutable turns : int
  ; permissions : Permission.t String.Table.t
  ; tools : Tool.t String.Table.t
  ; mutable cost : float option
  ; compacted : String.Hash_set.t (* compactionIds already reported *)
  ; quirks : Acp_agent.Quirks.t
  ; questions : (Json.t * User_question.t list) String.Table.t
    (* Grok's [x.ai/ask_user_question]s waiting for the user: their ids *)
  }

let create ?(quirks = Acp_agent.Quirks.none) ?cost_total () =
  { next_id = 0
  ; quirks
  ; pending = Int.Table.create ()
  ; capabilities = None
  ; session_id = None
  ; loading = false
  ; config = Session_config.empty
  ; models = None
  ; turn = None
  ; turns = 0
  ; permissions = String.Table.create ()
  ; tools = String.Table.create ()
  ; cost = cost_total
  ; compacted = String.Hash_set.create ()
  ; questions = String.Table.create ()
  }
;;

let session_id t = t.session_id
let cost_total t = t.cost
let turn_id t = Option.map t.turn ~f:(fun turn -> turn.id)
let in_turn t = Option.is_some t.turn
let agent_capabilities t = t.capabilities
let session_config t = t.config

let line fields =
  Json.to_string (`Object (("jsonrpc", `String "2.0") :: fields))
;;

let request t pending method_ params =
  let id = t.next_id in
  t.next_id <- id + 1;
  Hashtbl.set t.pending ~key:id ~data:pending;
  line
    [ "id", `Number (Int.to_string id)
    ; "method", `String method_
    ; "params", params
    ]
;;

let notification method_ params =
  line [ "method", `String method_; "params", params ]
;;

let result_line id result = line [ "id", id; "result", result ]

let error_line id ~code ~message =
  line
    [ "id", id
    ; ( "error"
      , `Object
          [ "code", `Number (Int.to_string code); "message", `String message ] )
    ]
;;

let initialize t ~client_name ~client_version =
  request
    t
    Initialize
    "initialize"
    (`Object
        [ "protocolVersion", `Number "1"
        ; ( "clientCapabilities"
          , `Object
              [ ( "fs"
                , `Object [ "readTextFile", `False; "writeTextFile", `False ] )
              ; "terminal", `False
              ; ( "session"
                , `Object [ "notices", `Object []; "compaction", `Object [] ] )
              ] )
        ; ( "clientInfo"
          , `Object
              [ "name", `String client_name; "version", `String client_version ]
          )
        ])
;;

let mcp_json servers = `Array (List.map servers ~f:Mcp_server.to_json)

let new_session t ~cwd ~mcp_servers =
  request
    t
    New_session
    "session/new"
    (`Object [ "cwd", `String cwd; "mcpServers", mcp_json mcp_servers ])
;;

let open_session t ~pending ~method_ ~session_id ~cwd ~mcp_servers =
  request
    t
    pending
    method_
    (`Object
        [ "sessionId", `String session_id
        ; "cwd", `String cwd
        ; "mcpServers", mcp_json mcp_servers
        ])
;;

let load_session t ~session_id ~cwd ~mcp_servers =
  t.loading <- true;
  open_session
    t
    ~pending:(Load_session session_id)
    ~method_:"session/load"
    ~session_id
    ~cwd
    ~mcp_servers
;;

let resume_session t ~session_id ~cwd ~mcp_servers =
  open_session
    t
    ~pending:(Resume_session session_id)
    ~method_:"session/resume"
    ~session_id
    ~cwd
    ~mcp_servers
;;

let close_session_for t ~session_id =
  request
    t
    Close_session
    "session/close"
    (`Object [ "sessionId", `String session_id ])
;;

let close_session t =
  match t.session_id, t.capabilities with
  | Some session_id, Some { close_session = true; _ } ->
    Some (close_session_for t ~session_id)
  | _ -> None
;;

let content_block : Input.t -> Json.t = function
  | Text text -> `Object [ "type", `String "text"; "text", `String text ]
  | Image { mime_type; data } ->
    `Object
      [ "type", `String "image"
      ; "mimeType", `String mime_type
      ; "data", `String data
      ]
;;

let refuses_images t inputs =
  List.exists inputs ~f:(function
    | Input.Image _ -> true
    | Text _ -> false)
  && not (Option.value_map t.capabilities ~default:false ~f:(fun c -> c.images))
;;

let prompt t inputs =
  match t.session_id, t.turn with
  | None, _ -> Or_error.error_string "no ACP session yet"
  | Some _, Some ({ autonomous = false; _ } as turn) ->
    Or_error.error_s [%message "a turn is already running" ~turn:turn.id]
  | Some session_id, Some ({ autonomous = true; _ } as turn) ->
    if refuses_images t inputs
    then Or_error.error_string "this agent does not take images"
    else (
      turn.autonomous <- false;
      turn.prompts <- turn.prompts + 1;
      let line =
        request
          t
          Prompt
          "session/prompt"
          (`Object
              [ "sessionId", `String session_id
              ; "prompt", `Array (List.map inputs ~f:content_block)
              ])
      in
      Ok [ Output.Send line ])
  | Some session_id, None ->
    if refuses_images t inputs
    then Or_error.error_string "this agent does not take images"
    else (
      t.turns <- t.turns + 1;
      let turn =
        { Turn.id = sprintf "turn-%d" t.turns
        ; cancelled = false
        ; autonomous = false
        ; saw_text = false
        ; text = None
        ; thought = None
        ; items = 0
        ; prompts = 1
        ; usage = None
        }
      in
      t.turn <- Some turn;
      let line =
        request
          t
          Prompt
          "session/prompt"
          (`Object
              [ "sessionId", `String session_id
              ; "prompt", `Array (List.map inputs ~f:content_block)
              ])
      in
      Ok [ Output.Send line; Event (Turn_started { turn_id = turn.id }) ])
;;

let steer t inputs =
  match t.session_id, t.turn, t.quirks.steer with
  | _, _, Between_turns ->
    Or_error.error_string "this agent takes new messages between turns"
  | None, _, _ | _, None, _ -> Or_error.error_string "no turn is running"
  | Some _, Some { cancelled = true; _ }, _ ->
    Or_error.error_string "the turn is being stopped"
  | Some _, Some _, _ when refuses_images t inputs ->
    Or_error.error_string "this agent does not take images"
  | Some session_id, Some turn, Prompt ->
    turn.autonomous <- false;
    turn.prompts <- turn.prompts + 1;
    Ok
      [ Output.Send
          (request
             t
             Prompt
             "session/prompt"
             (`Object
                 [ "sessionId", `String session_id
                 ; "prompt", `Array (List.map inputs ~f:content_block)
                 ]))
      ]
  | Some session_id, Some _, Xai_interject ->
    let text =
      List.filter_map inputs ~f:(function
        | Input.Text text -> Some text
        | Image _ -> None)
      |> String.concat ~sep:"\n\n"
    in
    let images =
      List.filter inputs ~f:(function
        | Input.Image _ -> true
        | Text _ -> false)
    in
    Ok
      [ Output.Send
          (request
             t
             Steer
             "_x.ai/interject"
             (`Object
                 ([ "sessionId", `String session_id; "text", `String text ]
                  @
                  if List.is_empty images
                  then []
                  else [ "content", `Array (List.map images ~f:content_block) ]
                 )))
      ]
;;

(* A [session/prompt] answer's [usage], for an agent whose counts are known:
   Antigravity's [inputTokens] leaves out [cachedReadTokens] and its
   [outputTokens] already counts [thoughtTokens]. *)
let prompt_usage t result =
  match t.quirks.prompt_usage, field "usage" result with
  | Gemini, Some (`Object _ as usage) ->
    let int name =
      Option.value (Option.bind (field name usage) ~f:Json.int) ~default:0
    in
    let cache_read = int "cachedReadTokens" in
    Some
      { Usage.input = int "inputTokens" + cache_read
      ; output = int "outputTokens"
      ; cache_read
      }
  | (Ignored | Gemini), _ -> None
;;

(* Keeps an answer's counts with its turn's. *)
let note_usage t result =
  match t.turn, prompt_usage t result with
  | Some turn, Some (u : Usage.t) ->
    turn.usage
    <- Some
         (match turn.usage with
          | None -> u
          | Some (o : Usage.t) ->
            { input = Int.max o.input u.input
            ; output = Int.max o.output u.output
            ; cache_read = Int.max o.cache_read u.cache_read
            })
  | _ -> ()
;;

let turn_usage t =
  match Option.bind t.turn ~f:(fun turn -> turn.usage) with
  | Some usage ->
    [ Output.Event
        (Usage
           { usage
           ; cost_usd = None
           ; context_tokens = None
           ; context_window = None
           })
    ]
  | None -> []
;;

let cancelled_outcome =
  `Object [ "outcome", `Object [ "outcome", `String "cancelled" ] ]
;;

let plan_abandoned =
  `Object
    [ "outcome", `String "abandoned"
    ; ( "feedback"
      , `String
          "The user stopped before deciding on your plan. Stop here and wait \
           for them." )
    ]
;;

let withdraw_permissions t ~reply =
  let waiting = Hashtbl.to_alist t.permissions in
  Hashtbl.clear t.permissions;
  List.concat_map
    (List.sort waiting ~compare:(fun (a, _) (b, _) -> String.compare a b))
    ~f:(fun (request_id, (p : Permission.t)) ->
      (if reply
       then (
         let outcome =
           match p.kind with
           | Options _ -> cancelled_outcome
           | Plan -> plan_abandoned
         in
         [ Output.Send (result_line p.id outcome) ])
       else [])
      @ [ Event (Request_withdrawn { request_id }) ])
;;

let question_cancelled = `Object [ "outcome", `String "cancelled" ]

(* Grok's questions still waiting, answered as cancelled. *)
let withdraw_questions t =
  let waiting = Hashtbl.to_alist t.questions in
  Hashtbl.clear t.questions;
  List.concat_map
    (List.sort waiting ~compare:(fun (a, _) (b, _) -> String.compare a b))
    ~f:(fun (request_id, (id, _)) ->
      [ Output.Send (result_line id question_cancelled)
      ; Event (Request_withdrawn { request_id })
      ])
;;

let cancel t =
  match t.turn, t.session_id with
  | Some turn, Some session_id when not turn.cancelled ->
    turn.cancelled <- true;
    (Output.Send
       (notification
          "session/cancel"
          (`Object [ "sessionId", `String session_id ]))
     :: withdraw_permissions t ~reply:true)
    @ withdraw_questions t
  | _ -> []
;;

let set_mode t ~mode_id =
  match t.session_id with
  | None -> Or_error.error_string "no ACP session yet"
  | Some session_id ->
    if not (List.exists t.config.modes ~f:(fun m -> String.equal m.id mode_id))
    then
      Or_error.error_s
        [%message
          "the agent offers no such mode"
            mode_id
            ~modes:(List.map t.config.modes ~f:(fun m -> m.id) : string list)]
    else
      Ok
        (request
           t
           (Set_mode mode_id)
           "session/set_mode"
           (`Object
               [ "sessionId", `String session_id; "modeId", `String mode_id ]))
;;

let set_config_option t ~config_id value =
  match t.session_id with
  | None -> Or_error.error_string "no ACP session yet"
  | Some session_id ->
    if String.equal config_id session_model_option
    then (
      match value, t.models with
      | `Id id, Some { value = Select { choices; _ }; _ }
        when List.exists choices ~f:(fun c -> String.equal c.value id) ->
        Ok
          (request
             t
             (Set_model id)
             "session/set_model"
             (`Object [ "sessionId", `String session_id; "modelId", `String id ]))
      | _ ->
        Or_error.error_s
          [%message
            "the agent offers no such model"
              ~model:(value : [ `Id of string | `Bool of bool ])])
    else (
      match
        List.find t.config.config_options ~f:(fun (o : Config_option.t) ->
          String.equal o.id config_id)
      with
      | None ->
        Or_error.error_s [%message "the agent offers no such option" config_id]
      | Some option ->
        let value_fields =
          match option.value, value with
          | Boolean _, `Bool b ->
            Ok
              ( Config_option.Value.Boolean b
              , [ "type", `String "boolean"
                ; ("value", if b then `True else `False)
                ] )
          | Select { choices; _ }, `Id id ->
            if List.exists choices ~f:(fun c -> String.equal c.value id)
            then
              Ok
                ( Config_option.Value.Select { current = id; choices }
                , [ "value", `String id ] )
            else
              Or_error.error_s
                [%message
                  "not one of the option's choices"
                    config_id
                    id
                    ~choices:
                      (List.map choices ~f:(fun c -> c.value) : string list)]
          | Boolean _, `Id _ ->
            Or_error.error_s
              [%message "the option takes true or false" config_id]
          | Select _, `Bool _ ->
            Or_error.error_s
              [%message "the option takes one of its choices" config_id]
        in
        Or_error.map value_fields ~f:(fun (asked, value_fields) ->
          request
            t
            (Set_config_option (config_id, asked))
            "session/set_config_option"
            (`Object
                ([ "sessionId", `String session_id
                 ; "configId", `String config_id
                 ]
                 @ value_fields))))
;;

let reply_approval t ~request_id (reply : Adapter_event.Approval_reply.t) =
  match Hashtbl.find_and_remove t.permissions request_id with
  | None -> None
  | Some { id; kind = Plan } ->
    (* Grok's plan: carried out, changed as the user says, or dropped. *)
    let outcome =
      match reply with
      | Allow | Allow_always -> `Object [ "outcome", `String "approved" ]
      | Deny feedback ->
        `Object
          [ "outcome", `String "request_changes"
          ; ( "feedback"
            , `String
                (Option.value
                   feedback
                   ~default:
                     "The user wants to keep planning: stay in plan mode and \
                      wait for what they want changed.") )
          ]
      | Cancel -> plan_abandoned
    in
    Some (result_line id outcome)
  | Some { id; kind = Options options } ->
    let of_kind kind =
      List.find_map options ~f:(fun (id, k) ->
        Option.some_if (String.equal k kind) id)
    in
    (* A persistent option only when the user chose one: approving once
       never leaves an "always allow" behind, nor denying once an "always
       reject". Without the option the user asked for, the request is
       cancelled. *)
    let choice =
      match reply with
      | Allow -> of_kind "allow_once"
      | Allow_always ->
        Option.first_some (of_kind "allow_always") (of_kind "allow_once")
      | Deny _ -> of_kind "reject_once"
      | Cancel -> None
    in
    let outcome =
      match choice with
      | None -> cancelled_outcome
      | Some option_id ->
        `Object
          [ ( "outcome"
            , `Object
                [ "outcome", `String "selected"; "optionId", `String option_id ]
            )
          ]
    in
    Some (result_line id outcome)
;;

(* Parsing what the agent says. *)

let parse_capabilities json : Agent_capabilities.t =
  let caps = obj (field "agentCapabilities" json) in
  let prompt = obj (field "promptCapabilities" caps) in
  let mcp = obj (field "mcpCapabilities" caps) in
  let session = obj (field "sessionCapabilities" caps) in
  { load_session = flag "loadSession" caps
  ; resume_session = present "resume" session
  ; close_session = present "close" session
  ; images = flag "image" prompt
  ; audio = flag "audio" prompt
  ; embedded_context = flag "embeddedContext" prompt
  ; mcp_http = flag "http" mcp
  ; mcp_sse = flag "sse" mcp
  }
;;

let parse_agent_info json : Agent_info.t option =
  match field "agentInfo" json with
  | Some (`Object _ as info) ->
    Option.map (str "name" info) ~f:(fun name ->
      { Agent_info.name
      ; title = str "title" info
      ; version = str "version" info
      })
  | _ -> None
;;

let parse_auth_methods json =
  List.filter_map (items "authMethods" json) ~f:(fun m ->
    Option.map (str "id" m) ~f:(fun id ->
      { Auth_method.id
      ; name = Option.value (str "name" m) ~default:id
      ; description = str "description" m
      ; terminal = Option.equal String.equal (str "type" m) (Some "terminal")
      }))
;;

let parse_modes json =
  match field "modes" json with
  | Some (`Object _ as modes) ->
    ( str "currentModeId" modes
    , List.filter_map (items "availableModes" modes) ~f:(fun m ->
        Option.map (str "id" m) ~f:(fun id ->
          { Mode.id
          ; name = Option.value (str "name" m) ~default:id
          ; description = str "description" m
          })) )
  | _ -> None, []
;;

let parse_choice ~group c =
  Option.map (str "value" c) ~f:(fun value ->
    { Config_option.Choice.value
    ; name = Option.value (str "name" c) ~default:value
    ; description = str "description" c
    ; group
    })
;;

let parse_config_option o : Config_option.t option =
  let open Option.Let_syntax in
  let%bind id = str "id" o in
  let%map value =
    match str "type" o with
    | Some "boolean" ->
      (match field "currentValue" o with
       | Some `True -> Some (Config_option.Value.Boolean true)
       | Some `False -> Some (Boolean false)
       | _ -> None)
    | Some "select" ->
      let%map current = str "currentValue" o in
      let choices =
        List.concat_map (items "options" o) ~f:(fun c ->
          if present "value" c
          then Option.to_list (parse_choice ~group:None c)
          else (
            let group = Some (Option.value (str "name" c) ~default:"") in
            List.filter_map (items "options" c) ~f:(parse_choice ~group)))
      in
      Config_option.Value.Select { current; choices }
    | _ -> None
  in
  { Config_option.id
  ; name = Option.value (str "name" o) ~default:id
  ; description = str "description" o
  ; category = str "category" o
  ; value
  }
;;

let parse_config_options l = List.filter_map l ~f:parse_config_option

(* The [models] state of ACP's unstable session model API, as a [model]
   option. *)
let parse_models json : Config_option.t option =
  let open Option.Let_syntax in
  let%bind models = field "models" json in
  let%map current = str "currentModelId" models in
  let choices =
    List.filter_map (items "availableModels" models) ~f:(fun m ->
      Option.map (str "modelId" m) ~f:(fun value ->
        { Config_option.Choice.value
        ; name = Option.value (str "name" m) ~default:value
        ; description = str "description" m
        ; group = None
        }))
  in
  { Config_option.id = session_model_option
  ; name = "Model"
  ; description = None
  ; category = Some "model"
  ; value = Select { current; choices }
  }
;;

(* The agent's options, with its session model state when no option is its
   model. *)
let with_models t options =
  if
    List.exists options ~f:(fun (o : Config_option.t) ->
      Option.equal String.equal o.category (Some "model"))
  then options
  else options @ Option.to_list t.models
;;

let parse_session_config t json : Session_config.t =
  let current_mode, modes = parse_modes json in
  Option.iter (parse_models json) ~f:(fun m -> t.models <- Some m);
  { current_mode
  ; modes
  ; config_options =
      with_models t (parse_config_options (items "configOptions" json))
  }
;;

let set_current_model t id =
  t.models
  <- Option.map t.models ~f:(fun (o : Config_option.t) ->
       match o.value with
       | Select s -> { o with value = Select { s with current = id } }
       | Boolean _ -> o);
  t.config
  <- { t.config with
       config_options =
         List.map t.config.config_options ~f:(fun (o : Config_option.t) ->
           if String.equal o.id session_model_option
           then Option.value t.models ~default:o
           else o)
     }
;;

let error_of json =
  ( Option.value (Option.bind (field "code" json) ~f:Json.int) ~default:0
  , Option.value (str "message" json) ~default:"error" )
;;

(* Content blocks as text: what a message or a tool's output shows. *)
let block_text block =
  match str "type" block with
  | Some "text" -> str "text" block
  | Some "resource_link" ->
    Option.first_some (str "uri" block) (str "name" block)
  | Some "resource" ->
    let resource = obj (field "resource" block) in
    Option.first_some (str "text" resource) (str "uri" resource)
  | _ -> None
;;

let block_image block : Image.t option =
  match str "type" block with
  | Some "image" ->
    (match str "data" block, str "mimeType" block with
     | Some data, Some mime_type -> Some { mime_type; data }
     | _ -> None)
  | _ -> None
;;

(* ACP's [search] kind covers web searches as well as searches of the
   code, so the agent's tool name (its [name], in its [_meta] or in its
   title, e.g. Antigravity's "Running search_web") and its input decide
   which a call is. *)
module Tool_class = struct
  type t =
    | Web_search
    | Web_fetch
    | Code_search

  let normalise s =
    String.lowercase s
    |> String.map ~f:(function
      | '-' -> '_'
      | c -> c)
  ;;

  let names =
    [ ( Web_search
      , [ "search_web"
        ; "web_search"
        ; "websearch"
        ; "google_search"
        ; "googlesearch"
        ; "google_web_search"
        ; "brave_search"
        ; "bing_search"
        ; "search_the_web"
        ] )
    ; ( Web_fetch
      , [ "fetch"
        ; "web_fetch"
        ; "webfetch"
        ; "fetch_url"
        ; "url_fetch"
        ; "read_url"
        ; "read_url_content"
        ; "open_url"
        ; "browse_url"
        ] )
    ; ( Code_search
      , [ "grep"
        ; "glob"
        ; "ripgrep"
        ; "rg"
        ; "find"
        ; "grep_search"
        ; "find_by_name"
        ; "find_files"
        ; "search_files"
        ; "file_search"
        ; "codebase_search"
        ; "code_search"
        ; "search_code"
        ] )
    ]
  ;;

  let of_name name =
    let name = normalise name in
    List.find_map names ~f:(fun (t, names) ->
      Option.some_if (List.mem names name ~equal:String.equal) t)
  ;;

  (* The words of a title's label (before any [:], where a query or path
     would follow), at most the first three. *)
  let label_words title =
    let label =
      match String.lsplit2 title ~on:':' with
      | Some (label, _) -> label
      | None -> title
    in
    String.split_on_chars
      (normalise label)
      ~on:[ ' '; '\t'; '('; ')'; ','; '"'; '\''; '`' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> Fn.flip List.take 3
  ;;

  (* The tool's own name: its [name], one in its [_meta], or a known one in
     its title's label. *)
  let tool_name ~name ~meta ~title =
    let in_meta =
      let from json =
        List.find_map [ "toolName"; "tool_name" ] ~f:(fun key -> str key json)
      in
      match meta with
      | Some (`Object fields as meta) ->
        Option.first_some
          (from meta)
          (List.find_map fields ~f:(fun (_, v) -> from v))
      | _ -> None
    in
    match Option.first_some name in_meta with
    | Some name -> Some name
    | None ->
      List.find (label_words title) ~f:(fun w -> Option.is_some (of_name w))
  ;;

  let keys raw =
    match raw with
    | Some (`Object fields) ->
      List.map fields ~f:(fun (k, _) -> String.lowercase k)
    | _ -> []
  ;;

  let has keys names = List.exists names ~f:(List.mem keys ~equal:String.equal)

  let of_input raw =
    let keys = keys raw in
    if has keys [ "url"; "uri" ]
    then Some Web_fetch
    else if
      has
        keys
        [ "pattern"
        ; "regex"
        ; "glob"
        ; "include"
        ; "includes"
        ; "path"
        ; "paths"
        ; "searchpath"
        ; "searchdirectory"
        ; "directory"
        ; "dir"
        ; "targetdirectories"
        ]
    then Some Code_search
    else if has keys [ "query"; "q"; "search_query" ]
    then Some Web_search
    else None
  ;;

  let of_title title =
    Option.some_if
      (List.mem (label_words title) "web" ~equal:String.equal)
      Web_search
  ;;
end

let mcp_of_name name =
  match String.chop_prefix name ~prefix:"mcp__" with
  | Some rest ->
    (match String.substr_index rest ~pattern:"__" with
     | Some i ->
       Some
         (Adapter_event.Tool_kind.Mcp
            { server = String.prefix rest i
            ; tool = String.drop_prefix rest (i + 2)
            })
     | None -> None)
  | None ->
    (match
       String.chop_prefix
         name
         ~prefix:("mcp_" ^ Injected_tools.server_name ^ "_")
     with
     | Some tool
       when Option.is_some
              (Injected_tools.canonical_of_mcp
                 ~server:Injected_tools.server_name
                 ~tool) ->
       Some (Mcp { server = Injected_tools.server_name; tool })
     | Some _ | None -> None)
;;

(* What a call is, and the name it goes by: when not sure, [Other] with the
   agent's own title, rather than a wrong prigh tool. *)
let tool_kind ~acp_kind ~name ~meta ~title ~raw_input
  : Adapter_event.Tool_kind.t * string
  =
  let tool_name = Tool_class.tool_name ~name ~meta ~title in
  let native default = Option.value tool_name ~default in
  match
    Option.first_some (Option.bind tool_name ~f:mcp_of_name) (mcp_of_name title)
  with
  | Some mcp -> mcp, native title
  | None ->
    let classified =
      match acp_kind with
      | "search" | "fetch" | "other" ->
        let by_name = Option.bind tool_name ~f:Tool_class.of_name in
        (match by_name, acp_kind with
         | Some c, _ -> Some c
         | None, "other" -> None
         | None, _ ->
           Option.first_some
             (Tool_class.of_input raw_input)
             (Option.first_some
                (Tool_class.of_title title)
                (Option.some_if
                   (String.equal acp_kind "fetch")
                   Tool_class.Web_fetch)))
      | _ -> None
    in
    (match classified, acp_kind with
     | Some (Web_search | Web_fetch), _ -> Web_search, native acp_kind
     | Some Code_search, _ -> Search, native acp_kind
     | None, "read" -> Read, native acp_kind
     | None, ("edit" | "delete" | "move") -> File_change, native acp_kind
     | None, "execute" -> Command, native acp_kind
     | None, ("think" | "switch_mode") -> Other acp_kind, native acp_kind
     | None, _ ->
       let native = native title in
       Other native, native)
;;

let next_item (turn : Turn.t) prefix =
  turn.items <- turn.items + 1;
  sprintf "%s-%s%d" turn.id prefix turn.items
;;

let close_text (turn : Turn.t) =
  match turn.text with
  | None -> []
  | Some { item; text; _ } ->
    turn.text <- None;
    [ Output.Event (Text_done { item; text = Buffer.contents text }) ]
;;

let close_thought (turn : Turn.t) =
  match turn.thought with
  | None -> []
  | Some { item; text; _ } ->
    turn.thought <- None;
    [ Output.Event (Thinking_done { item; text = Buffer.contents text }) ]
;;

let close_items t =
  match t.turn with
  | None -> []
  | Some turn -> close_text turn @ close_thought turn
;;

(* When the agent resumes on its own between prompts (e.g. woken by a
   background task), open an autonomous turn so its streaming updates, tool
   calls and approvals are not dropped. *)
let ensure_autonomous_turn t =
  match t.turn, t.session_id with
  | None, Some _ when not t.loading ->
    t.turns <- t.turns + 1;
    let turn =
      { Turn.id = sprintf "turn-%d" t.turns
      ; cancelled = false
      ; autonomous = true
      ; saw_text = false
      ; text = None
      ; thought = None
      ; items = 0
      ; prompts = 0
      ; usage = None
      }
    in
    t.turn <- Some turn;
    [ Output.Event (Turn_started { turn_id = turn.id }) ]
  | _ -> []
;;

(* Appends to the open item of the kind, opening one (and closing the other
   kind's) when there is none or the agent's message id changed. *)
let chunk t ~thought ~message_id text =
  match t.turn with
  | None -> []
  | Some turn ->
    if not thought then turn.saw_text <- true;
    let current = if thought then turn.thought else turn.text in
    let continues =
      match current, message_id with
      | Some open_, Some id ->
        Option.equal String.equal open_.message_id (Some id)
      | Some _, None -> true
      | None, _ -> false
    in
    let closed, item =
      if continues
      then [], Option.value_exn current
      else (
        let closed =
          if thought
          then close_thought turn @ close_text turn
          else close_text turn @ close_thought turn
        in
        let item =
          { Open_item.item =
              Option.value
                message_id
                ~default:(next_item turn (if thought then "t" else "m"))
          ; message_id
          ; text = Buffer.create 256
          }
        in
        if thought then turn.thought <- Some item else turn.text <- Some item;
        closed, item)
    in
    Buffer.add_string item.text text;
    closed
    @ [ Output.Event
          (if thought
           then Thinking_delta { item = item.item; text }
           else Text_delta { item = item.item; text })
      ]
;;

let first_location_path update =
  List.find_map (items "locations" update) ~f:(fun loc -> str "path" loc)
;;

let has_path_field fields =
  List.exists fields ~f:(fun (k, _) ->
    List.mem
      [ "file_path"; "path"; "notebook_path" ]
      (String.lowercase k)
      ~equal:String.equal)
;;

let has_command_field fields =
  List.exists fields ~f:(fun (k, _) ->
    List.mem
      [ "command"; "command_line"; "commandline" ]
      (String.lowercase k)
      ~equal:String.equal)
;;

let tool_input ?location ~(kind : Adapter_event.Tool_kind.t) ~title raw : Json.t
  =
  let fields =
    match raw with
    | Some (`Object fields) -> fields
    | None | Some `Null -> []
    | Some other -> [ "input", other ]
  in
  match kind with
  | Command when not (has_command_field fields) ->
    `Object (fields @ [ "command", `String title ])
  | Read | File_change ->
    (match location with
     | Some path when not (has_path_field fields) ->
       `Object (fields @ [ "path", `String path ])
     | Some _ | None -> `Object fields)
  | _ -> `Object fields
;;

let change ~acp_kind diff : Adapter_event.Change.t option =
  match str "path" diff, str "newText" diff with
  | Some path, Some new_ ->
    let old = str "oldText" diff in
    let kind : Adapter_event.Change.Kind.t =
      match old, acp_kind with
      | _, "delete" -> Delete
      | None, _ -> Add
      | Some _, _ -> Update { move_to = None }
    in
    let diff =
      (Text_diff.unified
         ~path:(String.chop_prefix_if_exists path ~prefix:"/")
         ~old:(Option.value old ~default:"")
         ~new_)
        .diff
    in
    Some { path; kind; diff }
  | _ -> None
;;

let error_field_text = function
  | Some (`String s) when not (String.is_empty s) -> Some s
  | Some (`Object _ as o) -> Option.first_some (str "message" o) (str "error" o)
  | _ -> None
;;

let content_texts blocks =
  List.filter_map blocks ~f:(fun b ->
    match str "type" b with
    | Some "content" -> block_text (obj (field "content" b))
    | _ -> block_text b)
;;

let raw_output_text = function
  | None | Some `Null -> None
  | Some (`String s) -> Option.some_if (not (String.is_empty s)) s
  | Some (`Object _ as json) ->
    (match field "content" json with
     | Some (`Array blocks) ->
       (match content_texts blocks with
        | _ :: _ as texts -> Some (String.concat ~sep:"\n" texts)
        | [] ->
          Option.first_some
            (error_field_text (field "error" json))
            (Some (Json.to_string json)))
     | _ ->
       Option.first_some
         (error_field_text (field "error" json))
         (Some (Json.to_string json)))
  | Some json -> Some (Json.to_string json)
;;

(* Applies a tool_call or tool_call_update: content replaces what the call
   showed so far; a growing output is streamed as its new part. *)
let apply_tool t id (tool : Tool.t) update =
  let content =
    match field "content" update with
    | Some (`Array blocks) -> Some blocks
    | _ -> None
  in
  let updates =
    match content with
    | None -> []
    | Some blocks ->
      let texts = content_texts blocks in
      let output = String.concat ~sep:"\n" texts in
      let changes =
        List.filter_map blocks ~f:(fun b ->
          match str "type" b with
          | Some "diff" -> change ~acp_kind:tool.acp_kind b
          | _ -> None)
      in
      let images =
        List.filter_map blocks ~f:(fun b ->
          match str "type" b with
          | Some "content" -> block_image (obj (field "content" b))
          | _ -> block_image b)
      in
      if not (List.is_empty blocks)
      then (
        tool.changes <- changes;
        tool.images <- images);
      let previous = tool.output in
      if (not (List.is_empty texts)) || String.is_empty previous
      then tool.output <- output;
      if
        String.length output > String.length previous
        && String.is_prefix output ~prefix:previous
      then
        [ Output.Event
            (Tool_update
               { id
               ; output = String.drop_prefix output (String.length previous)
               })
        ]
      else []
  in
  (* Said to run: it starts now, so that its time counts from here. *)
  let running =
    match str "status" update with
    | Some "in_progress" when not (tool.running || tool.ended) ->
      tool.running <- true;
      if List.is_empty updates
      then [ Output.Event (Tool_update { id; output = "" }) ]
      else []
    | _ -> []
  in
  let ended =
    match str "status" update with
    | Some (("completed" | "failed") as status) when not tool.ended ->
      tool.ended <- true;
      let output =
        if String.is_empty tool.output
        then
          Option.first_some
            (raw_output_text (field "rawOutput" update))
            (error_field_text (field "error" update))
          |> Option.value ~default:""
        else tool.output
      in
      [ Output.Event
          (Tool_end
             { id
             ; ok = String.equal status "completed"
             ; output
             ; changes = tool.changes
             ; images = tool.images
             })
      ]
    | _ -> []
  in
  ignore t;
  running @ updates @ ended
;;

(* The call [update] names, started (with [Tool_start]) if it is new or
   re-opening in a new turn after staying open across [end_turn Done]. *)
let tool_of_update t update =
  match str "toolCallId" update with
  | None -> None
  | Some id ->
    (match Hashtbl.find t.tools id with
     | Some tool when tool.backgrounded ->
       tool.backgrounded <- false;
       let start =
         Output.Event
           (Tool_start
              { id
              ; kind = tool.kind
              ; native_name = tool.native_name
              ; title = tool.title
              ; input = tool.input
              ; parent = None
              })
       in
       Some (id, tool, close_items t @ [ start ])
     | Some tool -> Some (id, tool, [])
     | None ->
       let acp_kind = Option.value (str "kind" update) ~default:"other" in
       let title = Option.value (str "title" update) ~default:acp_kind in
       let raw_input = field "rawInput" update in
       let kind, native_name =
         tool_kind
           ~acp_kind
           ~name:(str "name" update)
           ~meta:(field "_meta" update)
           ~title
           ~raw_input
       in
       let input =
         tool_input
           ?location:(first_location_path update)
           ~kind
           ~title
           raw_input
       in
       let tool =
         { Tool.acp_kind
         ; kind
         ; native_name
         ; title
         ; input
         ; output = ""
         ; changes = []
         ; images = []
         ; running = false
         ; ended = false
         ; backgrounded = false
         }
       in
       Hashtbl.set t.tools ~key:id ~data:tool;
       let start =
         Output.Event
           (Tool_start { id; kind; native_name; title; input; parent = None })
       in
       Some (id, tool, close_items t @ [ start ]))
;;

let on_tool_update t update =
  match tool_of_update t update with
  | None -> []
  | Some (id, tool, started) -> started @ apply_tool t id tool update
;;

let plan_status = function
  | Some "in_progress" -> Plan.Status.In_progress
  | Some "completed" -> Completed
  | _ -> Pending
;;

let on_plan entries =
  let steps =
    List.mapi entries ~f:(fun i e ->
      let content = String.strip (Option.value (str "content" e) ~default:"") in
      { Plan.Step.step =
          (if String.is_empty content
           then sprintf "Step %d" (i + 1)
           else content)
      ; status = plan_status (str "status" e)
      })
  in
  if List.is_empty steps
  then []
  else [ Output.Event (Plan { explanation = None; steps }) ]
;;

let on_usage t update =
  let int name =
    match field name update with
    | Some json -> Json.int json
    | None -> None
  in
  let cost_usd =
    match field "cost" update with
    | Some (`Object _ as cost) ->
      (match
         str "currency" cost, Option.bind (field "amount" cost) ~f:Json.float
       with
       | Some currency, Some amount when String.Caseless.equal currency "USD" ->
         let previous = Option.value t.cost ~default:0. in
         t.cost <- Some amount;
         Some (Float.max 0. (amount -. previous))
       | _ -> None)
    | _ -> None
  in
  [ Output.Event
      (Usage
         { usage = Usage.zero
         ; cost_usd
         ; context_tokens = int "used"
         ; context_window = Option.filter (int "size") ~f:(fun s -> s > 0)
         })
  ]
;;

let on_config_change t config =
  t.config <- config;
  [ Output.Config_changed config ]
;;

let on_update t params =
  let update = obj (field "update" params) in
  let replay =
    t.loading
    ||
    match field "_meta" params with
    | Some (`Object _ as meta) -> flag "isReplay" meta
    | _ -> false
  in
  match str "sessionUpdate" update with
  | Some "current_mode_update" ->
    on_config_change
      t
      { t.config with current_mode = str "currentModeId" update }
  | Some "current_model_update" ->
    (match str "currentModelId" update with
     | Some id ->
       set_current_model t id;
       [ Output.Config_changed t.config ]
     | None -> [])
  | Some "config_option_update" ->
    on_config_change
      t
      { t.config with
        config_options =
          with_models t (parse_config_options (items "configOptions" update))
      }
  | Some "available_commands_update" ->
    [ Output.Commands
        (List.filter_map (items "availableCommands" update) ~f:(fun c ->
           Option.map (str "name" c) ~f:(fun name ->
             { Command.name
             ; description = Option.value (str "description" c) ~default:""
             ; input_hint =
                 Option.bind (field "input" c) ~f:(fun i -> str "hint" i)
             })))
    ]
  | Some "session_info_update" when Option.is_some (field "title" update) ->
    [ Output.Title (str "title" update) ]
  | Some "usage_update" when replay ->
    (* The replay of a loaded session: its running cost total is where the
       live updates count from. *)
    ignore (on_usage t update : Output.t list);
    []
  | _ when replay -> []
  | Some "agent_message_chunk" ->
    (match block_text (obj (field "content" update)) with
     | Some text when not (String.is_empty text) ->
       ensure_autonomous_turn t
       @ chunk t ~thought:false ~message_id:(str "messageId" update) text
     | _ -> [])
  | Some "agent_thought_chunk" ->
    (match block_text (obj (field "content" update)) with
     | Some text when not (String.is_empty text) ->
       ensure_autonomous_turn t
       @ chunk t ~thought:true ~message_id:(str "messageId" update) text
     | _ -> [])
  | Some ("tool_call" | "tool_call_update") ->
    let opened = ensure_autonomous_turn t in
    (match t.turn with
     | None -> []
     | Some turn ->
       let events = on_tool_update t update in
       if
         List.exists events ~f:(function
           | Output.Event (Tool_start _) -> true
           | _ -> false)
       then turn.saw_text <- false;
       opened @ events)
  | Some "plan" ->
    if Option.is_some t.turn then on_plan (items "entries" update) else []
  | Some "usage_update" -> on_usage t update
  | Some "notice" ->
    let title = String.strip (Option.value (str "title" update) ~default:"") in
    if String.is_empty title
    then []
    else (
      let text =
        match str "severity" update with
        | None | Some "info" -> title
        | Some severity -> severity ^ ": " ^ title
      in
      let text =
        match str "description" update with
        | Some d when not (String.is_empty (String.strip d)) -> text ^ "\n" ^ d
        | _ -> text
      in
      [ Output.Event (Notice text) ])
  | Some "compaction_update" ->
    (match str "status" update with
     | Some "completed" ->
       (* An upsert: later patches of the same compaction are not new ones. *)
       (match str "compactionId" update with
        | Some id when Hash_set.mem t.compacted id -> []
        | id ->
          Option.iter id ~f:(Hash_set.add t.compacted);
          [ Output.Event
              (Compacted { tokens_before = None; tokens_after = None })
          ])
     | Some (("failed" | "cancelled") as status) ->
       [ Output.Event
           (Notice
              (sprintf
                 "compaction %s%s"
                 status
                 (Option.value_map (str "error" update) ~default:"" ~f:(fun e ->
                    ": " ^ e))))
       ]
     | _ -> [])
  | _ -> []
;;

let settle_tools ~(stop : Adapter_event.Stop.t) t =
  let open_ =
    Hashtbl.to_alist t.tools
    |> List.filter ~f:(fun (_, (tool : Tool.t)) ->
      (not tool.ended) && not tool.backgrounded)
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
  match stop with
  | Done ->
    Hashtbl.filter_inplace t.tools ~f:(fun (tool : Tool.t) -> not tool.ended);
    List.map open_ ~f:(fun (id, (tool : Tool.t)) ->
      tool.backgrounded <- true;
      let output =
        if String.is_empty tool.output
        then "[running in background]"
        else sprintf "%s\n[running in background]" (String.rstrip tool.output)
      in
      Output.Event
        (Tool_end
           { id
           ; ok = true
           ; output
           ; changes = tool.changes
           ; images = tool.images
           }))
  | Interrupted | Failed _ ->
    Hashtbl.clear t.tools;
    List.map open_ ~f:(fun (id, (tool : Tool.t)) ->
      Output.Event
        (Tool_end
           { id
           ; ok = false
           ; output = tool.output
           ; changes = tool.changes
           ; images = tool.images
           }))
;;

let end_turn t (stop : Adapter_event.Stop.t) =
  match t.turn with
  | None -> []
  | Some turn ->
    let closed = close_items t in
    let settled = settle_tools ~stop t in
    let withdrawn = withdraw_permissions t ~reply:false in
    let unasked = withdraw_questions t in
    t.turn <- None;
    closed
    @ settled
    @ withdrawn
    @ unasked
    @ [ Output.Event (Turn_end { turn_id = Some turn.id; stop }) ]
;;

let is_autonomous_turn t =
  match t.turn with
  | Some { autonomous = true; _ } -> true
  | Some _ | None -> false
;;

let is_cancelled_turn t =
  match t.turn with
  | Some { cancelled = true; _ } -> true
  | Some _ | None -> false
;;

let autonomous_has_text t =
  match t.turn with
  | Some { autonomous = true; saw_text; _ } -> saw_text
  | Some _ | None -> false
;;

let has_open_work t =
  Hashtbl.exists t.tools ~f:(fun (tool : Tool.t) -> not tool.ended)
  || Hashtbl.length t.permissions > 0
  || Hashtbl.length t.questions > 0
;;

let finish_autonomous t =
  match t.turn with
  | Some { autonomous = true; cancelled; _ } ->
    end_turn t (if cancelled then Interrupted else Done)
  | Some _ | None -> []
;;

let known_stop_reasons =
  [ "end_turn"; "cancelled"; "max_tokens"; "max_turn_requests"; "refusal" ]
;;

let stop_of_reason ~cancelled reason : Adapter_event.Stop.t =
  match reason with
  | Some "end_turn" -> Done
  | Some "cancelled" -> Interrupted
  | _ when cancelled -> Interrupted
  | Some "max_tokens" ->
    Failed
      { kind = Other
      ; message = "the agent reached its maximum number of tokens"
      }
  | Some "max_turn_requests" ->
    Failed
      { kind = Other
      ; message =
          "the agent reached its maximum number of requests for one turn"
      }
  | Some "refusal" ->
    Failed { kind = Other; message = "the agent refused to continue" }
  | Some _ ->
    (* A newer agent's reason: the turn ended, not failed. *)
    Done
  | None ->
    Failed
      { kind = Protocol
      ; message = "session/prompt answered without a stopReason"
      }
;;

(* A [session/prompt] answered while a later one, steering the turn, is still
   open: the turn goes on under the later one. *)
let steered t =
  match t.turn with
  | Some turn when turn.prompts > 1 ->
    turn.prompts <- turn.prompts - 1;
    true
  | _ -> false
;;

let on_response t ~id ~result ~error =
  match Hashtbl.find_and_remove t.pending id with
  | None -> []
  | Some pending ->
    let kind = Pending.kind pending in
    (match error with
     | Some error ->
       let code, message = error_of error in
       (match pending with
        | Load_session _ -> t.loading <- false
        | _ -> ());
       let not_taken () =
         [ Output.Event
             (Notice
                (sprintf
                   "the agent did not take your message sent during its turn \
                    (%s): it never saw it, so send it again once this turn \
                    ends"
                   message))
         ]
       in
       (match pending with
        | Prompt when steered t -> not_taken ()
        | Steer -> not_taken ()
        | Prompt ->
          let cancelled =
            Option.value_map t.turn ~default:false ~f:(fun turn ->
              turn.cancelled)
          in
          end_turn
            t
            (if cancelled
             then Interrupted
             else (
               let kind : Adapter_event.Error_kind.t =
                 if code = -32000
                 then Auth_required { hint = message }
                 else Other
               in
               Failed { kind; message }))
        | _ -> [ Output.Response (Failed { kind; code; message }) ])
     | None ->
       let result = obj result in
       (match pending with
        | Initialize ->
          let capabilities = parse_capabilities result in
          t.capabilities <- Some capabilities;
          [ Output.Response
              (Initialized
                 { protocol_version =
                     Option.value
                       (Option.bind
                          (field "protocolVersion" result)
                          ~f:Json.int)
                       ~default:0
                 ; agent = parse_agent_info result
                 ; capabilities
                 ; auth_methods = parse_auth_methods result
                 })
          ]
        | New_session | Load_session _ | Resume_session _ ->
          let session_id =
            match pending with
            | Load_session id | Resume_session id -> Some id
            | _ -> str "sessionId" result
          in
          t.loading <- false;
          (match session_id with
           | None ->
             [ Output.Response
                 (Failed
                    { kind
                    ; code = -32603
                    ; message = "session/new gave no sessionId"
                    })
             ]
           | Some session_id ->
             (* A new session counts its cost from nothing. *)
             (match pending with
              | New_session -> t.cost <- None
              | _ -> ());
             t.session_id <- Some session_id;
             let config = parse_session_config t result in
             t.config <- config;
             [ Output.Response (Session_opened { kind; session_id; config }) ])
        | Close_session ->
          t.session_id <- None;
          [ Output.Response Session_closed ]
        | Steer -> []
        | Prompt when steered t ->
          note_usage t result;
          []
        | Prompt ->
          note_usage t result;
          let usage = turn_usage t in
          let cancelled =
            Option.value_map t.turn ~default:false ~f:(fun turn ->
              turn.cancelled)
          in
          let reason = str "stopReason" result in
          let unknown =
            match reason with
            | Some r
              when not (List.mem known_stop_reasons r ~equal:String.equal) ->
              [ Output.Event
                  (Notice
                     (sprintf
                        "the agent ended its turn for a reason prigh does not \
                         know: %s"
                        r))
              ]
            | _ -> []
          in
          usage @ unknown @ end_turn t (stop_of_reason ~cancelled reason)
        | Set_mode mode_id ->
          t.config <- { t.config with current_mode = Some mode_id };
          [ Output.Response (Mode_set mode_id); Config_changed t.config ]
        | Set_config_option (config_id, asked) ->
          let options = parse_config_options (items "configOptions" result) in
          t.config
          <- { t.config with
               config_options =
                 (if List.is_empty options
                  then
                    List.map t.config.config_options ~f:(fun o ->
                      if String.equal o.id config_id
                      then { o with value = asked }
                      else o)
                  else with_models t options)
             };
          [ Output.Response (Config_set options); Config_changed t.config ]
        | Set_model id ->
          set_current_model t id;
          [ Output.Response (Config_set (Option.to_list t.models))
          ; Config_changed t.config
          ]))
;;

let request_id_string = function
  | `String s -> s
  | json -> Json.to_string json
;;

let on_permission t ~id params =
  let request_id = request_id_string id in
  let tool_call = obj (field "toolCall" params) in
  let options =
    List.filter_map (items "options" params) ~f:(fun o ->
      match str "optionId" o, str "kind" o with
      | Some option_id, Some kind -> Some (option_id, kind)
      | _ -> None)
  in
  match t.turn with
  | Some { cancelled = true; _ } ->
    (* After [session/cancel], every permission request is answered
       cancelled. *)
    [ Output.Send (result_line id cancelled_outcome) ]
  | _ ->
    let opened = ensure_autonomous_turn t in
    Hashtbl.set
      t.permissions
      ~key:request_id
      ~data:{ id; kind = Options options };
    let started, kind, tool_id =
      match tool_of_update t tool_call with
      | Some (tool_id, tool, started) -> started, tool.kind, Some tool_id
      | None -> [], Other "other", None
    in
    let title = Option.value (str "title" tool_call) ~default:"" in
    opened
    @ started
    @ [ Output.Event
          (Approval_request
             { request_id
             ; tool_id
             ; kind
             ; summary = title
             ; input =
                 tool_input
                   ?location:(first_location_path tool_call)
                   ~kind
                   ~title
                   (field "rawInput" tool_call)
             ; offers_always =
                 List.exists options ~f:(fun (_, k) ->
                   String.equal k "allow_always")
             })
      ]
;;

let of_our_session t params =
  match t.session_id, str "sessionId" params with
  | Some ours, Some theirs -> String.equal ours theirs
  | None, _ -> t.loading
  | Some _, None -> false
;;

let xai_method method_ =
  Option.map
    (List.find [ "_x.ai/"; "x.ai/" ] ~f:(fun prefix ->
       String.is_prefix method_ ~prefix))
    ~f:(fun prefix -> String.drop_prefix method_ (String.length prefix))
;;

(* Grok wraps some requests' params as [{method, params}]. *)
let xai_params params =
  match field "params" params with
  | Some (`Object _ as inner) when Option.is_some (field "method" params) ->
    inner
  | _ -> params
;;

let xai_questions params =
  List.map (items "questions" params) ~f:(fun q ->
    { User_question.question = Option.value (str "question" q) ~default:""
    ; header = None
    ; choices =
        List.filter_map (items "options" q) ~f:(fun o ->
          Option.map (str "label" o) ~f:(fun label ->
            { User_question.Choice.label; description = str "description" o }))
    ; multiple = flag "multiSelect" q
    })
;;

(* Grok's requests and notifications beyond ACP ([x.ai/...]): its questions
   go to the user; its plan, when it leaves plan mode, is put to the
   user as Claude Code's is (start working or keep planning); its
   background tasks and subagents are noted. *)
let on_xai t ~id name params =
  let params = xai_params params in
  match name, id with
  | "ask_user_question", Some id ->
    (match t.turn with
     | Some { cancelled = true; _ } | None ->
       [ Output.Send (result_line id question_cancelled) ]
     | Some _ ->
       let request_id = request_id_string id in
       let questions = xai_questions params in
       Hashtbl.set t.questions ~key:request_id ~data:(id, questions);
       [ Output.Event (User_question { request_id; questions }) ])
  | "exit_plan_mode", Some id ->
    let plan =
      match Option.map (str "planContent" params) ~f:String.strip with
      | Some plan when not (String.is_empty plan) -> plan
      | _ -> "(no plan written)"
    in
    (match t.turn with
     | Some { cancelled = true; _ } | None ->
       [ Output.Send (result_line id plan_abandoned) ]
     | Some _ ->
       let request_id = request_id_string id in
       Hashtbl.set t.permissions ~key:request_id ~data:{ id; kind = Plan };
       [ Output.Event
           (Approval_request
              { request_id
              ; tool_id = str "toolCallId" params
              ; kind = Other "ExitPlanMode"
              ; summary = plan
              ; input = params
              ; offers_always = false
              })
       ])
  | ("task_backgrounded" | "task_completed"), None ->
    let update = obj (field "update" params) in
    let snapshot = obj (field "task_snapshot" update) in
    let label =
      List.find_map
        [ "description", update
        ; "description", snapshot
        ; "display_command", snapshot
        ; "command", snapshot
        ; "command", update
        ]
        ~f:(fun (key, json) -> str key json)
    in
    let what =
      match label with
      | Some label -> sprintf "a background task (%s)" label
      | None -> "a background task"
    in
    [ Output.Event
        (Notice
           (if String.equal name "task_backgrounded"
            then sprintf "the agent runs %s" what
            else sprintf "%s ended" (String.capitalize what)))
    ]
  | _, Some id ->
    [ Output.Send
        (error_line
           id
           ~code:(-32601)
           ~message:(sprintf "method not found: x.ai/%s" name))
    ]
  | _, None -> []
;;

let answer t ~request_id (reply : User_question.Reply.t) =
  match Hashtbl.find_and_remove t.questions request_id with
  | None -> None
  | Some (id, questions) ->
    let result =
      match reply with
      | Answered answers ->
        (* Keyed by the question's text, as Grok asks; what the user typed
           rather than picked goes as [Other] with a note, as T3 sends it. *)
        let answered =
          List.zip_with_remainder questions answers
          |> fst
          |> List.filter_map ~f:(fun ((q : User_question.t), picks) ->
            let picks =
              List.filter_map picks ~f:(fun p ->
                Option.some_if (not (String.is_empty (String.strip p))) p)
            in
            if String.is_empty q.question || List.is_empty picks
            then None
            else (
              let labels, typed =
                List.partition_tf picks ~f:(fun p ->
                  List.exists q.choices ~f:(fun (c : User_question.Choice.t) ->
                    String.equal c.label p))
              in
              Some
                ( q.question
                , (if List.is_empty labels then [ "Other" ] else labels)
                , typed )))
        in
        let annotations =
          List.filter_map answered ~f:(fun (question, _, typed) ->
            if List.is_empty typed
            then None
            else
              Some
                ( question
                , `Object [ "notes", `String (String.concat ~sep:"\n" typed) ]
                ))
        in
        `Object
          ([ "outcome", `String "accepted"
           ; ( "answers"
             , `Object
                 (List.map answered ~f:(fun (question, labels, _) ->
                    question, `Array (List.map labels ~f:(fun l -> `String l))))
             )
           ]
           @
           if List.is_empty annotations
           then []
           else [ "annotations", `Object annotations ])
      | Declined | Cancelled | Timed_out _ | Unavailable _ -> question_cancelled
    in
    Some (result_line id result)
;;

let handle_line t raw =
  if String.is_empty (String.strip raw)
  then []
  else (
    match Json.parse raw with
    | Error _ ->
      [ Output.Event
          (Notice
             (sprintf
                "the ACP agent wrote a line that is not JSON: %s"
                (String.prefix raw 200)))
      ]
    | Ok json ->
      let id = field "id" json in
      let params = obj (field "params" json) in
      (match str "method" json, id with
       | Some "session/request_permission", Some id ->
         if of_our_session t params
         then on_permission t ~id params
         else
           [ Output.Send
               (error_line id ~code:(-32602) ~message:"unknown session")
           ]
       | Some method_, id
         when t.quirks.xai_extensions && Option.is_some (xai_method method_) ->
         on_xai t ~id (Option.value_exn (xai_method method_)) params
       | Some method_, Some id ->
         [ Output.Send
             (error_line
                id
                ~code:(-32601)
                ~message:
                  (sprintf
                     "method not found: %s (this client does not offer it)"
                     method_))
         ]
       | Some "session/update", None ->
         if of_our_session t params then on_update t params else []
       | Some "$/cancel_request", None ->
         (match field "requestId" params with
          | Some request ->
            let request_id = request_id_string request in
            (match Hashtbl.find_and_remove t.permissions request_id with
             | None -> []
             | Some p ->
               [ Output.Send
                   (error_line p.id ~code:(-32800) ~message:"request cancelled")
               ; Event (Request_withdrawn { request_id })
               ])
          | None -> [])
       | Some _, None -> []
       | None, Some id ->
         (match Json.int id with
          | None -> []
          | Some id ->
            on_response
              t
              ~id
              ~result:(field "result" json)
              ~error:(field "error" json))
       | None, None -> []))
;;

let process_exited ?(name = "the ACP agent") t ~code ~stderr_tail =
  let message =
    match code with
    | Some code -> sprintf "%s exited (code %d)" name code
    | None -> sprintf "%s exited" name
  in
  let turn = end_turn t (Failed { kind = Crashed { stderr_tail }; message }) in
  let withdrawn = withdraw_permissions t ~reply:false in
  let pending =
    Hashtbl.to_alist t.pending
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.filter_map ~f:(fun (_, pending) ->
      match (pending : Pending.t) with
      | Prompt -> None
      | _ ->
        Some
          (Output.Response
             (Failed { kind = Pending.kind pending; code = -32603; message })))
  in
  Hashtbl.clear t.pending;
  t.loading <- false;
  turn @ withdrawn @ pending
;;

let assume_loaded t ~session_id =
  Hashtbl.filter_inplace t.pending ~f:(function
    | Pending.Load_session _ -> false
    | _ -> true);
  t.loading <- false;
  t.session_id <- Some session_id
;;
