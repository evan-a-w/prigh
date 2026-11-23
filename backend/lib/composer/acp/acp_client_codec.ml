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
    | Prompt
    | Set_mode of string
    | Set_config_option

  let kind : t -> Request_kind.t = function
    | Initialize -> Initialize
    | New_session -> New_session
    | Load_session _ -> Load_session
    | Resume_session _ -> Resume_session
    | Prompt -> Prompt
    | Set_mode _ -> Set_mode
    | Set_config_option -> Set_config_option
  ;;
end

module Tool = struct
  type t =
    { acp_kind : string
    ; kind : Adapter_event.Tool_kind.t
    ; mutable output : string
    ; mutable changes : Adapter_event.Change.t list
    ; mutable images : Image.t list
    ; mutable ended : bool
    }
end

module Permission = struct
  type t =
    { id : Json.t
    ; options : (string * string) list (** option id, kind *)
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
    ; mutable text : Open_item.t option
    ; mutable thought : Open_item.t option
    ; mutable items : int
    }
end

type t =
  { mutable next_id : int
  ; pending : Pending.t Int.Table.t
  ; mutable capabilities : Agent_capabilities.t option
  ; mutable session_id : string option
  ; mutable loading : bool
  ; mutable config : Session_config.t
  ; mutable turn : Turn.t option
  ; mutable turns : int
  ; permissions : Permission.t String.Table.t
  ; tools : Tool.t String.Table.t
  ; mutable cost : float option
  }

let create () =
  { next_id = 0
  ; pending = Int.Table.create ()
  ; capabilities = None
  ; session_id = None
  ; loading = false
  ; config = Session_config.empty
  ; turn = None
  ; turns = 0
  ; permissions = String.Table.create ()
  ; tools = String.Table.create ()
  ; cost = None
  }
;;

let session_id t = t.session_id
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

let content_block : Input.t -> Json.t = function
  | Text text -> `Object [ "type", `String "text"; "text", `String text ]
  | Image { mime_type; data } ->
    `Object
      [ "type", `String "image"
      ; "mimeType", `String mime_type
      ; "data", `String data
      ]
;;

let prompt t inputs =
  match t.session_id, t.turn with
  | None, _ -> Or_error.error_string "no ACP session yet"
  | Some _, Some turn ->
    Or_error.error_s [%message "a turn is already running" ~turn:turn.id]
  | Some session_id, None ->
    let has_image =
      List.exists inputs ~f:(function
        | Input.Image _ -> true
        | Text _ -> false)
    in
    let images =
      Option.value_map t.capabilities ~default:false ~f:(fun c -> c.images)
    in
    if has_image && not images
    then Or_error.error_string "this agent does not take images"
    else (
      t.turns <- t.turns + 1;
      let turn =
        { Turn.id = sprintf "turn-%d" t.turns
        ; cancelled = false
        ; text = None
        ; thought = None
        ; items = 0
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

let cancelled_outcome =
  `Object [ "outcome", `Object [ "outcome", `String "cancelled" ] ]
;;

let withdraw_permissions t ~reply =
  let waiting = Hashtbl.to_alist t.permissions in
  Hashtbl.clear t.permissions;
  List.concat_map
    (List.sort waiting ~compare:(fun (a, _) (b, _) -> String.compare a b))
    ~f:(fun (request_id, (p : Permission.t)) ->
      (if reply
       then [ Output.Send (result_line p.id cancelled_outcome) ]
       else [])
      @ [ Event (Request_withdrawn { request_id }) ])
;;

let cancel t =
  match t.turn, t.session_id with
  | Some turn, Some session_id when not turn.cancelled ->
    turn.cancelled <- true;
    Output.Send
      (notification
         "session/cancel"
         (`Object [ "sessionId", `String session_id ]))
    :: withdraw_permissions t ~reply:true
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
    (match
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
             [ "type", `String "boolean"
             ; ("value", if b then `True else `False)
             ]
         | Select { choices; _ }, `Id id ->
           if List.exists choices ~f:(fun c -> String.equal c.value id)
           then Ok [ "value", `String id ]
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
       Or_error.map value_fields ~f:(fun value_fields ->
         request
           t
           Set_config_option
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
  | Some p ->
    let of_kind kind =
      List.find_map p.options ~f:(fun (id, k) ->
        Option.some_if (String.equal k kind) id)
    in
    let choice =
      match reply with
      | Allow -> of_kind "allow_once"
      | Allow_always ->
        Option.first_some (of_kind "allow_always") (of_kind "allow_once")
      | Deny _ ->
        Option.first_some (of_kind "reject_once") (of_kind "reject_always")
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
    Some (result_line p.id outcome)
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

let parse_session_config json : Session_config.t =
  let current_mode, modes = parse_modes json in
  { current_mode
  ; modes
  ; config_options = parse_config_options (items "configOptions" json)
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

let tool_kind ~acp_kind ~name : Adapter_event.Tool_kind.t =
  match acp_kind with
  | "read" -> Read
  | "edit" | "delete" | "move" -> File_change
  | "search" -> Search
  | "execute" -> Command
  | "fetch" -> Web_search
  | "think" | "switch_mode" -> Other acp_kind
  | _ -> Other (Option.value name ~default:"other")
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

(* Appends to the open item of the kind, opening one (and closing the other
   kind's) when there is none or the agent's message id changed. *)
let chunk t ~thought ~message_id text =
  match t.turn with
  | None -> []
  | Some turn ->
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

let tool_input ~(kind : Adapter_event.Tool_kind.t) ~title raw : Json.t =
  let fields =
    match raw with
    | Some (`Object fields) -> fields
    | None | Some `Null -> []
    | Some other -> [ "input", other ]
  in
  match kind with
  | Command when not (List.Assoc.mem fields ~equal:String.equal "command") ->
    `Object (fields @ [ "command", `String title ])
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

let raw_output_text = function
  | None | Some `Null -> None
  | Some (`String s) -> Some s
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
      let texts =
        List.filter_map blocks ~f:(fun b ->
          match str "type" b with
          | Some "content" -> block_text (obj (field "content" b))
          | _ -> None)
      in
      let output = String.concat ~sep:"\n" texts in
      tool.changes
      <- List.filter_map blocks ~f:(fun b ->
           match str "type" b with
           | Some "diff" -> change ~acp_kind:tool.acp_kind b
           | _ -> None);
      tool.images
      <- List.filter_map blocks ~f:(fun b ->
           match str "type" b with
           | Some "content" -> block_image (obj (field "content" b))
           | _ -> None);
      let previous = tool.output in
      tool.output <- output;
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
  let ended =
    match str "status" update with
    | Some (("completed" | "failed") as status) when not tool.ended ->
      tool.ended <- true;
      let output =
        if String.is_empty tool.output
        then
          Option.value (raw_output_text (field "rawOutput" update)) ~default:""
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
  updates @ ended
;;

(* The call [update] names, started (with [Tool_start]) if it is new. *)
let tool_of_update t update =
  match str "toolCallId" update with
  | None -> None
  | Some id ->
    (match Hashtbl.find t.tools id with
     | Some tool -> Some (id, tool, [])
     | None ->
       let acp_kind = Option.value (str "kind" update) ~default:"other" in
       let name = str "name" update in
       let kind = tool_kind ~acp_kind ~name in
       let title = Option.value (str "title" update) ~default:acp_kind in
       let tool =
         { Tool.acp_kind
         ; kind
         ; output = ""
         ; changes = []
         ; images = []
         ; ended = false
         }
       in
       Hashtbl.set t.tools ~key:id ~data:tool;
       let start =
         Output.Event
           (Tool_start
              { id
              ; kind
              ; native_name = Option.value name ~default:acp_kind
              ; title
              ; input = tool_input ~kind ~title (field "rawInput" update)
              ; parent = None
              })
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
  | Some "config_option_update" ->
    on_config_change
      t
      { t.config with
        config_options = parse_config_options (items "configOptions" update)
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
  | _ when replay -> []
  | Some "agent_message_chunk" ->
    (match block_text (obj (field "content" update)) with
     | Some text when not (String.is_empty text) ->
       chunk t ~thought:false ~message_id:(str "messageId" update) text
     | _ -> [])
  | Some "agent_thought_chunk" ->
    (match block_text (obj (field "content" update)) with
     | Some text when not (String.is_empty text) ->
       chunk t ~thought:true ~message_id:(str "messageId" update) text
     | _ -> [])
  | Some ("tool_call" | "tool_call_update") -> on_tool_update t update
  | Some "plan" -> on_plan (items "entries" update)
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
       [ Output.Event (Compacted { tokens_before = None; tokens_after = None })
       ]
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

let settle_tools t =
  let open_ =
    Hashtbl.to_alist t.tools
    |> List.filter ~f:(fun (_, (tool : Tool.t)) -> not tool.ended)
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
  in
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
    let settled = settle_tools t in
    let withdrawn = withdraw_permissions t ~reply:false in
    t.turn <- None;
    closed
    @ settled
    @ withdrawn
    @ [ Output.Event (Turn_end { turn_id = Some turn.id; stop }) ]
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
  | Some other ->
    Failed { kind = Protocol; message = "unknown stopReason " ^ other }
  | None ->
    Failed
      { kind = Protocol
      ; message = "session/prompt answered without a stopReason"
      }
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
       (match pending with
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
             t.session_id <- Some session_id;
             let config = parse_session_config result in
             t.config <- config;
             [ Output.Response (Session_opened { kind; session_id; config }) ])
        | Prompt ->
          let cancelled =
            Option.value_map t.turn ~default:false ~f:(fun turn ->
              turn.cancelled)
          in
          end_turn t (stop_of_reason ~cancelled (str "stopReason" result))
        | Set_mode mode_id ->
          t.config <- { t.config with current_mode = Some mode_id };
          [ Output.Response (Mode_set mode_id); Config_changed t.config ]
        | Set_config_option ->
          let options = parse_config_options (items "configOptions" result) in
          if not (List.is_empty options)
          then t.config <- { t.config with config_options = options };
          [ Output.Response (Config_set options); Config_changed t.config ]))
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
  Hashtbl.set t.permissions ~key:request_id ~data:{ id; options };
  let started, kind, tool_id =
    match tool_of_update t tool_call with
    | Some (tool_id, tool, started) -> started, tool.kind, Some tool_id
    | None -> [], Other "other", None
  in
  let title = Option.value (str "title" tool_call) ~default:"" in
  started
  @ [ Output.Event
        (Approval_request
           { request_id
           ; tool_id
           ; kind
           ; summary = title
           ; input = tool_input ~kind ~title (field "rawInput" tool_call)
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

let handle_line t raw =
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
         [ Output.Send (error_line id ~code:(-32602) ~message:"unknown session")
         ]
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
     | None, None -> [])
;;

let process_exited t ~code ~stderr_tail =
  let message =
    match code with
    | Some code -> sprintf "the ACP agent exited (code %d)" code
    | None -> "the ACP agent exited"
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
