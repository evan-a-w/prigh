open! Core
open! Import

module Account = struct
  type t =
    { email : string option
    ; organization : string option
    ; subscription_type : string option
    ; token_source : string option
    ; api_key_source : string option
    ; api_provider : string option
    }
  [@@deriving sexp_of]

  let logged_in t =
    let set = function
      | None | Some "" | Some "none" -> false
      | Some _ -> true
    in
    set t.token_source
    || set t.api_key_source
    || set t.email
    || set t.subscription_type
    ||
    match t.api_provider with
    | None | Some "firstParty" -> false
    | Some _ -> true
  ;;
end

module Model_info = struct
  type t =
    { value : string
    ; display_name : string
    ; description : string option
    }
  [@@deriving sexp_of]
end

module Command_info = struct
  type t =
    { name : string
    ; description : string option
    }
  [@@deriving sexp_of]
end

module Init_info = struct
  type t =
    { version : string option
    ; account : Account.t option
    ; models : Model_info.t list
    ; commands : Command_info.t list
    }
  [@@deriving sexp_of]
end

module Response = struct
  module Command = struct
    type t =
      | Initialize
      | Interrupt
      | Set_permission_mode
      | Set_model
      | Set_max_thinking_tokens
    [@@deriving sexp_of, equal]
  end

  type t =
    | Initialized of Init_info.t
    | Done of Command.t
    | Failed of
        { command : Command.t
        ; message : string
        }
  [@@deriving sexp_of]
end

module Mcp_request = struct
  type t =
    { request_id : string
    ; server : string
    ; message : Json.t
    }
  [@@deriving sexp_of]
end

module Output = struct
  type t =
    | Event of Adapter_event.t
    | Send of string
    | Response of Response.t
    | Mcp of Mcp_request.t
  [@@deriving sexp_of]
end

(* A [can_use_tool] request awaiting the user. *)
module Permission = struct
  type t =
    { request_id : string
    ; input : Json.t
    ; suggestions : Json.t list
    }
end

module Open_tool = struct
  type t =
    { id : string
    ; name : string
    ; input : Json.t
    }
end

type t =
  { mutable next_id : int
  ; ours : Response.Command.t String.Table.t
  ; mutable permissions : Permission.t list (* oldest first *)
  ; mutable tools : Open_tool.t list (* oldest first *)
  ; mutable session_id : string option
  ; mutable model : string option
  ; mutable turn : string option
  ; mutable turns : int
  ; mutable interrupting : bool
  ; mutable cost_total : float
  ; mutable context_tokens : int option
  ; mutable rate_limited : bool
  ; mutable auth_failed : bool
  ; mutable limit_resets_at : float option
  ; blocks : int String.Table.t (* message id -> content blocks seen *)
  ; mutable streaming : string option (* the top-level message being streamed *)
  ; warned : String.Hash_set.t
  }

let create () =
  { next_id = 0
  ; ours = String.Table.create ()
  ; permissions = []
  ; tools = []
  ; session_id = None
  ; model = None
  ; turn = None
  ; turns = 0
  ; interrupting = false
  ; cost_total = 0.
  ; context_tokens = None
  ; rate_limited = false
  ; auth_failed = false
  ; limit_resets_at = None
  ; blocks = String.Table.create ()
  ; streaming = None
  ; warned = String.Hash_set.create ()
  }
;;

let session_id t = t.session_id
let in_turn t = Option.is_some t.turn

(* JSON helpers *)

let member key json = Json.member key json
let str json key = Option.bind (member key json) ~f:Json.string
let int json key = Option.bind (member key json) ~f:Json.int
let num json key = Option.bind (member key json) ~f:Json.float
let bool json key = Option.bind (member key json) ~f:Json.bool

let list json key =
  Option.value (Option.bind (member key json) ~f:Json.list) ~default:[]
;;

let json_int n = `Number (Int.to_string n)
let line json = Json.to_string json

(* Outgoing frames *)

let control_request t command subtype fields =
  t.next_id <- t.next_id + 1;
  let request_id = sprintf "req_%d_prigh" t.next_id in
  Hashtbl.set t.ours ~key:request_id ~data:command;
  line
    (`Object
        [ "type", `String "control_request"
        ; "request_id", `String request_id
        ; "request", `Object (("subtype", `String subtype) :: fields)
        ])
;;

let success ~request_id response =
  line
    (`Object
        [ "type", `String "control_response"
        ; ( "response"
          , `Object
              [ "subtype", `String "success"
              ; "request_id", `String request_id
              ; "response", response
              ] )
        ])
;;

let error_response ~request_id message =
  line
    (`Object
        [ "type", `String "control_response"
        ; ( "response"
          , `Object
              [ "subtype", `String "error"
              ; "request_id", `String request_id
              ; "error", `String message
              ] )
        ])
;;

let initialize t ~sdk_mcp_servers =
  control_request
    t
    Initialize
    "initialize"
    [ "hooks", `Null
    ; "sdkMcpServers", `Array (List.map sdk_mcp_servers ~f:(fun s -> `String s))
    ]
;;

let user_message t ?uuid ~text ~images () =
  ignore (t : t);
  let images =
    List.map images ~f:(fun (image : Image.t) ->
      `Object
        [ "type", `String "image"
        ; ( "source"
          , `Object
              [ "type", `String "base64"
              ; "media_type", `String image.mime_type
              ; "data", `String image.data
              ] )
        ])
  in
  let content =
    (if String.is_empty text
     then []
     else [ `Object [ "type", `String "text"; "text", `String text ] ])
    @ images
  in
  line
    (`Object
        ([ "type", `String "user"
         ; "session_id", `String ""
         ; "parent_tool_use_id", `Null
         ; ( "message"
           , `Object [ "role", `String "user"; "content", `Array content ] )
         ]
         @
         match uuid with
         | Some uuid -> [ "uuid", `String uuid ]
         | None -> []))
;;

let compact t ~instructions =
  let text =
    match instructions with
    | Some i when not (String.is_empty (String.strip i)) ->
      "/compact " ^ String.strip i
    | Some _ | None -> "/compact"
  in
  user_message t ~text ~images:[] ()
;;

let interrupt t =
  t.interrupting <- true;
  control_request t Interrupt "interrupt" []
;;

let set_permission_mode t mode =
  control_request
    t
    Set_permission_mode
    "set_permission_mode"
    [ "mode", `String (Claude_permission_mode.to_string mode) ]
;;

let set_model t model =
  control_request
    t
    Set_model
    "set_model"
    [ ( "model"
      , match model with
        | Some m -> `String m
        | None -> `Null )
    ]
;;

let set_max_thinking_tokens t tokens =
  control_request
    t
    Set_max_thinking_tokens
    "set_max_thinking_tokens"
    [ ( "max_thinking_tokens"
      , match tokens with
        | Some n -> json_int n
        | None -> `Null )
    ]
;;

let mcp_reply ~request_id response =
  let response =
    match response with
    | Some json -> json
    | None -> `Object [ "jsonrpc", `String "2.0"; "result", `Object [] ]
  in
  success ~request_id (`Object [ "mcp_response", response ])
;;

(* Turns *)

let ensure_turn t =
  match t.turn with
  | Some _ -> []
  | None ->
    t.turns <- t.turns + 1;
    let turn_id = sprintf "turn-%d" t.turns in
    t.turn <- Some turn_id;
    [ Output.Event (Turn_started { turn_id }) ]
;;

(* Close what the turn left open: tool calls never answered, requests
   never decided. *)
let settle t ~output =
  let tools =
    List.map t.tools ~f:(fun tool ->
      Output.Event
        (Tool_end
           { id = tool.id; ok = false; output; changes = []; images = [] }))
  in
  let requests =
    List.map t.permissions ~f:(fun p ->
      Output.Event (Request_withdrawn { request_id = p.request_id }))
  in
  t.tools <- [];
  t.permissions <- [];
  tools @ requests
;;

let end_turn t (stop : Adapter_event.Stop.t) =
  let outs = settle t ~output:"The turn ended before this call finished." in
  let turn_id = t.turn in
  t.turn <- None;
  t.interrupting <- false;
  t.rate_limited <- false;
  t.auth_failed <- false;
  t.limit_resets_at <- None;
  t.streaming <- None;
  outs @ [ Output.Event (Turn_end { turn_id; stop }) ]
;;

(* Messages *)

let usage_of json : Usage.t =
  let get key = Option.value (int json key) ~default:0 in
  let cache_read = get "cache_read_input_tokens" in
  { input = get "input_tokens" + get "cache_creation_input_tokens" + cache_read
  ; output = get "output_tokens"
  ; cache_read
  }
;;

let handle_system t json =
  match str json "subtype" with
  | Some "init" ->
    let model = str json "model" in
    if Option.is_some model then t.model <- model;
    (match str json "session_id" with
     | Some session_id
       when not ([%equal: string option] t.session_id (Some session_id)) ->
       t.session_id <- Some session_id;
       [ Output.Event (Thread_started { thread_id = session_id; model }) ]
     | Some _ | None -> [])
  | Some "status" ->
    (match str json "status" with
     | Some ("requesting" | "compacting") -> ensure_turn t
     | Some _ | None -> [])
  | Some "compact_boundary" ->
    let meta =
      Option.value (member "compact_metadata" json) ~default:(`Object [])
    in
    [ Output.Event
        (Compacted
           { tokens_before = int meta "pre_tokens"
           ; tokens_after = int meta "post_tokens"
           })
    ]
  | Some "api_retry" ->
    let message =
      match str json "error", int json "error_status" with
      | Some e, Some s -> sprintf "%s (%d)" e s
      | Some e, None -> e
      | None, Some s -> sprintf "HTTP %d" s
      | None, None -> "retrying"
    in
    [ Output.Event (Retrying { attempt = int json "attempt"; message }) ]
  | Some _ | None -> []
;;

let handle_stream_event t json =
  match member "parent_tool_use_id" json, member "event" json with
  | (None | Some `Null), Some event ->
    let item () =
      match t.streaming, int event "index" with
      | Some message, Some index -> Some (sprintf "%s:%d" message index)
      | _ -> None
    in
    let outs = ensure_turn t in
    (match str event "type" with
     | Some "message_start" ->
       t.streaming
       <- Option.bind (member "message" event) ~f:(fun m -> str m "id");
       outs
     | Some "content_block_delta" ->
       let delta = Option.value (member "delta" event) ~default:`Null in
       (match item (), str delta "type" with
        | Some item, Some "text_delta" ->
          outs
          @ [ Output.Event
                (Text_delta
                   { item; text = Option.value (str delta "text") ~default:"" })
            ]
        | Some item, Some "thinking_delta" ->
          outs
          @ [ Output.Event
                (Thinking_delta
                   { item
                   ; text = Option.value (str delta "thinking") ~default:""
                   })
            ]
        | _ -> outs)
     | Some _ | None -> outs)
  | Some _, _ | None, None -> []
;;

let handle_assistant t json =
  let parent =
    match member "parent_tool_use_id" json with
    | Some (`String p) -> Some p
    | Some _ | None -> None
  in
  let message = Option.value (member "message" json) ~default:(`Object []) in
  let synthetic =
    [%equal: string option] (str message "model") (Some "<synthetic>")
  in
  (match str json "error" with
   | Some "rate_limit" -> t.rate_limited <- true
   | Some ("authentication_failed" | "oauth_org_not_allowed") ->
     t.auth_failed <- true
   | Some _ | None -> ());
  if Option.is_none parent && not synthetic
  then
    Option.iter (member "usage" message) ~f:(fun usage ->
      let u = usage_of usage in
      t.context_tokens <- Some (u.input + u.output));
  let id = Option.value (str message "id") ~default:"msg" in
  let outs = ensure_turn t in
  let blocks =
    List.concat_map (list message "content") ~f:(fun block ->
      let index = Option.value (Hashtbl.find t.blocks id) ~default:0 in
      Hashtbl.set t.blocks ~key:id ~data:(index + 1);
      let item = sprintf "%s:%d" id index in
      match str block "type" with
      | Some "text" when Option.is_none parent && not synthetic ->
        [ Output.Event
            (Text_done
               { item; text = Option.value (str block "text") ~default:"" })
        ]
      | Some "thinking" when Option.is_none parent ->
        [ Output.Event
            (Thinking_done
               { item; text = Option.value (str block "thinking") ~default:"" })
        ]
      | Some "tool_use" ->
        let tool_id = Option.value (str block "id") ~default:item in
        let name = Option.value (str block "name") ~default:"tool" in
        let input = Option.value (member "input" block) ~default:(`Object []) in
        t.tools <- t.tools @ [ { Open_tool.id = tool_id; name; input } ];
        let start =
          Output.Event
            (Tool_start
               { id = tool_id
               ; kind = Claude_tool.kind name
               ; native_name = name
               ; title = Claude_tool.title ~name ~input
               ; input
               ; parent
               })
        in
        let plan =
          match name, parent with
          | "TodoWrite", None ->
            Option.to_list
              (Option.map (Claude_tool.plan_of_todos input) ~f:(fun plan ->
                 Output.Event (Plan plan)))
          | _ -> []
        in
        start :: plan
      | Some _ | None -> [])
  in
  outs @ blocks
;;

let tool_result_content json =
  match member "content" json with
  | Some (`String s) -> s, []
  | Some (`Array parts) ->
    let texts, images =
      List.partition_map parts ~f:(fun part ->
        match str part "type" with
        | Some "image" ->
          let source = Option.value (member "source" part) ~default:`Null in
          (match str source "media_type", str source "data" with
           | Some mime_type, Some data ->
             Second (Some { Image.mime_type; data })
           | _ -> Second None)
        | Some _ | None -> First (Option.value (str part "text") ~default:""))
    in
    String.concat ~sep:"\n" texts, List.filter_opt images
  | Some _ | None -> "", []
;;

let handle_user t json =
  match bool json "isReplay" with
  | Some true -> []
  | Some false | None ->
    let message = Option.value (member "message" json) ~default:`Null in
    let result = member "tool_use_result" json in
    List.concat_map (list message "content") ~f:(fun block ->
      match str block "type", str block "tool_use_id" with
      | Some "tool_result", Some id ->
        let tool, rest =
          List.partition_tf t.tools ~f:(fun tool -> String.equal tool.id id)
        in
        t.tools <- rest;
        let output, images = tool_result_content block in
        let ok = not (Option.value (bool block "is_error") ~default:false) in
        let changes =
          match tool with
          | [ tool ] when ok ->
            let result =
              match result with
              | Some (`Object _ as r) -> Some r
              | Some _ | None -> None
            in
            Claude_tool.changes ~name:tool.name ~input:tool.input ~result
          | _ -> []
        in
        ensure_turn t
        @ [ Output.Event (Tool_end { id; ok; output; changes; images }) ]
      | _ -> [])
;;

let context_window t json =
  let models =
    match member "modelUsage" json with
    | Some (`Object models) -> models
    | Some _ | None -> []
  in
  let window (_, usage) = int usage "contextWindow" in
  match
    Option.bind t.model ~f:(fun model ->
      List.find models ~f:(fun (name, _) -> String.equal name model))
  with
  | Some entry -> window entry
  | None -> List.find_map models ~f:window
;;

(* "Claude AI usage limit reached|1764000000": the legacy form, with the
   reset time. *)
let legacy_limit_reset text =
  match String.lsplit2 text ~on:'|' with
  | Some (prefix, epoch)
    when String.is_substring prefix ~substring:"usage limit" ->
    Option.map (Float.of_string_opt (String.strip epoch)) ~f:Fn.id
  | _ -> None
;;

let handle_result t json =
  let outs = ensure_turn t in
  let usage =
    match member "usage" json with
    | Some usage
      when Option.exists (num json "total_cost_usd") ~f:(fun c ->
             Float.(c > 0.))
           ||
           let u = usage_of usage in
           u.input + u.output > 0 ->
      let cost_usd =
        Option.map (num json "total_cost_usd") ~f:(fun total ->
          let delta =
            if Float.( >= ) total t.cost_total
            then total -. t.cost_total
            else total
          in
          t.cost_total <- total;
          delta)
      in
      [ Output.Event
          (Usage
             { usage = usage_of usage
             ; cost_usd
             ; context_tokens = t.context_tokens
             ; context_window = context_window t json
             })
      ]
    | Some _ | None -> []
  in
  let is_error = Option.value (bool json "is_error") ~default:false in
  let subtype = Option.value (str json "subtype") ~default:"success" in
  let stop : Adapter_event.Stop.t =
    if (not is_error) && String.equal subtype "success"
    then Done
    else (
      let aborted =
        match str json "terminal_reason" with
        | Some reason -> String.is_prefix reason ~prefix:"aborted"
        | None -> false
      in
      if t.interrupting || aborted
      then Interrupted
      else (
        let message =
          match str json "result", list json "errors" with
          | Some r, _ when not (String.is_empty r) -> r
          | _, (_ :: _ as errors) ->
            String.concat ~sep:"\n" (List.filter_map errors ~f:Json.string)
          | _ -> subtype
        in
        let status = int json "api_error_status" in
        let legacy = legacy_limit_reset message in
        let kind : Adapter_event.Error_kind.t =
          if
            t.rate_limited
            || [%equal: int option] status (Some 429)
            || Option.is_some legacy
          then
            Usage_limit
              { resets_at = Option.first_some t.limit_resets_at legacy }
          else if
            t.auth_failed
            || [%equal: int option] status (Some 401)
            || String.is_substring message ~substring:"authenticate"
          then
            Auth_required { hint = "run `claude auth login` on the tool host" }
          else Other
        in
        Failed { kind; message }))
  in
  outs @ usage @ end_turn t stop
;;

let handle_rate_limit t json =
  let info = Option.value (member "rate_limit_info" json) ~default:`Null in
  let resets_at = num info "resetsAt" in
  let kind = Option.value (str info "rateLimitType") ~default:"usage" in
  match str info "status" with
  | Some "rejected" ->
    t.rate_limited <- true;
    t.limit_resets_at <- resets_at;
    []
  | Some "allowed_warning" ->
    let key =
      sprintf
        "%s:%s"
        kind
        (Option.value_map resets_at ~default:"" ~f:Float.to_string)
    in
    if Hash_set.mem t.warned key
    then []
    else (
      Hash_set.add t.warned key;
      let used =
        match num info "utilization" with
        | Some u -> sprintf "%.0f%% of" (u *. 100.)
        | None -> "close to"
      in
      [ Output.Event
          (Notice (sprintf "Claude Code has used %s its %s limit." used kind))
      ])
  | Some _ | None -> []
;;

let questions_of input =
  List.filter_map (list input "questions") ~f:(fun q ->
    let%map.Option question = str q "question" in
    { User_question.question
    ; header = str q "header"
    ; choices =
        List.filter_map (list q "options") ~f:(fun o ->
          let%map.Option label = str o "label" in
          { User_question.Choice.label; description = str o "description" })
    ; multiple = Option.value (bool q "multiSelect") ~default:false
    })
;;

let handle_control_request t json =
  let request_id = Option.value (str json "request_id") ~default:"" in
  let request = Option.value (member "request" json) ~default:`Null in
  match str request "subtype" with
  | Some "can_use_tool" ->
    let tool_name = Option.value (str request "tool_name") ~default:"tool" in
    let input = Option.value (member "input" request) ~default:(`Object []) in
    let suggestions = list request "permission_suggestions" in
    t.permissions
    <- t.permissions @ [ { Permission.request_id; input; suggestions } ];
    let event : Adapter_event.t =
      match tool_name with
      | "AskUserQuestion" ->
        User_question { request_id; questions = questions_of input }
      | "ExitPlanMode" ->
        Approval_request
          { request_id
          ; tool_id = str request "tool_use_id"
          ; kind = Other "ExitPlanMode"
          ; summary =
              Option.value
                (str input "plan")
                ~default:"Leave plan mode and carry out the plan"
          ; input
          ; offers_always = false
          }
      | _ ->
        Approval_request
          { request_id
          ; tool_id = str request "tool_use_id"
          ; kind = Claude_tool.kind tool_name
          ; summary = Claude_tool.title ~name:tool_name ~input
          ; input
          ; offers_always = not (List.is_empty suggestions)
          }
    in
    ensure_turn t @ [ Output.Event event ]
  | Some "hook_callback" -> [ Output.Send (success ~request_id (`Object [])) ]
  | Some "mcp_message" ->
    [ Output.Mcp
        { request_id
        ; server = Option.value (str request "server_name") ~default:""
        ; message = Option.value (member "message" request) ~default:`Null
        }
    ]
  | Some subtype ->
    [ Output.Send
        (error_response
           ~request_id
           (sprintf "prigh does not handle %s" subtype))
    ]
  | None ->
    [ Output.Send
        (error_response ~request_id "control request without a subtype")
    ]
;;

let init_info json : Init_info.t =
  let account =
    Option.map (member "account" json) ~f:(fun a ->
      { Account.email = str a "email"
      ; organization = str a "organization"
      ; subscription_type = str a "subscriptionType"
      ; token_source = str a "tokenSource"
      ; api_key_source = str a "apiKeySource"
      ; api_provider = str a "apiProvider"
      })
  in
  { version = str json "claude_code_version"
  ; account
  ; models =
      List.filter_map (list json "models") ~f:(fun m ->
        let%map.Option value = str m "value" in
        { Model_info.value
        ; display_name = Option.value (str m "displayName") ~default:value
        ; description = str m "description"
        })
  ; commands =
      List.filter_map (list json "commands") ~f:(fun c ->
        let%map.Option name = str c "name" in
        { Command_info.name; description = str c "description" })
  }
;;

let handle_control_response t json =
  let response = Option.value (member "response" json) ~default:`Null in
  match str response "request_id" with
  | None -> []
  | Some request_id ->
    (match Hashtbl.find_and_remove t.ours request_id with
     | None -> []
     | Some command ->
       let result : Response.t =
         match str response "subtype" with
         | Some "success" ->
           (match command with
            | Initialize ->
              Initialized
                (init_info
                   (Option.value
                      (member "response" response)
                      ~default:(`Object [])))
            | command -> Done command)
         | Some _ | None ->
           Failed
             { command
             ; message = Option.value (str response "error") ~default:"failed"
             }
       in
       [ Output.Response result ])
;;

let handle_cancel t json =
  match str json "request_id" with
  | None -> []
  | Some request_id ->
    let cancelled, rest =
      List.partition_tf t.permissions ~f:(fun p ->
        String.equal p.request_id request_id)
    in
    t.permissions <- rest;
    List.map cancelled ~f:(fun p ->
      Output.Event (Request_withdrawn { request_id = p.request_id }))
;;

let handle_line t text =
  match Json.parse text with
  | Error _ ->
    if String.is_empty (String.strip text)
    then []
    else
      [ Output.Event
          (Notice
             (sprintf
                "Claude Code wrote a line prigh could not read: %s"
                (String.prefix text 200)))
      ]
  | Ok json ->
    (match str json "type" with
     | Some "control_request" -> handle_control_request t json
     | Some "control_response" -> handle_control_response t json
     | Some "control_cancel_request" -> handle_cancel t json
     | Some "system" -> handle_system t json
     | Some "stream_event" -> handle_stream_event t json
     | Some "assistant" -> handle_assistant t json
     | Some "user" -> handle_user t json
     | Some "result" -> handle_result t json
     | Some "rate_limit_event" -> handle_rate_limit t json
     | Some _ | None -> [])
;;

let process_exited t ~code ~stderr_tail =
  let failed =
    Hashtbl.to_alist t.ours
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> List.map ~f:(fun (_, command) ->
      Output.Response (Failed { command; message = "Claude Code exited" }))
  in
  Hashtbl.clear t.ours;
  let turn =
    match t.turn with
    | None -> settle t ~output:"Claude Code exited."
    | Some _ ->
      let stop : Adapter_event.Stop.t =
        if t.interrupting
        then Interrupted
        else
          Failed
            { kind = Crashed { stderr_tail }
            ; message =
                (match code with
                 | Some code -> sprintf "Claude Code exited (code %d)" code
                 | None -> "Claude Code exited")
            }
      in
      end_turn t stop
  in
  failed @ turn
;;

(* Answers *)

let take_permission t request_id =
  match
    List.partition_tf t.permissions ~f:(fun p ->
      String.equal p.request_id request_id)
  with
  | [ p ], rest ->
    t.permissions <- rest;
    Ok p
  | _ ->
    Or_error.errorf "Claude Code is not waiting for an answer to %s" request_id
;;

let allow ~request_id ?permissions input =
  success
    ~request_id
    (`Object
        ([ "behavior", `String "allow"; "updatedInput", input ]
         @
         match permissions with
         | Some p -> [ "updatedPermissions", `Array p ]
         | None -> []))
;;

let deny t ~request_id ~message ~stop =
  if stop then t.interrupting <- true;
  success
    ~request_id
    (`Object
        ([ "behavior", `String "deny"; "message", `String message ]
         @ if stop then [ "interrupt", `True ] else []))
;;

(* Claude Code's suggestions, kept to this session. *)
let for_session suggestion =
  match suggestion with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, value) ->
         if String.equal key "destination"
         then key, `String "session"
         else key, value))
  | other -> other
;;

let respond_approval t ~request_id (reply : Adapter_event.Approval_reply.t) =
  let%map.Or_error p = take_permission t request_id in
  match reply with
  | Allow -> allow ~request_id p.input
  | Allow_always ->
    allow
      ~request_id
      ~permissions:(List.map p.suggestions ~f:for_session)
      p.input
  | Deny reason ->
    deny
      t
      ~request_id
      ~message:
        (Option.value reason ~default:"The user declined this tool call.")
      ~stop:false
  | Cancel ->
    deny t ~request_id ~message:"The user stopped the turn." ~stop:true
;;

let answer t ~request_id (reply : User_question.Reply.t) =
  let%map.Or_error p = take_permission t request_id in
  match reply with
  | Answered answers ->
    let questions = questions_of p.input in
    let answers =
      List.zip_with_remainder questions answers
      |> fst
      |> List.map ~f:(fun ((q : User_question.t), picks) ->
        q.question, `String (String.concat ~sep:", " picks))
    in
    let input =
      match p.input with
      | `Object fields ->
        `Object
          (List.filter fields ~f:(fun (k, _) -> not (String.equal k "answers"))
           @ [ "answers", `Object answers ])
      | other -> other
    in
    allow ~request_id input
  | Declined ->
    deny t ~request_id ~message:"The user declined to answer." ~stop:false
  | Timed_out _ ->
    deny t ~request_id ~message:"The user did not answer in time." ~stop:false
  | Unavailable message -> deny t ~request_id ~message ~stop:false
  | Cancelled ->
    deny t ~request_id ~message:"The user stopped the turn." ~stop:true
;;
