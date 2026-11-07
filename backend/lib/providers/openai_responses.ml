open! Core
open! Import

module Endpoint = struct
  type t =
    | Openai of { api_key : string }
    | Codex of
        { access_token : string
        ; account_id : string
        }
    | Custom of
        { provider : Provider_id.t
        ; api_key : string option
        ; headers : (string * string) list
        }

  let default_base_url = function
    | Openai _ -> "https://api.openai.com/v1"
    | Codex _ -> "https://chatgpt.com/backend-api"
    | Custom _ -> "http://localhost/v1"
  ;;

  let url t ~base_url =
    match t with
    | Openai _ | Custom _ -> base_url ^ "/responses"
    | Codex _ -> base_url ^ "/codex/responses"
  ;;

  let headers t =
    match t with
    | Openai { api_key } -> [ "Authorization", "Bearer " ^ api_key ]
    | Codex { access_token; account_id } ->
      [ "Authorization", "Bearer " ^ access_token
      ; "chatgpt-account-id", account_id
      ; "originator", "prigh"
      ; "OpenAI-Beta", "responses=experimental"
      ; "User-Agent", "prigh/" ^ Version.to_string
      ]
    | Custom { api_key; headers; provider = _ } ->
      Option.value_map api_key ~default:[] ~f:(fun key ->
        [ "Authorization", "Bearer " ^ key ])
      @ headers
  ;;

  let provider_id : t -> Provider_id.t = function
    | Openai _ -> Openai
    | Codex _ -> Openai_codex
    | Custom { provider; _ } -> provider
  ;;
end

let effort_of_thinking : Thinking.t -> string option = function
  | Off -> None
  | On None -> Some "medium"
  | On (Some Low) -> Some "low"
  | On (Some High) -> Some "high"
  | On (Some Max) -> Some "xhigh"
;;

let same_provider ~provider (a : Message.Assistant.t) =
  Model.written_by a.model provider
;;

let input_text text =
  `Object [ "type", `String "input_text"; "text", `String text ]
;;

let input_image (image : Image.t) =
  `Object
    [ "type", `String "input_image"
    ; "detail", `String "auto"
    ; ( "image_url"
      , `String (sprintf "data:%s;base64,%s" image.mime_type image.data) )
    ]
;;

(* The text a freeform tool call carries: its [input] argument. *)
let freeform_input arguments =
  match Json.parse arguments with
  | Ok (`Object fields) ->
    (match List.Assoc.find fields ~equal:String.equal "input" with
     | Some (`String input) -> input
     | _ -> arguments)
  | Ok _ | Error _ -> arguments
;;

(* The model's built-in computer tool answers a [computer_call] (a batch of
   actions, stored as one call each: [call_id], [call_id#1], ...) with one
   [computer_call_output]: the screenshot after the batch, acknowledging the
   call's safety checks. Errors follow it as text; a screenshot left out of
   the request ({!Computer_screenshots}) is sent as a blank image. *)
let is_computer name = String.equal name Tool_computer.name

let computer_group id =
  match String.lsplit2 id ~on:'#' with
  | Some (group, _) -> group
  | None -> id
;;

let blank_png =
  "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII="
;;

let computer_call (a : Message.Assistant.t) (call : Content.Tool_call.t) =
  if not (String.equal (computer_group call.id) call.id)
  then None
  else (
    let members =
      List.filter_map a.content ~f:(function
        | Tool_call c
          when is_computer c.name && String.equal (computer_group c.id) call.id
          -> Some c
        | _ -> None)
    in
    let _, checks, item_id = Computer_native.to_openai call.arguments in
    Some
      (`Object
          ([ "type", `String "computer_call" ]
           @ Option.value_map item_id ~default:[] ~f:(fun id ->
             [ "id", `String id ])
           @ [ "call_id", `String call.id
             ; ( "actions"
               , `Array
                   (List.map members ~f:(fun c ->
                      let action, _, _ =
                        Computer_native.to_openai c.arguments
                      in
                      action)) )
             ; "pending_safety_checks", `Array checks
             ; "status", `String "completed"
             ])))
;;

let computer_outputs (messages : Message.t list) (r : Message.Tool_result.t) =
  let group = computer_group r.tool_call_id in
  let results =
    List.filter_map messages ~f:(function
      | Tool_result r'
        when is_computer r'.tool_name
             && String.equal (computer_group r'.tool_call_id) group -> Some r'
      | _ -> None)
  in
  match List.last results with
  | Some last when not (String.equal last.tool_call_id r.tool_call_id) -> []
  | Some _ | None ->
    let checks =
      List.find_map messages ~f:(function
        | Assistant a ->
          List.find_map a.content ~f:(function
            | Tool_call c when String.equal c.id group ->
              let _, checks, _ = Computer_native.to_openai c.arguments in
              Some checks
            | _ -> None)
        | User _ | Tool_result _ -> None)
      |> Option.value ~default:[]
    in
    let image_url =
      match
        List.find_map (List.rev results) ~f:(fun r -> List.last r.images)
      with
      | Some image -> sprintf "data:%s;base64,%s" image.mime_type image.data
      | None -> "data:image/png;base64," ^ blank_png
    in
    let errors = List.filter results ~f:(fun r -> r.is_error) in
    [ `Object
        ([ "type", `String "computer_call_output"
         ; "call_id", `String group
         ; ( "output"
           , `Object
               [ "type", `String "computer_screenshot"
               ; "image_url", `String image_url
               ; "detail", `String "original"
               ] )
         ]
         @
         if List.is_empty checks
         then []
         else [ "acknowledged_safety_checks", `Array checks ])
    ]
    @
    if List.is_empty errors
    then []
    else
      [ `Object
          [ "type", `String "message"
          ; "role", `String "user"
          ; ( "content"
            , `Array
                [ input_text
                    (String.concat
                       ~sep:"\n"
                       (List.map errors ~f:(fun r -> "computer: " ^ r.text)))
                ] )
          ]
      ]
;;

let input_items ~provider ~freeform ~computer (messages : Message.t list) =
  List.concat_mapi messages ~f:(fun message_index m ->
    match m with
    | Tool_result r when computer && is_computer r.tool_name ->
      computer_outputs messages r
    | User { text; images; origin = _ } ->
      [ `Object
          [ "type", `String "message"
          ; "role", `String "user"
          ; ( "content"
            , `Array
                ((if String.is_empty text && not (List.is_empty images)
                  then []
                  else [ input_text text ])
                 @ List.map images ~f:input_image) )
          ]
      ]
    | Tool_result r when freeform r.tool_name ->
      [ `Object
          [ "type", `String "custom_tool_call_output"
          ; "call_id", `String r.tool_call_id
          ; "output", `String r.text
          ]
      ]
    | Tool_result r ->
      [ `Object
          [ "type", `String "function_call_output"
          ; "call_id", `String r.tool_call_id
          ; ( "output"
            , match r.images with
              | [] -> `String r.text
              | images ->
                `Array (input_text r.text :: List.map images ~f:input_image) )
          ]
      ]
    | Assistant a ->
      let replay_reasoning = same_provider ~provider a in
      let text_index = ref 0 in
      List.filter_map a.content ~f:(function
        | Content.Text "" -> None
        | Text text ->
          let id =
            if !text_index = 0
            then sprintf "msg_prigh_%d" message_index
            else sprintf "msg_prigh_%d_%d" message_index !text_index
          in
          incr text_index;
          Some
            (`Object
                [ "type", `String "message"
                ; "role", `String "assistant"
                ; "id", `String id
                ; "status", `String "completed"
                ; ( "content"
                  , `Array
                      [ `Object
                          [ "type", `String "output_text"
                          ; "text", `String text
                          ; "annotations", `Array []
                          ]
                      ] )
                ])
        | Thinking { signature = Some item; _ } when replay_reasoning ->
          Result.ok (Json.parse item)
        | Thinking _ -> None
        | Server_tool { raw = Some item; _ } when replay_reasoning ->
          Result.ok (Json.parse item)
        | Server_tool tool ->
          Some
            (`Object
                [ "type", `String "message"
                ; "role", `String "assistant"
                ; ( "id"
                  , `String (sprintf "msg_prigh_%d_%s" message_index tool.id) )
                ; "status", `String "completed"
                ; ( "content"
                  , `Array
                      [ `Object
                          [ "type", `String "output_text"
                          ; "text", `String (Content.Server_tool.as_text tool)
                          ; "annotations", `Array []
                          ]
                      ] )
                ])
        | Tool_call call when computer && is_computer call.name ->
          computer_call a call
        | Tool_call call when is_computer call.name ->
          Some
            (`Object
                [ "type", `String "function_call"
                ; "call_id", `String call.id
                ; "name", `String call.name
                ; "arguments", `String (Computer_native.generic call.arguments)
                ])
        | Tool_call call when freeform call.name ->
          Some
            (`Object
                [ "type", `String "custom_tool_call"
                ; "call_id", `String call.id
                ; "name", `String call.name
                ; "input", `String (freeform_input call.arguments)
                ])
        | Tool_call call ->
          Some
            (`Object
                [ "type", `String "function_call"
                ; "call_id", `String call.id
                ; "name", `String call.name
                ; "arguments", `String call.arguments
                ])))
;;

let wire_tool ~computer (t : Tool_spec.t) =
  if Tool_web_search.is_spec t
  then `Object [ "type", `String "web_search" ]
  else if computer && Option.is_some t.native
  then `Object [ "type", `String "computer" ]
  else (
    match t.freeform with
    | Some grammar ->
      `Object
        [ "type", `String "custom"
        ; "name", `String t.name
        ; ( "description"
          , `String
              (t.description
               ^ "\n\n\
                  This is a FREEFORM tool: send the input as plain text, not \
                  wrapped in JSON.") )
        ; ( "format"
          , `Object
              [ "type", `String "grammar"
              ; "syntax", `String "lark"
              ; "definition", `String grammar
              ] )
        ]
    | None ->
      `Object
        [ "type", `String "function"
        ; "name", `String t.name
        ; "description", `String t.description
        ; "parameters", t.parameters
        ; "strict", `False
        ])
;;

(* Whether [computer] is offered as the model's built-in computer tool. *)
let native_computer ~(endpoint : Endpoint.t) (r : Provider.Request.t) =
  match endpoint, r.model.native_computer with
  | Openai _, Some Openai ->
    List.exists r.tools ~f:(fun t -> Option.is_some t.native)
  | (Openai _ | Codex _ | Custom _), _ -> false
;;

let request_body ~(endpoint : Endpoint.t) (r : Provider.Request.t) : Json.t =
  let r = Provider.Request.prepare r in
  let computer = native_computer ~endpoint r in
  let provider = Endpoint.provider_id endpoint in
  let freeform name =
    List.exists r.tools ~f:(fun (t : Tool_spec.t) ->
      String.equal t.name name && Option.is_some t.freeform)
  in
  let reasoning =
    if not r.model.supports_thinking
    then []
    else
      Option.value_map
        (effort_of_thinking r.thinking)
        ~default:[]
        ~f:(fun effort ->
          [ ( "reasoning"
            , `Object [ "effort", `String effort; "summary", `String "auto" ] )
          ])
  in
  `Object
    (List.concat
       [ [ "model", `String r.model.id
         ; "stream", `True
         ; "store", `False
         ; ( "instructions"
           , `String
               (Option.value r.system ~default:"You are a helpful assistant.") )
         ; ( "input"
           , `Array (input_items ~provider ~freeform ~computer r.messages) )
         ; ( "include"
           , `Array
               (`String "reasoning.encrypted_content"
                ::
                (if List.exists r.tools ~f:Tool_web_search.is_spec
                 then [ `String "web_search_call.action.sources" ]
                 else [])) )
         ; "tool_choice", `String "auto"
         ; "parallel_tool_calls", `True
         ]
       ; (if List.is_empty r.tools
          then []
          else [ "tools", `Array (List.map r.tools ~f:(wire_tool ~computer)) ])
       ; reasoning
       ; (* Custom servers may reject fields they do not know. *)
         (match endpoint, r.cache_key with
          | (Openai _ | Codex _), Some key ->
            [ "prompt_cache_key", `String key ]
          | (Openai _ | Codex _ | Custom _), _ -> [])
       ; (match endpoint with
          | Codex _ -> [ "text", `Object [ "verbosity", `String "low" ] ]
          | Openai _ | Custom _ ->
            Option.value_map r.max_tokens ~default:[] ~f:(fun n ->
              [ "max_output_tokens", `Number (Int.to_string n) ]))
       ])
;;

module Stream_state = struct
  type t =
    { mutable tools : (int * int) list (* output_index -> tool index *)
    ; mutable tool_arguments_seen : int list
    ; mutable tool_count : int
    ; mutable saw_tool_call : bool
    ; mutable incomplete_reason : string option
    ; mutable finished : bool
    ; mutable usage : Usage.t
    ; mutable error : string option
    }

  let create () =
    { tools = []
    ; tool_arguments_seen = []
    ; tool_count = 0
    ; saw_tool_call = false
    ; incomplete_reason = None
    ; finished = false
    ; usage = Usage.zero
    ; error = None
    }
  ;;
end

let member = Sse_request.member_string
let int_member name json = Option.bind (Json.member name json) ~f:Json.int

let parse_usage json =
  let get name = Option.value (int_member name json) ~default:0 in
  let cached =
    Option.bind
      (Json.member "input_tokens_details" json)
      ~f:(int_member "cached_tokens")
    |> Option.value ~default:0
  in
  { Usage.input = get "input_tokens"
  ; output = get "output_tokens"
  ; cache_read = cached
  }
;;

(* A custom tool call's free text streams as the [input] string of JSON
   arguments, escaped piece by piece. *)
let json_string_fragment text =
  let quoted = Json.to_string (`String text) in
  String.sub quoted ~pos:1 ~len:(String.length quoted - 2)
;;

let freeform_open = {|{"input":"|}
let freeform_close = {|"}|}

(* A [web_search_call] item's [action]: [search] (with [query] and, when
   asked for, [sources]), [open_page] or [find_in_page]. *)
let web_search_use item : Assistant_event.t option =
  match Json.member "action" item, member "id" item with
  | Some (`Object (_ :: _ as fields)), Some id ->
    let input =
      `Object
        (List.filter fields ~f:(fun (key, _) ->
           not (String.equal key "sources")))
    in
    Some
      (Server_tool_use
         { id; name = Tool_web_search.name; input = Json.to_string input })
  | _ -> None
;;

let web_search_result item : Assistant_event.t =
  let id = Option.value (member "id" item) ~default:"" in
  let action = Json.member "action" item in
  let status = Option.value (member "status" item) ~default:"completed" in
  let output, is_error =
    match status with
    | "failed" | "incomplete" -> sprintf "search %s" status, true
    | _ ->
      (match Option.bind action ~f:(member "type") with
       | Some "open_page" ->
         ( sprintf
             "opened %s"
             (Option.value
                (Option.bind action ~f:(member "url"))
                ~default:"a page")
         , false )
       | Some "find_in_page" | Some "find" ->
         ( sprintf
             "searched %s for %S"
             (Option.value
                (Option.bind action ~f:(member "url"))
                ~default:"a page")
             (Option.value
                (Option.bind action ~f:(member "pattern"))
                ~default:"")
         , false )
       | _ ->
         (match Option.bind action ~f:(Json.member "sources") with
          | Some (`Array (_ :: _ as sources)) ->
            ( Tool_web_search.results_text
                (List.filter_map sources ~f:(fun source ->
                   Option.map (member "url" source) ~f:(fun url ->
                     Option.value (member "title" source) ~default:"", url, None)))
            , false )
          | _ -> "searched", false))
  in
  Server_tool_result { id; output; is_error; raw = Some (Json.to_string item) }
;;

let parse_event (state : Stream_state.t) (event : Sse.Event.t)
  : Assistant_event.t list
  =
  match Json.parse event.data with
  | Error e ->
    state.error
    <- Some (sprintf "bad JSON in stream: %s" (Error.to_string_hum e));
    []
  | Ok json ->
    let output_index () =
      Option.value (int_member "output_index" json) ~default:0
    in
    let delta () = Option.value (member "delta" json) ~default:"" in
    let item () = Json.member "item" json in
    let item_type () = Option.bind (item ()) ~f:(member "type") in
    (match Option.value (member "type" json) ~default:"" with
     | "response.output_item.added" ->
       (match item_type (), item () with
        | Some "web_search_call", Some item ->
          Option.to_list (web_search_use item)
        | Some (("function_call" | "custom_tool_call") as type_), Some item ->
          let tool_index = state.tool_count in
          state.tool_count <- tool_index + 1;
          state.tools <- (output_index (), tool_index) :: state.tools;
          state.saw_tool_call <- true;
          let start =
            Assistant_event.Tool_call_start
              { index = tool_index
              ; id = Option.value (member "call_id" item) ~default:""
              ; name = Option.value (member "name" item) ~default:""
              }
          in
          if String.equal type_ "custom_tool_call"
          then
            [ start
            ; Tool_call_delta { index = tool_index; arguments = freeform_open }
            ]
          else [ start ]
        | _ -> [])
     | "response.output_text.delta" | "response.refusal.delta" ->
       let d = delta () in
       if String.is_empty d then [] else [ Text_delta d ]
     | "response.reasoning_summary_text.delta" | "response.reasoning_text.delta"
       ->
       let d = delta () in
       if String.is_empty d then [] else [ Thinking_delta d ]
     | "response.reasoning_summary_part.added" ->
       if Option.value (int_member "summary_index" json) ~default:0 > 0
       then [ Thinking_delta "\n\n" ]
       else []
     | ( "response.function_call_arguments.delta"
       | "response.custom_tool_call_input.delta" ) as type_ ->
       (match
          List.Assoc.find state.tools ~equal:Int.equal (output_index ())
        with
        | Some index ->
          let d = delta () in
          if String.is_empty d
          then []
          else (
            state.tool_arguments_seen <- index :: state.tool_arguments_seen;
            let arguments =
              if String.equal type_ "response.custom_tool_call_input.delta"
              then json_string_fragment d
              else d
            in
            [ Tool_call_delta { index; arguments } ])
        | None -> [])
     | "response.output_item.done" ->
       (match item_type (), item () with
        | Some "web_search_call", Some item ->
          Option.to_list (web_search_use item) @ [ web_search_result item ]
        | Some "reasoning", Some item ->
          (match Json.member "encrypted_content" item with
           | Some (`String _) -> [ Thinking_signature (Json.to_string item) ]
           | _ -> [])
        | Some "function_call", Some item ->
          (match
             List.Assoc.find state.tools ~equal:Int.equal (output_index ())
           with
           | Some index
             when not
                    (List.mem state.tool_arguments_seen index ~equal:Int.equal)
             ->
             (match member "arguments" item with
              | Some arguments when not (String.is_empty arguments) ->
                [ Tool_call_delta { index; arguments } ]
              | _ -> [])
           | _ -> [])
        | Some "computer_call", Some item ->
          state.saw_tool_call <- true;
          let call_id = Option.value (member "call_id" item) ~default:"" in
          let actions =
            match Json.member "actions" item, Json.member "action" item with
            | Some (`Array actions), _ -> actions
            | _, Some action -> [ action ]
            | _ -> [ `Object [ "type", `String "screenshot" ] ]
          in
          let safety_checks =
            match Json.member "pending_safety_checks" item with
            | Some (`Array checks) -> checks
            | _ -> []
          in
          List.concat_mapi actions ~f:(fun k action ->
            let index = state.tool_count in
            state.tool_count <- index + 1;
            let arguments =
              if k = 0
              then
                Computer_native.of_openai
                  ?item_id:(member "id" item)
                  ~safety_checks
                  action
              else Computer_native.of_openai action
            in
            [ Assistant_event.Tool_call_start
                { index
                ; id = (if k = 0 then call_id else sprintf "%s#%d" call_id k)
                ; name = Tool_computer.name
                }
            ; Tool_call_delta { index; arguments }
            ])
        | Some "custom_tool_call", Some item ->
          (match
             List.Assoc.find state.tools ~equal:Int.equal (output_index ())
           with
           | Some index ->
             let rest =
               if List.mem state.tool_arguments_seen index ~equal:Int.equal
               then []
               else
                 Option.to_list (member "input" item)
                 |> List.map ~f:json_string_fragment
             in
             List.map (rest @ [ freeform_close ]) ~f:(fun arguments ->
               Assistant_event.Tool_call_delta { index; arguments })
           | None -> [])
        | _ -> [])
     | ("response.completed" | "response.incomplete") as type_ ->
       (* The response's status says how it ended, as pi reads it: a
          completed one may still carry [incomplete_details: null]. *)
       state.finished <- true;
       let response = Json.member "response" json in
       Option.iter
         (Option.bind response ~f:(Json.member "usage"))
         ~f:(fun u -> state.usage <- parse_usage u);
       let status =
         match Option.bind response ~f:(member "status") with
         | Some status -> status
         | None -> String.chop_prefix_exn type_ ~prefix:"response."
       in
       (match status with
        | "incomplete" ->
          state.incomplete_reason
          <- Some
               (Option.bind response ~f:(Json.member "incomplete_details")
                |> Option.bind ~f:(member "reason")
                |> Option.value ~default:"no reason given")
        | "failed" | "cancelled" ->
          state.error <- Some (sprintf "the response was %s" status)
        | _ -> ());
       []
     | "response.failed" ->
       state.finished <- true;
       let message =
         Option.bind (Json.member "response" json) ~f:(Json.member "error")
         |> Option.bind ~f:(member "message")
         |> Option.value ~default:event.data
       in
       state.error <- Some message;
       []
     | "error" ->
       state.finished <- true;
       let message =
         Option.first_some
           (member "message" json)
           (Option.bind (Json.member "error" json) ~f:(member "message"))
         |> Option.value ~default:event.data
       in
       state.error <- Some message;
       []
     | _ -> [])
;;

let stop_reason (state : Stream_state.t) : Stop_reason.t =
  match state.error, state.incomplete_reason with
  | Some e, _ -> Error e
  | None, Some "max_output_tokens" -> Length
  | None, Some reason ->
    Error (sprintf "the response was cut short (%s)" reason)
  | None, None when not state.finished ->
    Error "the stream ended before the response finished"
  | None, None -> if state.saw_tool_call then Tool_use else End_turn
;;

let stream
      ~env
      ~base_url
      ~timeout
      ~(endpoint : Endpoint.t)
      (request : Provider.Request.t)
      ~cancel
      ~on_event
  =
  let builder = Assistant_builder.create ~model:(Model.key request.model) in
  let state = Stream_state.create () in
  let outcome =
    Sse_request.run
      ~env
      ?timeout
      ~cancel
      ~url:(Endpoint.url endpoint ~base_url)
      ~headers:(Endpoint.headers endpoint)
      ~body:
        (Trace.span ~detailed:true "request_build" (fun () ->
           Json.to_string (request_body ~endpoint request)))
      ~on_event:(fun event ->
        List.iter (parse_event state event) ~f:(fun e ->
          Assistant_builder.apply builder e;
          on_event e))
      ()
  in
  let stop_reason : Stop_reason.t =
    match outcome with
    | Aborted -> Aborted
    | Failed e -> Error e
    | Completed -> stop_reason state
  in
  Assistant_builder.finish builder ~stop_reason ~usage:state.usage
;;

let create ~env ?base_url ?timeout ~endpoint () =
  let base_url =
    Option.value base_url ~default:(Endpoint.default_base_url endpoint)
  in
  { Provider.name = Provider_id.to_string (Endpoint.provider_id endpoint)
  ; stream = stream ~env ~base_url ~timeout ~endpoint
  }
;;

module For_testing = struct
  let request_body = request_body
  let headers = Endpoint.headers

  let parse_events events =
    let state = Stream_state.create () in
    let events =
      List.concat_map events ~f:(fun data ->
        parse_event state { Sse.Event.event = None; data; id = None })
    in
    events, stop_reason state, state.usage
  ;;
end
