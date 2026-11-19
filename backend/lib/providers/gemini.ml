open! Core
open! Import

let default_base_url = "https://generativelanguage.googleapis.com/v1beta"
let local_id_prefix = "gemini_local_"
let signature_prefix = "gemini:tool:"
let member_string = Sse_request.member_string

let member_array key json =
  match Json.member key json with
  | Some (`Array parts) -> parts
  | _ -> []
;;

let string_field key value = key, `String value
let text_part text : Json.t = `Object [ string_field "text" text ]

let image_data ?name (image : Image.t) : Json.t =
  `Object
    ([ string_field "mimeType" image.mime_type; string_field "data" image.data ]
     @ Option.value_map name ~default:[] ~f:(fun n ->
       [ string_field "displayName" n ]))
;;

let inline_image ?name image : Json.t =
  `Object [ "inlineData", image_data ?name image ]
;;

let content role parts : Json.t =
  `Object [ string_field "role" role; "parts", `Array parts ]
;;

let arguments (call : Content.Tool_call.t) =
  Result.ok (Content.Tool_call.parse_arguments call)
  |> Option.value ~default:(`Object [])
;;

let tool_signatures (a : Message.Assistant.t) =
  let rec loop signatures = function
    | Content.Tool_call call
      :: Content.Thinking { signature = Some signature; _ }
      :: rest ->
      let signatures =
        match String.chop_prefix signature ~prefix:signature_prefix with
        | Some signature -> Map.set signatures ~key:call.id ~data:signature
        | None -> signatures
      in
      loop signatures rest
    | _ :: rest -> loop signatures rest
    | [] -> signatures
  in
  loop String.Map.empty a.content
;;

let assistant_parts ~model ~wire_ids (a : Message.Assistant.t) =
  let own = Model.written_by a.model Gemini in
  let same_model = String.equal a.model (Model.key model) in
  let signatures = if same_model then tool_signatures a else String.Map.empty in
  let first_call = ref true in
  List.filter_map a.content ~f:(function
    | Text s -> if String.is_empty s then None else Some (text_part s)
    | Server_tool s -> Some (text_part (Content.Server_tool.as_text s))
    | Thinking _ -> None
    | Tool_call c ->
      let wire_id =
        if own && not (String.is_prefix c.id ~prefix:local_id_prefix)
        then (
          Hash_set.add wire_ids c.id;
          [ string_field "id" c.id ])
        else []
      in
      (* Parallel calls need the first call's signature, not a copy on every
         call. Google documents this sentinel for turns imported from another
         model (and tools inserted by a client). *)
      let signature =
        match Map.find signatures c.id with
        | Some s -> [ string_field "thoughtSignature" s ]
        | None when !first_call ->
          [ string_field "thoughtSignature" "skip_thought_signature_validator" ]
        | None -> []
      in
      first_call := false;
      Some
        (`Object
            (( "functionCall"
             , `Object
                 ([ string_field "name" c.name; "args", arguments c ] @ wire_id)
             )
             :: signature)))
;;

let response_part ~wire_ids (r : Message.Tool_result.t) : Json.t =
  let image_names =
    List.mapi r.images ~f:(fun i image ->
      sprintf "tool-image-%d" (i + 1), image)
  in
  let images =
    match image_names with
    | [] -> []
    | names ->
      [ ( "images"
        , `Array
            (List.map names ~f:(fun (n, _) -> `Object [ string_field "$ref" n ]))
        )
      ]
  in
  `Object
    [ ( "functionResponse"
      , `Object
          ([ string_field "name" r.tool_name
           ; ( "response"
             , `Object
                 (string_field (if r.is_error then "error" else "output") r.text
                  :: images) )
           ]
           @ (if Hash_set.mem wire_ids r.tool_call_id
              then [ string_field "id" r.tool_call_id ]
              else [])
           @
           if List.is_empty image_names
           then []
           else
             [ ( "parts"
               , `Array
                   (List.map image_names ~f:(fun (name, image) ->
                      inline_image ~name image)) )
             ]) )
    ]
;;

let wire_messages ~model messages =
  let wire_ids = String.Hash_set.create () in
  let flush_tool_results acc pending =
    if List.is_empty pending
    then acc
    else content "user" (List.rev pending) :: acc
  in
  let rec loop acc pending = function
    | [] -> List.rev (flush_tool_results acc pending)
    | Message.Tool_result r :: rest ->
      loop acc (response_part ~wire_ids r :: pending) rest
    | User u :: rest ->
      let acc = flush_tool_results acc pending in
      let parts =
        (if String.is_empty u.text && not (List.is_empty u.images)
         then []
         else [ text_part u.text ])
        @ List.map u.images ~f:(inline_image ?name:None)
      in
      loop (content "user" parts :: acc) [] rest
    | Assistant a :: rest ->
      let acc = flush_tool_results acc pending in
      let parts = assistant_parts ~model ~wire_ids a in
      let acc =
        if List.is_empty parts then acc else content "model" parts :: acc
      in
      loop acc [] rest
  in
  loop [] [] messages
;;

let wire_tool (t : Tool_spec.t) : Json.t =
  `Object
    [ string_field "name" t.name
    ; string_field "description" t.description
    ; "parametersJsonSchema", t.parameters
    ]
;;

let request_body (r : Provider.Request.t) : Json.t =
  let r = Provider.Request.prepare r in
  let thinking =
    match r.thinking with
    | On (Some level) ->
      [ string_field
          "thinkingLevel"
          (String.uppercase (Thinking.Level.to_string level))
      ]
    | On None | Off -> []
  in
  `Object
    ([ "contents", `Array (wire_messages ~model:r.model r.messages)
     ; ( "generationConfig"
       , `Object
           (("thinkingConfig", `Object (("includeThoughts", `True) :: thinking))
            :: Option.value_map r.max_tokens ~default:[] ~f:(fun n ->
              [ "maxOutputTokens", `Number (Int.to_string n) ])) )
     ]
     @ Option.value_map r.system ~default:[] ~f:(fun s ->
       [ "systemInstruction", `Object [ "parts", `Array [ text_part s ] ] ])
     @
     if List.is_empty r.tools
     then []
     else
       [ ( "tools"
         , `Array
             [ `Object
                 [ ( "functionDeclarations"
                   , `Array (List.map r.tools ~f:wire_tool) )
                 ]
             ] )
       ])
;;

let usage json =
  let get name =
    Option.bind (Json.member name json) ~f:Json.int |> Option.value ~default:0
  in
  { Usage.input = get "promptTokenCount"
  ; output = get "candidatesTokenCount" + get "thoughtsTokenCount"
  ; cache_read = get "cachedContentTokenCount"
  }
;;

let stop_reason ~saw_tool_call : string -> Stop_reason.t = function
  | "STOP" -> if saw_tool_call then Tool_use else End_turn
  | "MAX_TOKENS" -> Length
  | reason -> Error ("Gemini stopped the response: " ^ reason)
;;

let response ~fresh_id model jsons ~on_event =
  let builder = Assistant_builder.create ~model in
  let count = ref 0 in
  let final_reason = ref None in
  let failure = ref None in
  let latest_usage = ref Usage.zero in
  let emit event =
    Assistant_builder.apply builder event;
    on_event event
  in
  let part json =
    match Json.member "functionCall" json with
    | Some call ->
      (match member_string "name" call with
       | None ->
         failure := Some "Gemini returned a function call without a name"
       | Some name ->
         let index = !count in
         Int.incr count;
         let id =
           Option.value_or_thunk (member_string "id" call) ~default:fresh_id
         in
         let arguments =
           Json.member "args" call
           |> Option.value ~default:(`Object [])
           |> Json.to_string
         in
         emit (Tool_call_start { index; id; name });
         emit (Tool_call_delta { index; arguments });
         Option.iter
           (member_string "thoughtSignature" json)
           ~f:(fun signature ->
             (* Put an opaque block after the call, preserving its association
              in the session without modifying tool ids or exposing it to UI. *)
             emit (Thinking_signature (signature_prefix ^ signature))))
    | None ->
      Option.iter (member_string "text" json) ~f:(fun text ->
        if not (String.is_empty text)
        then
          emit
            (match Json.member "thought" json with
             | Some `True -> Thinking_delta text
             | _ -> Text_delta text))
  in
  let accept (json : Json.t) =
    Option.iter (Json.member "error" json) ~f:(fun error ->
      failure
      := Some
           (Option.value
              (member_string "message" error)
              ~default:(Json.to_string error)));
    Option.iter (Json.member "promptFeedback" json) ~f:(fun feedback ->
      Option.iter (member_string "blockReason" feedback) ~f:(fun reason ->
        failure := Some ("Gemini blocked the prompt: " ^ reason)));
    Option.iter
      (List.hd (member_array "candidates" json))
      ~f:(fun candidate ->
        Option.iter (Json.member "content" candidate) ~f:(fun content ->
          List.iter (member_array "parts" content) ~f:part);
        Option.iter (member_string "finishReason" candidate) ~f:(fun reason ->
          final_reason := Some reason));
    Option.iter (Json.member "usageMetadata" json) ~f:(fun json ->
      latest_usage := usage json)
  in
  let outcome : Sse_request.Outcome.t =
    jsons ~accept ~fail:(fun error -> failure := Some error)
  in
  let stop_reason : Stop_reason.t =
    match outcome with
    | Aborted -> Aborted
    | Failed e -> Error e
    | Completed ->
      (match !failure, !final_reason with
       | Some e, _ -> Error e
       | None, Some reason -> stop_reason ~saw_tool_call:(!count > 0) reason
       | None, None -> Error "Gemini stream ended without a finish reason")
  in
  Assistant_builder.finish builder ~stop_reason ~usage:!latest_usage
;;

let stream
      ~env
      ~base_url
      ~timeout
      ~headers
      (request : Provider.Request.t)
      ~cancel
      ~on_event
  =
  let model = Model.key request.model in
  let fresh_id () = local_id_prefix ^ Pkce.random_hex 12 in
  response ~fresh_id model ~on_event (fun ~accept ~fail ->
    let url =
      base_url
      ^ "/models/"
      ^ request.model.id
      ^ ":streamGenerateContent?alt=sse"
    in
    Sse_request.run
      ~env
      ?timeout
      ~cancel
      ~url
      ~headers
      ~body:
        (Trace.span ~detailed:true "request_build" (fun () ->
           Json.to_string (request_body request)))
      ~on_event:(fun event ->
        match Json.parse event.data with
        | Ok json -> accept json
        | Error e -> fail ("bad JSON in Gemini stream: " ^ Error.to_string_hum e))
      ())
;;

let create ~env ?(base_url = default_base_url) ?timeout ~headers () =
  { Provider.name = "gemini"; stream = stream ~env ~base_url ~timeout ~headers }
;;

module For_testing = struct
  let request_body = request_body

  let response ~model jsons =
    let n = ref 0 in
    let fresh_id () =
      Int.incr n;
      local_id_prefix ^ Int.to_string !n
    in
    response
      ~fresh_id
      (Model.key model)
      ~on_event:ignore
      (fun ~accept ~fail:_ ->
         List.iter jsons ~f:accept;
         Sse_request.Outcome.Completed)
  ;;
end
