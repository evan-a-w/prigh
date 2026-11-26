open! Core
open! Import

module Thinking_param = struct
  type t =
    | Deepseek
    | Reasoning_effort
  [@@deriving sexp_of]
end

module Quirks = struct
  type t =
    { thinking_param : Thinking_param.t
    ; replay_reasoning : bool
    ; gemini : bool
    }
  [@@deriving sexp_of]

  let deepseek =
    { thinking_param = Deepseek; replay_reasoning = true; gemini = false }
  ;;

  let generic =
    { thinking_param = Reasoning_effort
    ; replay_reasoning = false
    ; gemini = false
    }
  ;;

  let gemini = { generic with gemini = true }

  let of_url url =
    match Uri.host (Uri.of_string url) with
    | Some host
      when String.equal host "googleapis.com"
           || String.is_suffix host ~suffix:".googleapis.com" -> gemini
    | Some _ | None -> generic
  ;;
end

(* Gemini's documented signature for function calls it did not write
   (another model's history), which skips the signature check. *)
let skip_signature = "skip_thought_signature_validator"

let google_signature (extra : Json.t option) =
  Option.bind extra ~f:(Json.member "google")
  |> Option.bind ~f:(Json.member "thought_signature")
  |> Option.is_some
;;

let with_skip_signature (extra : Json.t option) : Json.t =
  let signature = "thought_signature", `String skip_signature in
  let others = function
    | Some (`Object fields) ->
      List.filter fields ~f:(fun (k, _) -> not (String.equal k "google"))
    | Some _ | None -> []
  in
  let google =
    match Option.bind extra ~f:(Json.member "google") with
    | Some (`Object fields) -> `Object (fields @ [ signature ])
    | Some _ | None -> `Object [ signature ]
  in
  `Object (("google", google) :: others extra)
;;

(* The calls' [extra_content], sent back only to the provider (for Gemini,
   the model) that wrote it. Gemini rejects a step whose calls carry no
   thought signature, so a step from elsewhere gets the documented dummy on
   its first call (Gemini signs only the first of parallel calls). *)
let tool_call_extras
      ~(quirks : Quirks.t)
      ~replay
      (calls : Content.Tool_call.t list)
  =
  let extras =
    List.map calls ~f:(fun c ->
      if not replay
      then None
      else
        Option.bind c.extra ~f:(fun s ->
          match Json.parse s with
          | Ok (`Object _ as json) -> Some json
          | Ok _ | Error _ -> None))
  in
  if quirks.gemini && not (List.exists extras ~f:google_signature)
  then (
    match extras with
    | first :: rest -> Some (with_skip_signature first) :: rest
    | [] -> [])
  else extras
;;

let wire_tool_call (c : Content.Tool_call.t) (extra : Json.t option) : Json.t =
  `Object
    ([ "id", `String c.id
     ; "type", `String "function"
     ; ( "function"
       , `Object [ "name", `String c.name; "arguments", `String c.arguments ] )
     ]
     @ Option.value_map extra ~default:[] ~f:(fun extra ->
       [ "extra_content", extra ]))
;;

let image_part (image : Image.t) : Json.t =
  `Object
    [ "type", `String "image_url"
    ; ( "image_url"
      , `Object
          [ ( "url"
            , `String (sprintf "data:%s;base64,%s" image.mime_type image.data) )
          ] )
    ]
;;

let text_part text : Json.t =
  `Object [ "type", `String "text"; "text", `String text ]
;;

let user_message ~text (images : Image.t list) : Json.t =
  let content =
    match images with
    | [] -> `String text
    | images ->
      `Array
        ((if String.is_empty text then [] else [ text_part text ])
         @ List.map images ~f:image_part)
  in
  `Object [ "role", `String "user"; "content", content ]
;;

(* This API has no server tools: another provider's searches become text. *)
let text_with_server_tools (a : Message.Assistant.t) =
  List.filter_map a.content ~f:(function
    | Content.Text s -> Some s
    | Server_tool tool -> Some ("\n" ^ Content.Server_tool.as_text tool ^ "\n")
    | Thinking _ | Tool_call _ -> None)
  |> String.concat
;;

let assistant_message
      ~(quirks : Quirks.t)
      ~(model : Model.t)
      (a : Message.Assistant.t)
  : Json.t
  =
  let thinking = Message.Assistant.thinking a in
  let tool_calls = Message.Assistant.tool_calls a in
  let replay =
    if quirks.gemini
    then String.equal a.model (Model.key model)
    else Model.written_by a.model model.provider
  in
  let extras = tool_call_extras ~quirks ~replay tool_calls in
  `Object
    (List.concat
       [ [ "role", `String "assistant"
         ; "content", `String (text_with_server_tools a)
         ]
       ; (if (not quirks.replay_reasoning) || String.is_empty thinking
          then []
          else [ "reasoning_content", `String thinking ])
       ; (if List.is_empty tool_calls
          then []
          else
            [ ( "tool_calls"
              , `Array (List.map2_exn tool_calls extras ~f:wire_tool_call) )
            ])
       ])
;;

let image_count n = if n = 1 then "1 image" else sprintf "%d images" n

(* A tool message's content is text only, so a result's images follow the
   run of tool messages (which must stay contiguous after the assistant's
   tool calls) as one user message. *)
let wire_messages ~quirks ~model (messages : Message.t list) : Json.t list =
  let tool_images = ref [] in
  let flush () =
    match List.rev !tool_images with
    | [] -> []
    | pending ->
      tool_images := [];
      [ `Object
          [ "role", `String "user"
          ; ( "content"
            , `Array
                (List.concat_map
                   pending
                   ~f:(fun ((r : Message.Tool_result.t), images) ->
                     text_part
                       (sprintf
                          "[%s from the %s result %s]"
                          (image_count (List.length images))
                          r.tool_name
                          r.tool_call_id)
                     :: List.map images ~f:image_part)) )
          ]
      ]
  in
  let wire (m : Message.t) =
    match m with
    | Tool_result ({ tool_call_id; text; images; _ } as r) ->
      let text =
        match images with
        | [] -> text
        | images ->
          tool_images := (r, images) :: !tool_images;
          sprintf
            "%s%s[%s attached in the next message]"
            text
            (if String.is_empty text then "" else "\n")
            (image_count (List.length images))
      in
      [ `Object
          [ "role", `String "tool"
          ; "tool_call_id", `String tool_call_id
          ; "content", `String text
          ]
      ]
    | User { text; images; origin = _ } ->
      flush () @ [ user_message ~text images ]
    | Assistant a -> flush () @ [ assistant_message ~quirks ~model a ]
  in
  let wired = List.concat_map messages ~f:wire in
  wired @ flush ()
;;

let wire_tool (t : Tool_spec.t) : Json.t =
  `Object
    [ "type", `String "function"
    ; ( "function"
      , `Object
          [ "name", `String t.name
          ; "description", `String t.description
          ; "parameters", t.parameters
          ] )
    ]
;;

let thinking_fields ~(quirks : Quirks.t) (r : Provider.Request.t) =
  if not (Model.supports_thinking r.model)
  then []
  else (
    let effort level =
      Option.value_map level ~default:[] ~f:(fun level ->
        [ "reasoning_effort", `String (Thinking.Level.to_string level) ])
    in
    match quirks.thinking_param, r.thinking with
    | Deepseek, Off -> [ "thinking", `Object [ "type", `String "disabled" ] ]
    | Deepseek, On level ->
      [ "thinking", `Object [ "type", `String "enabled" ] ] @ effort level
    | Reasoning_effort, Off ->
      (match r.model.thinking_style with
       | Effort { off_is_none = true } -> [ "reasoning_effort", `String "none" ]
       | Effort { off_is_none = false }
       | Budget _ | Adaptive | Adaptive_between_tools -> [])
    | Reasoning_effort, On level -> effort level)
;;

let request_body ~quirks (r : Provider.Request.t) : Json.t =
  let r = Provider.Request.prepare r in
  let system =
    match r.system with
    | None -> []
    | Some s -> [ `Object [ "role", `String "system"; "content", `String s ] ]
  in
  `Object
    (List.concat
       [ [ "model", `String r.model.id
         ; ( "messages"
           , `Array (system @ wire_messages ~quirks ~model:r.model r.messages) )
         ; "stream", `True
         ; "stream_options", `Object [ "include_usage", `True ]
         ]
       ; (if List.is_empty r.tools
          then []
          else [ "tools", `Array (List.map r.tools ~f:wire_tool) ])
       ; Option.value_map r.max_tokens ~default:[] ~f:(fun n ->
           [ "max_tokens", `Number (Int.to_string n) ])
       ; thinking_fields ~quirks r
       ])
;;

module Chunk = struct
  type t =
    { events : Assistant_event.t list
    ; finish_reason : string option
    ; usage : Usage.t option
    }
  [@@deriving sexp_of]
end

(* Servers differ in how they stream tool calls: some repeat the id and name
   in every delta, some omit [index] or the id. *)
module Tool_calls = struct
  type t =
    { mutable started : Int.Set.t
    ; mutable ids : (string * int) list
    ; mutable last : int
    ; mutable started_ids : (string * int) list
    ; mutable extras : Json.t Int.Map.t
      (** each call's latest [extra_content] (Gemini's thought signature),
        which may come on any of its deltas *)
    }

  let create () =
    { started = Int.Set.empty
    ; ids = []
    ; last = 0
    ; started_ids = []
    ; extras = Int.Map.empty
    }
  ;;

  let attach_extras t (a : Message.Assistant.t) =
    if Map.is_empty t.extras
    then a
    else
      { a with
        content =
          List.map a.content ~f:(function
            | Content.Tool_call c ->
              let extra =
                List.Assoc.find t.started_ids ~equal:String.equal c.id
                |> Option.bind ~f:(Map.find t.extras)
                |> Option.map ~f:Json.to_string
              in
              Content.Tool_call
                { c with extra = Option.first_some extra c.extra }
            | (Text _ | Thinking _ | Server_tool _) as block -> block)
      }
  ;;
end

let member_string name json =
  match Json.member name json with
  | Some (`String s) -> Some s
  | _ -> None
;;

let member_int name json = Option.bind (Json.member name json) ~f:Json.int

let parse_usage json =
  let get name = Option.value (member_int name json) ~default:0 in
  let cache_read =
    match member_int "prompt_cache_hit_tokens" json with
    | Some n -> n
    | None ->
      Option.bind
        (Json.member "prompt_tokens_details" json)
        ~f:(member_int "cached_tokens")
      |> Option.value ~default:0
  in
  { Usage.input = get "prompt_tokens"
  ; output = get "completion_tokens"
  ; cache_read
  }
;;

let parse_tool_call_delta (state : Tool_calls.t) json : Assistant_event.t list =
  let function_ = Json.member "function" json in
  let name =
    Option.bind function_ ~f:(member_string "name")
    |> Option.filter ~f:(Fn.non String.is_empty)
  in
  let arguments = Option.bind function_ ~f:(member_string "arguments") in
  let id =
    member_string "id" json |> Option.filter ~f:(Fn.non String.is_empty)
  in
  let index =
    match member_int "index" json with
    | Some index -> index
    | None ->
      (match
         Option.bind id ~f:(List.Assoc.find state.ids ~equal:String.equal), name
       with
       | Some index, _ -> index
       | None, Some _ when not (Set.is_empty state.started) ->
         1 + Option.value_exn (Set.max_elt state.started)
       | None, _ -> state.last)
  in
  state.last <- index;
  (match Json.member "extra_content" json with
   | Some (`Object _ as extra) ->
     state.extras <- Map.set state.extras ~key:index ~data:extra
   | _ -> ());
  Option.iter id ~f:(fun id ->
    if not (List.Assoc.mem state.ids ~equal:String.equal id)
    then state.ids <- (id, index) :: state.ids);
  let start =
    match name with
    | Some name when not (Set.mem state.started index) ->
      state.started <- Set.add state.started index;
      let id = Option.value id ~default:(sprintf "call_%d" index) in
      state.started_ids <- (id, index) :: state.started_ids;
      [ Assistant_event.Tool_call_start { index; id; name } ]
    | _ -> []
  in
  let delta =
    match arguments with
    | Some arguments when not (String.is_empty arguments) ->
      [ Assistant_event.Tool_call_delta { index; arguments } ]
    | _ -> []
  in
  start @ delta
;;

let parse_chunk ?(tool_calls = Tool_calls.create ()) (json : Json.t)
  : Chunk.t Or_error.t
  =
  match Json.member "error" json with
  | Some err ->
    let message =
      Option.value (member_string "message" err) ~default:(Json.to_string err)
    in
    Or_error.error_string message
  | None ->
    let choice =
      match Json.member "choices" json with
      | Some (`Array (c :: _)) -> Some c
      | _ -> None
    in
    let delta = Option.bind choice ~f:(Json.member "delta") in
    let events =
      match delta with
      | None -> []
      | Some delta ->
        let text s =
          if String.is_empty s then [] else [ Assistant_event.Text_delta s ]
        in
        let thinking s =
          if String.is_empty s then [] else [ Assistant_event.Thinking_delta s ]
        in
        let reasoning =
          match member_string "reasoning_content" delta with
          | Some s -> Some s
          | None -> member_string "reasoning" delta
        in
        List.concat
          [ Option.value_map reasoning ~default:[] ~f:thinking
          ; Option.value_map (member_string "content" delta) ~default:[] ~f:text
          ; (match Json.member "tool_calls" delta with
             | Some (`Array calls) ->
               List.concat_map calls ~f:(parse_tool_call_delta tool_calls)
             | _ -> [])
          ]
    in
    let finish_reason = Option.bind choice ~f:(member_string "finish_reason") in
    let usage =
      match Json.member "usage" json with
      | Some (`Object _ as u) -> Some (parse_usage u)
      | _ -> None
    in
    Ok { Chunk.events; finish_reason; usage }
;;

(* Some servers report "stop" after tool calls. *)
let stop_reason_of_finish ~saw_tool_call = function
  | Some ("tool_calls" | "function_call") -> Stop_reason.Tool_use
  | Some "length" -> Length
  | Some "content_filter" ->
    Error "the reply was stopped by the server's content filter"
  | Some ("stop" | "end_turn" | "eos") | None ->
    if saw_tool_call then Tool_use else End_turn
  | Some other -> Error ("unexpected finish_reason: " ^ other)
;;

let stream
      ~env
      ~url
      ~timeout
      ~headers
      ~quirks
      (request : Provider.Request.t)
      ~cancel
      ~on_event
  =
  let builder = Assistant_builder.create ~model:(Model.key request.model) in
  let tool_calls = Tool_calls.create () in
  let finish_reason = ref None in
  let usage = ref Usage.zero in
  let api_error = ref None in
  let handle_event (event : Sse.Event.t) =
    if not (String.equal (String.strip event.data) "[DONE]")
    then (
      match Json.parse event.data with
      | Error e ->
        api_error
        := Some (sprintf "bad JSON in stream: %s" (Error.to_string_hum e))
      | Ok json ->
        (match parse_chunk ~tool_calls json with
         | Error e -> api_error := Some (Error.to_string_hum e)
         | Ok chunk ->
           List.iter chunk.events ~f:(fun e ->
             Assistant_builder.apply builder e;
             on_event e);
           Option.iter chunk.finish_reason ~f:(fun r -> finish_reason := Some r);
           Option.iter chunk.usage ~f:(fun u -> usage := u)))
  in
  let outcome =
    Sse_request.run
      ~env
      ?timeout
      ~cancel
      ~url
      ~headers
      ~body:
        (Trace.span ~detailed:true "request_build" (fun () ->
           Json.to_string (request_body ~quirks request)))
      ~on_event:handle_event
      ()
  in
  let stop_reason : Stop_reason.t =
    match outcome with
    | Aborted -> Aborted
    | Failed e -> Error e
    | Completed ->
      (match !api_error with
       | Some e -> Error e
       | None ->
         stop_reason_of_finish
           ~saw_tool_call:(not (Set.is_empty tool_calls.started))
           !finish_reason)
  in
  Assistant_builder.finish builder ~stop_reason ~usage:!usage
  |> Tool_calls.attach_extras tool_calls
;;

let create ~env ?timeout ~name ~url ~headers ~quirks () =
  { Provider.name; stream = stream ~env ~url ~timeout ~headers ~quirks }
;;

module For_testing = struct
  let request_body = request_body

  module Chunk = Chunk

  let parse_stream jsons =
    let tool_calls = Tool_calls.create () in
    List.map jsons ~f:(parse_chunk ~tool_calls)
  ;;

  let parse_reply jsons =
    let builder = Assistant_builder.create ~model:"test/m" in
    let tool_calls = Tool_calls.create () in
    List.iter jsons ~f:(fun json ->
      match parse_chunk ~tool_calls json with
      | Ok chunk -> List.iter chunk.events ~f:(Assistant_builder.apply builder)
      | Error e -> Error.raise e);
    Assistant_builder.finish builder ~stop_reason:Tool_use ~usage:Usage.zero
    |> Tool_calls.attach_extras tool_calls
  ;;

  let parse_chunk json = parse_chunk json
end
