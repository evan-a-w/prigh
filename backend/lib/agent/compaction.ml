open! Core
open! Import

let image_lines (images : Image.t list) =
  List.map images ~f:(fun i -> sprintf "\n[image: %s]" i.mime_type)
  |> String.concat
;;

let should_compact (model : Model.t) ~input_tokens =
  Float.( > )
    (Float.of_int input_tokens)
    (Float.of_int model.context_window *. 0.8)
;;

let render_transcript messages =
  String.concat
    ~sep:"\n\n"
    (List.map messages ~f:(function
       | Message.User u -> "USER:\n" ^ u.text ^ image_lines u.images
       | Tool_result r ->
         sprintf
           "TOOL RESULT (%s%s):\n%s%s"
           r.tool_name
           (if r.is_error then ", error" else "")
           (String.prefix r.text 2000)
           (image_lines r.images)
       | Assistant a ->
         let text = Message.Assistant.text a in
         let calls =
           List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
             sprintf "[called %s %s]" c.name (String.prefix c.arguments 500))
         in
         "ASSISTANT:\n"
         ^ String.concat
             ~sep:"\n"
             ((if String.is_empty text then [] else [ text ]) @ calls)))
;;

let summary_instructions =
  "Summarise the conversation below so that another assistant can continue the \
   work without the original transcript. Include: the user's goals and \
   constraints, what has been done (files read, changed, commands run and \
   their outcomes), current state, open problems and next steps. Preserve \
   exact identifiers, paths, commands and error messages that may be needed. \
   Write plain prose and lists; no preamble."
;;

let default_keep_recent_tokens = 8000

module Outcome = struct
  type t =
    { summary : string
    ; usage : Usage.t
    }
  [@@deriving sexp_of]
end

module Plan = struct
  type t =
    { kept_from : int
    ; pinned : int option
    }
  [@@deriving sexp_of]
end

let is_user_message (m : Message.t) =
  match m with
  | User _ -> not (Session.is_summary_message m)
  | Assistant _ | Tool_result _ -> false
;;

(* The kept part starts at a user message or at an assistant message (whose
   tool results follow it), never at a tool result, so every tool call keeps
   its result. It starts as early as fits in [keep_recent_tokens], but always
   leaves something to summarise and keeps at least the last turn. The user's
   latest message is pinned when it would otherwise be summarised, as in a
   long run on a single prompt. *)
let plan ~keep_recent_tokens messages =
  let messages = Array.of_list messages in
  let n = Array.length messages in
  let candidate i =
    i >= 1
    &&
    match messages.(i) with
    | Message.User _ -> is_user_message messages.(i)
    | Assistant _ -> true
    | Tool_result _ -> false
  in
  let rec walk i tokens best =
    if i < 1
    then best
    else (
      let tokens = tokens + Message.estimate_tokens [ messages.(i) ] in
      if tokens > keep_recent_tokens
      then best
      else walk (i - 1) tokens (if candidate i then Some i else best))
  in
  let latest = List.find (List.range ~stride:(-1) (n - 1) 0) ~f:candidate in
  match Option.first_some (walk (n - 1) 0 None) latest with
  | None -> None
  | Some kept_from ->
    let last_user =
      List.find
        (List.range ~stride:(-1) (n - 1) (-1))
        ~f:(fun i -> is_user_message messages.(i))
    in
    let pinned = Option.filter last_user ~f:(fun i -> i < kept_from) in
    let summarised =
      List.exists (List.range 0 kept_from) ~f:(fun i ->
        (not (Session.is_summary_message messages.(i)))
        && not (Option.equal Int.equal (Some i) pinned))
    in
    if summarised then Some { Plan.kept_from; pinned } else None
;;

let nothing_to_compact = "nothing to compact: conversation is short"

(* The summariser has to fit the transcript in its own window: the middle of
   an oversized one goes. *)
let bounded_transcript (model : Model.t) messages =
  let text = render_transcript messages in
  let limit = Int.max 20_000 ((model.context_window - 20_000) * 3) in
  if String.length text <= limit
  then text
  else (
    let head = limit / 5 in
    let tail = limit - head in
    sprintf
      "%s\n\n[... %d characters omitted ...]\n\n%s"
      (String.prefix text head)
      (String.length text - limit)
      (String.suffix text tail))
;;

let summarise ~(provider : Provider.t) ~model ~cancel ?instructions older =
  let request =
    { Provider.Request.model
    ; system =
        Some
          (match instructions with
           | None -> summary_instructions
           | Some extra ->
             summary_instructions
             ^ "\n\nThe user's instructions for this summary: "
             ^ extra)
    ; messages = [ Message.user (bounded_transcript model older) ]
    ; tools = []
    ; thinking = Off
    ; max_tokens = None
    ; cache_key = None
    }
  in
  let reply = provider.stream request ~cancel ~on_event:ignore in
  match reply.stop_reason with
  | Error e -> Or_error.error_string ("compaction failed: " ^ e)
  | Aborted -> Or_error.error_string "compaction aborted"
  | End_turn | Tool_use | Length ->
    Ok { Outcome.summary = Message.Assistant.text reply; usage = reply.usage }
;;

let compact_messages
      ~provider
      ~model
      ?(keep_recent_tokens = default_keep_recent_tokens)
      ?(cancel = Cancellation.never)
      ?instructions
      messages
  =
  match plan ~keep_recent_tokens messages with
  | None -> Or_error.error_string nothing_to_compact
  | Some { kept_from; pinned } ->
    let older = List.take messages kept_from in
    Or_error.map
      (summarise ~provider ~model ~cancel ?instructions older)
      ~f:(fun (outcome : Outcome.t) ->
        let pinned =
          Option.value_map pinned ~default:[] ~f:(fun i ->
            [ List.nth_exn messages i ])
        in
        ( pinned
          @ [ Session.summary_message outcome.summary ]
          @ List.drop messages kept_from
        , outcome ))
;;

let compact
      ~env:_
      ~provider
      ~model
      ?(keep_recent_tokens = default_keep_recent_tokens)
      ?(cancel = Cancellation.never)
      ?instructions
      session
  =
  let messages = Session.messages session in
  let ids = Array.of_list (Session.context_ids session) in
  match plan ~keep_recent_tokens messages with
  | None -> Or_error.error_string nothing_to_compact
  | Some { kept_from; pinned } ->
    (match ids.(kept_from) with
     | None -> Or_error.error_string nothing_to_compact
     | Some kept_from_id ->
       let older = List.take messages kept_from in
       Or_error.map
         (summarise ~provider ~model ~cancel ?instructions older)
         ~f:(fun (outcome : Outcome.t) ->
           let pinned = Option.bind pinned ~f:(fun i -> ids.(i)) in
           let (_ : Session.Entry.t) =
             Session.append_compaction
               ?pinned
               session
               ~summary:outcome.summary
               ~kept_from:kept_from_id
           in
           outcome))
;;
