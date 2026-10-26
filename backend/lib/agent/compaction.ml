open! Core
open! Import

let image_lines (images : Image.t list) =
  List.map images ~f:(fun i -> sprintf "\n[image: %s]" i.mime_type)
  |> String.concat
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

let default_keep_recent_tokens = 8000

(* The user's earlier messages carried over (shortened), at most. *)
let pinned_budget_tokens = 6000
let summary_max_tokens = 8192

module Prompt = struct
  type t =
    { system : string option
    ; tools : Tool_spec.t list
    ; thinking : Thinking.t
    ; cache_key : string option
    }

  let none = { system = None; tools = []; thinking = Off; cache_key = None }
end

module Outcome = struct
  type t =
    { summary : string
    ; usage : Usage.t
    }
  [@@deriving sexp_of]
end

module Plan = struct
  module Pin = struct
    type t =
      { index : int
      ; shortened : bool
      }
    [@@deriving sexp_of]
  end

  type t =
    { kept_from : int
    ; pinned : Pin.t list
    }
  [@@deriving sexp_of]
end

let is_user_message (m : Message.t) =
  match m with
  | User _ -> not (Session.is_summary_message m)
  | Assistant _ | Tool_result _ -> false
;;

(* What the user wrote, not what the harness wrote in their place (a
   summary, reminder, goal, report of background work...). *)
let is_users_own (m : Message.t) =
  match m with
  | User u ->
    (match Injected.of_message ~origin:u.origin u.text with
     | None | Some { source = None | Some (Review_comments _); _ } -> true
     | Some { source = Some _; _ } -> false)
  | Assistant _ | Tool_result _ -> false
;;

(* The kept part starts at a user message or at an assistant message (whose
   tool results follow it), never at a tool result, so every tool call keeps
   its result. It starts as early as fits in [keep_recent_tokens], but always
   leaves something to summarise and keeps at least the last turn. The user's
   latest message is pinned when it would otherwise be summarised, as in a
   long run on a single prompt, and so are their earlier ones, shortened, the
   newest first while they fit in [pinned_budget_tokens]. *)
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
    let latest = Option.filter last_user ~f:(fun i -> i < kept_from) in
    let rec fit budget = function
      | [] -> []
      | i :: older ->
        let tokens =
          Message.estimate_tokens [ Pinned_messages.shorten messages.(i) ]
        in
        if tokens > budget then [] else i :: fit (budget - tokens) older
    in
    let earlier =
      List.range ~stride:(-1) (kept_from - 1) (-1)
      |> List.filter ~f:(fun i ->
        is_users_own messages.(i)
        && not (Option.equal Int.equal (Some i) latest))
      |> fit pinned_budget_tokens
      |> List.rev
    in
    let pinned =
      List.map earlier ~f:(fun index -> { Plan.Pin.index; shortened = true })
      @ Option.value_map latest ~default:[] ~f:(fun index ->
        [ { Plan.Pin.index; shortened = false } ])
    in
    let summarised =
      List.exists (List.range 0 kept_from) ~f:(fun i ->
        (not (Session.is_summary_message messages.(i)))
        && not (List.exists pinned ~f:(fun (pin : Plan.Pin.t) -> pin.index = i)))
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

let sections =
  {|Use exactly these sections, as Markdown headings, and write "None" under a section with nothing to say:

## Goal and constraints
What the user wants, and every requirement, constraint and preference they stated, in their words where the wording matters.
## Decisions
What was decided, and why.
## Failed approaches and exact errors
What was tried and did not work, with the exact error messages, failing commands and test names. Do not smooth over failures: this evidence is what keeps them from being repeated.
## Files
What was learned from the files read, and what was changed in each and why. A list of the paths read and modified is appended automatically: do not repeat it.
## Current state
What works, what does not, what is in progress.
## Next steps
What remains to be done, in order.

Keep exact identifiers, paths, commands, values and error messages that may be needed. Reply with the summary only, no preamble.|}
;;

let with_user_instructions text = function
  | None -> text
  | Some extra ->
    text ^ "\n\nThe user's instructions for this summary: " ^ extra
;;

(* The request the agent would send next, ending with the request for a
   summary: the same system prompt, tools, thinking and cache key, so the
   provider's prompt cache serves the conversation. A summariser other than
   the agent's model ([compaction_model]) gets the same request, which its
   cache cannot serve, with thinking off if it has none. *)
let full_request
      (model : Model.t)
      (prompt : Prompt.t)
      ~update
      ?instructions
      older
  =
  let ask =
    String.concat
      ~sep:"\n\n"
      ([ "[prigh: compaction] The conversation up to here is about to be \
          replaced by a summary, which is all of it that will remain (with the \
          user's own messages, kept verbatim). Do not call any tools: reply \
          with the summary as text."
       ]
       @ (if update
          then
            [ "The conversation begins with the summary of earlier work: \
               update it rather than starting over. Merge what happened since \
               into its sections, keep every item that still matters, and drop \
               only what is obsolete or resolved."
            ]
          else [])
       @ [ sections ])
  in
  let thinking : Thinking.t =
    if model.supports_thinking then prompt.thinking else Off
  in
  { Provider.Request.model
  ; system = prompt.system
  ; messages =
      older @ [ Message.user (with_user_instructions ask instructions) ]
  ; tools = prompt.tools
  ; thinking
  ; max_tokens =
      Some
        (Int.min
           model.max_output
           (summary_max_tokens
            + Option.value (Thinking.budget_tokens thinking) ~default:0))
  ; cache_key = prompt.cache_key
  }
;;

let fits (request : Provider.Request.t) =
  let prompt_tokens =
    (String.length (Option.value request.system ~default:"")
     + List.sum
         (module Int)
         request.tools
         ~f:(fun (t : Tool_spec.t) ->
           String.length t.name
           + String.length t.description
           + String.length (Json.to_string t.parameters)))
    / 4
  in
  prompt_tokens
  + Message.estimate_tokens request.messages
  + Option.value request.max_tokens ~default:0
  <= request.model.context_window
;;

let transcript_system =
  "You summarise a coding agent's conversation with its user, so that the \
   agent can carry on from the summary alone."
;;

(* The conversation rendered as text in a request of its own, for when it
   does not fit as it is: cheaper, without images, thinking or long tool
   output. *)
let transcript_request (model : Model.t) ~previous ?instructions older =
  let older =
    List.filter older ~f:(fun m -> not (Session.is_summary_message m))
  in
  let ask =
    String.concat
      ~sep:"\n\n"
      ([ "Summarise the conversation in <conversation>." ]
       @ (if Option.is_some previous
          then
            [ "<previous-summary> is the summary of the work before it: update \
               it rather than starting over. Merge what happened since into \
               its sections, keep every item that still matters, and drop only \
               what is obsolete or resolved."
            ]
          else [])
       @ [ sections ])
  in
  let text =
    String.concat
      ~sep:"\n\n"
      (Option.value_map previous ~default:[] ~f:(fun summary ->
         [ "<previous-summary>\n" ^ summary ^ "\n</previous-summary>" ])
       @ [ "<conversation>\n"
           ^ bounded_transcript model older
           ^ "\n</conversation>"
         ; with_user_instructions ask instructions
         ])
  in
  { Provider.Request.model
  ; system = Some transcript_system
  ; messages = [ Message.user text ]
  ; tools = []
  ; thinking = Off
  ; max_tokens = Some (Int.min model.max_output summary_max_tokens)
  ; cache_key = None
  }
;;

let summarise
      ~(provider : Provider.t)
      ~model
      ~prompt
      ~overflowed
      ~cancel
      ?cwd
      ?(files_of = Fn.id)
      ?instructions
      older
  =
  let previous, files =
    match List.find_map (List.rev older) ~f:Session.summary_of_message with
    | None -> None, Compaction_files.empty
    | Some summary ->
      let text, files = Compaction_files.of_summary summary in
      Some text, files
  in
  let files = Compaction_files.add_calls ?cwd files (files_of older) in
  let stream request = provider.stream request ~cancel ~on_event:ignore in
  let transcript () =
    stream (transcript_request model ~previous ?instructions older)
  in
  let full =
    full_request
      model
      prompt
      ~update:(Option.is_some previous)
      ?instructions
      older
  in
  let reply, usage =
    if overflowed || not (fits full)
    then transcript (), Usage.zero
    else (
      let reply = stream full in
      let fall_back =
        match reply.stop_reason with
        | Error e -> Context_overflow.matches e
        | Aborted -> false
        | End_turn | Tool_use | Length ->
          String.is_empty (String.strip (Message.Assistant.text reply))
      in
      if fall_back && not (Cancellation.is_cancelled cancel)
      then transcript (), reply.usage
      else reply, Usage.zero)
  in
  match reply.stop_reason with
  | Error e -> Or_error.error_string ("compaction failed: " ^ e)
  | Aborted -> Or_error.error_string "compaction aborted"
  | End_turn | Tool_use | Length ->
    Ok
      { Outcome.summary =
          Compaction_files.append
            (String.strip (Message.Assistant.text reply))
            files
      ; usage = Usage.add usage reply.usage
      }
;;

let compact_messages
      ~provider
      ~model
      ~prompt
      ?(overflowed = false)
      ?(keep_recent_tokens = default_keep_recent_tokens)
      ?(cancel = Cancellation.never)
      ?cwd
      ?instructions
      ?unmasked
      messages
  =
  match plan ~keep_recent_tokens messages with
  | None -> Or_error.error_string nothing_to_compact
  | Some { kept_from; pinned } ->
    let older = List.take messages kept_from in
    let files_of =
      Option.map unmasked ~f:(fun unmasked older ->
        List.take unmasked (List.length older))
    in
    Or_error.map
      (summarise
         ~provider
         ~model
         ~prompt
         ~overflowed
         ~cancel
         ~cwd:(fun _ -> cwd)
         ?files_of
         ?instructions
         older)
      ~f:(fun (outcome : Outcome.t) ->
        let pinned =
          List.map pinned ~f:(fun { index; shortened } ->
            let m = List.nth_exn messages index in
            if shortened then Pinned_messages.shorten m else m)
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
      ~prompt
      ?(overflowed = false)
      ?(keep_recent_tokens = default_keep_recent_tokens)
      ?(cancel = Cancellation.never)
      ?instructions
      session
  =
  let messages = Session.messages session in
  let ids = Array.of_list (Session.context_ids session) in
  let cwds = Array.of_list (Session.context_cwds session) in
  (* The summary is asked for with the messages as the agent sends them
     (masked), so that the prompt cache serves it; the files come from them
     as they were (a masked [apply_patch] has lost its paths). *)
  let unmasked =
    List.map (Session.timed_messages session) ~f:(fun m -> m.message)
  in
  match plan ~keep_recent_tokens messages with
  | None -> Or_error.error_string nothing_to_compact
  | Some { kept_from; pinned } ->
    (match ids.(kept_from) with
     | None -> Or_error.error_string nothing_to_compact
     | Some kept_from_id ->
       let older = List.take messages kept_from in
       Or_error.map
         (summarise
            ~provider
            ~model
            ~prompt
            ~overflowed
            ~cancel
            ~cwd:(fun i -> Some cwds.(i))
            ~files_of:(fun older -> List.take unmasked (List.length older))
            ?instructions
            older)
         ~f:(fun (outcome : Outcome.t) ->
           let pinned =
             List.filter_map pinned ~f:(fun { index; shortened } ->
               Option.map ids.(index) ~f:(fun id ->
                 { Pinned_messages.Pin.id; shortened }))
           in
           let (_ : Session.Entry.t) =
             Session.append_compaction
               ~pinned
               session
               ~summary:outcome.summary
               ~kept_from:kept_from_id
           in
           outcome))
;;
