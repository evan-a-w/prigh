open! Core
open! Import

let estimate_tokens messages =
  let chars =
    List.sum
      (module Int)
      messages
      ~f:(function
        | Message.User u -> String.length u.text
        | Tool_result r -> String.length r.text
        | Assistant a ->
          List.sum
            (module Int)
            a.content
            ~f:(function
              | Text s -> String.length s
              | Thinking th -> String.length th.text
              | Tool_call c -> String.length c.arguments + String.length c.name))
  in
  chars / 4
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
       | Message.User u -> "USER:\n" ^ u.text
       | Tool_result r ->
         sprintf
           "TOOL RESULT (%s%s):\n%s"
           r.tool_name
           (if r.is_error then ", error" else "")
           (String.prefix r.text 2000)
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

(* The earliest user message such that everything from it onwards fits in
   [keep_recent_tokens]; everything before it gets summarised. *)
let split_point ~keep_recent_tokens (entries : Session.Entry.t list) =
  let rec go rev tokens candidate =
    match rev with
    | [] -> candidate
    | (e : Session.Entry.t) :: rest ->
      (match e.payload with
       | Message m ->
         let tokens = tokens + estimate_tokens [ m ] in
         if tokens > keep_recent_tokens
         then candidate
         else (
           let candidate =
             match m with
             | User _ -> Some e.id
             | Assistant _ | Tool_result _ -> candidate
           in
           go rest tokens candidate)
       | Model _ | Compaction _ | Name _ | Cwd _ | System_prompt _ ->
         go rest tokens candidate)
  in
  go (List.rev entries) 0 None
;;

let compact
      ~env:_
      ~(provider : Provider.t)
      ~model
      ?(keep_recent_tokens = 8000)
      ?(cancel = Cancellation.never)
      session
  =
  let path = Session.active_path session in
  let messages_before = Session.messages session in
  match split_point ~keep_recent_tokens path with
  | None -> Or_error.error_string "nothing to compact: conversation is short"
  | Some kept_from ->
    let kept_count =
      List.drop_while path ~f:(fun e -> not (String.equal e.id kept_from))
      |> List.count ~f:(fun e ->
        match e.payload with
        | Message _ -> true
        | Model _ | Compaction _ | Name _ | Cwd _ | System_prompt _ -> false)
    in
    let older =
      List.take messages_before (List.length messages_before - kept_count)
    in
    if List.is_empty older
    then Or_error.error_string "nothing to compact: conversation is short"
    else (
      let request =
        { Provider.Request.model
        ; system = Some summary_instructions
        ; messages = [ Message.user (render_transcript older) ]
        ; tools = []
        ; thinking = Off
        ; max_tokens = None
        }
      in
      let reply = provider.stream request ~cancel ~on_event:ignore in
      match reply.stop_reason with
      | Error e -> Or_error.error_string ("compaction failed: " ^ e)
      | Aborted -> Or_error.error_string "compaction aborted"
      | End_turn | Tool_use | Length ->
        let summary = Message.Assistant.text reply in
        let (_ : Session.Entry.t) =
          Session.append_compaction session ~summary ~kept_from
        in
        Ok summary)
;;
