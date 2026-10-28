open! Core
open! Import

let own_call_note =
  "This call started you: you are the subagent it forked from this \
   conversation. Your task follows."
;;

let other_call_note =
  "[not run here: your caller runs this call and gets its result; you, its \
   fork, do not see it]"
;;

let context ({ messages; _ } : Conversation.t) ~call_id =
  let pending =
    match List.last messages with
    | Some (Assistant assistant) -> Message.Assistant.tool_calls assistant
    | Some (User _ | Tool_result _) | None -> []
  in
  messages
  @ List.map pending ~f:(fun (call : Content.Tool_call.t) ->
    Message.Tool_result
      { tool_call_id = call.id
      ; tool_name = call.name
      ; text =
          (if String.equal call.id call_id
           then own_call_note
           else other_call_note)
      ; is_error = false
      ; images = []
      })
;;

let shared_tools tools ~depth =
  let available =
    match Tools.for_context ~parent:tools ~depth () with
    | Ok available -> available
    | Error _ -> []
  in
  let shared =
    List.map tools ~f:(fun (tool : Tool.t) ->
      let name = Tool.name tool in
      match
        List.find available ~f:(fun t -> String.equal (Tool.name t) name)
      with
      | Some own -> `Shared { own with spec = tool.spec }
      | None ->
        let why =
          Option.value
            (Tools.unavailable ~depth name)
            ~default:"you are a subagent"
        in
        let text =
          sprintf
            "%s is not available to you: %s. You see it because you were \
             forked from your caller's conversation, with its tools."
            name
            why
        in
        `Refused
          { Tool.spec = { tool.spec with on_host = false; destructive = Never }
          ; run = (fun _ _ -> Tool.Result.error text)
          })
  in
  ( List.map shared ~f:(function `Shared tool | `Refused tool -> tool)
  , List.filter_map shared ~f:(function
      | `Shared _ -> None
      | `Refused tool -> Some (Tool.name tool)) )
;;

module Framing = struct
  type t =
    { task : string
    ; extra_context : string option
    ; parent_cwd : string
    ; cwd : string
    ; worktree : Agent_worktree.t option
    ; refused : string list
    ; sync_subagents : bool
    }
end

let prompt (f : Framing.t) =
  let paragraphs =
    [ Some
        "[You are now a subagent forked from the conversation above. Your \
         caller carries on separately: you do not see what it does next, and \
         it sees only your final report. Do only the task below (not the rest \
         of the conversation's work), then end with a concise report of what \
         you found or changed and anything your caller must know.]"
    ; (match f.worktree with
       | Some w ->
         Some
           (sprintf
              "You work in a git worktree made for you, %s, on branch %s: \
               paths in the conversation above refer to your caller's working \
               directory, %s; work on their counterparts in yours, %s."
              w.path
              w.branch
              f.parent_cwd
              f.cwd)
       | None when not (String.equal f.cwd f.parent_cwd) ->
         Some
           (sprintf
              "Your working directory is %s (your caller's, which paths in the \
               conversation above refer to, is %s)."
              f.cwd
              f.parent_cwd)
       | None -> None)
    ; (match f.refused with
       | [] -> None
       | refused ->
         Some
           (sprintf
              "You have your caller's tools, but not all of them are yours to \
               use: %s refuse%s."
              (String.concat ~sep:", " refused)
              (if List.length refused = 1 then "s" else "")))
    ; (if f.sync_subagents
       then
         Some
           "Your own subagents run synchronously: subagent blocks until its \
            subagent finishes and returns its report, and they cannot delegate \
            further."
       else None)
    ; Option.map f.extra_context ~f:(fun extra -> "Context:\n" ^ extra)
    ; Some ("Your task:\n" ^ f.task)
    ]
  in
  Message.user (String.concat ~sep:"\n\n" (List.filter_opt paragraphs))
;;
