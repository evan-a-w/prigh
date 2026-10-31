open! Core
open! Import

let all =
  [ Tool_bash.tool
  ; Tool_read.tool
  ; Tool_write.tool
  ; Tool_edit.tool
  ; Tool_ls.tool
  ; Tool_grep.tool
  ; Tool_find.tool
  ; Tool_web_fetch.tool
  ; Tool_web_search.tool
  ]
;;

(* What the main agent uses to talk to the user: [update_plan], [ask_user]. *)
let interaction = [ Plan.tool; Tool_ask_user.tool ]
let variants = [ Tool_apply_patch.tool ]
let builtin = all @ variants
let find name = List.find builtin ~f:(fun t -> String.equal (Tool.name t) name)

let uses_apply_patch (model : Model.t) =
  match model.provider with
  | Openai | Openai_codex ->
    (match String.chop_prefix model.id ~prefix:"gpt-" with
     | Some rest ->
       (match Int.of_string_opt (String.take_while rest ~f:Char.is_digit) with
        | Some major ->
          major >= 5 && not (String.is_substring rest ~substring:"-chat")
        | None -> false)
     | None -> false)
  | Anthropic | Deepseek | Custom _ -> false
;;

let editors = [ Tool_edit.tool; Tool_apply_patch.tool ]

let is_editor t =
  List.exists editors ~f:(fun e -> String.equal (Tool.name e) (Tool.name t))
;;

let editor tools = List.find tools ~f:is_editor

let for_model model tools =
  let editor =
    if uses_apply_patch model then Tool_apply_patch.tool else Tool_edit.tool
  in
  let replaced = ref false in
  List.filter_map tools ~f:(fun t ->
    if
      Tool_web_search.is_spec t.Tool.spec
      && not (Tool_web_search.supported model)
    then None
    else if not (is_editor t)
    then Some t
    else if !replaced
    then None
    else (
      replaced := true;
      Some editor))
;;

let specs tools = List.map tools ~f:(fun (t : Tool.t) -> t.spec)

(* A subagent's own subagents are synchronous. *)
let subagent_control =
  [ "subagent_status"; "subagent_cancel"; "subagent_message" ]
;;

(* The session's plan, goal, plan mode and the user belong to the main
   agent. *)
let main_agent_only =
  [ Plan.tool_name; Tool_ask_user.name; Goal.tool_name; Tool_review_reply.name ]
;;

(* Why a tool is not available at [depth], if it is not. *)
let unavailable ~depth name =
  let mem = List.mem ~equal:String.equal in
  if depth >= 2 && mem [ "subagent"; "subagent_continue" ] name
  then
    Some
      "you are a subagent's subagent, which cannot delegate further: do the \
       work yourself"
  else if depth >= 1 && mem subagent_control name
  then
    Some
      "you are a subagent, and background subagents are the main agent's \
       (yours run synchronously: subagent returns their report)"
  else if depth >= 1 && mem main_agent_only name
  then
    Some
      "it is the main agent's (the user's plan, questions and goal), and you \
       are a subagent: put what you need from the user in your report"
  else None
;;

let for_context ~parent ~depth ?only () =
  let available =
    List.filter_map parent ~f:(fun t ->
      let name = Tool.name t in
      if Option.is_some (unavailable ~depth name)
      then None
      else if depth >= 1 && String.equal name "bash"
      then Some Tool_bash.subagent_tool
      else Some t)
  in
  match only with
  | None -> Ok available
  | Some names ->
    (* Either editor's name stands for the one this model has. *)
    let names =
      match editor available with
      | None -> names
      | Some editor ->
        List.map names ~f:(fun name ->
          if List.exists editors ~f:(fun e -> String.equal (Tool.name e) name)
          then Tool.name editor
          else name)
    in
    let valid = List.map available ~f:Tool.name in
    let unknown =
      List.filter names ~f:(fun name ->
        not (List.mem valid name ~equal:String.equal))
    in
    if not (List.is_empty unknown)
    then
      Or_error.errorf
        "unknown tool%s %s; valid tools: %s"
        (if List.length unknown = 1 then "" else "s")
        (String.concat ~sep:", " (List.map unknown ~f:(sprintf "%S")))
        (String.concat ~sep:", " valid)
    else
      Ok
        (List.filter available ~f:(fun t ->
           List.mem names (Tool.name t) ~equal:String.equal))
;;
