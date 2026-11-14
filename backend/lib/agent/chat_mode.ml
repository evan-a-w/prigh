open! Core
open! Import

(* An allow list, so that a new tool is not offered in chat mode until it is
   known to need no machine. *)
let names =
  String.Set.of_list
    [ Tool_web_search.name
    ; Tool_web_fetch.name
    ; Plan.tool_name
    ; Tool_ask_user.name
    ; Tool_render_ui.name
    ; Tool_memory.name
    ; Tool_custom_instructions.name
    ; Tool_sessions.name
    ; Tool_ask_session.name
    ; Tool_answer_session.name
    ; Goal.tool_name
    ; Tool_subagent.name
    ; Tool_subagent.continue_name
    ; "subagent_status"
    ; "subagent_cancel"
    ; "subagent_message"
    ]
;;

let allowed name = Set.mem names name

let tools ~allow_local tools =
  List.filter_map tools ~f:(fun tool ->
    let name = Tool.name tool in
    if String.equal name Tool_web_fetch.name
    then Some (Tool_web_fetch.in_backend ~allow_local)
    else Option.some_if (allowed name) tool)
;;


let refusal name =
  sprintf
    "%s is not available: this is a chat (chat mode), with no access to any \
     machine. Answer without it; only the user can leave chat mode."
    name
;;
