open! Core

let call ~id ~name args =
  sprintf
    {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"tool_call","id":"%s","name":"%s","arguments":%s}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"faux"}}|}
    id
    name
    (Jsonaf.to_string (`String args))
;;

let text_reply text =
  sprintf
    {|{"event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":%s}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"faux"}}|}
    (Jsonaf.to_string (`String text))
;;

let tool_start ~id ~name args =
  sprintf
    {|{"event":"tool_start","call":{"id":"%s","name":"%s","arguments":%s}}|}
    id
    name
    (Jsonaf.to_string (`String args))
;;

let tool_end ~id ~name text =
  sprintf
    {|{"event":"tool_end","call":{"id":"%s","name":"%s","arguments":"{}"},"result":{"role":"tool_result","tool_call_id":"%s","tool_name":"%s","text":%s,"is_error":false}}|}
    id
    name
    id
    name
    (Jsonaf.to_string (`String text))
;;

let inner ~call ~agent json =
  sprintf
    {|{"event":"subagent","call_id":"%s","agent_id":"%s","inner":%s}|}
    call
    agent
    json
;;

let start ?(forked = false) ?(continued = false) ~call ~agent task =
  sprintf
    {|{"event":"subagent_start","call_id":"%s","agent_id":"%s","task":"%s","model":"deepseek-flash","tools":["bash"]%s%s}|}
    call
    agent
    task
    (if forked then {|,"forked":true|} else "")
    (if continued then {|,"continued":true|} else "")
;;

let end_ ?(error = false) ~call ~agent ~turns text =
  sprintf
    {|{"event":"subagent_end","call_id":"%s","agent_id":"%s","usage":{"input":0,"output":0,"cache_read":0},"turns":%d,"cost_usd":0.012,"result":{"text":%s,"is_error":%b}}|}
    call
    agent
    turns
    (Jsonaf.to_string (`String text))
    error
;;
