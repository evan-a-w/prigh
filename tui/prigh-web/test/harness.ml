open! Core
open Prigh_web
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

type t =
  { mutable model : App.Model.t
  ; mutable pending : (string * App.Reply_tag.t) list
  }

let model t = t.model

let act t action =
  let model, commands = App.update t.model action in
  t.model <- model;
  List.iter commands ~f:(fun (command : App.Command.t) ->
    (match command with
     | Rpc { method_; tag; _ } -> t.pending <- t.pending @ [ method_, tag ]
     | Reconnect _ | Set_url_session _ -> ());
    print_s [%sexp (command : App.Command.t)])
;;

let reply t method_ json =
  match List.findi t.pending ~f:(fun _ (m, _) -> String.equal m method_) with
  | None -> raise_s [%message "no pending request" method_]
  | Some (i, (_, tag)) ->
    t.pending <- List.filteri t.pending ~f:(fun j _ -> j <> i);
    act t (Reply (tag, Ok (Jsonaf.of_string json)))
;;

let event t json =
  match Prigh_protocol.Event.of_json (Jsonaf.of_string json) with
  | Ok e -> act t (Event e)
  | Error e -> raise_s [%message "bad event" (e : Error.t)]
;;

let state_json ?(fields = []) () =
  let defaults =
    [ "session_id", `String "s1"
    ; "session_path", `String "/sessions/s1.jsonl"
    ; "session_name", `Null
    ; "session_description", `Null
    ; "cwd", `String "/work"
    ; "git_branch", `String "main"
    ; ( "model"
      , Jsonaf.of_string
          {|{"id":"claude-opus-5-5","provider":"anthropic","key":"anthropic/claude-opus-5-5","name":"Claude Opus 5.5","context_window":200000,"max_output":64000,"supports_thinking":true,"cost":{"input":5,"output":25,"cache_read":0.5}}|}
      )
    ; "thinking", `String "on"
    ; "running", `False
    ; "message_count", `Number "0"
    ; "usage", Jsonaf.of_string {|{"input":0,"output":0,"cache_read":0}|}
    ; "cost_usd", `Number "0"
    ; "context_tokens", `Number "0"
    ; "active_host", `String "backend"
    ; "hosts", `Array []
    ; "subagents", `Array []
    ; "jobs", `Array []
    ]
  in
  let fields =
    List.map defaults ~f:(fun (k, v) ->
      k, Option.value (List.Assoc.find fields ~equal:String.equal k) ~default:v)
  in
  Jsonaf.to_string (`Object fields)
;;

let create () =
  let t = { model = App.init; pending = [] } in
  act t Start;
  reply t "get_state" (state_json ());
  reply t "list_models" "[]";
  reply t "get_messages" "[]";
  reply t "list_sessions" "[]";
  t
;;

let node t ?selector () =
  let node =
    Node_helpers.unsafe_convert_exn
      (View.view t.model ~inject:(fun _ -> Virtual_dom.Vdom.Effect.Ignore))
  in
  match selector with
  | None -> [ node ]
  | Some selector -> Node_helpers.select node ~selector
;;

let show ?selector t =
  List.iter (node t ?selector ()) ~f:(fun node ->
    print_endline (Node_helpers.to_string_html node))
;;

let text ?selector t =
  List.iter (node t ?selector ()) ~f:(fun node ->
    print_endline (Node_helpers.inner_text node))
;;
