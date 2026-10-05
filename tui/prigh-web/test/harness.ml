open! Core
open Prigh_web
module Node_helpers = Virtual_dom_test_helpers.Node_helpers

type t =
  { mutable model : App.Model.t
  ; mutable pending : (string * App.Reply_tag.t) list
  ; mutable quiet : bool
  }

let model t = t.model

let act t action =
  let model, commands = App.update t.model action in
  t.model <- model;
  List.iter commands ~f:(fun (command : App.Command.t) ->
    (match command with
     | Rpc { method_; tag; _ } -> t.pending <- t.pending @ [ method_, tag ]
     | Reconnect _
     | Set_url_session _
     | Expire_toast _
     | Focus _
     | Save_history _
     | Sign_out
     | Reveal _
     | Copy _
     | Switch_account _
     | Add_account
     | Scroll_chat _
     | Jump_to_user_message _ -> ());
    if not t.quiet then print_s [%sexp (command : App.Command.t)])
;;

let reply t method_ json =
  match List.findi t.pending ~f:(fun _ (m, _) -> String.equal m method_) with
  | None -> raise_s [%message "no pending request" method_]
  | Some (i, (_, tag)) ->
    t.pending <- List.filteri t.pending ~f:(fun j _ -> j <> i);
    act t (Reply (tag, Ok (Jsonaf.of_string json)))
;;

let fail t method_ error =
  match List.findi t.pending ~f:(fun _ (m, _) -> String.equal m method_) with
  | None -> raise_s [%message "no pending request" method_]
  | Some (i, (_, tag)) ->
    t.pending <- List.filteri t.pending ~f:(fun j _ -> j <> i);
    act t (Reply (tag, Error error))
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

let model_json ?(thinking = true) ~provider ~id ~name () =
  sprintf
    {|{"id":"%s","provider":"%s","key":"%s/%s","name":"%s","context_window":200000,"max_output":64000,"supports_thinking":%b,"cost":{"input":3,"output":15,"cache_read":0.3}}|}
    id
    provider
    provider
    id
    name
    thinking
;;

let models_json =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ model_json ~provider:"openai" ~id:"gpt-6" ~name:"GPT-6" ()
       ; model_json
           ~provider:"anthropic"
           ~id:"claude-opus-5-5"
           ~name:"Claude Opus 5.5"
           ()
       ; model_json
           ~provider:"anthropic"
           ~id:"claude-sonnet-5"
           ~name:"Claude Sonnet 5"
           ()
       ; model_json
           ~thinking:false
           ~provider:"deepseek"
           ~id:"deepseek-chat"
           ~name:"DeepSeek Chat"
           ()
       ])
;;

let auth_json =
  {|[{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Claude subscription"},{"method":"api_key","label":"API key"}],"configured":{"method":"oauth","source":"auth.json"}},
     {"provider":"openai","name":"OpenAI","methods":[{"method":"api_key","label":"API key"}],"configured":null},
     {"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":null}]|}
;;

let session_json
      ?name
      ?description
      ?first_prompt
      ?(cwd = "/work")
      ?(updated_at = "2026-10-05 09:58:00Z")
      ?(messages = 4)
      ?(live = false)
      ?(running = false)
      id
  =
  let opt = Option.value_map ~default:"null" ~f:(sprintf "%S") in
  sprintf
    {|{"id":"%s","path":"/sessions/%s.jsonl","name":%s,"description":%s,"cwd":"%s","created_at":"2026-10-01 08:00:00Z","updated_at":"%s","first_prompt":%s,"message_count":%d,"parent":null,"live":%b,"running":%b,"clients":0}|}
    id
    id
    (opt name)
    (opt description)
    cwd
    updated_at
    (opt first_prompt)
    messages
    live
    running
;;

let config_json =
  {|{"scoped_models":[],"confirm_tools":false,"default_model":null,"default_thinking":null}|}
;;

let now = Time_ns.of_string_with_utc_offset "2026-10-05 10:00:00Z"

let key
      ?(shift = false)
      ?(alt = false)
      ?(ctrl = false)
      ?(meta = false)
      ?(selection = false)
      ?code
      ?target
      t
      key
  =
  let target : Keys.Target.t =
    match target with
    | Some target -> target
    | None -> Editor { cursor = String.length t.model.draft }
  in
  let code =
    match code with
    | Some code -> code
    | None when String.length key = 1 && Char.is_alpha key.[0] ->
      "Key" ^ String.uppercase key
    | None -> key
  in
  match
    Keys.handle t.model { key; code; shift; alt; ctrl; meta; selection; target }
  with
  | None -> print_endline "(browser default)"
  | Some action ->
    print_s [%sexp (action : App.Action.t)];
    act t action
;;

let type_ t text = act t (Edit { text; cursor = String.length text })

let create ?(verbose = false) ?(sessions = "[]") ?(state = state_json ()) () =
  let t = { model = App.init; pending = []; quiet = not verbose } in
  act t Start;
  reply t "get_state" state;
  reply t "list_models" models_json;
  reply t "auth_status" auth_json;
  reply t "get_config" config_json;
  reply t "get_messages" "[]";
  reply t "list_sessions" sessions;
  act t (Tick now);
  t.quiet <- false;
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

let block_tags =
  [ "div"
  ; "p"
  ; "li"
  ; "tr"
  ; "h1"
  ; "h2"
  ; "h3"
  ; "header"
  ; "footer"
  ; "aside"
  ; "main"
  ; "pre"
  ; "ul"
  ; "table"
  ; "label"
  ; "section"
  ; "details"
  ; "summary"
  ]
;;

let render node =
  let lines = Queue.create () in
  let line = Buffer.create 80 in
  let flush () =
    let l = String.strip (Buffer.contents line) in
    if not (String.is_empty l) then Queue.enqueue lines l;
    Buffer.clear line
  in
  let add s =
    let s = String.strip s in
    if not (String.is_empty s)
    then (
      if Buffer.length line > 0 then Buffer.add_char line ' ';
      Buffer.add_string line s)
  in
  let rec inline (node : Node_helpers.t) =
    match node with
    | Text s -> [ String.strip s ]
    | Widget -> []
    | Element { tag_name = "icon"; _ } -> []
    | Element e -> List.concat_map e.children ~f:inline
  in
  let rec walk (node : Node_helpers.t) =
    match node with
    | Text s -> add s
    | Widget -> ()
    | Element { tag_name = "icon"; _ } -> ()
    | Element ({ tag_name = "input" | "textarea"; _ } as e) ->
      let value =
        List.Assoc.find e.string_properties ~equal:String.equal "value"
        |> Option.value ~default:""
      in
      add ("[" ^ value ^ "]")
    | Element ({ tag_name = "button" | "a"; _ } as e) ->
      let label =
        match
          List.filter
            (List.concat_map e.children ~f:inline)
            ~f:(Fn.non String.is_empty)
        with
        | [] ->
          List.Assoc.find e.attributes ~equal:String.equal "title"
          |> Option.value ~default:""
        | words -> String.concat ~sep:" " words
      in
      add ("(" ^ label ^ ")")
    | Element e when List.mem block_tags e.tag_name ~equal:String.equal ->
      flush ();
      List.iter e.children ~f:walk;
      flush ()
    | Element e -> List.iter e.children ~f:walk
  in
  walk node;
  flush ();
  Queue.iter lines ~f:print_endline
;;

let text ?selector t = List.iter (node t ?selector ()) ~f:render
