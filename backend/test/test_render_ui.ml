open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* [render_ui] in a session over RPC: which client prompted decides whether
   it runs and what the model is told; answers through components come back
   as [ui_action] prompts. *)

let png =
  Base64.decode_exn
    "iVBORw0KGgoAAAANSUhEUgAAAAMAAAACAQMAAACnuvRZAAAAA1BMVEX/AAAZ4gk3AAAADElEQVQI12NgYGAAAAAEAAEnNCcKAAAAAElFTkSuQmCC"
;;

let render ~id components =
  Reply.tool_call
    ~id
    ~name:"render_ui"
    ~arguments:(sprintf {|{"components": [%s]}|} components)
    ()
;;

module S = struct
  type t =
    { sandbox : Tool_test_helpers.t
    ; h : Test_rpc.H.t
    ; requests : Provider.Request.t Queue.t
    }

  let call_as t client ?(params = "{}") meth =
    let reply =
      Rpc_server.handle
        t.h.server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": %S, "params": %s}|} meth params))
    in
    match Json.member "error" reply with
    | Some error -> printf "%s: error %s\n" meth (Json.to_string error)
    | None -> printf "%s: ok\n" meth
  ;;

  let agent t = Test_rpc.current t.h

  (* What the model got last: the user message's text and notes, then the
     results of the tool calls since. *)
  let print_last_turn t =
    Agent.wait_idle (agent t);
    let request : Provider.Request.t = Queue.last_exn t.requests in
    let rec from_last_user acc = function
      | [] -> acc
      | (Message.User _ as m) :: _ -> m :: acc
      | m :: rest -> from_last_user (m :: acc) rest
    in
    List.iter
      (from_last_user [] (List.rev request.messages))
      ~f:(function
        | Message.User u ->
          let notes =
            Option.value_map
              (Injected.of_message ~origin:u.origin u.text)
              ~default:[]
              ~f:(fun (i : Injected.t) -> i.notes)
          in
          printf "user: %S\n" (Message.User.body u);
          List.iter notes ~f:(fun n ->
            printf "  note %s: %s\n" (Injected.Note.title n) n.text)
        | Tool_result r ->
          printf
            "%s %s: %s\n"
            r.tool_name
            (if r.is_error then "error" else "ok")
            (mask t.sandbox r.text)
        | Assistant _ -> ());
    Queue.clear t.requests
  ;;
end

let with_session ?tools replies f =
  with_sandbox
  @@ fun sandbox ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create ~on_request:(Queue.enqueue requests) (replies sandbox)
  in
  let tools = Option.value tools ~default:(Tools.all @ Tools.interaction) in
  let _, h = Test_rpc.make_server sandbox ~sw ~provider ~tools in
  f { S.sandbox; h; requests }
;;

let%expect_test
    "hello's client: a TUI and prigh-web on one session; the latest prompt's \
     client decides, and the model is told of each switch"
  =
  let chart =
    {|{"type": "chart", "kind": "bar", "title": "Build times", "labels": ["a", "b"],
       "series": [{"name": "s", "values": [1, 2]}]}|}
  in
  with_session (fun _ ->
    [ render ~id:"c1" chart
    ; Reply.text "shown"
    ; render ~id:"c2" chart
    ; Reply.text "in text then"
    ; render ~id:"c3" chart
    ; Reply.text "shown again"
    ; Reply.text "still web"
    ])
  @@ fun s ->
  let web = s.h.client in
  S.call_as s web ~params:{|{"client": "web"}|} "hello";
  let tui = Rpc_server.connect s.h.server ~send:ignore in
  S.call_as
    s
    tui
    ~params:(Test_rpc.hello_params ~session:(S.agent s) {|"client": "tui"|})
    "hello";
  S.call_as s web ~params:{|{"text": "how fast?"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    hello: ok
    prompt: ok
    user: "how fast?"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]
    render_ui ok: Shown: chart "Build times".
    |}];
  S.call_as s tui ~params:{|{"text": "and now?"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    prompt: ok
    user: "and now?"
      note Using prigh-tui: [Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]
    render_ui error: The user is on prigh-tui, which cannot display components: answer in text.
    |}];
  S.call_as s web ~params:{|{"text": "back"}|} "follow_up";
  S.print_last_turn s;
  S.call_as s web ~params:{|{"text": "more"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    follow_up: ok
    user: "back"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]
    render_ui ok: Shown: chart "Build times".
    prompt: ok
    user: "more"
    |}];
  (* A client that does not say is another client; so is an unknown one. *)
  let other = Rpc_server.connect s.h.server ~send:ignore in
  S.call_as
    s
    other
    ~params:(Test_rpc.hello_params ~session:(S.agent s) {|"client": "emacs"|})
    "hello";
  print_s [%sexp (Client_kind.of_hello (Some "emacs") : Client_kind.t)];
  print_s [%sexp (Client_kind.of_hello None : Client_kind.t)];
  [%expect
    {|
    hello: ok
    Other
    Other
    |}]
;;

let%expect_test
    "the told client survives a reload: no note for the same client, one for \
     another"
  =
  with_session (fun _ -> List.init 3 ~f:(fun _ -> Reply.text "ok"))
  @@ fun s ->
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "one"}|} "prompt";
  S.print_last_turn s;
  let path = (Agent.state (S.agent s)).session_path in
  S.call_as s s.h.client "new_session";
  S.call_as
    s
    s.h.client
    ~params:(sprintf {|{"path": %S}|} path)
    "switch_session";
  S.call_as s s.h.client ~params:{|{"text": "two"}|} "prompt";
  S.print_last_turn s;
  S.call_as s s.h.client ~params:{|{"client": "tui"}|} "hello";
  S.call_as
    s
    s.h.client
    ~params:(sprintf {|{"path": %S}|} path)
    "switch_session";
  S.call_as s s.h.client ~params:{|{"text": "three"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    prompt: ok
    user: "one"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]
    new_session: ok
    switch_session: ok
    prompt: ok
    user: "two"
    hello: ok
    switch_session: ok
    prompt: ok
    user: "three"
      note Using prigh-tui: [Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]
    |}]
;;

let%expect_test "invalid components: every problem by path, nothing shown" =
  with_session (fun _ ->
    [ render
        ~id:"c1"
        {|{"type": "chart", "kind": "line", "labels": ["a", "b", "c"],
           "series": [{"name": "s", "values": [1, 2]}]},
          {"type": "tabel"}|}
    ; Reply.text "oops"
    ])
  @@ fun s ->
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "go"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    prompt: ok
    user: "go"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]
    render_ui error: Nothing was shown. Fix these and call render_ui again:
    - components[0].series[0].values: 2 values for 3 labels; give one per label (null for a gap).
    - components[1].type: unknown type "tabel"; did you mean "table"?
    |}]
;;

let%expect_test "images are checked on the tool host" =
  with_session (fun sandbox ->
    let dir = sandbox.dir in
    let image path = sprintf {|{"type": "image", "path": %S}|} path in
    let one ~id path = render ~id (image path) in
    [ one ~id:"c1" (dir ^/ "shot.png")
    ; one ~id:"c2" "shot.png"
    ; one ~id:"c3" (dir ^/ "missing.png")
    ; one ~id:"c4" (dir ^/ "notes.txt")
    ; one ~id:"c5" (dir ^/ "huge.png")
    ; one ~id:"c6" dir
    ; render
        ~id:"c7"
        (sprintf
           {|{"type": "gallery", "images": [{"path": %S}, {"path": %S}]}|}
           (dir ^/ "logo.svg")
           (dir ^/ "notes.txt"))
    ; Reply.text "done"
    ])
  @@ fun s ->
  let dir = s.sandbox.dir in
  Out_channel.write_all (dir ^/ "shot.png") ~data:png;
  Out_channel.write_all (dir ^/ "notes.txt") ~data:"just text\n";
  Out_channel.write_all
    (dir ^/ "logo.svg")
    ~data:{|<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"/>|};
  (* Sparse: 21 MB that take no space, with a PNG's header. *)
  Out_channel.with_file (dir ^/ "huge.png") ~f:(fun oc ->
    Out_channel.output_string oc png;
    Out_channel.seek oc 21_000_000L;
    Out_channel.output_string oc "x");
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "show"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    prompt: ok
    user: "show"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]
    render_ui ok: Shown: image $DIR/shot.png.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].path: "shot.png" is relative; give the absolute path: use $DIR/shot.png.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].path: $DIR/missing.png does not exist; give the absolute path of an image file.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].path: $DIR/notes.txt is not an image (png, jpeg, gif, webp or svg); give an image file.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].path: $DIR/huge.png is 21.0 MB, over the 20 MB that are shown; make a smaller copy and show that.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].path: $DIR is a directory; give the absolute path of an image file.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].images[1].path: $DIR/notes.txt is not an image (png, jpeg, gif, webp or svg); give an image file.
    |}]
;;

let%expect_test
    "answers through components: the exact text, the origin, and errors"
  =
  let form =
    {|{"type": "form", "id": "deploy", "title": "Deploy options",
       "fields": [{"name": "env", "label": "Env", "kind": "select", "options": ["dev", "prod"]},
                  {"name": "replicas", "label": "Replicas", "kind": "number"}]},
      {"type": "card", "children": [{"type": "choices", "id": "next", "options": [{"label": "a"}]}]}|}
  in
  with_session (fun _ ->
    [ render ~id:"c1" form
    ; Reply.text "fill it in"
    ; Reply.text "deploying"
    ; Reply.text "noted"
    ])
  @@ fun s ->
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "deploy"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    prompt: ok
    user: "deploy"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]
    render_ui ok: Shown: form "Deploy options", card. The user can answer through form "Deploy options", choices "next"; an answer arrives as their next message.
    |}];
  let action ?(text = {|""|}) ?(meth = "prompt") fields =
    S.call_as
      s
      s.h.client
      ~params:(sprintf {|{"text": %s, "ui_action": {%s}}|} text fields)
      meth
  in
  action
    {|"call_id": "c9", "component": "deploy", "title": "Deploy options", "values": {}|};
  action
    {|"call_id": "c1", "component": "deplyo", "title": "Deploy options", "values": {}|};
  action ~text:{|"hi"|} {|"call_id": "c1", "component": "deploy", "values": {}|};
  action ~meth:"steer" {|"call_id": "c1", "component": "deploy", "values": {}|};
  action {|"call_id": "c1", "component": "deploy"|};
  action {|"component": "deploy", "values": 1|};
  S.call_as s s.h.client ~params:{|{"text": "", "ui_action": 3}|} "prompt";
  [%expect
    {|
    prompt: error "unknown render_ui call \"c9\" in this session: answer a component the session shows"
    prompt: error "render_ui call c1 has no interactive component \"deplyo\" (its forms, choices and checklists with submit: deploy, next)"
    prompt: error "a prompt with ui_action takes no text: send text \"\""
    steer: error "steer does not take ui_action: send it with prompt, or follow_up while a run is in progress"
    prompt: error "ui_action needs \"values\""
    prompt: error "ui_action needs \"call_id\""
    prompt: error "param \"ui_action\" must be an object {call_id, component, title, values}"
    |}];
  (* From prigh-tui too, which then tells the model of the switch after the
     answer. *)
  let tui = Rpc_server.connect s.h.server ~send:ignore in
  S.call_as
    s
    tui
    ~params:(Test_rpc.hello_params ~session:(S.agent s) {|"client": "tui"|})
    "hello";
  S.call_as
    s
    tui
    ~params:
      {|{"text": "", "ui_action": {"call_id": "c1", "component": "deploy", "title": "Deploy options", "values": {"env": "prod", "replicas": 3}}}|}
    "prompt";
  Agent.wait_idle (S.agent s);
  let request : Provider.Request.t = Queue.last_exn s.requests in
  (match List.last_exn request.messages with
   | Message.User u -> print_endline u.text
   | _ -> print_endline "not a user message");
  let user =
    List.find_map_exn
      (List.rev (Agent.messages (S.agent s)))
      ~f:(function
        | Message.User u -> Some u
        | _ -> None)
  in
  print_endline (Json.to_string (Message.jsonaf_of_t (User user)));
  Queue.clear s.requests;
  [%expect
    {|
    hello: ok
    prompt: ok
    The user responded through "Deploy options" (render_ui call c1, component deploy):
    {"env":"prod","replicas":3}
    [Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]
    ["User",{"text":"The user responded through \"Deploy options\" (render_ui call c1, component deploy):\n{\"env\":\"prod\",\"replicas\":3}\n[Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]","origin":{"notes":[{"kind":["Ui_action",{"call_id":"c1","component":"deploy","summary":"Sent \"Deploy options\": env = prod, replicas = 3"}],"text":"The user responded through \"Deploy options\" (render_ui call c1, component deploy):\n{\"env\":\"prod\",\"replicas\":3}"},{"kind":["Client",{"client":["Tui"]}],"text":"[Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]"}],"attachments":[]}}]
    |}];
  (* What frontends get: the note's title is the summary, the body empty. *)
  S.call_as
    s
    s.h.client
    ~params:
      {|{"text": "", "ui_action": {"call_id": "c1", "component": "next", "values": ["a", "b \"quoted\"", "ünïcode"]}}|}
    "prompt";
  Agent.wait_idle (S.agent s);
  let user_messages =
    List.filter_map (Queue.to_list s.h.sent) ~f:(fun m ->
      match Json.member "event" m, Json.member "message" m with
      | Some (`String "message_end"), Some (`Object fields) ->
        (match List.Assoc.find fields "role" ~equal:String.equal with
         | Some (`String "user") ->
           Some
             (`Object
                 (List.filter fields ~f:(fun (k, _) ->
                    not (String.equal k "at"))))
         | _ -> None)
      | _ -> None)
  in
  print_endline (Json.to_string (List.last_exn user_messages));
  [%expect
    {|
    prompt: ok
    {"role":"user","text":"The user responded through \"next\" (render_ui call c1, component next):\n[\"a\",\"b \\\"quoted\\\"\",\"ünïcode\"]\n[Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]","origin":{"body":"","notes":[{"kind":"ui_action","title":"Sent \"next\": a, b \"quoted\", ünïcode","text":"The user responded through \"next\" (render_ui call c1, component next):\n[\"a\",\"b \\\"quoted\\\"\",\"ünïcode\"]"},{"kind":"client","title":"Using prigh-web","text":"[Client: the user is on prigh-web, which renders render_ui components inline: use them when a visual or interactive answer is clearer than prose.]"}]}}
    |}]
;;

let%expect_test "only the main agent has render_ui" =
  let names depth =
    Tools.for_context ~parent:(Tools.all @ Tools.interaction) ~depth ()
    |> Or_error.ok_exn
    |> List.map ~f:Tool.name
    |> List.filter ~f:(String.equal "render_ui")
  in
  print_s [%sexp (names 0 : string list), (names 1 : string list)];
  print_s [%sexp (Tools.unavailable ~depth:1 "render_ui" : string option)];
  [%expect
    {|
    ((render_ui) ())
    ("it is the main agent's (the user's plan, questions, components and goal), and you are a subagent: put what you need from the user in your report")
    |}]
;;
