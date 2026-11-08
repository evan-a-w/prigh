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
    ; hold : (unit -> unit) option ref
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

  (* The next request to the model waits until [release]; [wait_held]
     returns once it is waiting. *)
  let hold_next t =
    let held, set_held = Eio.Promise.create () in
    let released, release = Eio.Promise.create () in
    t.hold
    := Some
         (fun () ->
           Eio.Promise.resolve set_held ();
           Eio.Promise.await released);
    let wait_held () = Eio.Promise.await held in
    wait_held, fun () -> Eio.Promise.resolve release ()
  ;;

  let print_messages t messages =
    List.iter messages ~f:(function
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
      | Assistant _ -> ())
  ;;

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
    print_messages t (from_last_user [] (List.rev request.messages));
    Queue.clear t.requests
  ;;
end

let with_session ?tools replies f =
  with_sandbox
  @@ fun sandbox ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let hold = ref None in
  let provider =
    Faux_provider.create
      ~on_request:(fun request ->
        Queue.enqueue requests request;
        Option.iter !hold ~f:(fun wait ->
          hold := None;
          wait ()))
      (replies sandbox)
  in
  let tools = Option.value tools ~default:(Tools.all @ Tools.interaction) in
  let _, h = Test_rpc.make_server sandbox ~sw ~provider ~tools in
  f { S.sandbox; h; requests; hold }
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
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
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
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
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
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
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
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
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
    ; one ~id:"c8" (dir ^/ "link.png")
    ; one ~id:"c9" (dir ^/ "pipe")
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
  (* A symlink is followed; a FIFO is refused without being opened. *)
  Core_unix.symlink ~target:(dir ^/ "shot.png") ~link_name:(dir ^/ "link.png");
  Core_unix.mkfifo (dir ^/ "pipe") ~perm:0o600;
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "show"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    prompt: ok
    user: "show"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
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
    render_ui ok: Shown: image $DIR/link.png.
    render_ui error: Nothing was shown. Fix this and call render_ui again:
    - components[0].path: $DIR/pipe is not a regular file; give the absolute path of an image file.
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
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
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
    {"role":"user","text":"The user responded through \"next\" (render_ui call c1, component next):\n[\"a\",\"b \\\"quoted\\\"\",\"ünïcode\"]\n[Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]","origin":{"body":"","notes":[{"kind":"ui_action","title":"Sent \"next\": a, b \"quoted\", ünïcode","text":"The user responded through \"next\" (render_ui call c1, component next):\n[\"a\",\"b \\\"quoted\\\"\",\"ünïcode\"]"},{"kind":"client","title":"Using prigh-web","text":"[Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]"}]}}
    |}]
;;

let%expect_test "images on a tool host that is too old, then away" =
  with_session (fun sandbox ->
    let image =
      sprintf {|{"type": "image", "path": %S}|} (sandbox.dir ^/ "a.png")
    in
    [ render ~id:"c1" image
    ; Reply.text "old"
    ; render ~id:"c2" image
    ; Reply.text "away"
    ])
  @@ fun s ->
  let sent = Queue.create () in
  let laptop = Rpc_server.connect s.h.server ~send:(Queue.enqueue sent) in
  S.call_as
    s
    laptop
    ~params:
      (Test_rpc.hello_params
         ~session:(S.agent s)
         {|"name": "laptop", "tools": true|})
    "hello";
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "show"}|} "prompt";
  let exec = Test_rpc.wait_for_exec s.h laptop sent in
  printf
    "the host is asked for %s\n"
    (Json.string_exn (Option.value_exn (Json.member "name" exec)));
  S.call_as
    s
    laptop
    ~params:
      (sprintf
         {|{"exec_id": %S, "text": "unknown host tool \"$fs_stat\"", "is_error": true}|}
         (Json.string_exn (Option.value_exn (Json.member "exec_id" exec))))
    "tool_exec_result";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    hello: ok
    prompt: ok
    (host answered $instructions)
    the host is asked for $fs_stat
    tool_exec_result: ok
    user: "show"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
    render_ui error: Nothing was shown: cannot check the images (tool host laptop runs a prigh too old to show its files: update prigh there). Tell the user, or answer without images.
    |}];
  (* Gone: no waiting for it to come back. *)
  Rpc_server.disconnect s.h.server laptop;
  S.call_as s s.h.client ~params:{|{"text": "again"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    prompt: ok
    user: "again"
    render_ui error: Nothing was shown: cannot check the images (waiting for tool host "laptop" to reconnect; /host picks another). Tell the user, or answer without images.
    |}]
;;

let%expect_test
    "ui_action refusals: control characters, components that do not reply, \
     failed calls, attachments; a title over lines becomes one"
  =
  let components =
    {|{"type": "form", "id": "deploy", "fields": [{"name": "env", "label": "Env", "kind": "text"}]},
      {"type": "checklist", "id": "todo", "items": [{"text": "a"}]},
      {"type": "checklist", "id": "done", "items": [{"text": "a"}], "submit": "Send"},
      {"type": "callout", "id": "note", "text": "hi"}|}
  in
  with_session (fun _ ->
    [ render ~id:"c1" components
    ; render ~id:"c2" {|{"type": "tabel"}|}
    ; Reply.text "fill it in"
    ; Reply.text "thanks"
    ])
  @@ fun s ->
  write s.sandbox "notes.txt" "notes\n";
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "deploy"}|} "prompt";
  Agent.wait_idle (S.agent s);
  let action ?(extra = "") fields =
    S.call_as
      s
      s.h.client
      ~params:(sprintf {|{"text": "", "ui_action": {%s}%s}|} fields extra)
      "prompt"
  in
  action {|"call_id": "c1\n", "component": "deploy", "values": {}|};
  action {|"call_id": "c1", "component": "dep\u0007loy", "values": {}|};
  action {|"call_id": "c1", "component": "todo", "values": []|};
  action {|"call_id": "c1", "component": "note", "values": {}|};
  action {|"call_id": "c2", "component": "deploy", "values": {}|};
  action
    ~extra:{|, "attachments": ["notes.txt"]|}
    {|"call_id": "c1", "component": "deploy", "values": {}|};
  action
    ~extra:
      (sprintf
         {|, "images": [{"mime_type": "image/png", "data": %S}]|}
         (Base64.encode_exn png))
    {|"call_id": "c1", "component": "deploy", "values": {}|};
  [%expect
    {|
    hello: ok
    prompt: ok
    prompt: error "ui_action.call_id \"c1\\n\" contains control characters: send the id as the render_ui call has it"
    prompt: error "ui_action.component \"dep\\007loy\" contains control characters: send the id as the render_ui call has it"
    prompt: error "render_ui call c1 has no interactive component \"todo\" (its forms, choices and checklists with submit: deploy, done)"
    prompt: error "render_ui call c1 has no interactive component \"note\" (its forms, choices and checklists with submit: deploy, done)"
    prompt: error "render_ui call c2 failed, so nothing of it was shown: answer a component the session shows"
    prompt: error "a prompt with ui_action takes no attachments or images: send them in a message of their own"
    prompt: error "a prompt with ui_action takes no attachments or images: send them in a message of their own"
    |}];
  action
    {|"call_id": "c1", "component": "deploy", "title": "Deploy\n  options\t\u0001now ", "values": {"env": "a\nb"}|};
  S.print_last_turn s;
  [%expect
    {|
    prompt: ok
    user: ""
      note Sent "Deploy options now": env = a b: The user responded through "Deploy options now" (render_ui call c1, component deploy):
    {"env":"a\nb"}
    |}]
;;

(* The queue's JSON for an answer through a component: [ui_action] says what
   it is, and there is no text to give back to the composer. *)
let%expect_test "ui_action answers in the queue are dropped, not given back" =
  let form =
    {|{"type": "form", "id": "deploy", "title": "Deploy", "fields": [{"name": "env", "label": "Env", "kind": "text"}]}|}
  in
  with_session (fun _ ->
    [ render ~id:"c1" form
    ; Reply.text "fill it in"
    ; Reply.text "working"
    ; Reply.text "after"
    ])
  @@ fun s ->
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "deploy"}|} "prompt";
  Agent.wait_idle (S.agent s);
  let wait_held, release = S.hold_next s in
  S.call_as s s.h.client ~params:{|{"text": "meanwhile"}|} "prompt";
  wait_held ();
  let call meth params =
    let reply =
      Rpc_server.handle
        s.h.server
        s.h.client
        (Json.of_string
           (sprintf {|{"id": "r", "method": %S, "params": %s}|} meth params))
    in
    printf
      "%s: %s\n"
      meth
      (Json.to_string
         (match Json.member "result" reply with
          | Some result -> result
          | None -> Option.value_exn (Json.member "error" reply)))
  in
  let answer =
    {|{"text": "", "ui_action": {"call_id": "c1", "component": "deploy", "title": "Deploy", "values": {"env": "prod"}}}|}
  in
  call "follow_up" answer;
  call "get_pending" "{}";
  call "dequeue" "{}";
  call "follow_up" answer;
  call "remove_queued" {|{"queue": "follow_up", "index": 0}|};
  call "follow_up" answer;
  call "follow_up" {|{"text": "plain"}|};
  call "abort" "{}";
  release ();
  Agent.wait_idle (S.agent s);
  call "get_pending" "{}";
  [%expect
    {|
    hello: ok
    prompt: ok
    prompt: ok
    follow_up: {}
    get_pending: {"steer_texts":[],"follow_up_texts":["Sent \"Deploy\": env = prod"],"agents":[],"confirms":[],"questions":[]}
    dequeue: {"text":"","attachments":[],"ui_action":{"call_id":"c1","component":"deploy","summary":"Sent \"Deploy\": env = prod"}}
    follow_up: {}
    remove_queued: {"text":"","attachments":[],"ui_action":{"call_id":"c1","component":"deploy","summary":"Sent \"Deploy\": env = prod"}}
    follow_up: {}
    follow_up: {}
    abort: {"restored":["plain"]}
    get_pending: {"steer_texts":[],"follow_up_texts":[],"agents":[],"confirms":[],"questions":[]}
    |}]
;;

let%expect_test
    "prigh-tui steering, or following up, during a prigh-web run: render_ui \
     later in that run is refused"
  =
  let chart =
    {|{"type": "chart", "kind": "bar", "labels": ["a"], "series": [{"name": "s", "values": [1]}]}|}
  in
  with_session (fun _ ->
    [ render ~id:"c1" chart
    ; render ~id:"c2" chart
    ; Reply.text "done"
    ; render ~id:"c3" chart
    ; Reply.text "first"
    ; render ~id:"c4" chart
    ; Reply.text "done again"
    ])
  @@ fun s ->
  let web = s.h.client in
  let tui = Rpc_server.connect s.h.server ~send:ignore in
  S.call_as
    s
    tui
    ~params:(Test_rpc.hello_params ~session:(S.agent s) {|"client": "tui"|})
    "hello";
  S.call_as s web ~params:{|{"client": "web"}|} "hello";
  let run ~meth =
    let before = List.length (Agent.messages (S.agent s)) in
    let wait_held, release = S.hold_next s in
    S.call_as s web ~params:{|{"text": "show me"}|} "prompt";
    wait_held ();
    S.call_as s tui ~params:{|{"text": "from the terminal"}|} meth;
    release ();
    Agent.wait_idle (S.agent s);
    S.print_messages s (List.drop (Agent.messages (S.agent s)) before)
  in
  run ~meth:"steer";
  [%expect
    {|
    hello: ok
    hello: ok
    prompt: ok
    steer: ok
    user: "show me"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
    render_ui ok: Shown: chart.
    user: "from the terminal"
      note Using prigh-tui: [Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]
    render_ui error: The user is on prigh-tui, which cannot display components: answer in text.
    |}];
  run ~meth:"follow_up";
  [%expect
    {|
    prompt: ok
    follow_up: ok
    user: "show me"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
    render_ui ok: Shown: chart.
    user: "from the terminal"
      note Using prigh-tui: [Client: the user is on prigh-tui, which cannot display components: do not call render_ui; answer in text (markdown is fine).]
    render_ui error: The user is on prigh-tui, which cannot display components: answer in text.
    |}]
;;

let%expect_test
    "a second hello without client keeps the first's; commands' prompts carry \
     their client"
  =
  with_session (fun _ -> [ Reply.text "ok"; Reply.text "written" ])
  @@ fun s ->
  S.call_as s s.h.client ~params:{|{"client": "web"}|} "hello";
  S.call_as s s.h.client ~params:{|{"cwd": "/tmp"}|} "hello";
  S.call_as s s.h.client ~params:{|{"text": "hi"}|} "prompt";
  S.print_last_turn s;
  [%expect
    {|
    hello: ok
    hello: ok
    prompt: ok
    user: "hi"
      note Using prigh-web: [Client: the user is on prigh-web, which renders render_ui components inline. Use them rarely: prose and markdown are the default, and render_ui is only for content that is clearly better as a component (a chart, images, a diff). Never use it to ask questions or get choices or input: ask in prose or with ask_user.]
    |}];
  let tui = Rpc_server.connect s.h.server ~send:ignore in
  S.call_as
    s
    tui
    ~params:(Test_rpc.hello_params ~session:(S.agent s) {|"client": "tui"|})
    "hello";
  S.call_as s tui "init";
  Agent.wait_idle (S.agent s);
  (match List.last_exn (Queue.last_exn s.requests).messages with
   | Message.User u ->
     List.iter
       (Option.value_map
          (Injected.of_message ~origin:u.origin u.text)
          ~default:[]
          ~f:(fun (i : Injected.t) -> i.notes))
       ~f:(fun n -> print_endline (Injected.Note.title n))
   | _ -> print_endline "not a user message");
  [%expect
    {|
    hello: ok
    init: ok
    Using prigh-tui
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
    ("it is the main agent's (the user's plan, questions, components, goal and memory), and you are a subagent: put what you need from the user in your report")
    |}]
;;
