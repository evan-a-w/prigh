open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* The review: comments the user leaves on changed lines, sent to the agent,
   which replies with [reply_to_comment] (possibly proposing edits the user
   accepts or rejects); approvals of files by hash; all recorded in the
   session. *)

let with_agent ?on_request replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let provider = Faux_provider.create ?on_request replies in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir ".sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  f t agent
;;

let show_review t (r : Review_state.t) =
  List.iter r.comments ~f:(fun c ->
    printf
      "%s %s:%s %s\n"
      c.id
      (mask t (Review_comment.file c))
      (Review_comment.Range.to_string c.range)
      (Review_comment.Status.to_string c.status);
    List.iter c.thread ~f:(fun p ->
      printf
        "  %s%s: %s%s\n"
        (match p.author with
         | User -> "user"
         | Agent -> "agent")
        (if p.delivered then "" else " (unsent)")
        p.text
        (match p.proposal with
         | None -> ""
         | Some { edits; state } ->
           sprintf
             " [proposes %d edit%s: %s]"
             (List.length edits)
             (if List.length edits = 1 then "" else "s")
             (match state with
              | Failed e -> "failed: " ^ mask t e
              | state -> Review_comment.Proposal.State.to_string state))));
  Map.iteri r.approvals ~f:(fun ~key:{ root; path } ~data ->
    printf
      "approved %s%s at %s\n"
      (Option.value_map root ~default:"" ~f:(fun r -> mask t r ^ ": "))
      path
      data)
;;

let print_result t = function
  | Ok _ -> ()
  | Error e -> printf "error: %s\n" (mask t (Error.to_string_hum e))
;;

let comment t agent ~path ~first ~last ~anchor text =
  Agent.update_review agent ~f:(fun r ->
    Review_state.add
      r
      ~root:t.dir
      ~path
      ~range:(Review_comment.Range.lines ~first ~last)
      ~anchor
      ~text
      ~at:0.)
  |> Result.map ~f:(fun (c : Review_comment.t) -> c.id)
;;

let reply_call ~id args =
  Reply.tool_call ~id ~name:"reply_to_comment" ~arguments:args ()
;;

let%expect_test "comments reach the agent, which replies and proposes edits" =
  let tools = Queue.create () in
  let sent = Queue.create () in
  with_agent
    ~on_request:(fun request ->
      Queue.enqueue
        tools
        (List.exists request.tools ~f:(fun s ->
           String.equal s.name "reply_to_comment"));
      match List.last request.messages with
      | Some (Message.User u) -> Queue.enqueue sent u.text
      | _ -> ())
    [ Reply.text "hello"
    ; Reply.tool_calls
        [ ( "r1"
          , "reply_to_comment"
          , {|{"id":"c1","text":"Renamed it.","resolve":true}|} )
        ; ( "r2"
          , "reply_to_comment"
          , {|{"id":"c2","text":"How about this?","edits":[{"old_text":"two","new_text":"2"}]}|}
          )
        ; "r3", "reply_to_comment", {|{"id":"c9","text":"?"}|}
        ]
    ; Reply.text "Done."
    ; reply_call
        ~id:"r4"
        {|{"id":"c2","text":"Then this.","edits":[{"old_text":"nowhere","new_text":"x"}]}|}
    ; Reply.text "Proposed."
    ]
  @@ fun t agent ->
  let events = Queue.create () in
  Agent.subscribe agent ~f:(function
    | Review_changed r ->
      Queue.enqueue events (sprintf "review: %d" (List.length r.comments))
    | _ -> ());
  write t "a.txt" "one\ntwo\nthree\n";
  (* No comments: no tool. *)
  Or_error.ok_exn (Agent.prompt agent "hi");
  Agent.wait_idle agent;
  print_result t (Agent.submit_review agent);
  [%expect
    {| error: no new comments to send: select text in a changed file to comment on it |}];
  print_result
    t
    (comment t agent ~path:"a.txt" ~first:1 ~last:1 ~anchor:"one" "  ");
  print_result t (comment t agent ~path:"a.txt" ~first:3 ~last:2 ~anchor:"" "x");
  [%expect
    {|
    error: the comment is empty: write something first
    error: lines 3-2 are not a range of lines: the first is 1 or more and the last no smaller
    |}];
  print_result
    t
    (comment
       t
       agent
       ~path:"a.txt"
       ~first:1
       ~last:1
       ~anchor:"one"
       "Name it better");
  print_result
    t
    (comment
       t
       agent
       ~path:"a.txt"
       ~first:2
       ~last:3
       ~anchor:"two\nthree"
       "Use digits?");
  show_review t (Agent.review_state agent);
  [%expect
    {|
    c1 $DIR/a.txt:1 open
      user (unsent): Name it better
    c2 $DIR/a.txt:2-3 open
      user (unsent): Use digits?
    |}];
  print_s [%sexp (Agent.submit_review agent : int Or_error.t)];
  Agent.wait_idle agent;
  [%expect {| (Ok 2) |}];
  (* What the model was sent, and the message's origin. *)
  print_endline (mask t (List.nth_exn (Queue.to_list sent) 1));
  [%expect
    {|
    The user left review comments on the changes in a worktree (each comment's file is its absolute path, in the worktree it names: edit that file, even when it is not under your directory; line numbers are as when they commented: the file may have changed since). Answer every comment with reply_to_comment: say what you did, or why not. Make the changes they ask for; where the user should decide, propose the change with the reply's edits instead of making it (it is applied when they accept it). Resolve a comment (resolve: true) when it needs nothing more. Posts marked new are what you have not seen yet. A comment on text the user selected marks it ⟦like this⟧ in its code and quotes it in <selected>; one with side="old" is on lines the change removed, numbered as in the file before it.

    <comment id="c1" worktree="$DIR" file="$DIR/a.txt" lines="1" status="open">
    <code>
    1  one
    </code>
    <post from="user" new="true">
    Name it better
    </post>
    </comment>

    <comment id="c2" worktree="$DIR" file="$DIR/a.txt" lines="2-3" status="open">
    <code>
    2  two
    3  three
    </code>
    <post from="user" new="true">
    Use digits?
    </post>
    </comment>
    |}];
  List.iter
    (Session.messages (Agent.session agent))
    ~f:(function
      | User { origin = Some origin; _ } ->
        print_s [%sexp (origin.source : Injected.Source.t option)]
      | Tool_result r -> print_endline (mask t r.text)
      | _ -> ());
  [%expect
    {|
    ((Review_comments (count 2)))
    replied to c1; it is resolved
    replied to c2, proposing the change for the user to accept
    no review comment "c9" (the comments: c1, c2)
    |}];
  print_s [%sexp (Queue.to_list tools : bool list)];
  [%expect {| (false true true) |}];
  show_review t (Agent.review_state agent);
  [%expect
    {|
    c1 $DIR/a.txt:1 resolved
      user: Name it better
      agent: Renamed it.
    c2 $DIR/a.txt:2-3 open
      user: Use digits?
      agent: How about this? [proposes 1 edit: pending]
    |}];
  (* Accepting applies the edits on the host; the agent hears of it with the
     next submission. *)
  print_result t (Agent.decide_proposal agent ~id:"c2" ~post:0 ~accept:true);
  print_result t (Agent.decide_proposal agent ~id:"c2" ~post:1 ~accept:true);
  print_result t (Agent.decide_proposal agent ~id:"c2" ~post:1 ~accept:false);
  print_endline (read t "a.txt");
  [%expect
    {|
    error: c2 has no proposed change at post 0
    error: that change was accepted already
    one
    2
    three
    |}];
  print_result
    t
    (Agent.update_review agent ~f:(fun r ->
       Review_state.reply r ~id:"c2" ~author:User ~text:"Thanks" ~at:0. ()));
  print_s [%sexp (Agent.submit_review agent : int Or_error.t)];
  Agent.wait_idle agent;
  print_endline (mask t (List.last_exn (Queue.to_list sent)));
  [%expect
    {|
    (Ok 1)
    The user left review comments on the changes in a worktree (each comment's file is its absolute path, in the worktree it names: edit that file, even when it is not under your directory; line numbers are as when they commented: the file may have changed since). Answer every comment with reply_to_comment: say what you did, or why not. Make the changes they ask for; where the user should decide, propose the change with the reply's edits instead of making it (it is applied when they accept it). Resolve a comment (resolve: true) when it needs nothing more. Posts marked new are what you have not seen yet. A comment on text the user selected marks it ⟦like this⟧ in its code and quotes it in <selected>; one with side="old" is on lines the change removed, numbered as in the file before it.

    <comment id="c2" worktree="$DIR" file="$DIR/a.txt" lines="2-3" status="open">
    <code>
    2  two
    3  three
    </code>
    <post from="user">
    Use digits?
    </post>
    <post from="you" proposal="accepted">
    How about this?
    </post>
    <post from="user" new="true">
    Accepted your proposed change: it is applied.
    </post>
    <post from="user" new="true">
    Thanks
    </post>
    </comment>
    |}];
  (* A proposal that no longer applies fails, and says why. *)
  print_result t (Agent.decide_proposal agent ~id:"c2" ~post:4 ~accept:true);
  show_review t (Agent.review_state agent);
  [%expect
    {|
    c1 $DIR/a.txt:1 resolved
      user: Name it better
      agent: Renamed it.
    c2 $DIR/a.txt:2-3 open
      user: Use digits?
      agent: How about this? [proposes 1 edit: accepted]
      user: Accepted your proposed change: it is applied.
      user: Thanks
      agent: Then this. [proposes 1 edit: failed: edit 1: old_text not found in file; read the file to get its current text]
      user (unsent): Accepted your proposed change, but it no longer applies (edit 1: old_text not found in file; read the file to get its current text): the file changed since.
    |}];
  (* Status, approvals, deletion. *)
  print_result
    t
    (Agent.update_review agent ~f:(fun r ->
       Review_state.set_status r ~id:"c1" Open));
  print_result
    t
    (Agent.update_review agent ~f:(fun _ ->
       Ok ((), Review_state.approve ~path:"a.txt" ~hash:"h1" ())));
  print_result
    t
    (Agent.update_review agent ~f:(fun r ->
       Or_error.map (Review_state.delete r ~id:"c1") ~f:(fun c -> (), c)));
  print_result
    t
    (Agent.update_review agent ~f:(fun r ->
       Or_error.map (Review_state.delete r ~id:"c1") ~f:(fun c -> (), c)));
  [%expect {| error: no review comment "c1" (the comments: c2) |}];
  let review = Agent.review_state agent in
  (* Everything is in the session file, also after a rewind. *)
  let first_message =
    List.find_map_exn
      (Session.entries (Agent.session agent))
      ~f:(fun (e : Session.Entry.t) ->
        match e.payload with
        | Message _ -> Some e.id
        | _ -> None)
  in
  Or_error.ok_exn (Agent.rewind agent ~to_:first_message);
  let loaded =
    Or_error.ok_exn (Session.load (Session.path (Agent.session agent)))
  in
  print_s
    [%sexp
      (Review_state.equal review (Session.review loaded) : bool)
    , (Review_state.equal review (Agent.review_state agent) : bool)];
  show_review t (Session.review loaded);
  print_endline (String.concat ~sep:", " (Queue.to_list events));
  [%expect
    {|
    (true true)
    c2 $DIR/a.txt:2-3 open
      user: Use digits?
      agent: How about this? [proposes 1 edit: accepted]
      user: Accepted your proposed change: it is applied.
      user: Thanks
      agent: Then this. [proposes 1 edit: failed: edit 1: old_text not found in file; read the file to get its current text]
      user (unsent): Accepted your proposed change, but it no longer applies (edit 1: old_text not found in file; read the file to get its current text): the file changed since.
    approved a.txt at h1
    review: 1, review: 2, review: 2, review: 2, review: 2, review: 2, review: 2, review: 2, review: 2, review: 2, review: 2, review: 2, review: 1
    |}]
;;

let dispatch t agent meth params =
  match Review_rpc.dispatch agent ~meth ~params:(Jsonaf.of_string params) with
  | None -> print_endline "not a review method"
  | Some (Ok json) ->
    print_endline
      (mask t (Jsonaf.to_string json)
       |> Re.replace_string
            (Re.compile (Re.Perl.re {|"at_ms":[0-9]+|}))
            ~by:{|"at_ms":<ms>|})
  | Some (Error e) -> printf "error: %s\n" (mask t (Error.to_string_hum e))
;;

let%expect_test "the RPC methods" =
  with_agent []
  @@ fun t agent ->
  let call = dispatch t agent in
  call "get_review" "{}";
  call
    "review_comment"
    (sprintf
       {|{"root":%S,"path":"a.ml","first":2,"last":2,"anchor":"let x = 1","text":"why?"}|}
       t.dir);
  call "review_comment" {|{"root":"/r","path":"a.ml","first":"2"}|};
  call "review_reply" {|{"id":"c1","text":"well?"}|};
  call "review_set_status" {|{"id":"c1","status":"done"}|};
  call "review_set_status" {|{"id":"c1","status":"resolved"}|};
  call "review_decide" {|{"id":"c1","post":0,"accept":true}|};
  call "review_approve" {|{"path":"a.ml","hash":"abc"}|};
  call "review_unapprove" {|{"path":"a.ml"}|};
  call "review_delete" {|{"id":"c1"}|};
  call "fs_list" "{}";
  [%expect
    {|
    {"comments":[],"approvals":[],"snapshots":[]}
    {"id":"c1","root":"$DIR","path":"a.ml","first":2,"last":2,"side":"new","anchor":"let x = 1","status":"open","thread":[{"author":"user","text":"why?","at_ms":<ms>,"delivered":false}]}
    error: param "first" must be a whole number
    {"id":"c1","root":"$DIR","path":"a.ml","first":2,"last":2,"side":"new","anchor":"let x = 1","status":"open","thread":[{"author":"user","text":"why?","at_ms":<ms>,"delivered":false},{"author":"user","text":"well?","at_ms":<ms>,"delivered":false}]}
    error: unknown status "done": open or resolved
    {"id":"c1","root":"$DIR","path":"a.ml","first":2,"last":2,"side":"new","anchor":"let x = 1","status":"resolved","thread":[{"author":"user","text":"why?","at_ms":<ms>,"delivered":false},{"author":"user","text":"well?","at_ms":<ms>,"delivered":false}]}
    error: c1 has no proposed change at post 0
    {"comments":[{"id":"c1","root":"$DIR","path":"a.ml","first":2,"last":2,"side":"new","anchor":"let x = 1","status":"resolved","thread":[{"author":"user","text":"why?","at_ms":<ms>,"delivered":false},{"author":"user","text":"well?","at_ms":<ms>,"delivered":false}]}],"approvals":[{"path":"a.ml","hash":"abc"}],"snapshots":[{"root":"$DIR","path":"a.ml","action":"commented","at_ms":<ms>,"content":{"kind":"absent"}},{"root":"$DIR","path":"a.ml","action":"replied","at_ms":<ms>,"content":{"kind":"absent"}}]}
    {"comments":[{"id":"c1","root":"$DIR","path":"a.ml","first":2,"last":2,"side":"new","anchor":"let x = 1","status":"resolved","thread":[{"author":"user","text":"why?","at_ms":<ms>,"delivered":false},{"author":"user","text":"well?","at_ms":<ms>,"delivered":false}]}],"approvals":[],"snapshots":[{"root":"$DIR","path":"a.ml","action":"commented","at_ms":<ms>,"content":{"kind":"absent"}},{"root":"$DIR","path":"a.ml","action":"replied","at_ms":<ms>,"content":{"kind":"absent"}}]}
    {}
    not a review method
    |}];
  (* The event every change broadcasts. *)
  print_endline
    (mask
       t
       (Jsonaf.to_string
          (Rpc_json.event (Review_changed (Agent.review_state agent))))
     |> Re.replace_string
          (Re.compile (Re.Perl.re {|"at_ms":[0-9]+|}))
          ~by:{|"at_ms":<ms>|});
  [%expect
    {| {"type":"event","event":"review","review":{"comments":[],"approvals":[],"snapshots":[{"root":"$DIR","path":"a.ml","action":"commented","at_ms":<ms>,"content":{"kind":"absent"}},{"root":"$DIR","path":"a.ml","action":"replied","at_ms":<ms>,"content":{"kind":"absent"}}]}} |}]
;;

let%expect_test "fs_write: the editor saves a file unless it changed" =
  with_agent []
  @@ fun t agent ->
  write t "a.txt" "one\n";
  let files meth params =
    match Files_rpc.dispatch agent ~meth ~params:(Jsonaf.of_string params) with
    | Some (Ok json) -> json
    | Some (Error e) -> `String ("error: " ^ mask t (Error.to_string_hum e))
    | None -> `String "none"
  in
  let path = Filename.concat t.dir "a.txt" in
  let md5 =
    match
      Jsonaf.member "md5" (files "fs_preview" (sprintf {|{"path":%S}|} path))
    with
    | Some (`String md5) -> md5
    | _ -> "none"
  in
  print_endline md5;
  let save ?expected text =
    print_endline
      (mask
         t
         (Jsonaf.to_string
            (files
               "fs_write"
               (Jsonaf.to_string
                  (`Object
                      ([ "path", `String path; "text", `String text ]
                       @ Option.value_map expected ~default:[] ~f:(fun e ->
                         [ "expected", `String e ])))))));
    print_string (read t "a.txt")
  in
  save ~expected:md5 "one\ntwo\n";
  (* The file is no longer what the editor started from. *)
  save ~expected:md5 "one\nthree\n";
  save "one\nthree\n";
  [%expect
    {|
    5bbf5a52328e7439ae6e719dfe712200
    {"path":"$DIR/a.txt","md5":"<id><id>"}
    one
    two
    "error: $DIR/a.txt changed since you opened it: reload it (your edits are lost) or save again to overwrite it"
    one
    two
    {"path":"$DIR/a.txt","md5":"<id><id>"}
    one
    three
    |}]
;;

let%expect_test "comments on selected text, and on removed lines" =
  let sent = Queue.create () in
  with_agent
    ~on_request:(fun request ->
      match List.last request.messages with
      | Some (Message.User u) -> Queue.enqueue sent u.text
      | _ -> ())
    [ Reply.text "Looking." ]
  @@ fun t agent ->
  let add ~first ~last ?(side = Review_comment.Side.New) ?columns ~anchor text =
    print_result
      t
      (Agent.update_review agent ~f:(fun r ->
         Review_state.add
           r
           ~root:t.dir
           ~path:"a.ml"
           ~range:{ first; last; side; columns }
           ~anchor
           ~text
           ~at:0.))
  in
  let cols start_col end_col = { Review_comment.Columns.start_col; end_col } in
  (* What does not fit the lines is refused, saying why. *)
  add ~first:3 ~last:3 ~columns:(cols 4 4) ~anchor:"let c = 3" "empty";
  add ~first:3 ~last:3 ~columns:(cols 4 40) ~anchor:"let c = 3" "past";
  add ~first:3 ~last:4 ~columns:(cols 4 5) ~anchor:"let c = 3" "short";
  [%expect
    {|
    error: columns 4-4 select nothing: select some text first
    error: columns 4-40 are outside the selected lines: they are byte offsets into the first and the last line
    error: a selection's anchor needs the text of its 2 lines, not 1
    |}];
  (* A word of one line; across two lines (the characters counted, not the
     bytes); and lines the change removed. *)
  add ~first:3 ~last:3 ~columns:(cols 4 5) ~anchor:"let c = 3 * 2" "Rename c";
  add
    ~first:4
    ~last:5
    ~columns:(cols 8 7)
    ~anchor:"let s = \"é…\"\nlet d = 4"
    "Join these";
  add ~first:3 ~last:3 ~side:Old ~anchor:"let c = 3" "Why drop it?";
  show_review t (Agent.review_state agent);
  [%expect
    {|
    c1 $DIR/a.ml:3 open
      user (unsent): Rename c
    c2 $DIR/a.ml:4-5 open
      user (unsent): Join these
    c3 $DIR/a.ml:old 3 open
      user (unsent): Why drop it?
    |}];
  print_s [%sexp (Agent.submit_review agent : int Or_error.t)];
  Agent.wait_idle agent;
  print_endline (mask t (Queue.last_exn sent));
  [%expect
    {|
    (Ok 3)
    The user left review comments on the changes in a worktree (each comment's file is its absolute path, in the worktree it names: edit that file, even when it is not under your directory; line numbers are as when they commented: the file may have changed since). Answer every comment with reply_to_comment: say what you did, or why not. Make the changes they ask for; where the user should decide, propose the change with the reply's edits instead of making it (it is applied when they accept it). Resolve a comment (resolve: true) when it needs nothing more. Posts marked new are what you have not seen yet. A comment on text the user selected marks it ⟦like this⟧ in its code and quotes it in <selected>; one with side="old" is on lines the change removed, numbered as in the file before it.

    <comment id="c1" worktree="$DIR" file="$DIR/a.ml" lines="3" selection="3:5-3:5" status="open">
    <code>
    3  let ⟦c⟧ = 3 * 2
    </code>
    <selected>
    c
    </selected>
    <post from="user" new="true">
    Rename c
    </post>
    </comment>

    <comment id="c2" worktree="$DIR" file="$DIR/a.ml" lines="4-5" selection="4:9-5:7" status="open">
    <code>
    4  let s = ⟦"é…"
    5  let d =⟧ 4
    </code>
    <selected>
    "é…"
    let d =
    </selected>
    <post from="user" new="true">
    Join these
    </post>
    </comment>

    <comment id="c3" worktree="$DIR" file="$DIR/a.ml" lines="3" side="old" status="open">
    <code>
    3  let c = 3
    </code>
    <post from="user" new="true">
    Why drop it?
    </post>
    </comment>
    |}];
  (* The RPC: a selection's side and columns, sent back as they came. *)
  let call = dispatch t agent in
  call
    "review_comment"
    (sprintf
       {|{"root":%S,"path":"a.ml","first":2,"last":2,"side":"old","start_col":4,"end_col":5,"anchor":"let x = 1","text":"x?"}|}
       t.dir);
  call
    "review_comment"
    {|{"root":"/r","path":"a.ml","first":2,"last":2,"start_col":4,"anchor":"let x = 1","text":"x?"}|};
  call
    "review_comment"
    {|{"root":"/r","path":"a.ml","first":2,"last":2,"side":"left","anchor":"let x = 1","text":"x?"}|};
  [%expect
    {|
    {"id":"c4","root":"$DIR","path":"a.ml","first":2,"last":2,"side":"old","start_col":4,"end_col":5,"anchor":"let x = 1","status":"open","thread":[{"author":"user","text":"x?","at_ms":<ms>,"delivered":false}]}
    error: a selection needs both start_col and end_col (or neither, for whole lines)
    error: unknown side "left": old or new
    |}];
  (* How the session keeps them; one saved before selections (no side, no
     columns) still reads, as whole lines of the file as it is. *)
  let range json = Review_comment.Range.t_of_jsonaf (Jsonaf.of_string json) in
  print_s [%sexp (range {|{"first":3,"last":4}|} : Review_comment.Range.t)];
  print_endline
    (Jsonaf.to_string
       (Review_comment.Range.jsonaf_of_t
          { first = 3; last = 4; side = Old; columns = Some (cols 1 2) }));
  print_endline
    (Jsonaf.to_string
       (Review_comment.Range.jsonaf_of_t
          (Review_comment.Range.lines ~first:3 ~last:4)));
  [%expect
    {|
    ((first 3) (last 4))
    {"first":3,"last":4,"side":["Old"],"columns":{"start_col":1,"end_col":2}}
    {"first":3,"last":4}
    |}];
  let loaded =
    Or_error.ok_exn (Session.load (Session.path (Agent.session agent)))
  in
  print_s
    [%sexp
      (Review_state.equal (Agent.review_state agent) (Session.review loaded)
       : bool)];
  [%expect {| true |}]
;;
