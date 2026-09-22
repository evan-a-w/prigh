open! Core
open! Prigh

let with_dir f =
  let dir = Filename_unix.temp_dir "prigh-session" "" in
  f dir
;;

let kinds t =
  List.map (Session.messages t) ~f:(function
    | Message.User u -> "user:" ^ u.text
    | Assistant a -> "assistant:" ^ Message.Assistant.text a
    | Tool_result r -> "tool:" ^ r.tool_name)
;;

let assistant text =
  Message.Assistant
    { content = [ Text text ]
    ; stop_reason = End_turn
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let%expect_test "append, persist, reload" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) =
    Session.set_model t ~model:"deepseek-flash" ~thinking:(On (Some High))
  in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "hi") in
  let (_ : Session.Entry.t) = Session.append_message t (assistant "hello") in
  print_s
    [%sexp
      (kinds t : string list), (Session.model t : (string * Thinking.t) option)];
  [%expect {| ((user:hi assistant:hello) ((deepseek-flash (On (High))))) |}];
  let loaded = Or_error.ok_exn (Session.load (Session.path t)) in
  print_s
    [%sexp
      { same_id = (String.equal (Session.id loaded) (Session.id t) : bool)
      ; cwd = (Session.cwd loaded : string)
      ; messages = (kinds loaded : string list)
      ; head_matches =
          ([%equal: string option] (Session.head loaded) (Session.head t)
           : bool)
      }];
  [%expect
    {|
    ((same_id true) (cwd /proj) (messages (user:hi assistant:hello))
     (head_matches true))
    |}];
  let lines = In_channel.read_lines (Session.path t) in
  print_s [%sexp (List.length lines : int)];
  print_endline (String.prefix (List.nth_exn lines 2) 15);
  [%expect
    {|
    4
    ["Entry",{"id":
    |}]
;;

let%expect_test "rewind branches the tree; the file records the head move" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let e1 = Session.append_message t (Message.user "q1") in
  let (_ : Session.Entry.t) = Session.append_message t (assistant "a1") in
  let (_ : Session.Entry.t) =
    Session.append_message t (Message.user "q2-bad")
  in
  Or_error.ok_exn (Session.rewind t ~to_:e1.id);
  print_s [%sexp (kinds t : string list)];
  [%expect {| (user:q1) |}];
  let (_ : Session.Entry.t) = Session.append_message t (assistant "a1-alt") in
  print_s
    [%sexp (kinds t : string list), (List.length (Session.entries t) : int)];
  [%expect {| ((user:q1 assistant:a1-alt) 4) |}];
  let loaded = Or_error.ok_exn (Session.load (Session.path t)) in
  print_s [%sexp (kinds loaded : string list)];
  [%expect {| (user:q1 assistant:a1-alt) |}];
  print_s [%sexp (Session.rewind t ~to_:"nope" : unit Or_error.t)];
  [%expect {| (Error ("no such entry" (to_ nope))) |}]
;;

let%expect_test "compaction replaces the prefix with a summary" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "old1") in
  let (_ : Session.Entry.t) = Session.append_message t (assistant "old2") in
  let kept = Session.append_message t (Message.user "recent") in
  let (_ : Session.Entry.t) = Session.append_message t (assistant "reply") in
  let (_ : Session.Entry.t) =
    Session.append_compaction
      t
      ~summary:"they said old things"
      ~kept_from:kept.id
  in
  print_s [%sexp (kinds t : string list)];
  [%expect
    {|
    ( "user:Summary of the conversation so far:\
     \nthey said old things" user:recent assistant:reply)
    |}];
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "after") in
  print_s
    [%sexp
      (List.length (kinds t) : int), (List.length (Session.entries t) : int)];
  [%expect {| (4 6) |}]
;;

let%expect_test "fork copies the active path up to a point" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "q1") in
  let e2 = Session.append_message t (assistant "a1") in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "q2") in
  let forked = Or_error.ok_exn (Session.fork t ~at:e2.id ~dir) in
  print_s
    [%sexp
      { forked = (kinds forked : string list)
      ; original = (kinds t : string list)
      ; distinct =
          (not (String.equal (Session.id forked) (Session.id t)) : bool)
      }];
  [%expect
    {|
    ((forked (user:q1 assistant:a1)) (original (user:q1 assistant:a1 user:q2))
     (distinct true))
    |}];
  let full = Or_error.ok_exn (Session.fork t ~dir) in
  print_s [%sexp (kinds full : string list)];
  [%expect {| (user:q1 assistant:a1 user:q2) |}];
  print_s [%sexp (Session.fork t ~at:"zzz" ~dir |> Or_error.is_error : bool)];
  [%expect {| true |}]
;;

let%expect_test "session stamps are strictly increasing" =
  with_dir
  @@ fun dir ->
  let sessions =
    let rec go n acc =
      if n = 0
      then List.rev acc
      else (
        let s = Session.create ~dir ~cwd:"/x" () in
        go (n - 1) (s :: acc))
    in
    go 5 []
  in
  let stamps =
    List.map sessions ~f:(fun s ->
      let base = Filename.basename (Session.path s) in
      List.hd_exn (String.split base ~on:'_'))
  in
  let sorted = List.sort stamps ~compare:String.compare in
  let strictly_increasing =
    List.for_all
      (List.range 0 (List.length stamps - 1))
      ~f:(fun i ->
        String.compare (List.nth_exn stamps i) (List.nth_exn stamps (i + 1)) < 0)
  in
  let ranks =
    List.map stamps ~f:(fun s ->
      fst
        (Option.value_exn (List.findi sorted ~f:(fun _ x -> String.equal x s))))
  in
  let sorted_equals_created = [%equal: string list] sorted stamps in
  print_s
    [%sexp
      { created_order = (ranks : int list)
      ; sorted_equals_created : bool
      ; strictly_increasing : bool
      }];
  [%expect
    {|
    ((created_order (0 1 2 3 4)) (sorted_equals_created true)
     (strictly_increasing true))
    |}]
;;

let%expect_test "list" =
  with_dir
  @@ fun dir ->
  print_s
    [%sexp
      (Session.list ~dir:(Filename.concat dir "none") : Session.Summary.t list)];
  [%expect {| () |}];
  let a = Session.create ~dir ~cwd:"/a" () in
  let (_ : Session.Entry.t) =
    Session.append_message a (Message.user "first question")
  in
  let (_ : Session.Entry.t) = Session.append_message a (assistant "answer") in
  let (_ : Session.t) = Session.create ~dir ~cwd:"/b" () in
  Out_channel.write_all (Filename.concat dir "junk.jsonl") ~data:"not json\n";
  print_s
    [%sexp
      (List.map (Session.list ~dir) ~f:(fun s ->
         s.cwd, s.first_prompt, s.message_count)
       : (string * string option * int) list)];
  [%expect {| ((/a ("first question") 2) (/b () 0)) |}]
;;

let%expect_test "load errors" =
  with_dir
  @@ fun dir ->
  let bad = Filename.concat dir "bad.jsonl" in
  Out_channel.write_all bad ~data:{|["Head","x"]|};
  print_s [%sexp (Session.load bad |> Or_error.is_error : bool)];
  print_s
    [%sexp
      (Session.load (Filename.concat dir "missing.jsonl") |> Or_error.is_error
       : bool)];
  [%expect
    {|
    true
    true
    |}]
;;

let time_re =
  Re.compile (Re.Perl.re {|\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}\.\d+Z|})
;;

let mask_times s = Re.replace_string time_re ~by:"<t>" s

let id_re =
  Re.compile (Re.repn (Re.alt [ Re.digit; Re.rg 'a' 'f' ]) 16 (Some 16))
;;

let mask_ids s = Re.replace_string id_re ~by:"<id>" s

let%expect_test "name entry round trips through save/load" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "hi") in
  print_s [%sexp (Session.name t : string option)];
  let (_ : Session.Entry.t) = Session.set_name t ~name:"first name" in
  print_s [%sexp (Session.name t : string option)];
  print_endline
    (mask_ids (List.last_exn (In_channel.read_lines (Session.path t))));
  let last_entry = List.last_exn (Session.entries t) in
  print_s [%sexp (last_entry.payload : Session.Entry.Payload.t)];
  let loaded = Or_error.ok_exn (Session.load (Session.path t)) in
  print_s [%sexp (Session.name loaded : string option)];
  let (_ : Session.Entry.t) = Session.set_name t ~name:"renamed" in
  print_s
    [%sexp
      (Session.name (Or_error.ok_exn (Session.load (Session.path t)))
       : string option)];
  [%expect
    {|
    ()
    ("first name")
    ["Entry",{"id":"<id>","parent":"<id>","payload":["Name",{"name":"first name"}]}]
    (Name (name "first name"))
    ("first name")
    (renamed)
    |}]
;;

let%expect_test "list reports name, parent, message count and updated_at" =
  with_dir
  @@ fun dir ->
  let a = Session.create ~dir ~cwd:"/a" () in
  let (_ : Session.Entry.t) = Session.append_message a (Message.user "first") in
  let (_ : Session.Entry.t) = Session.set_name a ~name:"alpha" in
  let forked = Or_error.ok_exn (Session.fork a ~dir) in
  let (_ : Session.Entry.t) = Session.set_name forked ~name:"beta" in
  print_s
    [%sexp
      (List.map (Session.list ~dir) ~f:(fun (s : Session.Summary.t) ->
         ( s.name
         , Option.equal String.equal s.parent (Some (Session.id a))
         , s.message_count
         , mask_times s.updated_at ))
       : (string option * bool * int * string) list)];
  [%expect {| (((alpha) false 1 <t>) ((beta) true 1 <t>)) |}]
;;

let%expect_test "markdown export renders user, assistant, tool and result" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let (_ : Session.Entry.t) = Session.append_message t (Message.user "hello") in
  let (_ : Session.Entry.t) =
    Session.append_message
      t
      (Message.Assistant
         { content =
             [ Content.Thinking { text = "let me think"; signature = None }
             ; Content.Text "hi there"
             ; Content.Tool_call
                 { Content.Tool_call.id = "c1"
                 ; name = "bash"
                 ; arguments = {|{"command":"ls -la"}|}
                 }
             ]
         ; stop_reason = Tool_use
         ; usage = Usage.zero
         ; model = "m"
         })
  in
  let (_ : Session.Entry.t) =
    Session.append_message
      t
      (Message.Tool_result
         { tool_call_id = "c1"
         ; tool_name = "bash"
         ; text = "file1\nfile2"
         ; is_error = false
         })
  in
  let (_ : Session.Entry.t) = Session.append_message t (assistant "done") in
  print_string
    (String.substr_replace_all
       (Session.to_markdown t)
       ~pattern:(Session.id t)
       ~with_:"<id>");
  [%expect
    {|
    # Session <id>

    ## User

    hello

    ## Assistant

    > let me think

    hi there

    ### Tool: bash

    ```json
    {
      "command": "ls -la"
    }
    ```

    ### Tool: bash

    ```
    file1
    file2
    ```

    ## Assistant

    done
    |}]
;;

let%expect_test "import copies a session into the sessions dir" =
  with_dir
  @@ fun dir ->
  let external_dir = Filename.concat dir "external" in
  let src = Session.create ~dir:external_dir ~cwd:"/src" () in
  let (_ : Session.Entry.t) =
    Session.append_message src (Message.user "imported")
  in
  let (_ : Session.Entry.t) = Session.set_name src ~name:"imported session" in
  let target = Filename.concat dir "sessions" in
  let imported =
    Or_error.ok_exn (Session.import ~dir:target (Session.path src))
  in
  print_s
    [%sexp
      { same_id = (String.equal (Session.id imported) (Session.id src) : bool)
      ; in_target =
          (String.is_prefix (Session.path imported) ~prefix:target : bool)
      ; name = (Session.name imported : string option)
      ; cwd = (Session.cwd imported : string)
      ; parent = (Session.parent imported : string option)
      ; messages =
          (List.map (Session.messages imported) ~f:(function
             | Message.User u -> u.text
             | Assistant _ | Tool_result _ -> "?")
           : string list)
      }];
  (* Importing the same file again collides on the id, so a fresh one is
     assigned. *)
  let second =
    Or_error.ok_exn (Session.import ~dir:target (Session.path src))
  in
  print_s
    [%sexp (not (String.equal (Session.id second) (Session.id src)) : bool)];
  [%expect
    {|
    ((same_id true) (in_target true) (name ("imported session")) (cwd /src)
     (parent ()) (messages (imported)))
    true
    |}]
;;

let%expect_test "headers written before parent existed still load" =
  with_dir
  @@ fun dir ->
  let path = Filename.concat dir "old.jsonl" in
  Out_channel.write_all
    path
    ~data:
      {|["Header",{"id":"abc","cwd":"/old","created_at":"2026-01-01 00:00:00.000000Z"}]
["Entry",{"id":"e1","parent":null,"payload":["Message",["User",{"text":"old"}]]}]
|};
  let t = Or_error.ok_exn (Session.load path) in
  print_s
    [%sexp
      (Session.parent t : string option)
    , (Session.cwd t : string)
    , (kinds t : string list)];
  [%expect {| (() /old (user:old)) |}]
;;

let%expect_test "fork ~at and rewind ~to address entries by id" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let e1 = Session.append_message t (Message.user "q1") in
  let (_ : Session.Entry.t) = Session.append_message t (assistant "a1") in
  let e3 = Session.append_message t (Message.user "q2") in
  let forked = Or_error.ok_exn (Session.fork t ~at:e1.id ~dir) in
  print_s
    [%sexp
      { forked_head_present = (Option.is_some (Session.head forked) : bool)
      ; forked_messages = (kinds forked : string list)
      }];
  Or_error.ok_exn (Session.rewind t ~to_:e3.id);
  print_s
    [%sexp
      { head_is_e3 =
          (Option.equal String.equal (Session.head t) (Some e3.id) : bool)
      ; messages = (kinds t : string list)
      }];
  [%expect
    {|
    ((forked_head_present true) (forked_messages (user:q1)))
    ((head_is_e3 true) (messages (user:q1 assistant:a1 user:q2)))
    |}]
;;
