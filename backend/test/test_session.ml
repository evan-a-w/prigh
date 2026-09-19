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
  let t = Session.create ~dir ~cwd:"/proj" in
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
  let t = Session.create ~dir ~cwd:"/proj" in
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
  let t = Session.create ~dir ~cwd:"/proj" in
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
  let t = Session.create ~dir ~cwd:"/proj" in
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

let%expect_test "list" =
  with_dir
  @@ fun dir ->
  print_s
    [%sexp
      (Session.list ~dir:(Filename.concat dir "none") : Session.Summary.t list)];
  [%expect {| () |}];
  let a = Session.create ~dir ~cwd:"/a" in
  let (_ : Session.Entry.t) =
    Session.append_message a (Message.user "first question")
  in
  let (_ : Session.Entry.t) = Session.append_message a (assistant "answer") in
  let (_ : Session.t) = Session.create ~dir ~cwd:"/b" in
  Out_channel.write_all (Filename.concat dir "junk.jsonl") ~data:"not json\n";
  print_s
    [%sexp
      (List.map (Session.list ~dir) ~f:(fun s ->
         s.cwd, s.first_prompt, s.message_count)
       : (string * string option * int) list)];
  [%expect {| ((/b () 0) (/a ("first question") 2)) |}]
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
