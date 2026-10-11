open! Core
open! Prigh

let show_cut ?above ?keep text =
  match Message_page.Elided.cut ?above ?keep text with
  | None -> print_endline "not cut"
  | Some (kept, elided) ->
    print_s [%sexp (elided : Message_page.Elided.t)];
    printf
      "head:\n%s\ntail:\n%s\n"
      (String.prefix kept elided.at)
      (String.drop_prefix kept elided.at)
;;

let numbered n =
  String.concat (List.map (List.range 1 (n + 1)) ~f:(sprintf "line %d\n"))
;;

let%expect_test "long outputs keep their first and last lines" =
  show_cut ~above:100 "short";
  [%expect {| not cut |}];
  show_cut ~above:300 ~keep:3 (numbered 60);
  [%expect
    {|
    ((at 21) (lines 60) (bytes 471))
    head:
    line 1
    line 2
    line 3

    tail:
    line 58
    line 59
    line 60
    |}];
  (* Without the trailing newline, and with fewer lines than [keep] at each
     end within a tenth of [above]. *)
  show_cut ~above:200 ~keep:10 (String.rstrip (numbered 40));
  [%expect
    {|
    ((at 14) (lines 40) (bytes 310))
    head:
    line 1
    line 2

    tail:
    line 39
    line 40
    |}];
  (* One long line is cut within it, never inside a character. *)
  show_cut ~above:60 (String.concat (List.init 32 ~f:(fun _ -> "é")));
  [%expect
    {|
    ((at 6) (lines 1) (bytes 64))
    head:
    ééé
    tail:
    ééé
    |}]
;;

let user text = Message.user text

let assistant ?(calls = []) text =
  Message.Assistant
    { content =
        Text text
        :: List.map calls ~f:(fun id ->
          Content.Tool_call { id; name = "bash"; arguments = "{}" })
    ; stop_reason = (if List.is_empty calls then End_turn else Tool_use)
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let result ?(text = "ok") id =
  Message.Tool_result
    { tool_call_id = id
    ; tool_name = "bash"
    ; text
    ; is_error = false
    ; images = []
    }
;;

(* Entry ids are random: shown as the message's text. *)
let labels = String.Table.create ()

let describe (json : Jsonaf.t) =
  let field name json = Jsonaf.member_exn name json in
  let text json =
    match Jsonaf.member "text" json, Jsonaf.member "content" json with
    | Some (`String s), _ -> s
    | _, Some (`Array (content :: _)) ->
      Jsonaf.string_exn (field "text" content)
    | _ -> "?"
  in
  let messages =
    Jsonaf.list_exn (field "messages" json)
    |> List.map ~f:(fun m ->
      let role = Jsonaf.string_exn (field "role" m) in
      let elided =
        match Jsonaf.member "elided" m with
        | Some e -> " elided " ^ Jsonaf.to_string e
        | None -> ""
      in
      sprintf
        "%s: %s%s"
        role
        (String.prefix (String.tr ~target:'\n' ~replacement:'|' (text m)) 24)
        elided)
  in
  let before =
    match field "before" json with
    | `String id -> Hashtbl.find labels id |> Option.value ~default:"?"
    | json -> Jsonaf.to_string json
  in
  print_s
    [%sexp
      { messages : string list
      ; older = (Jsonaf.to_string (field "older" json) : string)
      ; before : string
      }]
;;

let with_session f =
  let dir = Filename_unix.temp_dir "prigh-message-page" "" in
  let t = Session.create ~dir ~cwd:"/proj" () in
  f t
;;

let append t ?(label = "") message =
  let id = (Session.append_message t ~at:1. message).id in
  Hashtbl.set labels ~key:id ~data:label;
  id
;;

let page ?before t ~limit =
  match
    Message_page.create
      ~elide_above:100
      (Session.identified_messages t)
      ~limit
      ~before
  with
  | Ok page ->
    describe (Message_page.to_json page);
    page.before
  | Error e ->
    print_s [%sexp (e : Error.t)];
    None
;;

let%expect_test "pages from the newest, never starting with a tool result" =
  with_session
  @@ fun t ->
  let (_ : string) = append t (user "first") ~label:"first" in
  let (_ : string) = append t (assistant "hello") ~label:"hello" in
  let (_ : string) = append t (user "run them") ~label:"run them" in
  let (_ : string) =
    append t (assistant ~calls:[ "a"; "b" ] "running") ~label:"running"
  in
  let (_ : string) = append t (result "a") in
  let (_ : string) = append t (result ~text:(numbered 40) "b") in
  let (_ : string) = append t (assistant "done") in
  (* 2 asked for, but the results come with their call. *)
  let before = page t ~limit:2 in
  [%expect
    {|
    ((messages
      ("assistant: running" "tool_result: ok"
       "tool_result: line 1|line 40| elided {\"at\":7,\"lines\":40,\"bytes\":311}"
       "assistant: done"))
     (older 3) (before running))
    |}];
  let before = page t ~limit:2 ?before in
  [%expect
    {| ((messages ("assistant: hello" "user: run them")) (older 1) (before hello)) |}];
  let before = page t ~limit:2 ?before in
  [%expect {| ((messages ("user: first")) (older 0) (before null)) |}];
  ignore (before : string option);
  ignore (page t ~limit:2 ~before:"nope" : string option);
  [%expect
    {| "message nope is not in the session any more: load its latest messages again" |}];
  ignore (page t ~limit:100 : string option);
  [%expect
    {|
    ((messages
      ("user: first" "assistant: hello" "user: run them" "assistant: running"
       "tool_result: ok"
       "tool_result: line 1|line 40| elided {\"at\":7,\"lines\":40,\"bytes\":311}"
       "assistant: done"))
     (older 0) (before null))
    |}]
;;

let%expect_test "a compaction's summary starts a page with its pinned message" =
  with_session
  @@ fun t ->
  let (_ : string) = append t (user "old") in
  let pinned = append t (user "pinned") ~label:"pinned" in
  let (_ : string) = append t (assistant "old reply") in
  let kept = append t (user "kept") ~label:"kept" in
  let (_ : Session.Entry.t) =
    Session.append_compaction t ~pinned ~summary:"it went well" ~kept_from:kept
  in
  let (_ : string) = append t (assistant "after") ~label:"after" in
  let before = page t ~limit:2 in
  [%expect
    {| ((messages ("user: kept" "assistant: after")) (older 2) (before kept)) |}];
  ignore (page t ~limit:1 ?before : string option);
  [%expect
    {|
    ((messages ("user: pinned" "user: Summary of the conversat")) (older 0)
     (before null))
    |}]
;;
