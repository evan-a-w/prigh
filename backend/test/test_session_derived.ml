open! Core
open! Prigh
open Tool_test_helpers

(* What a session answers about its active path is kept per head and
   advanced entry by entry ([Session.For_testing.cached]); these check it
   against the same derived again from all the entries ([uncached]) after
   every kind of change: appends, settings, compactions, masking, trims,
   deletions and their undoing, rewinds, forks and reloads. *)

let check ~what session =
  let cached = Session.For_testing.cached session in
  let uncached = Session.For_testing.uncached session in
  if not (Sexp.equal cached uncached)
  then
    print_s
      [%message
        "cached and uncached differ" what (cached : Sexp.t) (uncached : Sexp.t)]
;;

let shape (m : Message.t) =
  match m with
  | User _ when Session.is_summary_message m -> "S"
  | User u -> sprintf "U(%s)" (Message.User.body u)
  | Assistant a ->
    (match Message.Assistant.tool_calls a with
     | [] -> sprintf "A(%s)" (Message.Assistant.text a)
     | calls ->
       sprintf
         "A[%s]"
         (String.concat ~sep:"," (List.map calls ~f:(fun c -> c.id))))
  | Tool_result r -> sprintf "T(%s)" r.tool_call_id
;;

let assistant ?(calls = []) ?(model = "m") ~input text : Message.t =
  Assistant
    { content =
        Content.Text text
        :: List.map calls ~f:(fun id ->
          Content.Tool_call { id; name = "bash"; arguments = "{}" })
    ; stop_reason = (if List.is_empty calls then End_turn else Tool_use)
    ; usage = { input; output = 10; cache_read = 0 }
    ; model
    }
;;

let result id : Message.t =
  Tool_result
    { tool_call_id = id
    ; tool_name = "bash"
    ; text = String.make 400 'x'
    ; is_error = false
    ; images = []
    }
;;

let append session m =
  ignore (Session.append_message session m : Session.Entry.t)
;;

(* The newest message entry with [text], on any branch. *)
let id_of_text session text =
  List.rev (Session.entries session)
  |> List.find_map_exn ~f:(fun (e : Session.Entry.t) ->
    match e.payload with
    | Message (User u) when String.equal (Message.User.body u) text -> Some e.id
    | Message (Assistant a) when String.equal (Message.Assistant.text a) text ->
      Some e.id
    | _ -> None)
;;

let%expect_test "each change, step by step" =
  with_sandbox
  @@ fun t ->
  let session = ref (Session.create ~dir:t.dir ~cwd:t.dir ()) in
  let step what f =
    f !session;
    check ~what !session;
    printf
      "%-22s %s | context %d tokens\n"
      what
      (String.concat ~sep:" " (List.map (Session.messages !session) ~f:shape))
      (Session.context_tokens !session)
  in
  step "user" (fun s -> append s (Message.user "one"));
  step "reply with a call" (fun s ->
    append s (assistant ~calls:[ "c1" ] ~input:1000 "two");
    append s (result "c1"));
  step "reply" (fun s -> append s (assistant ~input:1500 "three"));
  step "settings" (fun s ->
    ignore (Session.set_system_prompt s ~text:"sys" : Session.Entry.t);
    ignore
      (Session.set_approval s ~mode:"auto" ~plan:true ~chat:false
       : Session.Entry.t);
    ignore (Session.set_name s ~name:"named" : Session.Entry.t);
    ignore
      (Session.add_spend s ~purpose:"approval" ~usage:Usage.zero ~cost_usd:0.5
       : Session.Entry.t));
  step "user" (fun s -> append s (Message.user "four"));
  step "reply with a call" (fun s ->
    append s (assistant ~calls:[ "c2" ] ~input:2000 "five");
    append s (result "c2"));
  step "mask" (fun s ->
    ignore
      (Session.append_masked s ~through:(id_of_text s "two") : Session.Entry.t));
  step "delete" (fun s ->
    ignore
      (Session.append_delete s ~message:(id_of_text s "five") : _ Or_error.t));
  step "undelete" (fun s -> ignore (Session.append_undelete s : _ Or_error.t));
  step "trim" (fun s ->
    ignore (Session.append_trim s ~before:(id_of_text s "three") : _ Or_error.t));
  step "compact" (fun s ->
    ignore
      (Session.append_compaction
         s
         ~pinned:[ { id = id_of_text s "four"; shortened = false } ]
         ~summary:"so far"
         ~kept_from:(id_of_text s "five")
       : Session.Entry.t));
  step "reply" (fun s -> append s (assistant ~input:800 "six"));
  step "untrim" (fun s -> ignore (Session.append_untrim s : _ Or_error.t));
  step "rewind to \"three\"" (fun s ->
    Session.rewind s ~to_:(id_of_text s "three") |> Or_error.ok_exn);
  step "edit: another four" (fun s -> append s (Message.user "four again"));
  step "fork at \"two\"" (fun s ->
    session
    := Session.fork s ~at:(id_of_text s "two") ~dir:t.dir |> Or_error.ok_exn);
  step "reply in the fork" (fun s -> append s (assistant ~input:300 "seven"));
  step "reload" (fun s ->
    session := Session.load (Session.path s) |> Or_error.ok_exn);
  step "reply after reloading" (fun s ->
    append s (assistant ~input:400 "eight"));
  [%expect
    {|
    user                   U(one) | context 0 tokens
    reply with a call      U(one) A[c1] T(c1) | context 1000 tokens
    reply                  U(one) A[c1] T(c1) A(three) | context 1500 tokens
    settings               U(one) A[c1] T(c1) A(three) | context 1500 tokens
    user                   U(one) A[c1] T(c1) A(three) U(four) | context 1500 tokens
    reply with a call      U(one) A[c1] T(c1) A(three) U(four) A[c2] T(c2) | context 2000 tokens
    mask                   U(one) A[c1] T(c1) A(three) U(four) A[c2] T(c2) | context 2000 tokens
    delete                 U(one) A[c1] T(c1) A(three) U(four) | context 1998 tokens
    undelete               U(one) A[c1] T(c1) A(three) U(four) A[c2] T(c2) | context 2100 tokens
    trim                   A(three) U(four) A[c2] T(c2) | context 1998 tokens
    compact                U(four) S A[c2] T(c2) | context 2007 tokens
    reply                  U(four) S A[c2] T(c2) A(six) | context 800 tokens
    untrim                 U(one) A[c1] T(c1) U(four) S A[c2] T(c2) A(six) | context 902 tokens
    rewind to "three"      U(one) A[c1] T(c1) A(three) | context 1500 tokens
    edit: another four     U(one) A[c1] T(c1) A(three) U(four again) | context 1500 tokens
    fork at "two"          U(one) A[c1] | context 1000 tokens
    reply in the fork      U(one) A[c1] A(seven) | context 300 tokens
    reload                 U(one) A[c1] A(seven) | context 300 tokens
    reply after reloading  U(one) A[c1] A(seven) A(eight) | context 400 tokens
    |}]
;;

let%expect_test "forgetting allowed calls, step by step" =
  with_sandbox
  @@ fun t ->
  let session = ref (Session.create ~dir:t.dir ~cwd:t.dir ()) in
  let call cwd =
    Allowed_call.Call.create ~tool:"bash" ~arguments:(`Object []) ~host:"h" ~cwd
  in
  let allow cwd s =
    ignore
      (Session.add_allowed_call
         s
         { call = call cwd; by = Allowed_call.you; reason = None }
       : Session.Entry.t)
  in
  let forget cwd s =
    ignore
      (Session.forget_allowed_call s (Option.map cwd ~f:call) : Session.Entry.t)
  in
  let step what f =
    f !session;
    check ~what !session;
    printf
      "%-26s allowed [%s] | since forget all: %s\n"
      what
      (String.concat
         ~sep:" "
         (List.map
            (Session.allowed_calls !session)
            ~f:(fun (a : Allowed_call.t) -> a.call.cwd)))
      (String.concat
         ~sep:" "
         (List.map (Session.messages_since_forget_all !session) ~f:shape))
  in
  step "user" (fun s -> append s (Message.user "one"));
  step "allow a, b, c" (fun s ->
    allow "a" s;
    allow "b" s;
    allow "c" s);
  step "forget b" (forget (Some "b"));
  step "forget b again" (forget (Some "b"));
  step "reply" (fun s -> append s (assistant ~input:100 "two"));
  step "forget all" (forget None);
  step "user" (fun s -> append s (Message.user "three"));
  step "allow d" (allow "d");
  step "reload" (fun s ->
    session := Session.load (Session.path s) |> Or_error.ok_exn);
  step "rewind to \"two\"" (fun s ->
    Session.rewind s ~to_:(id_of_text s "two") |> Or_error.ok_exn);
  step "forget a" (forget (Some "a"));
  step "fork at \"two\"" (fun s ->
    session
    := Session.fork s ~at:(id_of_text s "two") ~dir:t.dir |> Or_error.ok_exn);
  step "forget all in the fork" (forget None);
  step "user" (fun s -> append s (Message.user "four"));
  [%expect
    {|
    user                       allowed [] | since forget all: U(one)
    allow a, b, c              allowed [a b c] | since forget all: U(one)
    forget b                   allowed [a c] | since forget all: U(one)
    forget b again             allowed [a c] | since forget all: U(one)
    reply                      allowed [a c] | since forget all: U(one) A(two)
    forget all                 allowed [] | since forget all:
    user                       allowed [] | since forget all: U(three)
    allow d                    allowed [d] | since forget all: U(three)
    reload                     allowed [d] | since forget all: U(three)
    rewind to "two"            allowed [a c] | since forget all: U(one) A(two)
    forget a                   allowed [c] | since forget all: U(one) A(two)
    fork at "two"              allowed [a c] | since forget all: U(one) A(two)
    forget all in the fork     allowed [] | since forget all:
    user                       allowed [] | since forget all: U(four)
    |}]
;;

(* Random sequences of changes, the cache looked at (and so kept and
   advanced) after some of them and not others. *)
module Op = struct
  type t =
    | User
    | Reply
    | Call
    | Setting
    | Spend
    | Compact
    | Mask
    | Trim
    | Trim_after
    | Delete
    | Untrim
    | Undelete
    | Rewind
    | Fork
    | Reload
  [@@deriving sexp_of, enumerate, compare]
end

let%expect_test "cached = uncached after random changes" =
  with_sandbox
  @@ fun t ->
  let rand = Random.State.make [| 42 |] in
  let pick l = List.nth_exn l (Random.State.int rand (List.length l)) in
  let maybe_pick l = if List.is_empty l then None else Some (pick l) in
  let counter = ref 0 in
  let fresh () =
    incr counter;
    Int.to_string !counter
  in
  let counts = Hashtbl.create (module String) in
  let differences = ref 0 in
  for _ = 1 to 60 do
    let session = ref (Session.create ~dir:t.dir ~cwd:t.dir ()) in
    let log = Queue.create () in
    for _ = 1 to 40 do
      let s = !session in
      let context_ids = List.filter_opt (Session.context_ids s) in
      let path_ids = List.map (Session.active_path s) ~f:(fun e -> e.id) in
      let op = pick Op.all in
      let ignore_result (r : _ Or_error.t) = ignore r in
      (match op with
       | User -> append s (Message.user ("u" ^ fresh ()))
       | Reply ->
         append
           s
           (assistant
              ~model:(pick [ "m"; "n" ])
              ~input:(Random.State.int rand 5000)
              ("a" ^ fresh ()))
       | Call ->
         let id = "c" ^ fresh () in
         append
           s
           (assistant ~calls:[ id ] ~input:(Random.State.int rand 5000) id);
         append s (result id)
       | Setting ->
         let (_ : Session.Entry.t) =
           match Random.State.int rand 10 with
           | 0 -> Session.set_system_prompt s ~text:(fresh ())
           | 1 ->
             Session.set_approval
               s
               ~mode:(pick [ "auto"; "smart" ])
               ~plan:(Random.State.bool rand)
               ~chat:false
           | 2 -> Session.set_goal s (Some (Goal.create (fresh ())))
           | 3 -> Session.set_budget s ~usd:(Some 1.5)
           | 4 -> Session.set_model s ~model:(fresh ()) ~thinking:Off
           | 5 -> Session.add_allow_rule s ~rule:(fresh ())
           | 6 ->
             Session.add_allowed_call
               s
               { call =
                   Allowed_call.Call.create
                     ~tool:"bash"
                     ~arguments:(`Object [])
                     ~host:"h"
                     ~cwd:(fresh ())
               ; by = Allowed_call.you
               ; reason = None
               }
           | 7 -> Session.set_name s ~name:(fresh ())
           | 8 -> Session.set_description s ~text:(fresh ())
           | _ ->
             Session.forget_allowed_call
               s
               (Option.map
                  (maybe_pick (Session.allowed_calls s))
                  ~f:(fun (a : Allowed_call.t) -> a.call))
         in
         if Random.State.bool rand
         then Session.set_resume_at s (Some (Float.of_int !counter))
       | Spend ->
         ignore
           (Session.add_spend
              s
              ?purpose:(pick [ None; Some "approval"; Some "other" ])
              ~usage:{ input = 3; output = 2; cache_read = 1 }
              ~cost_usd:0.25
            : Session.Entry.t)
       | Compact ->
         Option.iter (maybe_pick context_ids) ~f:(fun kept_from ->
           let pinned =
             List.filter context_ids ~f:(fun _ -> Random.State.int rand 4 = 0)
             |> List.map ~f:(fun id ->
               { Pinned_messages.Pin.id; shortened = Random.State.bool rand })
           in
           ignore
             (Session.append_compaction s ~pinned ~summary:(fresh ()) ~kept_from
              : Session.Entry.t))
       | Mask ->
         Option.iter (maybe_pick path_ids) ~f:(fun through ->
           ignore (Session.append_masked s ~through : Session.Entry.t))
       | Trim ->
         Option.iter (maybe_pick context_ids) ~f:(fun before ->
           ignore_result (Session.append_trim s ~before))
       | Trim_after ->
         Option.iter (maybe_pick context_ids) ~f:(fun after ->
           ignore_result (Session.append_trim_after s ~after))
       | Delete ->
         Option.iter (maybe_pick context_ids) ~f:(fun message ->
           ignore_result (Session.append_delete s ~message))
       | Untrim -> ignore_result (Session.append_untrim s)
       | Undelete ->
         let entry =
           maybe_pick (List.map (Session.hidden s) ~f:(fun h -> h.entry))
         in
         ignore_result (Session.append_undelete ?entry s)
       | Rewind ->
         Option.iter
           (maybe_pick (List.map (Session.entries s) ~f:(fun e -> e.id)))
           ~f:(fun to_ -> Session.rewind s ~to_ |> Or_error.ok_exn)
       | Fork ->
         Option.iter (maybe_pick path_ids) ~f:(fun at ->
           session := Session.fork s ~at ~dir:t.dir |> Or_error.ok_exn)
       | Reload ->
         if Session.persisted s
         then session := Session.load (Session.path s) |> Or_error.ok_exn);
      Queue.enqueue log op;
      Hashtbl.incr counts (Sexp.to_string [%sexp (op : Op.t)]);
      if Random.State.bool rand
      then (
        let cached = Session.For_testing.cached !session in
        let uncached = Session.For_testing.uncached !session in
        if not (Sexp.equal cached uncached)
        then (
          incr differences;
          if !differences <= 1
          then
            print_s
              [%message
                "cached and uncached differ"
                  ~after:(Queue.to_list log : Op.t list)
                  (cached : Sexp.t)
                  (uncached : Sexp.t)]))
    done;
    check ~what:"at the end" !session
  done;
  print_s [%message (!differences : int)];
  print_s
    [%sexp
      (Hashtbl.to_alist counts
       |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
       : (string * int) list)];
  [%expect
    {|
    (!differences 0)
    ((Call 160) (Compact 171) (Delete 146) (Fork 161) (Mask 175) (Reload 166)
     (Reply 151) (Rewind 176) (Setting 158) (Spend 140) (Trim 162)
     (Trim_after 170) (Undelete 168) (Untrim 149) (User 147))
    |}]
;;
