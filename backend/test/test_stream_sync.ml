open! Core
open! Prigh

let%expect_test "deltas while a client has the stream; a resync when it has not" =
  let t = Stream_sync.create () in
  let send ?(key = "") stream seq =
    let decision = Stream_sync.send t { key; stream; seq } in
    printf
      "%s %d.%d -> %s\n"
      (if String.is_empty key then "main" else key)
      stream
      seq
      (Sexp.to_string [%sexp (decision : Stream_sync.Send.t)])
  in
  (* From the start, in order. *)
  send 1 1;
  send 1 2;
  (* A subagent's stream is followed on its own. *)
  send ~key:"/a1" 2 1;
  send 1 3;
  (* Attached while it streamed: the whole message, then deltas. *)
  send 3 5;
  send 3 6;
  (* Asked for a resync: each stream's next update is whole. *)
  Stream_sync.reset t;
  send 3 7;
  send ~key:"/a1" 2 2;
  send 3 8;
  (* A new stream starts with deltas again. *)
  send 4 1;
  [%expect
    {|
    main 1.1 -> Delta
    main 1.2 -> Delta
    /a1 2.1 -> Delta
    main 1.3 -> Delta
    main 3.5 -> Resync
    main 3.6 -> Delta
    main 3.7 -> Resync
    /a1 2.2 -> Resync
    main 3.8 -> Delta
    main 4.1 -> Delta
    |}]
;;
