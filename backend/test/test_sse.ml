open! Core
open! Prigh

let feed t s = print_s [%sexp (Sse.feed t s : Sse.Event.t list)]

let%expect_test "basic events, byte-at-a-time and all-at-once agree" =
  let input = "data: hello\n\ndata: {\"a\":1}\ndata: more\n\n" in
  let all = Sse.feed (Sse.create ()) input in
  let t = Sse.create () in
  let bytewise =
    String.to_list input
    |> List.concat_map ~f:(fun c -> Sse.feed t (String.of_char c))
  in
  print_s [%sexp (all : Sse.Event.t list)];
  print_s
    [%sexp
      (List.equal
         (fun a b ->
            Sexp.equal [%sexp (a : Sse.Event.t)] [%sexp (b : Sse.Event.t)])
         all
         bytewise
       : bool)];
  [%expect
    {|
    (((event ()) (data hello) (id ()))
     ((event ()) (data  "{\"a\":1}\
                       \nmore") (id ())))
    true
    |}]
;;

let%expect_test "crlf, comments, event and id fields, no-space-after-colon" =
  let t = Sse.create () in
  feed t ": keepalive\r\nevent: ping\r\nid: 7\r\ndata:x\r\n\r\n";
  [%expect {| (((event (ping)) (data x) (id (7)))) |}]
;;

let%expect_test "chunk boundaries inside a line" =
  let t = Sse.create () in
  feed t "da";
  feed t "ta: par";
  feed t "tial\n";
  feed t "\n";
  [%expect
    {|
    ()
    ()
    ()
    (((event ()) (data partial) (id ())))
    |}]
;;

let%expect_test "blank lines without fields produce nothing; finish flushes" =
  let t = Sse.create () in
  feed t "\n\n\n";
  feed t "data: tail";
  print_s [%sexp (Sse.finish t : Sse.Event.t option)];
  print_s [%sexp (Sse.finish t : Sse.Event.t option)];
  [%expect
    {|
    ()
    ()
    (((event ()) (data tail) (id ())))
    ()
    |}]
;;

let%expect_test "deepseek-style stream" =
  let t = Sse.create () in
  feed
    t
    "data: {\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}\n\ndata: [DONE]\n\n";
  [%expect
    {|
    (((event ()) (data "{\"choices\":[{\"delta\":{\"content\":\"Hi\"}}]}")
      (id ()))
     ((event ()) (data [DONE]) (id ())))
    |}]
;;
