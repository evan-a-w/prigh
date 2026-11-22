open! Core

let entry ?parent id kind =
  sprintf
    {|{"id":"%s","parent":%s,%s}|}
    id
    (Option.value_map parent ~default:"null" ~f:(sprintf "%S"))
    kind
;;

let user text =
  sprintf {|"kind":"message","message":{"role":"user","text":"%s"}|} text
;;

let assistant text =
  sprintf
    {|"kind":"message","message":{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
;;

let entries ~head list =
  sprintf {|{"head":"%s","entries":[%s]}|} head (String.concat ~sep:"," list)
;;

let%expect_test "/tree of a very long conversation" =
  let n = 50_000 in
  let json =
    entries
      ~head:(sprintf "e%d" n)
      (List.init n ~f:(fun i ->
         entry
           ?parent:(if i = 0 then None else Some (sprintf "e%d" i))
           (sprintf "e%d" (i + 1))
           (if i % 2 = 0 then user "question" else assistant "answer")))
  in
  let head, list =
    Prigh_protocol.Json.parse json
    |> Or_error.ok_exn
    |> Prigh_web.Session_tree.of_json
    |> Or_error.ok_exn
  in
  let items = Prigh_web.Session_tree.tree_items list ~head in
  print_s
    [%sexp (List.length items : int), ((List.last_exn items).label : string)];
  [%expect {| (50000 "\194\183 answer") |}]
;;
