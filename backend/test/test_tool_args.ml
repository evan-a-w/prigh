open! Core
open! Prigh

let json =
  Jsonaf.of_string
    {|{"s":"x","i":3,"f":2.0,"b":true,"l":[1],"n":null,"bad":1.5}|}
;;

let try_ f =
  match f () with
  | s -> print_endline s
  | exception Tool_args.Invalid m -> print_endline ("Invalid: " ^ m)
;;

let%expect_test "accessors" =
  try_ (fun () -> Tool_args.string json "s");
  try_ (fun () -> Tool_args.string json "missing");
  try_ (fun () -> Tool_args.string json "i");
  try_ (fun () ->
    Sexp.to_string [%sexp (Tool_args.string_opt json "n" : string option)]);
  try_ (fun () -> Int.to_string (Option.value_exn (Tool_args.int_opt json "i")));
  try_ (fun () -> Int.to_string (Option.value_exn (Tool_args.int_opt json "f")));
  try_ (fun () ->
    Int.to_string (Option.value_exn (Tool_args.int_opt json "bad")));
  try_ (fun () ->
    Bool.to_string (Option.value_exn (Tool_args.bool_opt json "b")));
  try_ (fun () ->
    Bool.to_string (Option.value_exn (Tool_args.bool_opt json "s")));
  try_ (fun () ->
    Int.to_string (List.length (Option.value_exn (Tool_args.list_opt json "l"))));
  try_ (fun () -> Tool_args.string (`Array []) "s");
  [%expect
    {|
    x
    Invalid: missing required argument "missing"
    Invalid: argument "i" must be a string
    ()
    3
    2
    Invalid: argument "bad" must be an integer
    true
    Invalid: argument "s" must be a boolean
    1
    Invalid: arguments must be a JSON object
    |}]
;;

let%expect_test "schema" =
  print_endline
    (Jsonaf.to_string
       (Tool_args.schema
          ~required:[ "a" ]
          [ "a", `String, "A"
          ; "b", `Integer, "B"
          ; "c", `Array (`Object [ "type", `String "string" ]), "C"
          ]));
  [%expect
    {| {"type":"object","properties":{"a":{"type":"string","description":"A"},"b":{"type":"integer","description":"B"},"c":{"type":"array","items":{"type":"string"},"description":"C"}},"required":["a"],"additionalProperties":false} |}]
;;
