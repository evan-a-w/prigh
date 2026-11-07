open! Core
open! Expect_test_helpers_core
open Prigh_protocol

let parse json =
  match Ui_spec.of_arguments json with
  | None -> print_endline "(not render_ui arguments)"
  | Some t ->
    print_s [%sexp (t : Ui_spec.t)];
    List.iter (Ui_spec.summary t) ~f:(fun line ->
      print_endline ("[" ^ line ^ "]"))
;;

let%expect_test "components parse into their types, with their summaries" =
  parse
    {|{"components":[
      {"type":"chart","kind":"bar","title":"Builds","labels":["a","b"],"series":[{"name":"x","values":[1,null]},{"name":"y","values":[2,3]}]},
      {"type":"chart","kind":"donut","slices":[{"label":"A","value":2}]},
      {"type":"table","title":"Slowest tests","columns":[{"key":"n","label":"Name","align":"right","format":"number","decimals":2}],"rows":[{"n":1.5},{"n":"x"},{"n":null},{"n":true}]},
      {"type":"image","path":"/home/u/p/shot.png","caption":"It"},
      {"type":"form","id":"deploy","title":"Deploy options","fields":[{"name":"env","kind":"select","options":["a","b"],"required":true}],"submit":"Go"},
      {"type":"tabs","tabs":[{"label":"One","children":[{"type":"markdown","text":"hi"}]}]}
    ]}|};
  [%expect
    {|
    (((id ())
      (body (
        Chart (
          (kind Bar)
          (title (Builds))
          (subtitle ())
          (unit     ())
          (x_label  ())
          (y_label  ())
          (height Medium)
          (data (
            Categories
            (labels (a b))
            (series (
              ((name x) (values ((1) ())))
              ((name y)
               (values (
                 (2)
                 (3))))))))))))
     ((id ())
      (body (
        Chart (
          (kind Donut)
          (title    ())
          (subtitle ())
          (unit     ())
          (x_label  ())
          (y_label  ())
          (height Medium)
          (data (
            Slices ((
              (label A)
              (value 2)))))))))
     ((id ())
      (body (
        Table (
          (title ("Slowest tests"))
          (columns ((
            (key      n)
            (label    Name)
            (align    Right)
            (format   Number)
            (currency USD)
            (decimals (2)))))
          (rows (
            ((n (Number 1.5)))
            ((n (Text   x)))
            ((n Null))
            ((n (Bool true)))))
          (sortable true)
          (caption ())))))
     ((id ())
      (body (
        Image
        (image ((path /home/u/p/shot.png) (caption (It)) (alt ())))
        (width Medium))))
     ((id (deploy))
      (body (
        Form
        (title ("Deploy options"))
        (description ())
        (fields ((
          (name  env)
          (label env)
          (kind  Select)
          (options (a b))
          (default ())
          (min     ())
          (max     ())
          (step    ())
          (required true)
          (placeholder ())
          (help        ()))))
        (submit Go))))
     ((id ()) (body (Tabs ((One (((id ()) (body (Markdown hi))))))))))
    [chart: Builds · 2 series]
    [chart · 1 slice]
    [table: Slowest tests · 4 rows]
    [image: /home/u/p/shot.png]
    [form: Deploy options]
    [tabs · One]
    |}]
;;

let%expect_test "tolerant: unknown types, bad fields, not JSON" =
  parse
    {|{"components":[{"type":"hologram","x":1},{"type":"stat","label":"L","value":{"no":1}},{"title":"no type"},{"type":"progress","value":"lots"}]}|};
  parse {|{"components":"nope"}|};
  parse {|{"components":[{"type":"chart"|};
  parse {|[]|};
  [%expect
    {|
    (((id ())
      (body (
        Unknown
        (type_ hologram)
        (raw (
          (type hologram)
          (x    1))))))
     ((id ())
      (body (
        Stat
        (label L)
        (value (Text "\226\128\148"))
        (unit    ())
        (delta   ())
        (trend   ())
        (good    ())
        (caption ())
        (spark   ()))))
     ((id ()) (body (Unknown (type_ "") (raw ((title "no type"))))))
     ((id ())
      (body (
        Progress
        (label ())
        (value 0)
        (max   100)
        (tone  Neutral)
        (caption ())))))
    [hologram]
    [stat: L · —]
    [component]
    [progress]
    (not render_ui arguments)
    (not render_ui arguments)
    (not render_ui arguments)
    |}]
;;

let%expect_test "walk and image paths reach nested components" =
  let t =
    Option.value_exn
      (Ui_spec.of_arguments
         {|{"components":[{"type":"card","children":[{"type":"gallery","images":[{"path":"/a.png"},{"path":"/b.png"}]},{"type":"accordion","items":[{"title":"t","children":[{"type":"image","path":"/c.png"}]}]}]}]}|})
  in
  List.iter (Ui_spec.walk t) ~f:(fun (path, c) ->
    printf "%s %s\n" path (Ui_spec.Component.type_name c));
  print_s [%sexp (Ui_spec.image_paths t : string list)];
  [%expect
    {|
    0 card
    0.0 gallery
    0.1 accordion
    0.1.0.0 image
    (/a.png /b.png /c.png)
    |}]
;;

let%expect_test "ui_action: the request, and finding a reply in the transcript" =
  let a =
    { Ui_action.call_id = "u1"
    ; component = "deploy"
    ; title = "Deploy options"
    ; values = `Object [ "env", `String "prod"; "replicas", `Number "3" ]
    }
  in
  print_endline (Json.to_string (Ui_action.to_json a));
  let text = Ui_action.message_text a in
  print_endline text;
  let found text =
    print_s [%sexp (Ui_action.Sent.of_text text : Ui_action.Sent.t option)]
  in
  found text;
  (* As the backend sends it, with the client note after it. *)
  found (text ^ "\n[Client: the user now uses prigh-web.]");
  (* A title holding the marker itself. *)
  found
    {|The user responded through "x (render_ui call u9, component z):" (render_ui call u2, component next):
"cache"|};
  found
    "The user responded through \"t\" (render_ui call u3, component c):\n\
     not json";
  found "hello";
  [%expect
    {|
    {"call_id":"u1","component":"deploy","title":"Deploy options","values":{"env":"prod","replicas":3}}
    The user responded through "Deploy options" (render_ui call u1, component deploy):
    {"env":"prod","replicas":3}
    ((
      (call_id   u1)
      (component deploy)
      (values ((
        (env      prod)
        (replicas 3))))))
    ((
      (call_id   u1)
      (component deploy)
      (values ((
        (env      prod)
        (replicas 3))))))
    ((
      (call_id   u2)
      (component next)
      (values (cache))))
    ((
      (call_id   u3)
      (component c)
      (values ())))
    ()
    |}]
;;
