open! Core
module H = Harness

(* Settings' naming model: what [/naming] sets, shown and picked on the
   settings page. *)

let config ?model () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; ( "naming_model"
          , Option.value_map model ~default:`Null ~f:(fun m -> `String m) )
        ])
;;

let settings () =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  h
;;

let%expect_test "naming model: shown, picked, back to the default" =
  let h = settings () in
  H.text h ~selector:".setting.naming-model";
  H.names h ~selector:".setting.naming-model button";
  H.click h ~selector:".naming-model-choice";
  H.text h ~selector:".modal";
  [%expect {| |}];
  H.quiet h (fun () -> H.act h (Picker_query "sonnet"));
  H.key h "Enter" ~target:Field;
  H.reply h "set_config" (config ~model:"anthropic/claude-sonnet-5" ());
  H.text h ~selector:".setting.naming-model";
  H.text h ~selector:".toast:last-child";
  [%expect {| |}];
  H.quiet h (fun () -> H.click h ~selector:".naming-model-choice");
  H.text h ~selector:".picker-item[aria-selected=true]";
  H.key h "Enter" ~target:Field;
  [%expect {| |}]
;;

let%expect_test "/naming from the composer, and a model the backend refuses" =
  let h = H.create () in
  H.quiet h (fun () -> H.type_ h "/naming");
  H.key h "Enter" ~target:Field;
  [%expect {| |}];
  H.quiet h (fun () -> H.type_ h "/naming model nosuch");
  H.key h "Enter" ~target:Field;
  H.fail h "set_config" {|naming_model: unknown model "nosuch"; did you mean: anthropic/claude-sonnet-5|};
  H.text h ~selector:".toast:last-child";
  [%expect {| |}]
;;
