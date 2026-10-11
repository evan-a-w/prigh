open! Core
module H = Harness

(* The model, the thinking level and the approval mode beside each other in
   the top bar: always shown, each opening its picker. *)

let controls h = H.text h ~selector:".controls"

let%expect_test "the thinking chip names the level and opens its picker" =
  let h = H.create () in
  controls h;
  H.show h ~selector:".thinking-chip";
  [%expect
    {|
    (Claude Opus 5.5) (Thinking on) (Auto: never asks)
    <button type="button"
            title="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
            aria-label="Thinking on: how much the model reasons before it answers (Alt+T cycles, /thinking)"
            class="btn chip-button thinking-chip"
            @on_click>
      <icon class="brain"> </icon>
      <span class="chip-key"> Thinking </span>
      <span class="chip-label"> on </span>
      <icon class="chevron"> </icon>
    </button>
    |}];
  H.click h ~selector:".thinking-chip";
  H.text h ~selector:".modal";
  [%expect
    {|
    Open_thinking_picker
    (Focus picker-input)
    Thinking level
    (Close (Esc))
    []
    off answer straight away
    low a little
    on the provider's default budget ✓
    high more
    max as much as the model allows
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  (* Esc closes it without a change. *)
  H.key h "Escape" ~target:Field;
  controls h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Claude Opus 5.5) (Thinking on) (Auto: never asks)
    |}];
  (* Alt+T: the next level shows at once, waiting on the backend. *)
  H.key h "t" ~alt:true;
  controls h;
  H.elements h ~selector:".thinking-chip";
  [%expect
    {|
    Cycle_thinking
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Thinking_set))
    (Claude Opus 5.5) (Thinking high) (Auto: never asks)
    <button class="awaiting btn chip-button thinking-chip">
    |}];
  H.text h ~selector:".thinking-chip .chip-label";
  [%expect {| Thinking high |}];
  H.reply h "set_thinking" "{}";
  H.event
    h
    (sprintf
       {|{"event":"state","state":%s}|}
       (H.state_json ~fields:[ "thinking", `String "high" ] ()));
  controls h;
  [%expect {| (Claude Opus 5.5) (Thinking high) (Auto: never asks) |}]
;;

let%expect_test "a model without thinking says so, and the chip tells why" =
  let deepseek =
    Jsonaf.of_string
      (H.model_json
         ~thinking:false
         ~provider:"deepseek"
         ~id:"deepseek-chat"
         ~name:"DeepSeek Chat"
         ())
  in
  let h =
    H.create
      ~state:
        (H.state_json
           ~fields:[ "model", deepseek; "thinking", `String "off" ]
           ())
      ()
  in
  controls h;
  [%expect {| (DeepSeek Chat) (No thinking) (Auto: never asks) |}];
  H.click h ~selector:".thinking-chip";
  H.elements h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    Open_thinking_picker
    DeepSeek Chat has no thinking levels: switch to a model that thinks with /model
    |}]
;;

let%expect_test "every approval mode, as the chip names it" =
  List.iter
    [ "ask", false; "auto-edit", false; "auto", false; "ask", true ]
    ~f:(fun (mode, plan) ->
      let h =
        H.create
          ~state:
            (H.state_json
               ~fields:
                 [ "approval_mode", `String mode
                 ; ("plan_mode", if plan then `True else `False)
                 ]
               ())
          ()
      in
      H.text h ~selector:".mode-chip";
      H.elements h ~selector:".mode-chip");
  [%expect
    {|
    (Asks: edits & commands)
    <button class="btn chip-button mode-chip">
    (Asks: commands)
    <button class="btn chip-button mode-chip">
    (Auto: never asks)
    <button class="btn chip-button mode-chip">
    (Plan mode)
    <button class="btn chip-button mode-chip plan-mode">
    |}]
;;
