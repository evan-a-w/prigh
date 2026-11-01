open! Core
open Prigh_web
module H = Harness

(* The composer's microphone: recorded, transcribed by the backend, put in
   at the caret; the browser's recognition when the backend cannot; and
   drawing on images. *)

let mic h = H.show h ~selector:".composer-row .mic"
let dictation h = print_s [%sexp ((H.model h).dictation : Dictation.t)]

let%expect_test "dictate: record, stop, transcribe, insert at the caret" =
  let h = H.create () in
  H.type_ h "Fix the";
  H.quiet h (fun () -> H.act h (Edit { text = "Fix the"; cursor = 7 }));
  H.click h ~selector:".composer-row .mic";
  dictation h;
  [%expect
    {|
    Dictate
    (Start_dictation Server)
    (Starting Server)
    |}];
  (* Listening: how long, how loud, and a way to throw it away. *)
  H.act h (Dictation_progress { elapsed_ms = 4200; level = 0.5 });
  mic h;
  H.text h ~selector:".dictation";
  [%expect
    {|
    <button type="button"
            title="Stop and put the words in"
            aria-label="Stop dictating"
            aria-pressed="true"
            class="btn icon mic on round"
            @on_click>
      <icon class="mic"> </icon>
    </button>
    <icon class="mic"> </icon>
    0:04 Recording (Discard the recording (Esc))
    |}];
  (* Clicked again: stopped, and sent to be transcribed. *)
  H.click h ~selector:".composer-row .mic";
  H.act h (Dictation_audio { mime_type = "audio/webm"; data = "AAAA" });
  mic h;
  [%expect
    {|
    Dictate
    Stop_dictation
    (Rpc (method_ transcribe) (params ((mime_type audio/webm) (data AAAA)))
     (tag Transcribed))
    <button type="button"
            title="Transcribing…"
            aria-label="Stop dictating"
            disabled=""
            aria-pressed="true"
            class="btn icon mic on round"
            @on_click>
      <icon class="mic"> </icon>
    </button>
    <icon class="mic"> </icon>
    |}];
  H.reply h "transcribe" {|{"text":"flaky test please."}|};
  print_s [%sexp ((H.model h).draft : string), ((H.model h).cursor : int)];
  dictation h;
  [%expect
    {|
    (Set_caret 26)
    ("Fix the flaky test please." 26)
    Idle
    |}]
;;

let%expect_test "Esc while listening throws it away" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Dictate);
  H.quiet h (fun () ->
    H.act h (Dictation_progress { elapsed_ms = 900; level = 0.1 }));
  H.key h "Escape";
  dictation h;
  [%expect
    {|
    Dictation_cancel
    Cancel_dictation
    Idle
    |}]
;;

let%expect_test "no transcription model: the browser's, or where to set one" =
  let no_provider =
    "No transcription provider: sign in to openai with an API key (/login \
     openai)"
  in
  let attempt h =
    H.quiet h (fun () -> H.act h Dictate);
    H.quiet h (fun () ->
      H.act h (Dictation_progress { elapsed_ms = 900; level = 0.1 }));
    H.quiet h (fun () -> H.act h Dictate);
    H.quiet h (fun () ->
      H.act h (Dictation_audio { mime_type = "audio/webm"; data = "AAAA" }));
    H.fail h "transcribe" no_provider;
    H.text h ~selector:".toast"
  in
  (* Without SpeechRecognition: said, pointing at the settings. *)
  let h = H.create () in
  attempt h;
  [%expect
    {| Dictation needs a transcription model: pick one in Settings → Models (an OpenAI key, or a custom provider with /audio/transcriptions). (Dismiss) |}];
  (* With it: the next press has the browser listen. *)
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Browser_speech true));
  attempt h;
  H.act h Dictate;
  H.act h (Dictation_progress { elapsed_ms = 300; level = 0. });
  H.act h Dictate;
  H.act h (Dictation_text "hello there");
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 6000))
    No transcription model is set up, so the browser's speech recognition listens instead: press the microphone again (Settings → Models sets one up).
    (Start_dictation Browser)
    Stop_dictation
    (Set_caret 11)
    "hello there"
    |}]
;;

let png = "iVBORw0KGgo="

let image : Prigh_protocol.Image.t =
  { mime_type = "image/png"; data = png; bytes = 8 }
;;

let%expect_test "drawing on an attached image replaces it" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Add_image image));
  H.quiet h (fun () ->
    H.act h (Add_image { image with mime_type = "image/jpeg" }));
  H.click h ~selector:".pending-thumb" ~nth:1;
  H.names h ~selector:".annotate-dialog .btn";
  [%expect
    {|
    (Annotate (image ((mime_type image/jpeg) (bytes 8))) (target (Pending 1)))
    (Focus dialog)
    Annotate_setup
    button "Close" -- Close (Esc)
    button "Undo" -- Undo the last stroke (Ctrl+Z)
    button "Clear" -- Rub out every stroke
    button "Cancel"
    button "Save"
    |}];
  (* Nothing drawn yet: nothing to save, undo or clear. *)
  H.elements h ~selector:".annotate-dialog .btn[disabled]";
  [%expect
    {|
    <button class="annotate-undo btn ghost small">
    <button class="annotate-clear btn ghost small">
    <button class="annotate-save btn primary">
    |}];
  H.act h (Annotate_strokes 2);
  H.click h ~selector:".annotate-swatch" ~nth:2;
  H.click h ~selector:".annotate-undo";
  H.click h ~selector:".annotate-save";
  [%expect
    {|
    (Annotate_color #32d74b)
    Annotate_undo
    Annotate_undo
    Annotate_save
    Annotate_export
    |}];
  H.act h (Annotated { image with data = "ZHJhd24=" });
  print_s
    [%sexp
      (List.map (H.model h).images ~f:(fun i -> i.mime_type, i.data)
       : (string * string) list)];
  H.elements h ~selector:".annotate-dialog";
  [%expect
    {|
    (Focus editor)
    ((image/png iVBORw0KGgo=) (image/png ZHJhd24=))
    |}]
;;

let%expect_test "drawing on an image in the chat attaches the drawing" =
  let h = H.create () in
  H.act h (Annotate { image; target = New });
  H.text h ~selector:".modal-head";
  H.act h (Annotate_strokes 1);
  H.key h "Enter" ~target:Page;
  H.act h (Annotated { image with data = "ZHJhd24=" });
  print_s [%sexp (List.length (H.model h).images : int)];
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus dialog)
    Annotate_setup
    Draw on the image
    (Close (Esc))
    Dialog_accept
    Annotate_export
    (Focus editor)
    1
    |}]
;;
