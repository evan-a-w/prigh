open! Core
open Prigh_web
module H = Harness

(* [/compaction]: when conversations are compacted and which model writes
   the summaries, as in the TUI. *)

let run h command =
  H.quiet h (fun () -> H.type_ h command);
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let config ?(at = 200_000) ?model ?(mask = true) () =
  Jsonaf.to_string
    (`Object
        [ "scoped_models", `Array []
        ; "compact_at_tokens", `Number (Int.to_string at)
        ; ( "compaction_model"
          , Option.value_map model ~default:`Null ~f:(fun m -> `String m) )
        ; ("mask_observations", if mask then `True else `False)
        ])
;;

let completions h =
  print_s
    [%sexp
      (Option.value_map (H.model h).completion ~default:[] ~f:(fun c ->
         List.map (Completion.items c) ~f:(fun i -> i.id))
       : string list)]
;;

let%expect_test "/compaction shows, sets only its own field, and refusals" =
  let h = H.create () in
  run h "/compaction";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value /compaction) (max 500))) (tag Ignore))
    (Expire_toast (id 0) (after_ms 6000))
    Compaction: at 200k tokens, or 80% of the model's window if that is less; summaries by the session's model; observation masking on (/compaction at TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)
    |}];
  run h "/compaction at 150k";
  H.reply h "set_config" (config ~at:150_000 ());
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compaction at 150k") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((compact_at_tokens 150000)))))
     (tag Compaction_saved))
    (Expire_toast (id 1) (after_ms 6000))
    Compaction: at 150k tokens, or 80% of the model's window if that is less; summaries by the session's model; observation masking on (/compaction at TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)
    |}];
  H.quiet h (fun () -> H.type_ h "/compaction ");
  completions h;
  H.quiet h (fun () -> H.type_ h "/compaction model ");
  completions h;
  H.quiet h (fun () -> H.type_ h "/compaction model gpt");
  completions h;
  [%expect
    {|
    (at model mask)
    (off openai/gpt-6 anthropic/claude-opus-5-5 anthropic/claude-sonnet-5
     deepseek/deepseek-chat)
    (openai/gpt-6)
    |}];
  run h "/compaction model gpt-6";
  H.reply h "set_config" (config ~at:150_000 ~model:"openai/gpt-6" ());
  H.text h ~selector:".toast:last-child";
  print_s
    [%sexp
      ((H.model h).config
       |> Option.map ~f:(fun (c : Prigh_protocol.Config.t) ->
         c.compact_at_tokens, c.compaction_model)
       : (int * string option) option)];
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compaction model gpt-6") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((compaction_model gpt-6)))))
     (tag Compaction_saved))
    (Expire_toast (id 2) (after_ms 6000))
    Compaction: at 150k tokens, or 80% of the model's window if that is less; summaries by openai/gpt-6; observation masking on (/compaction at TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)
    ((150000 (openai/gpt-6)))
    |}];
  run h "/compaction model off";
  H.fail
    h
    "set_config"
    "cannot save /home/u/.prigh/config.json: Permission denied";
  H.text h ~selector:".toast:last-child";
  run h "/compaction at lots";
  H.text h ~selector:".toast:last-child";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compaction model off") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((compaction_model null)))))
     (tag Compaction_saved))
    Couldn't set compaction: cannot save /home/u/.prigh/config.json: Permission denied (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compaction at lots") (max 500)))
     (tag Ignore))
    "lots" is not a number of tokens (e.g. 150k or 200000; off: at 80% of the model's window only); usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off or /compaction mask on|off (Dismiss)
    |}]
;;

let%expect_test "/compaction mask: completed, then set" =
  let h = H.create () in
  H.quiet h (fun () -> H.type_ h "/compaction mask ");
  completions h;
  [%expect {| (on off) |}];
  run h "/compaction mask off";
  H.reply h "set_config" (config ~mask:false ());
  H.text h ~selector:".toast:last-child";
  print_s
    [%sexp
      (Option.map (H.model h).config ~f:(fun (c : Prigh_protocol.Config.t) ->
         c.mask_observations)
       : bool option)];
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/compaction mask off") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((mask_observations false)))))
     (tag Compaction_saved))
    (Expire_toast (id 0) (after_ms 6000))
    Compaction: at 200k tokens, or 80% of the model's window if that is less; summaries by the session's model; observation masking off (/compaction at TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)
    (false)
    |}]
;;
