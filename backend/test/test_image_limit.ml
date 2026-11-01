open! Core
open! Prigh
open Tool_test_helpers
module F = Image_fixtures
module Reply = Faux_provider.Reply

let claude = Option.value_exn (Model.find "anthropic/claude-opus-4-6")
let gpt = Option.value_exn (Model.find "openai/gpt-5.5")

let image ~width ~height =
  { Image.mime_type = "image/png"
  ; data = Base64.encode_string (F.png_header ~width ~height)
  }
;;

let show_limit limit = print_endline (Image.Limit.describe limit)

let%expect_test "known limits, per request" =
  Image_limit.reset ();
  show_limit (Image_limit.known claude);
  show_limit (Image_limit.known gpt);
  show_limit (Image_limit.known Model.default);
  (* Anthropic: 2000 px each in a request of more than 20 images. *)
  show_limit (Image_limit.for_request claude ~images:20);
  show_limit (Image_limit.for_request claude ~images:21);
  show_limit (Image_limit.for_request gpt ~images:21);
  [%expect
    {|
    3.4 MB, 8000 px
    13.5 MB, 8000 px
    3.4 MB, 8000 px
    3.4 MB, 8000 px
    3.4 MB, 2000 px
    13.5 MB, 8000 px
    |}]
;;

let%expect_test "provider errors about an image's size" =
  let current = Image.Limit.default in
  List.iter
    [ "HTTP 400: invalid_request_error: messages.12.content.0: image exceeds 5 \
       MB maximum: 5308416 bytes > 5242880 bytes"
    ; "HTTP 400: invalid_request_error: messages.3.content.1: image exceeds 5 \
       MB maximum"
    ; "HTTP 400: messages.0.content.0.image.source.base64.data: image \
       dimensions exceed max allowed size: 2000 pixels"
    ; "HTTP 400: Image too large"
    ; "HTTP 400: prompt is too long: 213462 tokens > 200000 maximum"
    ; "HTTP 429: rate limit exceeded"
    ; "HTTP 500: the image service exploded"
    ]
    ~f:(fun error ->
      printf
        "%-5b %s\n"
        (Image_limit.matches error)
        (Option.value_map
           (Image_limit.of_error error ~current)
           ~default:"-"
           ~f:Image.Limit.describe));
  [%expect
    {|
    true  1.7 MB, 2000 px
    true  1.7 MB, 2000 px
    true  3.4 MB, 2000 px
    true  1.7 MB, 2000 px
    false -
    false -
    false -
    |}]
;;

let%expect_test "learned and tightened limits" =
  Image_limit.reset ();
  Image_limit.learn
    claude
    { max_base64_bytes = 1_000_000; max_dimension = 9000 };
  show_limit (Image_limit.for_model claude);
  (* Never loosened. *)
  Image_limit.learn claude Image.Limit.default;
  show_limit (Image_limit.for_model claude);
  show_limit (Image_limit.tighten claude);
  show_limit (Image_limit.tighten claude);
  show_limit (Image_limit.for_model gpt);
  Image_limit.reset ();
  show_limit (Image_limit.for_model claude);
  [%expect
    {|
    0.7 MB, 8000 px
    0.7 MB, 8000 px
    0.4 MB, 2000 px
    0.2 MB, 1500 px
    13.5 MB, 8000 px
    3.4 MB, 8000 px
    |}]
;;

(* A stand-in for ImageMagick that writes [out] to its last argument. *)
let fake_magick t out =
  let bin = Filename.concat t.dir "bin" in
  Core_unix.mkdir_p bin;
  write t "out" out;
  write
    t
    "bin/magick"
    (sprintf "#!/bin/sh\nfor last; do :; done\ncp %s/out \"$last\"\n" t.dir);
  Core_unix.chmod (Filename.concat bin "magick") ~perm:0o755;
  [ bin ]
;;

let sizes messages =
  List.concat_map messages ~f:(function
    | Message.User u -> u.images
    | Tool_result r -> r.images
    | Assistant _ -> [])
  |> List.map ~f:(fun (i : Image.t) ->
    match Image.dimensions (Base64.decode_exn i.data) with
    | Some (w, h) -> sprintf "%dx%d" w h
    | None -> "?")
  |> String.concat ~sep:" "
;;

let%expect_test
    "images over the limit are sent downscaled; the session keeps them"
  =
  with_sandbox
  @@ fun t ->
  Image_fit.reset ();
  let limit = { Image.Limit.default with max_dimension = 2000 } in
  let messages =
    [ Message.user
        ~images:[ image ~width:4000 ~height:3000; image ~width:800 ~height:600 ]
        "look"
    ; Tool_result
        { tool_call_id = "c1"
        ; tool_name = "read"
        ; text = ""
        ; is_error = false
        ; images = [ image ~width:3000 ~height:100 ]
        }
    ]
  in
  let show search_path =
    let sent, outcome =
      Image_fit.messages ~env:t.env ~search_path limit messages
    in
    printf "sent %s; kept %s\n" (sizes sent) (sizes messages);
    print_s [%sexp (outcome : Image_fit.Outcome.t)]
  in
  show (fake_magick t (F.png_header ~width:2000 ~height:1500));
  (* Without ImageMagick: what to do. *)
  Image_fit.reset ();
  show [];
  [%expect
    {|
    sent 2000x1500 800x600 2000x1500; kept 4000x3000 800x600 3000x100
    ((downscaled 2) (unfit ()))
    sent 4000x3000 800x600 3000x100; kept 4000x3000 800x600 3000x100
    ((downscaled 0)
     (unfit
      ("no image tool to downscale it with: install ImageMagick (magick) where the backend runs"
       "no image tool to downscale it with: install ImageMagick (magick) where the backend runs")))
    |}]
;;

let config : Agent_loop.Config.t =
  { model = claude
  ; thinking = Off
  ; system = None
  ; tools = []
  ; max_turns = None
  ; max_tokens = None
  ; retries = 0
  ; cache_key = None
  ; compaction = Compaction_settings.window_only
  }
;;

let run t ?(image_tools = []) replies =
  Image_limit.reset ();
  Image_fit.reset ();
  let provider =
    Faux_provider.create
      ~on_request:(fun r -> printf "request: images %s\n" (sizes r.messages))
      replies
  in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config
      ~cwd:t.dir
      ~image_tools
      ~emit:(function
        | Retry { attempt; max_attempts; error; _ } ->
          printf "retry %d/%d: %s\n" attempt max_attempts error
        | _ -> ())
      ~context:[]
      ~prompts:
        [ Message.user ~images:[ image ~width:4000 ~height:3000 ] "look" ]
      ()
  in
  List.iter added ~f:(function
    | Message.Assistant a ->
      printf
        "assistant: %s%s\n"
        (Message.Assistant.text a)
        (match a.stop_reason with
         | Error e -> "ERROR " ^ e
         | _ -> "")
    | User _ | Tool_result _ -> ());
  printf "kept: %s\n" (sizes added)
;;

let refused =
  "HTTP 400: messages.0.content.0: image dimensions exceed max allowed size: \
   2000 pixels"
;;

let%expect_test "a refused image teaches the limit and the request goes again" =
  with_sandbox
  @@ fun t ->
  run
    t
    ~image_tools:(fake_magick t (F.png_header ~width:2000 ~height:1500))
    [ Reply.error refused; Reply.text "A picture." ];
  [%expect
    {|
    request: images 4000x3000
    retry 2/3: HTTP 400: messages.0.content.0: image dimensions exceed max allowed size: 2000 pixels; downsizing the images to 3.4 MB, 2000 px
    request: images 2000x1500
    assistant: A picture.
    kept: 4000x3000
    |}]
;;

let%expect_test "refused again and again: what to do" =
  with_sandbox
  @@ fun t ->
  run
    t
    ~image_tools:(fake_magick t (F.png_header ~width:500 ~height:375))
    [ Reply.error refused
    ; Reply.error "HTTP 400: image too large"
    ; Reply.error "HTTP 400: image too large"
    ];
  [%expect
    {|
    request: images 4000x3000
    retry 2/3: HTTP 400: messages.0.content.0: image dimensions exceed max allowed size: 2000 pixels; downsizing the images to 3.4 MB, 2000 px
    request: images 500x375
    retry 3/3: HTTP 400: image too large; downsizing the images to 1.7 MB, 1500 px
    request: images 500x375
    assistant: ERROR HTTP 400: image too large (Claude Opus 4.6 still refused the images downsized to 1.7 MB, 1500 px: /downsize-images makes them smaller and tries again)
    kept: 4000x3000
    |}]
;;

let%expect_test "no image tool: what to do" =
  with_sandbox
  @@ fun t ->
  run t [ Reply.error refused ];
  [%expect
    {|
    request: images 4000x3000
    assistant: ERROR HTTP 400: messages.0.content.0: image dimensions exceed max allowed size: 2000 pixels (an image is too large for Claude Opus 4.6 and prigh could not downsize it: no image tool to downscale it with: install ImageMagick (magick) where the backend runs; or remove the image: /rewind to before it)
    kept: 4000x3000
    |}]
;;
