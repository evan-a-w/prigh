open! Core
module H = Harness
module Image_size = Prigh_web.Image_size

let base64 s =
  let alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  in
  let out = Buffer.create ((String.length s * 4 / 3) + 4) in
  let byte i = if i < String.length s then Char.to_int s.[i] else 0 in
  let rec go i =
    if i < String.length s
    then (
      let n = (byte i lsl 16) lor (byte (i + 1) lsl 8) lor byte (i + 2) in
      let left = String.length s - i in
      List.iteri [ 18; 12; 6; 0 ] ~f:(fun k shift ->
        if k <= left
        then Buffer.add_char out alphabet.[(n lsr shift) land 63]
        else Buffer.add_char out '=');
      go (i + 3))
  in
  go 0;
  Buffer.contents out
;;

let be32 n =
  String.init 4 ~f:(fun i -> Char.of_int_exn ((n lsr (8 * (3 - i))) land 0xff))
;;

(* A PNG's signature and header: enough for its dimensions. *)
let png ~width ~height =
  "\x89PNG\r\n\x1a\n\x00\x00\x00\x0dIHDR"
  ^ be32 width
  ^ be32 height
  ^ "\x08\x06\x00\x00\x00"
;;

let image ?(mime_type = "image/png") ?bytes raw =
  { Prigh_protocol.Image.mime_type
  ; data = base64 raw
  ; bytes = Option.value bytes ~default:(String.length raw)
  }
;;

let limit = Image_size.default_limit

let%expect_test "dimensions, verdicts and targets" =
  let show (i : Prigh_protocol.Image.t) =
    printf
      !"%{sexp: (int * int) option}  %{sexp: Image_size.Verdict.t}  %{sexp: \
        Image_size.Target.t option}\n"
      (Image_size.dimensions i)
      (Image_size.check limit i)
      (Image_size.target limit i)
  in
  show (image (png ~width:800 ~height:600));
  show (image (png ~width:9000 ~height:200));
  show (image (png ~width:2000 ~height:16000));
  (* Within the pixels but too many bytes: the same size, fewer bytes. *)
  show (image ~bytes:4_500_000 (png ~width:3000 ~height:2000));
  (* No header to read: the bytes decide. *)
  show (image ~mime_type:"image/jpeg" ~bytes:4_500_000 "not an image");
  print_s [%sexp (limit : Prigh_protocol.Model.Image_limit.t)];
  [%expect
    {|
    ((800 600))  Fits  ()
    ((9000 200))  (Too_large "9000\195\151200 px, over 8000")  (((width 8000) (height 177) (max_bytes 3185049)))
    ((2000 16000))  (Too_large "2000\195\15116000 px, over 8000")  (((width 1000) (height 8000) (max_bytes 3185049)))
    ((3000 2000))  (Too_large "4.3 MB, over 3.4 MB")  (((width 3000) (height 2000) (max_bytes 3185049)))
    ()  (Too_large "4.3 MB, over 3.4 MB")  (((width 8000) (height 8000) (max_bytes 3185049)))
    ((max_bytes 3538944) (max_dimension 8000))
    |}]
;;

let%expect_test "sniffing, and which files are worth reading as images" =
  List.iter
    [ png ~width:1 ~height:1
    ; "\xff\xd8\xff\xe0"
    ; "GIF89a...."
    ; "RIFF\x00\x00\x00\x00WEBPVP8 "
    ; "<svg xmlns='http://www.w3.org/2000/svg'/>"
    ]
    ~f:(fun raw ->
      print_s [%sexp (Image_size.sniff (base64 raw) : string option)]);
  List.iter
    [ "image/png", "shot.png"
    ; "image/heic", "IMG_1.HEIC"
    ; "", "photo.JPG"
    ; "application/octet-stream", "scan.webp"
    ; "", "notes.txt"
    ; "image/svg+xml", "logo.svg"
    ; "text/plain", "x.png"
    ]
    ~f:(fun (mime_type, name) ->
      printf
        "%-26s %-12s %b\n"
        mime_type
        name
        (Image_size.looks_like_image ~mime_type ~name));
  print_endline (Image_size.size_hum 655_360);
  print_endline (Image_size.size_hum 300);
  print_endline (Image_size.size_hum 5_000_000);
  [%expect
    {|
    (image/png)
    (image/jpeg)
    (image/gif)
    (image/webp)
    ()
    image/png                  shot.png     true
    image/heic                 IMG_1.HEIC   true
                               photo.JPG    true
    application/octet-stream   scan.webp    true
                               notes.txt    false
    image/svg+xml              logo.svg     false
    text/plain                 x.png        false
    640 KB
    300 B
    4.8 MB
    |}]
;;

let%expect_test "an image too large for the model: downsized in the page" =
  let h = H.create () in
  let big = image ~bytes:2_400_000 (png ~width:9000 ~height:200) in
  H.act h (Add_image (image (png ~width:800 ~height:600)));
  H.act h (Add_image big);
  H.elements h ~selector:".pending-image";
  H.show h ~selector:".downsize-image";
  [%expect
    {|
    <div class="pending-image">
    <div class="pending-image too-large">
    <button type="button"
            title="Too large for the model (9000×200 px, over 8000). Downsize it here, or send it as it is and prigh tries to send it smaller"
            aria-label="Downsize the image: too large for the model, 9000×200 px, over 8000"
            class="downsize-image"
            @on_click>
      <icon class="warning"> </icon>
      <span class="downsize-text">
        <span class="downsize-why"> Too large </span>
        <span class="downsize-label"> Downsize </span>
      </span>
    </button>
    |}];
  H.click h ~selector:".downsize-image";
  [%expect
    {|
    (Downsize_image 1)
    (Downsize_image (image ((mime_type image/png) (bytes 2400000)))
     (target ((width 8000) (height 177) (max_bytes 3185049))))
    |}];
  (* Once is enough while it works. *)
  H.act h (Downsize_image 1);
  H.elements h ~selector:".pending-image";
  H.text h ~selector:".downsize-image";
  [%expect
    {|
    <div class="pending-image">
    <div class="downsizing pending-image too-large">
    (Too large Downsizing)
    |}];
  let small = image ~bytes:310_000 (png ~width:8000 ~height:177) in
  H.act
    h
    (Downsized
       { original = (H.model h).images |> Fn.flip List.nth_exn 1
       ; image = small
       });
  H.elements h ~selector:".pending-image";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 3020))
    <div class="pending-image">
    <div class="pending-image">
    Downsized the image: 2.3 MB → 303 KB
    |}];
  (* Failing says why, and the tile can be tried again or sent as it is. *)
  H.act h (Add_image big);
  H.act h (Downsize_image 2);
  H.act
    h
    (Downsize_failed
       { original = big; reason = "the browser could not decode it" });
  H.elements h ~selector:".pending-image";
  [%expect
    {|
    (Downsize_image (image ((mime_type image/png) (bytes 2400000)))
     (target ((width 8000) (height 177) (max_bytes 3185049))))
    <div class="pending-image">
    <div class="pending-image">
    <div class="pending-image too-large">
    |}]
;;

let%expect_test "the verdict follows the model's limit" =
  let model_with_limit =
    H.state_json
      ~fields:
        [ ( "model"
          , Jsonaf.of_string
              {|{"id":"tiny","provider":"custom","key":"custom/tiny","name":"Tiny","context_window":1000,"max_output":100,"supports_thinking":false,"supports_images":true,"image_limit":{"max_bytes":1000000,"max_dimension":1000},"cost":{"input":0,"output":0,"cache_read":0}}|}
          )
        ]
      ()
  in
  let h = H.create ~state:model_with_limit () in
  H.act h (Add_image (image (png ~width:1500 ~height:1000)));
  H.elements h ~selector:".pending-image";
  H.act h (Downsize_image 0);
  [%expect
    {|
    <div class="pending-image too-large">
    (Downsize_image (image ((mime_type image/png) (bytes 29)))
     (target ((width 1000) (height 666) (max_bytes 900000))))
    |}]
;;
