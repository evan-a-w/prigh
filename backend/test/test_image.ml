open! Core
open! Prigh
open Tool_test_helpers
module F = Image_fixtures

let%expect_test "sniff and dimensions" =
  List.iter
    [ "png", F.png_3x2
    ; "gif", F.gif_5x4
    ; "jpeg", F.jpeg_7x6
    ; "webp (VP8)", F.webp_lossy_9x8
    ; "webp (VP8L)", F.webp_lossless_11x10
    ; "webp (VP8X)", F.webp_extended_13x12
    ; "bmp", F.bmp_2x2
    ]
    ~f:(fun (name, base64) ->
      let bytes = F.bytes base64 in
      print_s
        [%message
          name
            ~mime:(Image.sniff bytes : string option)
            ~size:(Image.dimensions bytes : (int * int) option)]);
  print_s
    [%message
      "truncated"
        ~png:
          (Image.dimensions (String.prefix (F.bytes F.png_3x2) 20)
           : (int * int) option)
        ~jpeg:
          (Image.dimensions (String.prefix (F.bytes F.jpeg_7x6) 100)
           : (int * int) option)
        ~text:(Image.sniff "hello" : string option)];
  [%expect
    {|
    (png (mime (image/png)) (size ((3 2))))
    (gif (mime (image/gif)) (size ((5 4))))
    (jpeg (mime (image/jpeg)) (size ((7 6))))
    ("webp (VP8)" (mime (image/webp)) (size ((9 8))))
    ("webp (VP8L)" (mime (image/webp)) (size ((11 10))))
    ("webp (VP8X)" (mime (image/webp)) (size ((13 12))))
    (bmp (mime ()) (size ()))
    (truncated (png ()) (jpeg ()) (text ()))
    |}]
;;

(* A stand-in for ImageMagick: logs its arguments and writes [out] (or fails
   when there is none) to its last argument, the output file. *)
let fake_magick t ?out () =
  let bin = Filename.concat t.dir "bin" in
  Core_unix.mkdir_p bin;
  Option.iter out ~f:(fun data -> write t "out" data);
  write
    t
    "bin/magick"
    (sprintf
       "#!/bin/sh\n\
        echo \"$@\" >> %s/log\n\
        for last; do :; done\n\
        [ -f %s/out ] || exit 1\n\
        cp %s/out \"$last\"\n"
       t.dir
       t.dir
       t.dir);
  Core_unix.chmod (Filename.concat bin "magick") ~perm:0o755;
  [ bin ]
;;

let temp_re = Re.compile (Re.Perl.re {|[^ \n]*prigh.image[^/ \n]*|})

let show_load t ?(search_path = []) bytes =
  (match Image.load ~env:t.env ~search_path bytes with
   | Ok loaded ->
     print_s
       [%message
         (Image.describe loaded)
           ~data_bytes:
             (String.length (Base64.decode_exn loaded.image.data) : int)
           ~note:(loaded.note : string option)]
   | Error e -> print_endline (mask t ("Error: " ^ Error.to_string_hum e)));
  match In_channel.read_all (Filename.concat t.dir "log") with
  | log ->
    print_string (Re.replace_string temp_re ~by:"$TMP" log);
    Core_unix.unlink (Filename.concat t.dir "log")
  | exception _ -> ()
;;

let%expect_test "small images are sent as they are" =
  with_sandbox
  @@ fun t ->
  let search_path = fake_magick t () in
  show_load t ~search_path (F.bytes F.png_3x2);
  show_load t ~search_path (F.bytes F.webp_lossy_9x8);
  show_load t (F.bytes F.bmp_2x2);
  [%expect
    {|
    ("image/png, 3x2" (data_bytes 84) (note ()))
    ("image/webp, 9x8" (data_bytes 68) (note ()))
    Error: not a supported image; models accept PNG, JPEG, GIF and WebP
    |}]
;;

let%expect_test "large images are downscaled" =
  with_sandbox
  @@ fun t ->
  let big = F.png_header ~width:4000 ~height:3000 in
  let search_path =
    fake_magick t ~out:(F.png_header ~width:2000 ~height:1500) ()
  in
  show_load t ~search_path big;
  [%expect
    {|
    ("image/png, 2000x1500" (data_bytes 29)
     (note
      ("[Image: original 4000x3000, displayed at 2000x1500. Multiply coordinates by 2.00 to map to the original image.]")))
    $TMP/input[0] -auto-orient -resize 2000x2000> -strip $TMP/output.png
    |}];
  (* A JPEG source goes straight to JPEG. Outputs that aren't what was asked
     for are rejected; when every attempt fails, an image within the
     providers' hard limits is sent as it is. *)
  let jpeg_header =
    "\xff\xd8\xff\xc0\x00\x11\x08\x0b\xb8\x0f\xa0" ^ String.make 20 '\x00'
  in
  show_load t ~search_path jpeg_header;
  [%expect
    {|
    ("image/jpeg, 4000x3000" (data_bytes 31) (note ()))
    $TMP/input[0] -auto-orient -resize 2000x2000> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 2000x2000> -strip -background white -flatten -quality 60 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1500x1500> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1500x1500> -strip -background white -flatten -quality 60 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1000x1000> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1000x1000> -strip -background white -flatten -quality 60 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 768x768> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 768x768> -strip -background white -flatten -quality 60 $TMP/output.jpg
    |}]
;;

let%expect_test "without a working downscaler" =
  with_sandbox
  @@ fun t ->
  (* Within the providers' hard limits: sent as it is. *)
  show_load t (F.png_header ~width:4000 ~height:3000);
  show_load t (F.png_header ~width:9000 ~height:100);
  let search_path = fake_magick t () in
  show_load t ~search_path (F.png_header ~width:4000 ~height:3000);
  [%expect
    {|
    ("image/png, 4000x3000" (data_bytes 29) (note ()))
    Error: image too large for the model (image/png 9000x100, 0.0 MB; at most 8000x8000 px and 3.4 MB): install ImageMagick (magick) or downscale it, e.g. `magick IN -resize 2000x2000\> OUT.png`, and read the result
    ("image/png, 4000x3000" (data_bytes 29) (note ()))
    $TMP/input[0] -auto-orient -resize 2000x2000> -strip $TMP/output.png
    $TMP/input[0] -auto-orient -resize 2000x2000> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 2000x2000> -strip -background white -flatten -quality 60 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1500x1500> -strip $TMP/output.png
    $TMP/input[0] -auto-orient -resize 1500x1500> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1500x1500> -strip -background white -flatten -quality 60 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1000x1000> -strip $TMP/output.png
    $TMP/input[0] -auto-orient -resize 1000x1000> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 1000x1000> -strip -background white -flatten -quality 60 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 768x768> -strip $TMP/output.png
    $TMP/input[0] -auto-orient -resize 768x768> -strip -background white -flatten -quality 85 $TMP/output.jpg
    $TMP/input[0] -auto-orient -resize 768x768> -strip -background white -flatten -quality 60 $TMP/output.jpg
    |}]
;;

let%expect_test "of_base64: images from clients" =
  with_sandbox
  @@ fun t ->
  let show ~mime_type data =
    match Image.of_base64 ~env:t.env ~search_path:[] ~mime_type data with
    | Ok image ->
      print_s
        [%message
          image.mime_type ~same_data:(String.equal image.data data : bool)]
    | Error e -> print_endline ("Error: " ^ Error.to_string_hum e)
  in
  show ~mime_type:"image/png" F.png_3x2;
  (* The bytes decide, not what the client declared. *)
  show ~mime_type:"image/jpg" F.jpeg_7x6;
  show ~mime_type:"image/bmp" F.bmp_2x2;
  show ~mime_type:"image/png" "not base64!";
  [%expect
    {|
    (image/png (same_data true))
    (image/jpeg (same_data true))
    Error: unsupported image type image/bmp; models accept image/png, image/jpeg, image/gif and image/webp
    Error: image data is not valid base64: Malformed input
    |}]
;;
