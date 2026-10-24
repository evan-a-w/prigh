open! Core
open! Import

type t =
  { mime_type : string
  ; data : string
  }
[@@deriving sexp, jsonaf, equal]

let max_dimension = 2000

(* Anthropic refuses images over 5 MB of base64 or 8000 px either way. *)
let max_base64_bytes = 4_718_592
let hard_max_dimension = 8000
let base64_length raw_length = (raw_length + 2) / 3 * 4

let sniff bytes =
  let has ?(at = 0) prefix =
    String.length bytes >= at + String.length prefix
    && String.equal
         (String.sub bytes ~pos:at ~len:(String.length prefix))
         prefix
  in
  if has "\x89PNG\r\n\x1a\n"
  then Some "image/png"
  else if has "\xff\xd8\xff"
  then Some "image/jpeg"
  else if has "GIF87a" || has "GIF89a"
  then Some "image/gif"
  else if has "RIFF" && has ~at:8 "WEBP"
  then Some "image/webp"
  else None
;;

let dimensions bytes =
  let len = String.length bytes in
  let byte i = Char.to_int bytes.[i] in
  let be16 i = (byte i lsl 8) lor byte (i + 1) in
  let le16 i = byte i lor (byte (i + 1) lsl 8) in
  let be32 i = (be16 i lsl 16) lor be16 (i + 2) in
  let le24 i = le16 i lor (byte (i + 2) lsl 16) in
  let positive (w, h) = if w > 0 && h > 0 then Some (w, h) else None in
  let jpeg () =
    (* Walks the segments up to the first start-of-frame marker. *)
    let rec segment i =
      if i + 9 >= len || byte i <> 0xff
      then None
      else (
        let marker = byte (i + 1) in
        if marker = 0xff
        then segment (i + 1)
        else if
          marker = 0xd8 || marker = 0x01 || (marker >= 0xd0 && marker <= 0xd7)
        then segment (i + 2)
        else if
          marker >= 0xc0
          && marker <= 0xcf
          && marker <> 0xc4
          && marker <> 0xc8
          && marker <> 0xcc
        then positive (be16 (i + 7), be16 (i + 5))
        else segment (i + 2 + be16 (i + 2)))
    in
    segment 0
  in
  let webp () =
    if len < 30
    then None
    else (
      match String.sub bytes ~pos:12 ~len:4 with
      | "VP8 " -> positive (le16 26 land 0x3fff, le16 28 land 0x3fff)
      | "VP8L" ->
        let b1 = byte 22
        and b2 = byte 23
        and b3 = byte 24 in
        positive
          ( 1 + (((b1 land 0x3f) lsl 8) lor byte 21)
          , 1
            + (((b3 land 0xf) lsl 10) lor (b2 lsl 2) lor ((b1 land 0xc0) lsr 6))
          )
      | "VP8X" -> positive (1 + le24 24, 1 + le24 27)
      | _ -> None)
  in
  match sniff bytes with
  | Some "image/png" when len >= 24 -> positive (be32 16, be32 20)
  | Some "image/gif" when len >= 10 -> positive (le16 6, le16 8)
  | Some "image/jpeg" -> jpeg ()
  | Some "image/webp" -> webp ()
  | _ -> None
;;

type image = t [@@deriving sexp_of]

module Loaded = struct
  type t =
    { image : image
    ; width : int option
    ; height : int option
    ; note : string option
    }
  [@@deriving sexp_of]
end

let describe (l : Loaded.t) =
  match l.width, l.height with
  | Some w, Some h -> sprintf "%s, %dx%d" l.image.mime_type w h
  | _ -> l.image.mime_type
;;

let loaded ?note bytes ~mime_type : Loaded.t =
  let size = dimensions bytes in
  { Loaded.image = { mime_type; data = Base64.encode_string bytes }
  ; width = Option.map size ~f:fst
  ; height = Option.map size ~f:snd
  ; note
  }
;;

let find_program ~search_path names =
  List.find_map names ~f:(fun name ->
    List.find_map search_path ~f:(fun dir ->
      let path = Filename.concat dir name in
      match Core_unix.access path [ `Exec ] with
      | Ok () when not (Sys_unix.is_directory_exn path) -> Some (name, path)
      | Ok () | Error _ -> None))
;;

module Attempt = struct
  type t =
    { size : int
    ; jpeg_quality : int option (** [None]: PNG *)
    }

  (* PNG keeps screenshots and diagrams sharp; JPEG is the fallback when that
     is still too large, and photos start there. *)
  let all ~mime_type =
    let sizes = [ max_dimension; 1500; 1000; 768 ] in
    List.concat_map sizes ~f:(fun size ->
      (if String.equal mime_type "image/jpeg"
       then []
       else [ { size; jpeg_quality = None } ])
      @ [ { size; jpeg_quality = Some 85 }; { size; jpeg_quality = Some 60 } ])
  ;;

  let extension t = if Option.is_some t.jpeg_quality then "jpg" else "png"

  let args t ~program ~input ~output =
    match program with
    | "sips" ->
      [ "-Z"; Int.to_string t.size ]
      @ (match t.jpeg_quality with
         | None -> [ "-s"; "format"; "png" ]
         | Some q ->
           [ "-s"; "format"; "jpeg"; "-s"; "formatOptions"; Int.to_string q ])
      @ [ input; "--out"; output ]
    | _ ->
      [ input ^ "[0]"
      ; "-auto-orient"
      ; "-resize"
      ; sprintf "%dx%d>" t.size t.size
      ; "-strip"
      ]
      @ (match t.jpeg_quality with
         | None -> []
         | Some q ->
           [ "-background"; "white"; "-flatten"; "-quality"; Int.to_string q ])
      @ [ output ]
  ;;
end

let downscale ~env ~cancel ~program:(name, path) bytes ~mime_type =
  let dir = Filename_unix.temp_dir "prigh-image" "" in
  Exn.protect
    ~finally:(fun () ->
      Array.iter (Sys_unix.readdir dir) ~f:(fun file ->
        Core_unix.unlink (Filename.concat dir file));
      Core_unix.rmdir dir)
    ~f:(fun () ->
      let input = Filename.concat dir "input" in
      Out_channel.write_all input ~data:bytes;
      List.find_map (Attempt.all ~mime_type) ~f:(fun attempt ->
        let output =
          Filename.concat dir ("output." ^ Attempt.extension attempt)
        in
        let result =
          Process.run_collect
            ~env
            ~cancel
            ~timeout:(Time_ns.Span.of_int_sec 60)
            ~prog:path
            ~args:(Attempt.args attempt ~program:name ~input ~output)
            ()
        in
        match result.exit, Sys_unix.file_exists_exn output with
        | Exited 0, true ->
          let out = In_channel.read_all output in
          Core_unix.unlink output;
          let mime_type =
            if Option.is_some attempt.jpeg_quality
            then "image/jpeg"
            else "image/png"
          in
          (match sniff out with
           | Some sniffed
             when String.equal sniffed mime_type
                  && base64_length (String.length out) <= max_base64_bytes ->
             Some (out, mime_type)
           | _ -> None)
        | _ -> None))
;;

let mb bytes = Float.of_int bytes /. 1_048_576.

let load
      ~env
      ?(cancel = Cancellation.never)
      ?(search_path =
        String.split (Option.value (Sys.getenv "PATH") ~default:"") ~on:':')
      bytes
  =
  match sniff bytes with
  | None ->
    Or_error.error_string
      "not a supported image; models accept PNG, JPEG, GIF and WebP"
  | Some mime_type ->
    let size = dimensions bytes in
    let base64 = base64_length (String.length bytes) in
    let fits limit =
      match size with
      | Some (w, h) -> w <= limit && h <= limit
      | None -> true
    in
    if fits max_dimension && base64 <= max_base64_bytes
    then Ok (loaded bytes ~mime_type)
    else (
      let acceptable = fits hard_max_dimension && base64 <= max_base64_bytes in
      let describe_size () =
        let dims =
          match size with
          | Some (w, h) -> sprintf "%dx%d, " w h
          | None -> ""
        in
        sprintf "%s %s%.1f MB" mime_type dims (mb (String.length bytes))
      in
      match find_program ~search_path [ "magick"; "convert"; "sips" ] with
      | None when acceptable -> Ok (loaded bytes ~mime_type)
      | None ->
        Or_error.errorf
          "image too large for the model (%s; at most %dx%d px and %.1f MB): \
           install ImageMagick (magick) or downscale it, e.g. `magick IN \
           -resize %dx%d\\> OUT.png`, and read the result"
          (describe_size ())
          hard_max_dimension
          hard_max_dimension
          (mb (max_base64_bytes * 3 / 4))
          max_dimension
          max_dimension
      | Some program ->
        (match downscale ~env ~cancel ~program bytes ~mime_type with
         | Some (out, out_mime) ->
           let note =
             match size, dimensions out with
             | Some (w, h), Some (w', h') when w' < w ->
               Some
                 (sprintf
                    "[Image: original %dx%d, displayed at %dx%d. Multiply \
                     coordinates by %.2f to map to the original image.]"
                    w
                    h
                    w'
                    h'
                    (Float.of_int w /. Float.of_int w'))
             | _ ->
               Some
                 (sprintf
                    "[Image: re-encoded as %s to fit the model's size limit.]"
                    out_mime)
           in
           Ok (loaded ?note out ~mime_type:out_mime)
         | None when acceptable -> Ok (loaded bytes ~mime_type)
         | None ->
           Or_error.errorf
             "image too large for the model (%s) and %s could not downscale it \
              under %.1f MB; crop or downscale it and read the result"
             (describe_size ())
             (fst program)
             (mb (max_base64_bytes * 3 / 4))))
;;

let of_base64 ~env ?search_path ~mime_type data =
  match Base64.decode data with
  | Error (`Msg msg) -> Or_error.errorf "image data is not valid base64: %s" msg
  | Ok bytes ->
    (match sniff bytes with
     | Some _ ->
       Or_error.map (load ~env ?search_path bytes) ~f:(fun l -> l.image)
     | None ->
       Or_error.errorf
         "unsupported image type %s; models accept image/png, image/jpeg, \
          image/gif and image/webp"
         mime_type)
;;
