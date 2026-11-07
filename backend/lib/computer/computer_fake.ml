open! Core
open! Import

type t =
  { info : Computer_info.t
  ; max_long_side : int
  ; max_pixels : int
  ; mutable cursor : int * int
  ; mutable left_at : int * int (** where this helper last put the pointer *)
  ; log : string Queue.t
  ; replies : string Queue.t
  ; mutable crash : unit -> unit (** kills the latest transport *)
  }

let create
      ?(width = 2560)
      ?(height = 1600)
      ?(max_long_side = 1280)
      ?(max_pixels = 1_150_000)
      ()
  =
  let scaled_width, scaled_height =
    Computer_info.scaled_size ~max_long_side ~max_pixels ~width ~height
  in
  let center = width / 2, height / 2 in
  { info =
      { backend = "fake"
      ; detail = None
      ; platform = "linux"
      ; output = Some "FAKE-1"
      ; width
      ; height
      ; scale = 1.
      ; scaled_width
      ; scaled_height
      }
  ; max_long_side
  ; max_pixels
  ; cursor = center
  ; left_at = center
  ; log = Queue.create ()
  ; replies = Queue.create ()
  ; crash = ignore
  }
;;

let info t = t.info
let log t = Queue.to_list t.log
let user_move t ~x ~y = t.cursor <- x, y

(* PNG: CRC-32 over chunks, zlib with stored (uncompressed) blocks. *)
let crc_table =
  lazy
    (Array.init 256 ~f:(fun n ->
       let c = ref n in
       for _ = 0 to 7 do
         c := if !c land 1 = 1 then 0xEDB88320 lxor (!c lsr 1) else !c lsr 1
       done;
       !c))
;;

let crc32 s =
  let table = Lazy.force crc_table in
  let c = ref 0xFFFFFFFF in
  String.iter s ~f:(fun ch ->
    c := table.(!c lxor Char.to_int ch land 0xFF) lxor (!c lsr 8));
  !c lxor 0xFFFFFFFF
;;

let be32 n =
  String.init 4 ~f:(fun i -> Char.of_int_exn ((n lsr (8 * (3 - i))) land 0xFF))
;;

let chunk kind data =
  be32 (String.length data) ^ kind ^ data ^ be32 (crc32 (kind ^ data))
;;

let zlib_stored data =
  let buf = Buffer.create (String.length data + 64) in
  Buffer.add_string buf "\x78\x01";
  let len = String.length data in
  let rec blocks pos =
    let n = Int.min 65535 (len - pos) in
    let last = pos + n >= len in
    Buffer.add_char buf (if last then '\x01' else '\x00');
    Buffer.add_char buf (Char.of_int_exn (n land 0xFF));
    Buffer.add_char buf (Char.of_int_exn (n lsr 8));
    Buffer.add_char buf (Char.of_int_exn (lnot n land 0xFF));
    Buffer.add_char buf (Char.of_int_exn ((lnot n lsr 8) land 0xFF));
    Buffer.add_substring buf data ~pos ~len:n;
    if not last then blocks (pos + n)
  in
  blocks 0;
  let a = ref 1
  and b = ref 0 in
  String.iter data ~f:(fun ch ->
    a := (!a + Char.to_int ch) mod 65521;
    b := (!b + !a) mod 65521);
  Buffer.add_string buf (be32 ((!b lsl 16) lor !a));
  Buffer.contents buf
;;

let png_with ~width ~height ~pixel =
  let raw = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    Buffer.add_char raw '\x00';
    for x = 0 to width - 1 do
      Buffer.add_char raw (pixel ~x ~y)
    done
  done;
  let ihdr = be32 width ^ be32 height ^ "\x08\x00\x00\x00\x00" in
  "\x89PNG\r\n\x1a\n"
  ^ chunk "IHDR" ihdr
  ^ chunk "IDAT" (zlib_stored (Buffer.contents raw))
  ^ chunk "IEND" ""
;;

let png ~width ~height =
  png_with ~width ~height ~pixel:(fun ~x:_ ~y:_ -> '\x80')
;;

let to_scaled t (x, y) =
  let map v ~real ~scaled =
    Float.iround_nearest_exn
      (Float.of_int v *. Float.of_int scaled /. Float.of_int real)
  in
  ( map x ~real:t.info.width ~scaled:t.info.scaled_width
  , map y ~real:t.info.height ~scaled:t.info.scaled_height )
;;

let screenshot ~width ~height ~cursor =
  let data =
    png_with ~width ~height ~pixel:(fun ~x ~y ->
      match cursor with
      | Some (cx, cy) when abs (x - cx) <= 1 && abs (y - cy) <= 1 -> '\x00'
      | _ -> '\x80')
  in
  let int n = `Number (Int.to_string n) in
  `Object
    [ "mime_type", `String "image/png"
    ; "data", `String (Base64.encode_exn data)
    ; "width", int width
    ; "height", int height
    ]
;;

let point (x, y) =
  `Object [ "x", `Number (Int.to_string x); "y", `Number (Int.to_string y) ]
;;

let act t (action : Computer_action.t) =
  let record fmt = ksprintf (fun s -> Queue.enqueue t.log s) fmt in
  let real (p : Computer_action.Point.t) =
    Computer_info.to_real t.info ~x:p.x ~y:p.y
  in
  let move_to p =
    let x, y = real p in
    record "move %d,%d" x y;
    t.cursor <- x, y
  in
  let move_opt = Option.iter ~f:move_to in
  let keys = Option.value_map ~default:"" ~f:(fun k -> " +" ^ k) in
  let button = Computer_action.Button.to_string in
  let full () =
    screenshot
      ~width:t.info.scaled_width
      ~height:t.info.scaled_height
      ~cursor:(Some (to_scaled t t.cursor))
  in
  let shot, text =
    match action with
    | Screenshot | Cursor_position -> Some (full ()), ""
    | Click { at; button = b; count; keys = k } ->
      move_opt at;
      record "click %s×%d%s" (button b) count (keys k);
      Some (full ()), ""
    | Move p ->
      move_to p;
      Some (full ()), ""
    | Drag { path; button = b; keys = k } ->
      move_to (List.hd_exn path);
      record "%s down%s" (button b) (keys k);
      List.iter (List.tl_exn path) ~f:move_to;
      record "%s up" (button b);
      Some (full ()), ""
    | Mouse_down { at; button = b } ->
      move_opt at;
      record "%s down" (button b);
      Some (full ()), ""
    | Mouse_up { at; button = b } ->
      move_opt at;
      record "%s up" (button b);
      Some (full ()), ""
    | Scroll { at; dx; dy; keys = k } ->
      move_opt at;
      record "scroll %d,%d%s" dx dy (keys k);
      Some (full ()), ""
    | Type text ->
      record "type %s" text;
      Some (full ()), ""
    | Key combo ->
      record "key %s" combo;
      Some (full ()), ""
    | Hold_key { combo; duration } ->
      record "hold %s %gs" combo duration;
      Some (full ()), ""
    | Wait s ->
      record "wait %gs" s;
      None, "waited"
    | Zoom { x0; y0; x1; y1 } ->
      let rx0, ry0 = Computer_info.to_real t.info ~x:x0 ~y:y0 in
      let rx1, ry1 = Computer_info.to_real t.info ~x:x1 ~y:y1 in
      let width, height =
        Computer_info.scaled_size
          ~max_long_side:t.max_long_side
          ~max_pixels:t.max_pixels
          ~width:(Int.max 1 (rx1 - rx0))
          ~height:(Int.max 1 (ry1 - ry0))
      in
      ( Some (screenshot ~width ~height ~cursor:None)
      , sprintf "(%d, %d)-(%d, %d) on the screen" rx0 ry0 rx1 ry1 )
  in
  t.left_at <- t.cursor;
  `Object
    (List.filter_opt
       [ Some ("ok", `True)
       ; Some ("text", `String text)
       ; Some ("cursor", point (to_scaled t t.cursor))
       ; Option.map (Computer_action.point action) ~f:(fun p ->
           "real", point (real p))
       ; Option.map shot ~f:(fun s -> "screenshot", s)
       ])
;;

let handle t json =
  let field name =
    match json with
    | `Object fields -> List.Assoc.find fields ~equal:String.equal name
    | _ -> None
  in
  let error ?(interrupted = false) message =
    `Object
      [ "ok", `False
      ; "error", `String message
      ; ("interrupted", if interrupted then `True else `False)
      ]
  in
  let reply =
    match field "op" with
    | Some (`String "info") ->
      `Object [ "ok", `True; "info", Computer_info.to_json t.info ]
    | Some (`String "act") ->
      if not ([%equal: int * int] t.cursor t.left_at)
      then (
        t.left_at <- t.cursor;
        error
          ~interrupted:true
          "you moved the mouse: stopped (computer use paused until the next \
           prompt)")
      else (
        match
          Computer_action.of_json (Option.value (field "action") ~default:`Null)
        with
        | Ok action -> act t action
        | Error e -> error (Error.to_string_hum e))
    | _ -> error "unknown op: use info or act"
  in
  let reply =
    match reply, field "id" with
    | `Object fields, Some id -> `Object (("id", id) :: fields)
    | reply, _ -> reply
  in
  Queue.enqueue t.replies (Json.to_string reply)
;;

let crash t = t.crash ()

let transport t =
  let dead = ref false in
  t.crash <- (fun () -> dead := true);
  { Computer_helper.Transport.send =
      (fun line ->
        if !dead then failwith "prigh-computer is gone";
        match Json.parse line with
        | Ok json -> handle t json
        | Error _ -> ())
  ; receive = (fun () -> if !dead then None else Queue.dequeue t.replies)
  ; kill = (fun () -> dead := true)
  }
;;
