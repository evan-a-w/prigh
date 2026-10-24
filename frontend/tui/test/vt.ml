open! Core
module Style = Prigh_ui.Style
module Text_width = Prigh_ui.Text_width
module Content = Prigh_ui.Content
open Style.Color

module Cell = struct
  type t =
    { text : string
    ; style : Style.t
    }
end

module Parser_state = struct
  type t =
    | Ground
    | Escape
    | Csi
    | Osc
    | Osc_esc
    | Skip_one
end

type t =
  { mutable width : int
  ; mutable height : int
  ; mutable grid : Cell.t array array
  ; mutable row : int
  ; mutable col : int
  ; mutable visible : bool
  ; mutable saved : (int * int) option
  ; mutable style : Style.t
  ; mutable link : string option
  ; mutable state : Parser_state.t
  ; csi : Buffer.t
  ; osc : Buffer.t
  ; utf8 : Buffer.t
  ; mutable utf8_need : int
  }

let blank_cell = { Cell.text = " "; style = Style.plain }

let clear_grid ~width ~height =
  Array.init height ~f:(fun _ -> Array.init width ~f:(fun _ -> blank_cell))
;;

(* Like tmux with an alternate-screen app: contents are cropped or padded, never
   reflowed, because Notty's next frame only rewrites changed rows. *)
let resize t ~width ~height =
  let width = Int.max 1 width
  and height = Int.max 1 height in
  let grid =
    Array.init height ~f:(fun r ->
      Array.init width ~f:(fun c ->
        if r < t.height && c < t.width then t.grid.(r).(c) else blank_cell))
  in
  t.width <- width;
  t.height <- height;
  t.grid <- grid;
  t.row <- Int.min t.row (height - 1);
  t.col <- Int.min t.col (width - 1)
;;

let create ~width ~height =
  let width = Int.max 1 width in
  let height = Int.max 1 height in
  { width
  ; height
  ; grid = clear_grid ~width ~height
  ; row = 0
  ; col = 0
  ; visible = true
  ; saved = None
  ; style = Style.plain
  ; link = None
  ; state = Ground
  ; csi = Buffer.create 16
  ; osc = Buffer.create 32
  ; utf8 = Buffer.create 8
  ; utf8_need = 0
  }
;;

let in_bounds t r c = r >= 0 && r < t.height && c >= 0 && c < t.width

let set_cell t r c text =
  if in_bounds t r c
  then t.grid.(r).(c) <- { Cell.text; style = { t.style with link = t.link } }
;;

let erase_cell t r c = if in_bounds t r c then t.grid.(r).(c) <- blank_cell

let erase_row t r c0 c1 =
  for c = c0 to c1 do
    erase_cell t r c
  done
;;

let scroll_up t =
  for r = 0 to t.height - 2 do
    t.grid.(r) <- t.grid.(r + 1)
  done;
  t.grid.(t.height - 1) <- Array.init t.width ~f:(fun _ -> blank_cell)
;;

let newline t =
  if t.row + 1 >= t.height then scroll_up t else t.row <- t.row + 1;
  t.col <- 0
;;

let write_utf8 t s =
  List.iter (Text_width.uchars s) ~f:(fun (text, w) ->
    if w = 0
    then (
      if t.col > 0
      then (
        let c = t.col - 1 in
        if in_bounds t t.row c
        then
          t.grid.(t.row).(c)
          <- { (t.grid.(t.row).(c)) with text = t.grid.(t.row).(c).text ^ text }))
    else (
      if t.col >= t.width then newline t;
      set_cell t t.row t.col text;
      if w >= 2 && t.col + 1 < t.width then set_cell t t.row (t.col + 1) "";
      t.col <- t.col + w))
;;

let utf8_len b =
  if b land 0xe0 = 0xc0
  then 2
  else if b land 0xf0 = 0xe0
  then 3
  else if b land 0xf8 = 0xf0
  then 4
  else 1
;;

let flush_utf8 t =
  if Buffer.length t.utf8 > 0
  then (
    write_utf8 t (Buffer.contents t.utf8);
    Buffer.clear t.utf8;
    t.utf8_need <- 0)
;;

let basic_color n =
  match n with
  | 0 -> Style.Color.Default
  | 1 -> Red
  | 2 -> Green
  | 3 -> Yellow
  | 4 -> Blue
  | 5 -> Magenta
  | 6 -> Cyan
  | 7 -> White
  | _ -> Default
;;

let light_color n =
  match n with
  | 90 -> Gray
  | 91 -> Red
  | 92 -> Green
  | 93 -> Yellow
  | 94 -> Blue
  | 95 -> Magenta
  | 96 -> Cyan
  | 97 -> White
  | _ -> Default
;;

let color_of_256 n =
  if n < 8
  then basic_color n
  else if n = 8
  then Gray
  else if n < 16
  then light_color (n + 82)
  else Default
;;

let reset_style t = t.style <- Style.plain

let handle_sgr t ~params =
  let set_fg c = t.style <- { t.style with fg = c } in
  let set_bold b = t.style <- { t.style with bold = b } in
  let set_dim b = t.style <- { t.style with dim = b } in
  let set_italic b = t.style <- { t.style with italic = b } in
  let set_underline b = t.style <- { t.style with underline = b } in
  let set_invert b = t.style <- { t.style with invert = b } in
  let rec go = function
    | [] -> ()
    | 0 :: rest ->
      reset_style t;
      go rest
    | 1 :: rest ->
      set_bold true;
      go rest
    | 2 :: rest ->
      set_dim true;
      go rest
    | 3 :: rest ->
      set_italic true;
      go rest
    | 4 :: rest ->
      set_underline true;
      go rest
    | 7 :: rest ->
      set_invert true;
      go rest
    | 22 :: rest ->
      set_bold false;
      go rest
    | 23 :: rest ->
      set_italic false;
      go rest
    | 24 :: rest ->
      set_underline false;
      go rest
    | 27 :: rest ->
      set_invert false;
      go rest
    | 39 :: rest ->
      set_fg Default;
      go rest
    | n :: rest when n >= 30 && n <= 37 ->
      set_fg (basic_color (n - 30));
      go rest
    | n :: rest when n >= 90 && n <= 97 ->
      set_fg (light_color n);
      go rest
    | 38 :: 5 :: n :: rest ->
      set_fg (color_of_256 n);
      go rest
    | 38 :: 2 :: _r :: _g :: _b :: rest -> go rest
    | 48 :: 5 :: _n :: rest -> go rest
    | 48 :: 2 :: _r :: _g :: _b :: rest -> go rest
    | _ :: rest -> go rest
  in
  go (if List.is_empty params then [ 0 ] else params)
;;

let csi_params s =
  let prefix = Buffer.create 4 in
  let nums = ref [] in
  let num = Buffer.create 4 in
  String.iter s ~f:(fun ch ->
    match ch with
    | '0' .. '9' -> Buffer.add_char num ch
    | ';' ->
      nums
      := (if Buffer.length num = 0
          then 0
          else Int.of_string (Buffer.contents num))
         :: !nums;
      Buffer.clear num
    | '?' | '>' | '!' | '<' | '=' -> Buffer.add_char prefix ch
    | _ -> ());
  if Buffer.length num > 0
  then nums := Int.of_string (Buffer.contents num) :: !nums;
  List.rev !nums, Buffer.contents prefix
;;

let clamp lo hi x = Int.max lo (Int.min hi x)

let erase_display t n =
  match n with
  | 0 ->
    erase_row t t.row t.col (t.width - 1);
    for r = t.row + 1 to t.height - 1 do
      erase_row t r 0 (t.width - 1)
    done
  | 1 ->
    for r = 0 to t.row - 1 do
      erase_row t r 0 (t.width - 1)
    done;
    erase_row t t.row 0 t.col
  | _ ->
    for r = 0 to t.height - 1 do
      erase_row t r 0 (t.width - 1)
    done
;;

let erase_line t n =
  match n with
  | 0 -> erase_row t t.row t.col (t.width - 1)
  | 1 -> erase_row t t.row 0 t.col
  | _ -> erase_row t t.row 0 (t.width - 1)
;;

let handle_csi t ~final =
  let params, prefix = csi_params (Buffer.contents t.csi) in
  let p1 =
    match params with
    | [] -> 1
    | x :: _ -> x
  in
  let p0 =
    match params with
    | [] -> 0
    | x :: _ -> x
  in
  let p2 =
    match params with
    | [] -> 1
    | _ :: x :: _ -> x
    | [ _ ] -> 1
  in
  match final with
  | 'H' | 'f' ->
    t.row <- clamp 0 (t.height - 1) (Int.max 0 (p1 - 1));
    t.col <- clamp 0 (t.width - 1) (Int.max 0 (p2 - 1))
  | 'A' -> t.row <- clamp 0 (t.height - 1) (t.row - Int.max 1 p1)
  | 'B' -> t.row <- clamp 0 (t.height - 1) (t.row + Int.max 1 p1)
  | 'C' -> t.col <- clamp 0 (t.width - 1) (t.col + Int.max 1 p1)
  | 'D' -> t.col <- clamp 0 (t.width - 1) (t.col - Int.max 1 p1)
  | 'E' ->
    t.row <- clamp 0 (t.height - 1) (t.row + Int.max 1 p1);
    t.col <- 0
  | 'F' ->
    t.row <- clamp 0 (t.height - 1) (t.row - Int.max 1 p1);
    t.col <- 0
  | 'G' | '`' -> t.col <- clamp 0 (t.width - 1) (Int.max 0 (p1 - 1))
  | 'd' -> t.row <- clamp 0 (t.height - 1) (Int.max 0 (p1 - 1))
  | 'J' -> erase_display t p0
  | 'K' -> erase_line t p0
  | 'm' -> handle_sgr t ~params
  | 'h' | 'l' ->
    let on = Char.equal final 'h' in
    if String.mem prefix '?'
    then
      List.iter params ~f:(fun n ->
        match n with
        | 25 -> t.visible <- on
        | 1049 ->
          for r = 0 to t.height - 1 do
            erase_row t r 0 (t.width - 1)
          done;
          t.row <- 0;
          t.col <- 0
        | _ -> ())
  | 's' -> t.saved <- Some (t.row, t.col)
  | 'u' ->
    (match t.saved with
     | Some (r, c) ->
       t.row <- clamp 0 (t.height - 1) r;
       t.col <- clamp 0 (t.width - 1) c
     | None -> ())
  | _ -> ()
;;

let handle_osc t =
  let s = Buffer.contents t.osc in
  if String.is_prefix s ~prefix:"8;"
  then (
    let rest = String.drop_prefix s 2 in
    let url =
      match String.index rest ';' with
      | Some i -> String.drop_prefix rest (i + 1)
      | None -> ""
    in
    t.link <- Option.some_if (not (String.is_empty url)) url)
;;

let write_ascii t ch =
  if t.col >= t.width then newline t;
  set_cell t t.row t.col (String.of_char ch);
  t.col <- t.col + 1
;;

let handle_ground_char t b =
  match Char.of_int_exn b with
  | '\r' -> t.col <- 0
  | '\n' -> newline t
  | '\b' -> t.col <- clamp 0 (t.width - 1) (t.col - 1)
  | '\t' -> t.col <- clamp 0 (t.width - 1) ((t.col / 8 * 8) + 8)
  | '\007' -> ()
  | ch when Char.to_int ch >= 0x20 && Char.to_int ch < 0x7f -> write_ascii t ch
  | _ -> ()
;;

let feed_byte t b =
  match t.state with
  | Ground ->
    if b = 0x1b
    then t.state <- Escape
    else if b < 0x80
    then (
      flush_utf8 t;
      handle_ground_char t b)
    else (
      if Buffer.length t.utf8 = 0 then t.utf8_need <- utf8_len b;
      Buffer.add_char t.utf8 (Char.of_int_exn b);
      if Buffer.length t.utf8 >= t.utf8_need then flush_utf8 t)
  | Escape ->
    (match Char.of_int_exn b with
     | '[' ->
       Buffer.clear t.csi;
       t.state <- Csi
     | ']' ->
       Buffer.clear t.osc;
       t.state <- Osc
     | '7' ->
       t.saved <- Some (t.row, t.col);
       t.state <- Ground
     | '8' ->
       (match t.saved with
        | Some (r, c) ->
          t.row <- clamp 0 (t.height - 1) r;
          t.col <- clamp 0 (t.width - 1) c
        | None -> ());
       t.state <- Ground
     | 'c' ->
       for r = 0 to t.height - 1 do
         erase_row t r 0 (t.width - 1)
       done;
       t.row <- 0;
       t.col <- 0;
       reset_style t;
       t.link <- None;
       t.visible <- true;
       t.state <- Ground
     | 'M' ->
       if t.row = 0 then scroll_up t else t.row <- t.row - 1;
       t.state <- Ground
     | 'D' ->
       newline t;
       t.state <- Ground
     | 'E' ->
       newline t;
       t.state <- Ground
     | '(' | ')' | '*' | '+' | '#' -> t.state <- Skip_one
     | _ -> t.state <- Ground)
  | Skip_one -> t.state <- Ground
  | Csi ->
    if b = 0x1b
    then t.state <- Escape
    else if b >= 0x40 && b <= 0x7e
    then (
      handle_csi t ~final:(Char.of_int_exn b);
      t.state <- Ground)
    else Buffer.add_char t.csi (Char.of_int_exn b)
  | Osc ->
    (match b with
     | 0x07 ->
       handle_osc t;
       t.state <- Ground
     | 0x1b -> t.state <- Osc_esc
     | _ -> Buffer.add_char t.osc (Char.of_int_exn b))
  | Osc_esc ->
    if b = Char.to_int '\\' then handle_osc t;
    t.state <- Ground
;;

let feed t s = String.iter s ~f:(fun ch -> feed_byte t (Char.to_int ch))

let to_plain t =
  let rows =
    List.init t.height ~f:(fun r ->
      let text =
        String.concat
          (List.map (Array.to_list t.grid.(r)) ~f:(fun c -> c.Cell.text))
      in
      let text =
        if t.visible
        then (
          match t.row = r with
          | false -> text
          | true ->
            let c = t.col in
            let padded = Text_width.pad_right text ~width:(c + 1) in
            let before, rest = Text_width.take padded ~width:c in
            let _, after = Text_width.take rest ~width:1 in
            before ^ "▏" ^ after)
        else text
      in
      String.rstrip text)
  in
  String.concat ~sep:"\n" rows
;;

let trimmed_cells t r =
  let row = t.grid.(r) in
  let last = ref (-1) in
  Array.iteri row ~f:(fun i (c : Cell.t) ->
    if not (String.for_all c.text ~f:Char.is_whitespace) then last := i);
  if !last < 0
  then []
  else Array.to_list (Array.sub row ~pos:0 ~len:(!last + 1))
;;

let to_styled t =
  let line r =
    let spans = ref [] in
    let buf = Buffer.create 32 in
    let style = ref Style.plain in
    let flush () =
      if Buffer.length buf > 0
      then (
        spans
        := { Content.Span.text = Buffer.contents buf; style = !style } :: !spans;
        Buffer.clear buf)
    in
    List.iter (trimmed_cells t r) ~f:(fun (c : Cell.t) ->
      if Buffer.length buf > 0 && not (Style.equal c.style !style)
      then (
        flush ();
        style := c.style);
      if Buffer.length buf = 0 then style := c.style;
      Buffer.add_string buf c.text);
    flush ();
    List.rev !spans
  in
  String.concat
    ~sep:"\n"
    (List.map (List.init t.height ~f:line) ~f:(fun l -> Content.to_styled [ l ]))
;;
