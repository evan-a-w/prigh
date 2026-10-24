open! Core

let span ?(style = Style.plain) text = { Content.Span.text; style }
let code_style = Style.fg Cyan
let dim = Style.dim Style.plain
let strike_style base = Style.strike (Style.dim base)

let parse_link (s : string) (i : int) : (string * string * int) option =
  match String.index_from s (i + 1) ']' with
  | None -> None
  | Some j ->
    if j + 1 < String.length s && Char.equal s.[j + 1] '('
    then (
      match String.index_from s (j + 2) ')' with
      | Some k ->
        Some
          ( String.sub s ~pos:(i + 1) ~len:(j - i - 1)
          , String.sub s ~pos:(j + 2) ~len:(k - j - 2)
          , k + 1 )
      | None -> None)
    else None
;;

let inline (line : string) : Content.Line.t =
  let rec parse (s : string) (base : Style.t) : Content.Span.t list =
    let n = String.length s in
    let spans = ref [] in
    let buf = Buffer.create 32 in
    let flush () =
      if Buffer.length buf > 0
      then (
        spans
        := { Content.Span.text = Buffer.contents buf; style = base } :: !spans;
        Buffer.clear buf)
    in
    let emit_text text style =
      flush ();
      spans := { Content.Span.text; style } :: !spans
    in
    let emit_spans l =
      flush ();
      List.iter l ~f:(fun sp -> spans := sp :: !spans)
    in
    let rec loop i =
      if i >= n
      then flush ()
      else (
        match s.[i] with
        | '`' ->
          (match String.index_from s (i + 1) '`' with
           | Some j when j > i + 1 ->
             emit_text (String.sub s ~pos:(i + 1) ~len:(j - i - 1)) code_style;
             loop (j + 1)
           | _ ->
             Buffer.add_char buf '`';
             loop (i + 1))
        | '*' when i + 1 < n && Char.equal s.[i + 1] '*' ->
          (match String.substr_index s ~pos:(i + 2) ~pattern:"**" with
           | Some j when j > i + 2 ->
             emit_spans
               (parse
                  (String.sub s ~pos:(i + 2) ~len:(j - i - 2))
                  (Style.bold base));
             loop (j + 2)
           | _ ->
             Buffer.add_string buf "**";
             loop (i + 2))
        | '*' ->
          (match String.index_from s (i + 1) '*' with
           | Some j when j > i + 1 ->
             emit_spans
               (parse
                  (String.sub s ~pos:(i + 1) ~len:(j - i - 1))
                  (Style.italic base));
             loop (j + 1)
           | _ ->
             Buffer.add_char buf '*';
             loop (i + 1))
        | '~' when i + 1 < n && Char.equal s.[i + 1] '~' ->
          (match String.substr_index s ~pos:(i + 2) ~pattern:"~~" with
           | Some j when j > i + 2 ->
             emit_spans
               (parse
                  (String.sub s ~pos:(i + 2) ~len:(j - i - 2))
                  (strike_style base));
             loop (j + 2)
           | _ ->
             Buffer.add_string buf "~~";
             loop (i + 2))
        | '[' ->
          (match parse_link s i with
           | Some (text, url, next) ->
             emit_text text (Style.link base url);
             loop next
           | None ->
             Buffer.add_char buf '[';
             loop (i + 1))
        | c ->
          Buffer.add_char buf c;
          loop (i + 1))
    in
    loop 0;
    List.rev !spans
  in
  parse line Style.plain
;;

let is_rule s =
  let s = String.strip s in
  String.length s >= 3
  && (String.for_all s ~f:(Char.equal '-')
      || String.for_all s ~f:(Char.equal '*')
      || String.for_all s ~f:(Char.equal '_'))
;;

let is_table_separator s =
  String.contains s '|'
  && String.for_all s ~f:(fun c ->
    Char.equal c '|' || Char.equal c '-' || Char.equal c ':' || Char.equal c ' ')
  && String.exists s ~f:(Char.equal '-')
;;

let is_table_start lines i =
  i + 1 < Array.length lines
  && String.contains lines.(i) '|'
  && is_table_separator lines.(i + 1)
;;

let split_row s =
  let s = String.strip s in
  let s =
    if String.is_prefix s ~prefix:"|" then String.drop_prefix s 1 else s
  in
  let s =
    if String.is_suffix s ~suffix:"|" then String.drop_suffix s 1 else s
  in
  String.split s ~on:'|' |> List.map ~f:String.strip
;;

let render_table ~width (header : string list) (rows : string list list)
  : Content.t
  =
  let all = header :: rows in
  let ncol =
    List.fold all ~init:0 ~f:(fun acc r -> Int.max acc (List.length r))
  in
  if ncol = 0
  then []
  else (
    let cell r i = Option.value (List.nth r i) ~default:"" in
    let natural =
      List.init ncol ~f:(fun i ->
        List.fold all ~init:0 ~f:(fun acc r ->
          Int.max acc (Text_width.string (cell r i))))
    in
    let gap = 3 in
    let available = Int.max ncol (width - (gap * (ncol - 1))) in
    let widths = ref natural in
    let rec shrink () =
      let sum = List.sum (module Int) !widths ~f:Fn.id in
      if sum > available
      then (
        match List.max_elt !widths ~compare:Int.compare with
        | None -> ()
        | Some maxw when maxw > 1 ->
          let reduced = ref false in
          widths
          := List.map !widths ~f:(fun w ->
               if (not !reduced) && w = maxw
               then (
                 reduced := true;
                 w - 1)
               else w);
          shrink ()
        | Some _ -> ())
    in
    shrink ();
    let widths = !widths in
    let pad cell w =
      Text_width.pad_right (Text_width.truncate cell ~width:w) ~width:w
    in
    let row_line style r =
      let spans =
        List.concat
          (List.mapi widths ~f:(fun i w ->
             (if i = 0 then [] else [ span ~style:dim " │ " ])
             @ [ span ~style (pad (cell r i) w) ]))
      in
      Content.Line.truncate spans ~width
    in
    let rule : Content.Line.t =
      List.concat
        (List.mapi widths ~f:(fun i w ->
           (if i = 0 then [] else [ span ~style:dim "─┼─" ])
           @ [ span ~style:dim (String.concat (List.init w ~f:(fun _ -> "─"))) ]))
    in
    row_line (Style.bold Style.plain) header
    :: rule
    :: List.map rows ~f:(row_line Style.plain))
;;

let is_heading s =
  String.length s >= 1
  && Char.equal s.[0] '#'
  && (String.length s = 1
      || Char.equal s.[1] '#'
      || Char.equal s.[1] ' '
      || Char.equal s.[1] '\t')
;;

let heading_level s =
  let rec go i =
    if i < String.length s && Char.equal s.[i] '#' then go (i + 1) else i
  in
  go 0
;;

let is_unordered s =
  String.is_prefix s ~prefix:"- "
  || String.is_prefix s ~prefix:"* "
  || String.is_prefix s ~prefix:"+ "
;;

let ordered_marker s =
  let n = String.length s in
  let rec digits i =
    if i < n && Char.is_digit s.[i] then digits (i + 1) else i
  in
  let d = digits 0 in
  if d > 0 && d + 1 < n && Char.equal s.[d] '.' && Char.equal s.[d + 1] ' '
  then Some (String.sub s ~pos:0 ~len:d, String.drop_prefix s (d + 2))
  else None
;;

let render ?(width = 80) (text : string) : Content.t =
  let lines = String.split_lines text in
  let n = List.length lines in
  let lines = Array.of_list lines in
  let out = ref [] in
  let add line = out := line :: !out in
  let add_lines ls = List.iter ls ~f:add in
  let in_code = ref false in
  let i = ref 0 in
  while !i < n do
    let raw = lines.(!i) in
    let stripped = String.lstrip raw in
    if !in_code
    then (
      if String.is_prefix stripped ~prefix:"```"
      then in_code := false
      else add (Content.Line.of_string ~style:(Style.fg Gray) raw);
      incr i)
    else if String.is_prefix stripped ~prefix:"```"
    then (
      let lang = String.strip (String.drop_prefix stripped 3) in
      add
        (Content.Line.of_string
           ~style:dim
           (if String.is_empty lang then "──" else "── " ^ lang ^ " ──"));
      in_code := true;
      incr i)
    else if is_rule stripped
    then (
      add
        (Content.Line.of_string
           ~style:dim
           (String.concat (List.init width ~f:(fun _ -> "─"))));
      incr i)
    else if is_table_start lines !i
    then (
      let header = split_row lines.(!i) in
      let rows = ref [] in
      let j = ref (!i + 2) in
      while
        !j < n
        && String.contains lines.(!j) '|'
        && not (is_table_separator lines.(!j))
      do
        rows := split_row lines.(!j) :: !rows;
        incr j
      done;
      add_lines (render_table ~width header (List.rev !rows));
      i := !j)
    else if String.is_prefix stripped ~prefix:">"
    then (
      let content = String.strip (String.drop_prefix stripped 1) in
      add (span ~style:dim "▎ " :: inline content);
      incr i)
    else if is_heading stripped
    then (
      let level = heading_level stripped in
      let title =
        String.strip (String.lstrip stripped ~drop:(Char.equal '#'))
      in
      let style =
        match level with
        | 1 -> Style.bold (Style.fg Cyan)
        | 2 -> Style.bold Style.plain
        | _ -> Style.bold (Style.dim Style.plain)
      in
      add (Content.Line.of_string ~style title);
      incr i)
    else if is_unordered stripped
    then (
      let indent = String.length raw - String.length stripped in
      let level = indent / 2 in
      let content = String.drop_prefix stripped 2 in
      add (span (String.make (2 * level) ' ' ^ "• ") :: inline content);
      incr i)
    else (
      match ordered_marker stripped with
      | Some (number, content) ->
        let indent = String.length raw - String.length stripped in
        let level = indent / 2 in
        add
          (span (String.make (2 * level) ' ' ^ number ^ ". ") :: inline content);
        incr i
      | None ->
        add (inline raw);
        incr i)
  done;
  List.rev !out
;;
