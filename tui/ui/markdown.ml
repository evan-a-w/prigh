open! Core

let code_style = Style.fg Cyan
let bold = Style.bold Style.plain

let inline line : Content.Line.t =
  let spans = ref [] in
  let buf = Buffer.create 32 in
  let flush style =
    if Buffer.length buf > 0
    then (
      spans := { Content.Span.text = Buffer.contents buf; style } :: !spans;
      Buffer.clear buf)
  in
  let n = String.length line in
  let rec go i =
    if i >= n
    then flush Style.plain
    else if Char.equal line.[i] '`'
    then (
      match String.index_from line (i + 1) '`' with
      | Some j when j > i + 1 ->
        flush Style.plain;
        Buffer.add_string buf (String.sub line ~pos:(i + 1) ~len:(j - i - 1));
        flush code_style;
        go (j + 1)
      | _ ->
        Buffer.add_char buf '`';
        go (i + 1))
    else if i + 1 < n && Char.equal line.[i] '*' && Char.equal line.[i + 1] '*'
    then (
      match String.substr_index line ~pos:(i + 2) ~pattern:"**" with
      | Some j when j > i + 2 ->
        flush Style.plain;
        Buffer.add_string buf (String.sub line ~pos:(i + 2) ~len:(j - i - 2));
        flush bold;
        go (j + 2)
      | _ ->
        Buffer.add_string buf "**";
        go (i + 2))
    else (
      Buffer.add_char buf line.[i];
      go (i + 1))
  in
  go 0;
  List.rev !spans
;;

let render text : Content.t =
  let in_code = ref false in
  List.map (String.split_lines text) ~f:(fun line ->
    if String.is_prefix (String.lstrip line) ~prefix:"```"
    then (
      in_code := not !in_code;
      let lang = String.strip (String.drop_prefix (String.lstrip line) 3) in
      Content.Line.of_string
        ~style:(Style.dim Style.plain)
        (if String.is_empty lang then "```" else "``` " ^ lang))
    else if !in_code
    then Content.Line.of_string ~style:code_style ("  " ^ line)
    else (
      let stripped = String.lstrip line in
      let indent = String.length line - String.length stripped in
      if String.is_prefix stripped ~prefix:"#"
      then (
        let title =
          String.lstrip (String.lstrip stripped ~drop:(Char.equal '#'))
        in
        Content.Line.of_string ~style:bold title)
      else if String.is_prefix stripped ~prefix:"- "
              || String.is_prefix stripped ~prefix:"* "
      then
        { Content.Span.text = String.make indent ' ' ^ "• "
        ; style = Style.plain
        }
        :: inline (String.drop_prefix stripped 2)
      else inline line))
;;
