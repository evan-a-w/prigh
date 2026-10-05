open! Core

let safe_href href =
  let lower = String.lowercase (String.strip href) in
  List.exists [ "http://"; "https://"; "mailto:" ] ~f:(fun prefix ->
    String.is_prefix lower ~prefix && String.length lower > String.length prefix)
;;

let is_punct c =
  Char.is_print c && (not (Char.is_alphanum c)) && not (Char.is_whitespace c)
;;

module Inline = struct
  type t =
    | Text of string
    | Code of string
    | Strong of t list
    | Emph of t list
    | Strike of t list
    | Link of
        { href : string
        ; children : t list
        }
    | Break
  [@@deriving sexp_of, equal]

  let run s i ~hi c =
    let rec go j = if j < hi && Char.equal s.[j] c then go (j + 1) else j in
    go i - i
  ;;

  (* A code span opening at [i]: its text and the index after it. *)
  let code_span s i ~hi =
    let n = run s i ~hi '`' in
    let rec find j =
      match String.index_from s j '`' with
      | None -> None
      | Some k when k >= hi -> None
      | Some k ->
        let m = run s k ~hi '`' in
        if m = n then Some (k, k + m) else find (k + m)
    in
    match find (i + n) with
    | None -> None
    | Some (k, after) ->
      let text = String.sub s ~pos:(i + n) ~len:(k - i - n) in
      let text = String.tr text ~target:'\n' ~replacement:' ' in
      let text =
        if
          String.length text >= 2
          && Char.equal text.[0] ' '
          && Char.equal text.[String.length text - 1] ' '
          && not (String.for_all text ~f:(Char.equal ' '))
        then String.sub text ~pos:1 ~len:(String.length text - 2)
        else text
      in
      Some (text, after)
  ;;

  (* The next unescaped [c] at depth zero outside code spans, from [i]. *)
  let find_matching s i ~hi ~open_ ~close =
    let rec go j depth =
      if j >= hi
      then None
      else (
        let c = s.[j] in
        if Char.equal c '\\'
        then go (j + 2) depth
        else if Char.equal c '`'
        then (
          match code_span s j ~hi with
          | Some (_, after) -> go after depth
          | None -> go (j + run s j ~hi '`') depth)
        else if Char.equal c close
        then if depth = 0 then Some j else go (j + 1) (depth - 1)
        else if Char.equal c open_
        then go (j + 1) (depth + 1)
        else go (j + 1) depth)
    in
    go i 0
  ;;

  (* The closing run of exactly [len] [c]s for an emphasis opened before [i]. *)
  let find_closer s i ~hi c ~len =
    let rec go j =
      if j >= hi
      then None
      else if Char.equal s.[j] '\\'
      then go (j + 2)
      else if Char.equal s.[j] '`'
      then (
        match code_span s j ~hi with
        | Some (_, after) -> go after
        | None -> go (j + run s j ~hi '`'))
      else if Char.equal s.[j] c
      then (
        let r = run s j ~hi c in
        let after = j + r in
        if
          r = len
          && j > i
          && (not (Char.is_whitespace s.[j - 1]))
          && not (Char.equal c '_' && after < hi && Char.is_alphanum s.[after])
        then Some j
        else go after)
      else go (j + 1)
    in
    go i
  ;;

  let url_end s i ~hi =
    let rec go j =
      if
        j < hi && (not (Char.is_whitespace s.[j])) && not (Char.equal s.[j] '<')
      then go (j + 1)
      else j
    in
    let stop = go i in
    let rec trim j =
      if j <= i
      then j
      else (
        match s.[j - 1] with
        | '.' | ',' | ';' | ':' | '!' | '?' | '*' | '_' | '~' | '\'' | '"' ->
          trim (j - 1)
        | ')' ->
          let url = String.sub s ~pos:i ~len:(j - i) in
          if
            String.count url ~f:(Char.equal ')')
            > String.count url ~f:(Char.equal '(')
          then trim (j - 1)
          else j
        | _ -> j)
    in
    trim stop
  ;;

  let starts_url s i ~hi =
    let at prefix =
      i + String.length prefix <= hi
      && String.is_substring_at s ~pos:i ~substring:prefix
    in
    (at "https://" || at "http://")
    && (i = 0 || not (Char.is_alphanum s.[i - 1]))
  ;;

  let link_destination raw =
    let raw = String.strip raw in
    let raw =
      match String.chop_prefix raw ~prefix:"<" with
      | Some rest ->
        (match String.lsplit2 rest ~on:'>' with
         | Some (dest, _) -> dest
         | None -> rest)
      | None ->
        (match String.lsplit2 raw ~on:' ' with
         | Some (dest, _) -> dest
         | None -> raw)
    in
    raw
  ;;

  let rec parse_range ?(partial = false) s lo hi =
    let out = ref [] in
    let buf = Buffer.create 16 in
    let flush () =
      if Buffer.length buf > 0
      then (
        out := Text (Buffer.contents buf) :: !out;
        Buffer.clear buf)
    in
    let emit node =
      flush ();
      out := node :: !out
    in
    let emit_all nodes =
      flush ();
      out := List.rev_append nodes !out
    in
    let text i j = Buffer.add_substring buf s ~pos:i ~len:(j - i) in
    let rec go i =
      if i >= hi
      then ()
      else (
        let c = s.[i] in
        match c with
        | '\\' when i + 1 < hi && Char.equal s.[i + 1] '\n' ->
          emit Break;
          go (i + 2)
        | '\\' when i + 1 < hi && is_punct s.[i + 1] ->
          Buffer.add_char buf s.[i + 1];
          go (i + 2)
        | '\n' ->
          let contents = Buffer.contents buf in
          Buffer.clear buf;
          Buffer.add_string buf (String.rstrip contents);
          emit Break;
          let rec skip j =
            if j < hi && Char.equal s.[j] ' ' then skip (j + 1) else j
          in
          go (skip (i + 1))
        | '`' ->
          (match code_span s i ~hi with
           | Some (code, after) ->
             emit (Code code);
             go after
           | None when partial ->
             let n = run s i ~hi '`' in
             if i + n < hi
             then emit (Code (String.sub s ~pos:(i + n) ~len:(hi - i - n)))
           | None ->
             let n = run s i ~hi '`' in
             text i (i + n);
             go (i + n))
        | '*' | '_' | '~' -> emphasis i c
        | '!' when i + 1 < hi && Char.equal s.[i + 1] '[' -> link i ~image:true
        | '[' -> link i ~image:false
        | '<' -> autolink i
        | 'h' when starts_url s i ~hi ->
          let stop = url_end s i ~hi in
          let url = String.sub s ~pos:i ~len:(stop - i) in
          emit (Link { href = url; children = [ Text url ] });
          go stop
        | _ ->
          Buffer.add_char buf c;
          go (i + 1))
    and emphasis i c =
      let r = run s i ~hi c in
      let after = i + r in
      let can_open =
        after < hi
        && (not (Char.is_whitespace s.[after]))
        && (match c with
            | '~' -> r = 2
            | _ -> r <= 3)
        && not (Char.equal c '_' && i > lo && Char.is_alphanum s.[i - 1])
      in
      let span inner =
        match c, r with
        | '~', _ -> Strike inner
        | _, 1 -> Emph inner
        | _, 2 -> Strong inner
        | _ -> Strong [ Emph inner ]
      in
      match if can_open then find_closer s after ~hi c ~len:r else None with
      | Some close ->
        emit (span (parse_range s after close));
        go (close + r)
      | None when partial && can_open ->
        emit (span (parse_range ~partial s after hi))
      | None when partial && after >= hi -> ()
      | None ->
        text i after;
        go after
    and link i ~image =
      let open_ = if image then i + 1 else i in
      let label_end = find_matching s (open_ + 1) ~hi ~open_:'[' ~close:']' in
      match label_end with
      | Some close when close + 1 < hi && Char.equal s.[close + 1] '(' ->
        (match find_matching s (close + 2) ~hi ~open_:'(' ~close:')' with
         | Some paren ->
           let href =
             link_destination
               (String.sub s ~pos:(close + 2) ~len:(paren - close - 2))
           in
           let children =
             if image
             then (
               let alt =
                 String.sub s ~pos:(open_ + 1) ~len:(close - open_ - 1)
               in
               [ Text (if String.is_empty alt then "image" else alt) ])
             else parse_range s (open_ + 1) close
           in
           if safe_href href
           then emit (Link { href; children })
           else emit_all children;
           go (paren + 1)
         | None when partial ->
           emit_all (parse_range s (open_ + 1) close)
         | None ->
           text i (open_ + 1);
           go (open_ + 1))
      | None when partial -> emit_all (parse_range ~partial s (open_ + 1) hi)
      | _ ->
        text i (open_ + 1);
        go (open_ + 1)
    and autolink i =
      match String.index_from s (i + 1) '>' with
      | Some close when close < hi ->
        let url = String.sub s ~pos:(i + 1) ~len:(close - i - 1) in
        if safe_href url && not (String.exists url ~f:Char.is_whitespace)
        then (
          let shown =
            Option.value (String.chop_prefix url ~prefix:"mailto:") ~default:url
          in
          emit (Link { href = url; children = [ Text shown ] });
          go (close + 1))
        else (
          Buffer.add_char buf '<';
          go (i + 1))
      | _ ->
        Buffer.add_char buf '<';
        go (i + 1)
    in
    go lo;
    flush ();
    List.rev !out
  ;;

  let parse ?partial s = parse_range ?partial s 0 (String.length s)

  let rec to_plain l =
    List.map l ~f:(function
      | Text s | Code s -> s
      | Strong l | Emph l | Strike l | Link { children = l; _ } -> to_plain l
      | Break -> " ")
    |> String.concat
  ;;
end

module Align = struct
  type t =
    | Default
    | Left
    | Center
    | Right
  [@@deriving sexp_of, equal]
end

module Block = struct
  type t =
    | Paragraph of Inline.t list
    | Heading of int * Inline.t list
    | Code of
        { lang : string
        ; text : string
        ; closed : bool
        }
    | Quote of t list
    | List of
        { start : int option
        ; tight : bool
        ; items : item list
        }
    | Rule
    | Table of
        { aligns : Align.t list
        ; header : Inline.t list list
        ; rows : Inline.t list list list
        }

  and item =
    { checked : bool option
    ; blocks : t list
    }
  [@@deriving sexp_of, equal]
end

let is_blank line = String.for_all line ~f:Char.is_whitespace

let indent line =
  match String.lfindi line ~f:(fun _ c -> not (Char.equal c ' ')) with
  | Some i -> i
  | None -> String.length line
;;

let drop_indent line n = String.drop_prefix line (Int.min n (indent line))

module Fence = struct
  type t =
    { char : char
    ; length : int
    ; indent : int
    ; info : string
    }

  let opening line =
    let ind = indent line in
    if ind > 3 || ind >= String.length line
    then None
    else (
      let c = line.[ind] in
      if not (Char.equal c '`' || Char.equal c '~')
      then None
      else (
        let n = Inline.run line ind ~hi:(String.length line) c in
        let info = String.strip (String.drop_prefix line (ind + n)) in
        if n < 3 || (Char.equal c '`' && String.mem info '`')
        then None
        else Some { char = c; length = n; indent = ind; info }))
  ;;

  let closes t line =
    let ind = indent line in
    ind <= 3
    && ind < String.length line
    && Char.equal line.[ind] t.char
    && Inline.run line ind ~hi:(String.length line) t.char >= t.length
    && is_blank
         (String.drop_prefix
            line
            (ind + Inline.run line ind ~hi:(String.length line) t.char))
  ;;
end

let heading line =
  let ind = indent line in
  if ind > 3
  then None
  else (
    let rest = String.drop_prefix line ind in
    let level = Inline.run rest 0 ~hi:(String.length rest) '#' in
    if
      level < 1
      || level > 6
      || (level < String.length rest && not (Char.equal rest.[level] ' '))
    then None
    else (
      let text = String.strip (String.drop_prefix rest level) in
      let text =
        let without = String.rstrip text ~drop:(Char.equal '#') in
        if String.is_empty without
        then without
        else if Char.equal without.[String.length without - 1] ' '
        then String.rstrip without
        else text
      in
      Some (level, text)))
;;

let is_rule line =
  indent line <= 3
  &&
  let chars = String.filter line ~f:(Fn.non Char.is_whitespace) in
  String.length chars >= 3
  && (match chars.[0] with
      | '-' | '*' | '_' -> true
      | _ -> false)
  && String.for_all chars ~f:(Char.equal chars.[0])
;;

let quote_content line =
  let ind = indent line in
  if ind <= 3 && ind < String.length line && Char.equal line.[ind] '>'
  then (
    let rest = String.drop_prefix line (ind + 1) in
    Some (Option.value (String.chop_prefix rest ~prefix:" ") ~default:rest))
  else None
;;

module Marker = struct
  type t =
    { start : int option
    ; indent : int
    ; offset : int (** where the item's content starts *)
    }

  let of_line line =
    let ind = indent line in
    let len = String.length line in
    if ind > 3 || ind >= len
    then None
    else (
      let marker_end =
        match line.[ind] with
        | '-' | '*' | '+' -> Some (None, ind + 1)
        | c when Char.is_digit c ->
          let digits =
            String.lfindi ~pos:ind line ~f:(fun _ c -> not (Char.is_digit c))
            |> Option.value ~default:len
          in
          if
            digits - ind <= 9
            && digits < len
            && (Char.equal line.[digits] '.' || Char.equal line.[digits] ')')
          then
            Some
              ( Some
                  (Int.of_string (String.sub line ~pos:ind ~len:(digits - ind)))
              , digits + 1 )
          else None
        | _ -> None
      in
      match marker_end with
      | None -> None
      | Some (start, e) when e = len ->
        Some { start; indent = ind; offset = e + 1 }
      | Some (start, e) when Char.equal line.[e] ' ' ->
        let spaces = indent (String.drop_prefix line e) in
        let offset = if spaces > 4 then e + 1 else e + spaces in
        Some { start; indent = ind; offset }
      | Some _ -> None)
  ;;

  let same_kind a b =
    Bool.equal (Option.is_some a.start) (Option.is_some b.start)
  ;;
end

let split_row line =
  let line = String.strip line in
  let line = Option.value (String.chop_prefix line ~prefix:"|") ~default:line in
  let line =
    if
      String.is_suffix line ~suffix:"|"
      && not (String.is_suffix line ~suffix:"\\|")
    then String.drop_suffix line 1
    else line
  in
  let cells = ref [] in
  let start = ref 0 in
  let hi = String.length line in
  let rec go i =
    if i >= hi
    then ()
    else (
      match line.[i] with
      | '\\' -> go (i + 2)
      | '`' ->
        (match Inline.code_span line i ~hi with
         | Some (_, after) -> go after
         | None -> go (i + Inline.run line i ~hi '`'))
      | '|' ->
        cells := String.sub line ~pos:!start ~len:(i - !start) :: !cells;
        start := i + 1;
        go (i + 1)
      | _ -> go (i + 1))
  in
  go 0;
  cells := String.sub line ~pos:!start ~len:(hi - !start) :: !cells;
  List.rev_map !cells ~f:String.strip
;;

let delimiter_row line =
  if not (String.mem line '-')
  then None
  else (
    let cells = split_row line in
    let align cell =
      let left = String.is_prefix cell ~prefix:":" in
      let right = String.is_suffix cell ~suffix:":" in
      let dashes = String.strip cell ~drop:(Char.equal ':') in
      if
        String.is_empty dashes
        || not (String.for_all dashes ~f:(Char.equal '-'))
      then None
      else
        Some
          (match left, right with
           | true, true -> Align.Center
           | true, false -> Left
           | false, true -> Right
           | false, false -> Default)
    in
    Option.all (List.map cells ~f:align))
;;

let table_start lines i =
  if i + 1 >= Array.length lines || not (String.mem lines.(i) '|')
  then None
  else (
    match delimiter_row lines.(i + 1) with
    | Some aligns
      when List.length aligns = List.length (split_row lines.(i))
           && (String.mem lines.(i + 1) '|' || List.length aligns > 1) ->
      Some aligns
    | _ -> None)
;;

let starts_block lines i =
  let line = lines.(i) in
  Option.is_some (Fence.opening line)
  || Option.is_some (heading line)
  || is_rule line
  || Option.is_some (quote_content line)
  || Option.is_some (Marker.of_line line)
  || Option.is_some (table_start lines i)
;;

let task_prefix text =
  List.find_map
    [ "[ ] ", false; "[x] ", true; "[X] ", true ]
    ~f:(fun (prefix, checked) ->
      Option.map (String.chop_prefix text ~prefix) ~f:(fun rest ->
        checked, rest))
;;

let rec parse_lines ?(partial = false) lines =
  let n = Array.length lines in
  let blocks = ref [] in
  let add b = blocks := b :: !blocks in
  let rec go i =
    if i >= n
    then ()
    else (
      let line = lines.(i) in
      if is_blank line
      then go (i + 1)
      else (
        match Fence.opening line with
        | Some fence -> go (code fence (i + 1))
        | None ->
          (match heading line with
           | Some (level, text) ->
             add
               (Block.Heading (level, Inline.parse ~partial:(partial && i + 1 = n) text));
             go (i + 1)
           | None ->
             if is_rule line
             then (
               add Rule;
               go (i + 1))
             else if Option.is_some (quote_content line)
             then go (quote i)
             else (
               match Marker.of_line line with
               | Some marker -> go (list i marker)
               | None ->
                 (match table_start lines i with
                  | Some aligns -> go (table i aligns)
                  | None -> go (paragraph i))))))
  and code (fence : Fence.t) i =
    let rec find j =
      if j >= n || Fence.closes fence lines.(j) then j else find (j + 1)
    in
    let stop = find i in
    let text =
      Array.sub lines ~pos:i ~len:(stop - i)
      |> Array.to_list
      |> List.map ~f:(fun l -> drop_indent l fence.indent)
      |> String.concat ~sep:"\n"
    in
    let lang =
      match String.lsplit2 fence.info ~on:' ' with
      | Some (lang, _) -> lang
      | None -> fence.info
    in
    add (Code { lang; text; closed = stop < n });
    stop + 1
  and quote i =
    let rec collect j acc ~lazy_ok =
      if j >= n
      then j, acc
      else (
        match quote_content lines.(j) with
        | Some content ->
          collect (j + 1) (content :: acc) ~lazy_ok:(not (is_blank content))
        | None ->
          if lazy_ok && (not (is_blank lines.(j))) && not (starts_block lines j)
          then collect (j + 1) (lines.(j) :: acc) ~lazy_ok
          else j, acc)
    in
    let stop, rev = collect i [] ~lazy_ok:false in
    add
      (Quote
         (parse_lines ~partial:(partial && stop = n) (Array.of_list (List.rev rev))));
    stop
  and table i aligns =
    let width = List.length aligns in
    let cells line =
      let cells = split_row line in
      let cells = List.take cells width in
      let cells =
        cells @ List.init (width - List.length cells) ~f:(fun _ -> "")
      in
      List.map cells ~f:Inline.parse
    in
    let rec rows j acc =
      if
        j < n
        && (not (is_blank lines.(j)))
        && String.mem lines.(j) '|'
        && not (Option.is_some (Fence.opening lines.(j)))
      then rows (j + 1) (cells lines.(j) :: acc)
      else j, List.rev acc
    in
    let stop, rows = rows (i + 2) [] in
    add (Table { aligns; header = cells lines.(i); rows });
    stop
  and paragraph i =
    let rec collect j =
      if j < n && (not (is_blank lines.(j))) && not (starts_block lines j)
      then collect (j + 1)
      else j
    in
    let stop = collect (i + 1) in
    let text =
      Array.sub lines ~pos:i ~len:(stop - i)
      |> Array.to_list
      |> List.map ~f:String.lstrip
      |> String.concat ~sep:"\n"
    in
    add
      (Paragraph (Inline.parse ~partial:(partial && stop = n) (String.rstrip text)));
    stop
  and list i (first : Marker.t) =
    let item i (marker : Marker.t) =
      let threshold = Int.min marker.offset (marker.indent + 2) in
      let first =
        String.drop_prefix
          lines.(i)
          (Int.min marker.offset (String.length lines.(i)))
      in
      let rec collect j acc ~gap ~lazy_ok =
        if j >= n
        then j, acc, gap
        else (
          let line = lines.(j) in
          if is_blank line
          then (
            let rec next k =
              if k < n && is_blank lines.(k) then next (k + 1) else k
            in
            let k = next j in
            if k < n && indent lines.(k) >= threshold
            then
              collect
                k
                (List.init (k - j) ~f:(fun _ -> "") @ acc)
                ~gap:true
                ~lazy_ok:false
            else j, acc, gap)
          else if indent line >= threshold
          then
            collect
              (j + 1)
              (drop_indent line marker.offset :: acc)
              ~gap
              ~lazy_ok:true
          else if lazy_ok && not (starts_block lines j)
          then collect (j + 1) (String.lstrip line :: acc) ~gap ~lazy_ok
          else j, acc, gap)
      in
      let stop, rev, gap =
        collect (i + 1) [ first ] ~gap:false ~lazy_ok:(not (is_blank first))
      in
      let content = List.rev rev in
      let checked, content =
        match content with
        | first :: rest ->
          (match task_prefix (first ^ " ") with
           | Some (checked, text) -> Some checked, String.rstrip text :: rest
           | None -> None, content)
        | [] -> None, content
      in
      ( { Block.checked
        ; blocks =
            parse_lines ~partial:(partial && stop = n) (Array.of_list content)
        }
      , stop
      , gap )
    in
    let rec items i acc ~tight =
      let it, stop, gap =
        item i (Marker.of_line lines.(i) |> Option.value_exn)
      in
      let tight = tight && not gap in
      let rec next k =
        if k < n && is_blank lines.(k) then next (k + 1) else k
      in
      let k = next stop in
      match if k < n then Marker.of_line lines.(k) else None with
      | Some m when Marker.same_kind m first && m.indent <= first.indent + 1 ->
        items k (it :: acc) ~tight:(tight && k = stop)
      | _ -> stop, List.rev (it :: acc), tight
    in
    let stop, items, tight = items i [] ~tight:true in
    add (List { start = first.start; tight; items });
    stop
  in
  go 0;
  List.rev !blocks
;;

let parse ?partial text =
  String.split_lines text
  |> List.map ~f:(String.substr_replace_all ~pattern:"\t" ~with_:"    ")
  |> Array.of_list
  |> parse_lines ?partial
;;

let rec first_inlines (blocks : Block.t list) =
  match blocks with
  | [] -> None
  | (Paragraph l | Heading (_, l)) :: _ -> Some l
  | Quote blocks :: rest -> Option.first_some (first_inlines blocks) (first_inlines rest)
  | List { items; _ } :: rest ->
    Option.first_some
      (List.find_map items ~f:(fun item -> first_inlines item.blocks))
      (first_inlines rest)
  | Table { header; _ } :: _ -> Some (List.concat header)
  | Code { text; _ } :: _ -> Some [ Text text ]
  | Rule :: rest -> first_inlines rest
;;

let preview text =
  String.split_lines text
  |> List.find ~f:(Fn.non is_blank)
  |> Option.bind ~f:(fun line -> first_inlines (parse line))
  |> Option.value_map ~default:"" ~f:(fun l -> String.strip (Inline.to_plain l))
;;
