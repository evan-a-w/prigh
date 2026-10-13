open! Core

module Page = struct
  type t =
    { title : string option
    ; text : string
    }
  [@@deriving sexp_of]
end

let named_entities =
  String.Map.of_alist_exn
    [ "amp", "&"
    ; "lt", "<"
    ; "gt", ">"
    ; "quot", "\""
    ; "apos", "'"
    ; "nbsp", "\xc2\xa0"
    ; "ndash", "\xe2\x80\x93"
    ; "mdash", "\xe2\x80\x94"
    ; "hellip", "\xe2\x80\xa6"
    ; "lsquo", "\xe2\x80\x98"
    ; "rsquo", "\xe2\x80\x99"
    ; "ldquo", "\xe2\x80\x9c"
    ; "rdquo", "\xe2\x80\x9d"
    ; "laquo", "\xc2\xab"
    ; "raquo", "\xc2\xbb"
    ; "middot", "\xc2\xb7"
    ; "bull", "\xe2\x80\xa2"
    ; "copy", "\xc2\xa9"
    ; "reg", "\xc2\xae"
    ; "trade", "\xe2\x84\xa2"
    ; "times", "\xc3\x97"
    ; "divide", "\xc3\xb7"
    ; "deg", "\xc2\xb0"
    ; "plusmn", "\xc2\xb1"
    ; "para", "\xc2\xb6"
    ; "sect", "\xc2\xa7"
    ; "euro", "\xe2\x82\xac"
    ; "pound", "\xc2\xa3"
    ; "yen", "\xc2\xa5"
    ; "cent", "\xc2\xa2"
    ; "larr", "\xe2\x86\x90"
    ; "rarr", "\xe2\x86\x92"
    ; "uarr", "\xe2\x86\x91"
    ; "darr", "\xe2\x86\x93"
    ; "harr", "\xe2\x86\x94"
    ; "shy", ""
    ; "zwj", "\xe2\x80\x8d"
    ; "zwnj", "\xe2\x80\x8c"
    ; "thinsp", "\xe2\x80\x89"
    ; "ensp", "\xe2\x80\x82"
    ; "emsp", "\xe2\x80\x83"
    ; "iexcl", "\xc2\xa1"
    ; "iquest", "\xc2\xbf"
    ; "eacute", "\xc3\xa9"
    ; "egrave", "\xc3\xa8"
    ; "aacute", "\xc3\xa1"
    ; "agrave", "\xc3\xa0"
    ; "ouml", "\xc3\xb6"
    ; "uuml", "\xc3\xbc"
    ; "auml", "\xc3\xa4"
    ; "szlig", "\xc3\x9f"
    ; "ccedil", "\xc3\xa7"
    ; "ntilde", "\xc3\xb1"
    ]
;;

let utf8_of_code_point n =
  if n <= 0 || n > 0x10FFFF || (n >= 0xD800 && n <= 0xDFFF)
  then "\xef\xbf\xbd"
  else (
    let b = Buffer.create 4 in
    Stdlib.Buffer.add_utf_8_uchar b (Stdlib.Uchar.of_int n);
    Buffer.contents b)
;;

let decode_entities s =
  if not (String.mem s '&')
  then s
  else (
    let len = String.length s in
    let b = Buffer.create len in
    let rec go i =
      if i >= len
      then ()
      else if Char.equal s.[i] '&'
      then (
        let stop =
          let rec find j =
            if j >= len || j - i > 32
            then None
            else if Char.equal s.[j] ';'
            then Some j
            else if Char.is_alphanum s.[j] || Char.equal s.[j] '#'
            then find (j + 1)
            else None
          in
          find (i + 1)
        in
        match stop with
        | None ->
          Buffer.add_char b '&';
          go (i + 1)
        | Some j ->
          let name = String.sub s ~pos:(i + 1) ~len:(j - i - 1) in
          let decoded =
            match String.chop_prefix name ~prefix:"#" with
            | Some num ->
              let n =
                match String.chop_prefix (String.lowercase num) ~prefix:"x" with
                | Some hex ->
                  Option.try_with (fun () -> Int.of_string ("0x" ^ hex))
                | None -> Int.of_string_opt num
              in
              Option.map n ~f:utf8_of_code_point
            | None -> Map.find named_entities name
          in
          (match decoded with
           | Some text -> Buffer.add_string b text
           | None -> Buffer.add_string b (String.sub s ~pos:i ~len:(j - i + 1)));
          go (j + 1))
      else (
        Buffer.add_char b s.[i];
        go (i + 1))
    in
    go 0;
    Buffer.contents b)
;;

module Token = struct
  type t =
    | Text of string
    | Open of
        { name : string
        ; attrs : (string * string) list
        ; self_closing : bool (** written [<name ... />] *)
        }
    | Close of string
end

let is_name_char c =
  Char.is_alphanum c || Char.equal c '-' || Char.equal c ':' || Char.equal c '_'
;;

(* Elements whose content is not markup. *)
let raw_text = [ "script"; "style"; "title"; "textarea"; "xmp" ]
let mem list name = List.mem list name ~equal:String.equal

let tokenize s =
  let len = String.length s in
  let tokens = Queue.create () in
  let text_start = ref 0 in
  let flush_text upto =
    if upto > !text_start
    then
      Queue.enqueue
        tokens
        (Token.Text (String.sub s ~pos:!text_start ~len:(upto - !text_start)))
  in
  let find_from i pattern =
    match String.substr_index s ~pos:i ~pattern with
    | Some j -> j
    | None -> len
  in
  (* Attributes from [i] to the closing [>]; returns them and the index
     after the tag. *)
  let attributes i =
    let attrs = Queue.create () in
    let self_closing = ref false in
    let rec skip_ws i =
      if i < len && Char.is_whitespace s.[i] then skip_ws (i + 1) else i
    in
    let rec go i =
      let i = skip_ws i in
      if i >= len
      then i
      else if Char.equal s.[i] '>'
      then i + 1
      else if Char.equal s.[i] '/'
      then (
        let next = skip_ws (i + 1) in
        if next < len && Char.equal s.[next] '>' then self_closing := true;
        go (i + 1))
      else (
        let name_end =
          let rec f j =
            if
              j < len
              && (not (Char.is_whitespace s.[j]))
              && not (List.mem [ '='; '>'; '/' ] s.[j] ~equal:Char.equal)
            then f (j + 1)
            else j
          in
          f i
        in
        let name_end = if name_end = i then i + 1 else name_end in
        let name = String.lowercase (String.sub s ~pos:i ~len:(name_end - i)) in
        let j = skip_ws name_end in
        if j < len && Char.equal s.[j] '='
        then (
          let j = skip_ws (j + 1) in
          if j < len && (Char.equal s.[j] '"' || Char.equal s.[j] '\'')
          then (
            let quote = s.[j] in
            let close =
              match String.index_from s (j + 1) quote with
              | Some k -> k
              | None -> len
            in
            Queue.enqueue
              attrs
              ( name
              , decode_entities (String.sub s ~pos:(j + 1) ~len:(close - j - 1))
              );
            go (Int.min len (close + 1)))
          else (
            let rec f k =
              if
                k < len
                && (not (Char.is_whitespace s.[k]))
                && not (Char.equal s.[k] '>')
              then f (k + 1)
              else k
            in
            let stop = f j in
            Queue.enqueue
              attrs
              (name, decode_entities (String.sub s ~pos:j ~len:(stop - j)));
            go stop))
        else (
          Queue.enqueue attrs (name, "");
          go j))
    in
    let next = go i in
    Queue.to_list attrs, !self_closing, next
  in
  (* Lowercased once, for finding raw text elements' close tags: a
     document with many of them must not be lowercased for each. *)
  let lower = lazy (String.lowercase s) in
  let rec go i =
    if i >= len
    then flush_text len
    else if not (Char.equal s.[i] '<')
    then go (i + 1)
    else if String.is_substring_at s ~pos:i ~substring:"<!--"
    then (
      flush_text i;
      let close = find_from (i + 4) "-->" in
      text_start := Int.min len (close + 3);
      go !text_start)
    else if i + 1 < len && (Char.equal s.[i + 1] '!' || Char.equal s.[i + 1] '?')
    then (
      flush_text i;
      let close = find_from i ">" in
      text_start := Int.min len (close + 1);
      go !text_start)
    else if i + 1 < len && Char.equal s.[i + 1] '/'
    then (
      let j = i + 2 in
      let rec f k = if k < len && is_name_char s.[k] then f (k + 1) else k in
      let stop = f j in
      if stop = j
      then go (i + 1)
      else (
        flush_text i;
        let close = find_from stop ">" in
        Queue.enqueue
          tokens
          (Close (String.lowercase (String.sub s ~pos:j ~len:(stop - j))));
        text_start := Int.min len (close + 1);
        go !text_start))
    else if i + 1 < len && Char.is_alpha s.[i + 1]
    then (
      flush_text i;
      let rec f k = if k < len && is_name_char s.[k] then f (k + 1) else k in
      let stop = f (i + 1) in
      let name =
        String.lowercase (String.sub s ~pos:(i + 1) ~len:(stop - i - 1))
      in
      let attrs, self_closing, next = attributes stop in
      Queue.enqueue tokens (Open { name; attrs; self_closing });
      if mem raw_text name && not self_closing
      then (
        (* Up to the matching close tag, case-insensitively. *)
        let close =
          match
            String.substr_index (force lower) ~pos:next ~pattern:("</" ^ name)
          with
          | Some k -> k
          | None -> len
        in
        Queue.enqueue tokens (Text (String.sub s ~pos:next ~len:(close - next)));
        Queue.enqueue tokens (Close name);
        let after = find_from close ">" in
        text_start := Int.min len (after + 1);
        go !text_start)
      else (
        text_start := next;
        go next))
    else go (i + 1)
  in
  go 0;
  Queue.to_list tokens
;;

(* Never rendered, nor anything inside them. *)
let skipped =
  [ "script"
  ; "style"
  ; "noscript"
  ; "template"
  ; "svg"
  ; "math"
  ; "canvas"
  ; "iframe"
  ; "object"
  ; "select"
  ; "button"
  ; "textarea"
  ; "nav"
  ; "head"
  ; "dialog"
  ; "xmp"
  ]
;;

let paragraph_like =
  [ "p"
  ; "ul"
  ; "ol"
  ; "dl"
  ; "table"
  ; "figure"
  ; "pre"
  ; "blockquote"
  ; "section"
  ; "article"
  ; "main"
  ; "header"
  ; "footer"
  ; "aside"
  ; "form"
  ; "fieldset"
  ; "details"
  ; "address"
  ; "h1"
  ; "h2"
  ; "h3"
  ; "h4"
  ; "h5"
  ; "h6"
  ]
;;

let line_like =
  [ "div"
  ; "li"
  ; "tr"
  ; "dt"
  ; "dd"
  ; "figcaption"
  ; "summary"
  ; "caption"
  ; "legend"
  ; "body"
  ; "center"
  ]
;;

let heading_level = function
  | "h1" -> Some 1
  | "h2" -> Some 2
  | "h3" -> Some 3
  | "h4" -> Some 4
  | "h5" -> Some 5
  | "h6" -> Some 6
  | _ -> None
;;

let attr attrs name = List.Assoc.find attrs ~equal:String.equal name

let resolve_url ~base href =
  let href = String.strip href in
  match base with
  | None -> href
  | Some base ->
    (try Uri.to_string (Uri.resolve "" base (Uri.of_string href)) with
     | _ -> href)
;;

module List_state = struct
  type t =
    { ordered : bool
    ; mutable next : int
    }
end

module Renderer = struct
  type t =
    { out : Buffer.t
    ; base : Uri.t option
    ; mutable skip : string list
      (** the skipped elements open, innermost first *)
    ; mutable pre : int
    ; mutable lists : List_state.t list
    ; mutable links : (int * string) list (* start offset, url *)
    ; mutable quotes : int list (* start offsets *)
    ; mutable cell : bool (* a cell was written in this row *)
    ; mutable pending_space : bool
    }

  let create ~base =
    { out = Buffer.create 4096
    ; base
    ; skip = []
    ; pre = 0
    ; lists = []
    ; links = []
    ; quotes = []
    ; cell = false
    ; pending_space = false
    }
  ;;

  let trailing_newlines t =
    let len = Buffer.length t.out in
    let rec count i n =
      if i < 0
      then n, true
      else (
        match Buffer.nth t.out i with
        | '\n' -> count (i - 1) (n + 1)
        | ' ' when n = 0 -> count (i - 1) n
        | _ -> n, false)
    in
    count (len - 1) 0
  ;;

  (* Ends the current line and leaves [n - 1] blank lines, unless the output
     is empty or already does. *)
  let break t n =
    t.pending_space <- false;
    let have, at_start = trailing_newlines t in
    if not at_start
    then
      for _ = have + 1 to n do
        Buffer.add_char t.out '\n'
      done
  ;;

  let at_line_start t =
    let len = Buffer.length t.out in
    len = 0 || Char.equal (Buffer.nth t.out (len - 1)) '\n'
  ;;

  let indent t =
    if at_line_start t
    then (
      match t.lists with
      | [] | [ _ ] -> ()
      | _ :: outer ->
        Buffer.add_string t.out (String.make (2 * List.length outer) ' '))
  ;;

  let add_inline t s =
    if t.pending_space && not (at_line_start t) then Buffer.add_char t.out ' ';
    t.pending_space <- false;
    Buffer.add_string t.out s
  ;;

  let text t s =
    if not (List.is_empty t.skip)
    then ()
    else if t.pre > 0
    then Buffer.add_string t.out (decode_entities s)
    else (
      let s = decode_entities s in
      let words =
        String.split_on_chars s ~on:[ ' '; '\t'; '\n'; '\r'; '\012' ]
      in
      let starts_space = String.length s > 0 && Char.is_whitespace s.[0] in
      let ends_space =
        String.length s > 0 && Char.is_whitespace s.[String.length s - 1]
      in
      if starts_space then t.pending_space <- true;
      let words = List.filter words ~f:(Fn.non String.is_empty) in
      List.iteri words ~f:(fun i word ->
        if i > 0 then t.pending_space <- true;
        add_inline t word);
      if ends_space && not (List.is_empty words) then t.pending_space <- true)
  ;;

  (* What may be in a [head]: anything else ends it, closed or not. *)
  let in_head =
    [ "title"
    ; "meta"
    ; "link"
    ; "style"
    ; "script"
    ; "base"
    ; "noscript"
    ; "template"
    ]
  ;;

  let open_ t name attrs ~self_closing =
    (match List.drop_while t.skip ~f:(Fn.non (String.equal "head")) with
     | "head" :: outside when not (mem in_head name) -> t.skip <- outside
     | _ -> ());
    if mem skipped name
    then (if not self_closing then t.skip <- name :: t.skip)
    else if not (List.is_empty t.skip)
    then ()
    else if t.pre > 0
    then (if String.equal name "br" then Buffer.add_char t.out '\n')
    else (
      (match heading_level name with
       | Some level ->
         break t 2;
         Buffer.add_string t.out (String.make level '#' ^ " ")
       | None -> ());
      match name with
      | "pre" ->
        break t 2;
        Buffer.add_string t.out "```\n";
        t.pre <- t.pre + 1
      | "br" ->
        t.pending_space <- false;
        Buffer.add_char t.out '\n'
      | "hr" ->
        break t 2;
        Buffer.add_string t.out "---";
        break t 2
      | "ul" | "ol" ->
        if List.is_empty t.lists then break t 2 else break t 1;
        t.lists
        <- { List_state.ordered = String.equal name "ol"; next = 1 } :: t.lists
      | "li" ->
        break t 1;
        indent t;
        (match t.lists with
         | ({ ordered = true; next } as l) :: _ ->
           l.next <- next + 1;
           Buffer.add_string t.out (sprintf "%d. " next)
         | _ -> Buffer.add_string t.out "- ")
      | "blockquote" ->
        break t 2;
        t.quotes <- Buffer.length t.out :: t.quotes
      | "a" ->
        (match attr attrs "href" with
         | Some href
           when (not (String.is_empty (String.strip href)))
                && (not (String.is_prefix href ~prefix:"#"))
                && not
                     (String.is_prefix
                        (String.lowercase href)
                        ~prefix:"javascript:") ->
           if t.pending_space && not (at_line_start t)
           then Buffer.add_char t.out ' ';
           t.pending_space <- false;
           t.links
           <- (Buffer.length t.out, resolve_url ~base:t.base href) :: t.links
         | _ -> t.links <- (-1, "") :: t.links)
      | "img" ->
        (match attr attrs "alt" with
         | Some alt when not (String.is_empty (String.strip alt)) ->
           add_inline t (sprintf "[image: %s]" (String.strip alt))
         | _ -> ())
      | "strong" | "b" -> add_inline t "**"
      | "em" | "i" -> add_inline t "_"
      | "code" | "kbd" | "samp" -> add_inline t "`"
      | "td" | "th" ->
        if t.cell then add_inline t "| ";
        t.cell <- true
      | "tr" ->
        break t 1;
        t.cell <- false
      | name when Option.is_some (heading_level name) -> ()
      | name when mem paragraph_like name -> break t 2
      | name when mem line_like name -> break t 1
      | _ -> ())
  ;;

  let close_link t =
    match t.links with
    | [] -> ()
    | (start, url) :: rest ->
      t.links <- rest;
      if start >= 0 && start <= Buffer.length t.out
      then (
        let label =
          String.strip
            (Buffer.To_string.sub
               t.out
               ~pos:start
               ~len:(Buffer.length t.out - start))
        in
        Stdlib.Buffer.truncate t.out start;
        if String.is_empty label
        then ()
        else if String.equal label url
        then Buffer.add_string t.out url
        else Buffer.add_string t.out (sprintf "[%s](%s)" label url))
  ;;

  let close_quote t =
    match t.quotes with
    | [] -> ()
    | start :: rest ->
      t.quotes <- rest;
      if start <= Buffer.length t.out
      then (
        let inner =
          String.strip
            (Buffer.To_string.sub
               t.out
               ~pos:start
               ~len:(Buffer.length t.out - start))
        in
        Stdlib.Buffer.truncate t.out start;
        Buffer.add_string
          t.out
          (String.split_lines inner
           |> List.map ~f:(fun line ->
             if String.is_empty line then ">" else "> " ^ line)
           |> String.concat ~sep:"\n");
        break t 2)
  ;;

  let close t name =
    if mem skipped name
    then (
      (* A close tag without its open one is ignored. *)
      match List.drop_while t.skip ~f:(Fn.non (String.equal name)) with
      | _ :: outside -> t.skip <- outside
      | [] -> ())
    else if not (List.is_empty t.skip)
    then ()
    else if t.pre > 0 && not (String.equal name "pre")
    then ()
    else (
      match name with
      | "pre" ->
        t.pre <- Int.max 0 (t.pre - 1);
        if not (at_line_start t) then Buffer.add_char t.out '\n';
        Buffer.add_string t.out "```";
        break t 2
      | "ul" | "ol" ->
        (match t.lists with
         | [] -> ()
         | _ :: rest -> t.lists <- rest);
        if List.is_empty t.lists then break t 2 else break t 1
      | "a" -> close_link t
      | "blockquote" -> close_quote t
      | "strong" | "b" -> Buffer.add_string t.out "**"
      | "em" | "i" -> Buffer.add_string t.out "_"
      | "code" | "kbd" | "samp" -> Buffer.add_string t.out "`"
      | "td" | "th" -> t.pending_space <- true
      | name when Option.is_some (heading_level name) -> break t 2
      | name when mem paragraph_like name -> break t 2
      | name when mem line_like name -> break t 1
      | _ -> ())
  ;;
end

(* Blank lines at most one at a time, no trailing spaces. *)
let tidy text =
  let lines = String.split_lines text |> List.map ~f:String.rstrip in
  let rec squeeze acc = function
    | [] -> List.rev acc
    | "" :: rest ->
      (match acc with
       | "" :: _ | [] -> squeeze acc rest
       | _ -> squeeze ("" :: acc) rest)
    | line :: rest -> squeeze (line :: acc) rest
  in
  String.concat ~sep:"\n" (squeeze [] lines) |> String.strip
;;

let convert ?base html =
  let tokens = tokenize html in
  let title =
    let rec find = function
      | Token.Open { name = "title"; _ } :: Text t :: _ ->
        Some
          (String.strip
             (String.concat
                ~sep:" "
                (String.split_on_chars
                   (decode_entities t)
                   ~on:[ '\n'; '\r'; '\t' ]
                 |> List.map ~f:String.strip
                 |> List.filter ~f:(Fn.non String.is_empty))))
      | _ :: rest -> find rest
      | [] -> None
    in
    Option.filter (find tokens) ~f:(Fn.non String.is_empty)
  in
  let focus =
    let has name =
      List.exists tokens ~f:(function
        | Token.Open o -> String.equal o.name name
        | Text _ | Close _ -> false)
    in
    if has "main"
    then Some "main"
    else if has "article"
    then Some "article"
    else None
  in
  let r = Renderer.create ~base in
  let depth = ref 0 in
  List.iter tokens ~f:(fun token ->
    let inside = Option.is_none focus || !depth > 0 in
    match token with
    | Open { name; attrs; self_closing } ->
      if Option.equal String.equal focus (Some name) then incr depth;
      if Option.is_none focus || !depth > 0
      then Renderer.open_ r name attrs ~self_closing
    | Close name ->
      if inside then Renderer.close r name;
      if Option.equal String.equal focus (Some name)
      then depth := Int.max 0 (!depth - 1)
    | Text s -> if inside then Renderer.text r s);
  { Page.title; text = tidy (Buffer.contents r.out) }
;;
