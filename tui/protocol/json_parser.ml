open! Core

exception Fail of int * string

let fail pos msg = raise (Fail (pos, msg))

let utf8 buf code =
  let add c = Buffer.add_char buf (Char.of_int_exn c) in
  if code < 0x80
  then add code
  else if code < 0x800
  then (
    add (0xc0 lor (code lsr 6));
    add (0x80 lor (code land 0x3f)))
  else if code < 0x10000
  then (
    add (0xe0 lor (code lsr 12));
    add (0x80 lor ((code lsr 6) land 0x3f));
    add (0x80 lor (code land 0x3f)))
  else (
    add (0xf0 lor (code lsr 18));
    add (0x80 lor ((code lsr 12) land 0x3f));
    add (0x80 lor ((code lsr 6) land 0x3f));
    add (0x80 lor (code land 0x3f)))
;;

let parse_exn s =
  let len = String.length s in
  let pos = ref 0 in
  let peek () = if !pos < len then s.[!pos] else '\000' in
  let rec skip_ws () =
    if !pos < len
    then (
      match s.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
        incr pos;
        skip_ws ()
      | _ -> ())
  in
  let expect c =
    if Char.equal (peek ()) c && !pos < len
    then incr pos
    else fail !pos (sprintf "expected %C" c)
  in
  let literal word value =
    if
      !pos + String.length word <= len
      && String.equal (String.sub s ~pos:!pos ~len:(String.length word)) word
    then (
      pos := !pos + String.length word;
      value)
    else fail !pos "unexpected token"
  in
  let hex4 () =
    if !pos + 4 > len then fail !pos "short \\u escape";
    let v = ref 0 in
    for i = 0 to 3 do
      let d =
        match s.[!pos + i] with
        | '0' .. '9' as c -> Char.to_int c - 48
        | 'a' .. 'f' as c -> Char.to_int c - 87
        | 'A' .. 'F' as c -> Char.to_int c - 55
        | _ -> fail (!pos + i) "bad \\u escape"
      in
      v := (!v * 16) + d
    done;
    pos := !pos + 4;
    !v
  in
  let string () =
    expect '"';
    let buf = Buffer.create 16 in
    let rec go () =
      if !pos >= len then fail !pos "unterminated string";
      match s.[!pos] with
      | '"' -> incr pos
      | '\\' ->
        if !pos + 1 >= len then fail !pos "unterminated string";
        let c = s.[!pos + 1] in
        pos := !pos + 2;
        (match c with
         | '"' -> Buffer.add_char buf '"'
         | '\\' -> Buffer.add_char buf '\\'
         | '/' -> Buffer.add_char buf '/'
         | 'b' -> Buffer.add_char buf '\b'
         | 'f' -> Buffer.add_char buf '\012'
         | 'n' -> Buffer.add_char buf '\n'
         | 'r' -> Buffer.add_char buf '\r'
         | 't' -> Buffer.add_char buf '\t'
         | 'u' ->
           let code = hex4 () in
           let code =
             if
               code >= 0xd800
               && code < 0xdc00
               && !pos + 6 <= len
               && Char.equal s.[!pos] '\\'
               && Char.equal s.[!pos + 1] 'u'
             then (
               let save = !pos in
               pos := !pos + 2;
               let low = hex4 () in
               if low >= 0xdc00 && low < 0xe000
               then 0x10000 + ((code - 0xd800) lsl 10) + (low - 0xdc00)
               else (
                 pos := save;
                 0xfffd))
             else if code >= 0xd800 && code < 0xe000
             then 0xfffd
             else code
           in
           utf8 buf code
         | _ -> fail (!pos - 1) "bad escape");
        go ()
      | c ->
        Buffer.add_char buf c;
        incr pos;
        go ()
    in
    go ();
    Buffer.contents buf
  in
  let number () =
    let start = !pos in
    let digits () =
      let d0 = !pos in
      while !pos < len && Char.is_digit s.[!pos] do
        incr pos
      done;
      if !pos = d0 then fail !pos "expected a digit"
    in
    if Char.equal (peek ()) '-' then incr pos;
    if Char.equal (peek ()) '0' && !pos + 1 < len && Char.is_digit s.[!pos + 1]
    then fail !pos "leading zero";
    digits ();
    if Char.equal (peek ()) '.'
    then (
      incr pos;
      digits ());
    (match peek () with
     | 'e' | 'E' ->
       incr pos;
       (match peek () with
        | '+' | '-' -> incr pos
        | _ -> ());
       digits ()
     | _ -> ());
    `Number (String.sub s ~pos:start ~len:(!pos - start))
  in
  let rec value () : Jsonaf.t =
    skip_ws ();
    match peek () with
    | '{' ->
      incr pos;
      skip_ws ();
      if Char.equal (peek ()) '}'
      then (
        incr pos;
        `Object [])
      else (
        let fields = ref [] in
        let continue = ref true in
        while !continue do
          skip_ws ();
          let key = string () in
          skip_ws ();
          expect ':';
          let v = value () in
          fields := (key, v) :: !fields;
          skip_ws ();
          match peek () with
          | ',' -> incr pos
          | '}' ->
            incr pos;
            continue := false
          | _ -> fail !pos "expected ',' or '}'"
        done;
        `Object (List.rev !fields))
    | '[' ->
      incr pos;
      skip_ws ();
      if Char.equal (peek ()) ']'
      then (
        incr pos;
        `Array [])
      else (
        let items = ref [] in
        let continue = ref true in
        while !continue do
          items := value () :: !items;
          skip_ws ();
          match peek () with
          | ',' -> incr pos
          | ']' ->
            incr pos;
            continue := false
          | _ -> fail !pos "expected ',' or ']'"
        done;
        `Array (List.rev !items))
    | '"' -> `String (string ())
    | 't' -> literal "true" `True
    | 'f' -> literal "false" `False
    | 'n' -> literal "null" `Null
    | '-' | '0' .. '9' -> number ()
    | _ -> fail !pos "unexpected character"
  in
  let v = value () in
  skip_ws ();
  if !pos < len then fail !pos "trailing characters";
  v
;;

let parse s =
  match parse_exn s with
  | v -> Ok v
  | exception Fail (pos, msg) ->
    Or_error.errorf "invalid JSON at byte %d: %s" pos msg
;;
