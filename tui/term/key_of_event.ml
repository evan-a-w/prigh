open! Core
module Key = Prigh_ui.Key

let code (k : Bonsai_term.Event.Key.t) : Key.Code.t option =
  match k with
  | Escape -> Some Escape
  | Enter -> Some Enter
  | Tab -> Some Tab
  | Backspace -> Some Backspace
  | Insert -> Some Insert
  | Delete -> Some Delete
  | Home -> Some Home
  | End -> Some End
  | Arrow `Up -> Some Up
  | Arrow `Down -> Some Down
  | Arrow `Left -> Some Left
  | Arrow `Right -> Some Right
  | Page `Up -> Some Page_up
  | Page `Down -> Some Page_down
  | Function n -> Some (Function n)
  | ASCII c ->
    (match c with
     | '\n' | '\r' -> Some Enter
     | '\t' -> Some Tab
     | '\127' | '\008' -> Some Backspace
     | '\027' -> Some Escape
     | c when Char.to_int c < 32 ->
       (* Ctrl+letter arrives as a control character on some terminals. *)
       Some (Char (String.of_char (Char.of_int_exn (Char.to_int c + 96))))
     | c -> Some (Char (String.of_char c)))
  | Uchar u ->
    let buf = Buffer.create 4 in
    Uutf_encode.add_utf_8 buf u;
    Some (Char (Buffer.contents buf))
;;

let key (event : Bonsai_term.Event.t) : Key.t option =
  match event with
  | Key_press { key; mods } ->
    Option.map (code key) ~f:(fun code ->
      let has m = List.mem mods m ~equal:Bonsai_term.Event.Modifier.equal in
      let ctrl =
        has Ctrl
        ||
        match key with
        | ASCII c ->
          Char.to_int c < 32
          && not
               (List.mem
                  [ '\n'; '\r'; '\t'; '\027'; '\008' ]
                  c
                  ~equal:Char.equal)
        | _ -> false
      in
      let code : Key.Code.t =
        match code with
        | Char c when ctrl -> Char (String.lowercase c)
        | c -> c
      in
      { Key.code; ctrl; alt = has Meta; shift = has Shift })
  | Mouse _ | Paste _ -> None
;;
