open! Core
open! Import

module Message = struct
  type t =
    | Text of string
    | Generate
  [@@deriving sexp_of, equal]
end

type t =
  { stage_tracked : bool
  ; amend : bool
  ; message : Message.t
  }
[@@deriving sexp_of, equal]

let usage =
  "usage: /commit [-a] [--amend] -m MESSAGE, or /commit [-a] --generate to \
   have the model draft the message"
;;

let flags =
  [ "-a", "stage every change to tracked files first"
  ; "-m", "MESSAGE: the commit message (the rest of the line, or quoted)"
  ; "--generate", "have the model draft the message from the staged changes"
  ; "--amend", "replace the last commit (no message: keep its own)"
  ]
;;

let message_flags = [ "-m"; "-am"; "-ma" ]
let is_message_flag word = List.mem message_flags word ~equal:String.equal

let quote message =
  let buf = Buffer.create (String.length message + 2) in
  Buffer.add_char buf '"';
  String.iter message ~f:(fun c ->
    if Char.equal c '"' || Char.equal c '\\' then Buffer.add_char buf '\\';
    Buffer.add_char buf c);
  Buffer.add_char buf '"';
  Buffer.contents buf
;;

let command ~amend message =
  sprintf "/commit %s-m %s" (if amend then "--amend " else "") (quote message)
;;

let unterminated q =
  sprintf
    "the message's closing %c is missing: /commit -m %cFix the parser%c"
    q
    q
    q
;;

let trailing =
  "there is text after the message's closing quote: quote all of the message \
   or none of it"
;;

let unquote text =
  let n = String.length text in
  if n = 0
  then Ok text
  else (
    match text.[0] with
    | '\'' ->
      (match String.index_from text 1 '\'' with
       | Some i when i = n - 1 -> Ok (String.sub text ~pos:1 ~len:(n - 2))
       | Some _ -> Error trailing
       | None -> Error (unterminated '\''))
    | '"' ->
      let buf = Buffer.create n in
      let rec go i =
        if i >= n
        then Error (unterminated '"')
        else (
          match text.[i] with
          | '\\'
            when i + 1 < n
                 && (Char.equal text.[i + 1] '"' || Char.equal text.[i + 1] '\\')
            ->
            Buffer.add_char buf text.[i + 1];
            go (i + 2)
          | '"' ->
            if i = n - 1 then Ok (Buffer.contents buf) else Error trailing
          | c ->
            Buffer.add_char buf c;
            go (i + 1))
      in
      go 1
    | _ -> Ok text)
;;

let first_word s =
  match String.lfindi s ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> s, ""
  | Some i -> String.prefix s i, String.drop_prefix s i
;;

let parse rest =
  let finish ~stage_tracked ~amend ~generate ~message =
    match generate, message with
    | true, Some _ -> Error "/commit takes -m MESSAGE or --generate, not both"
    | true, None -> Ok { stage_tracked; amend; message = Generate }
    | false, Some "" when not amend ->
      Error "/commit -m needs a message: /commit -m \"Fix the parser\""
    | false, Some text -> Ok { stage_tracked; amend; message = Text text }
    | false, None when amend -> Ok { stage_tracked; amend; message = Text "" }
    | false, None -> Error usage
  in
  let rec go s ~stage_tracked ~amend ~generate =
    let s = String.lstrip s in
    if String.is_empty s
    then finish ~stage_tracked ~amend ~generate ~message:None
    else (
      let word, after = first_word s in
      match word with
      | "-a" | "--all" -> go after ~stage_tracked:true ~amend ~generate
      | "--amend" -> go after ~stage_tracked ~amend:true ~generate
      | "--generate" -> go after ~stage_tracked ~amend ~generate:true
      | word when is_message_flag word ->
        Result.bind
          (unquote (String.strip after))
          ~f:(fun message ->
            finish
              ~stage_tracked:(stage_tracked || not (String.equal word "-m"))
              ~amend
              ~generate
              ~message:(Some message))
      | word when String.is_prefix word ~prefix:"-" ->
        Error (sprintf "unknown /commit flag %s; %s" word usage)
      | _ ->
        Error
          (sprintf
             "put the message after -m: /commit -m %s"
             (quote (String.strip s))))
  in
  go rest ~stage_tracked:false ~amend:false ~generate:false
;;

let complete rest =
  let start =
    match String.rfindi rest ~f:(fun _ c -> Char.is_whitespace c) with
    | None -> 0
    | Some i -> i + 1
  in
  let words =
    String.split_on_chars (String.prefix rest start) ~on:[ ' '; '\t'; '\n' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  let current = String.drop_prefix rest start in
  if
    List.exists words ~f:(fun w ->
      is_message_flag w || not (String.is_prefix w ~prefix:"-"))
    || not (String.is_empty current || String.is_prefix current ~prefix:"-")
  then None
  else (
    let used flag =
      List.mem words flag ~equal:String.equal
      || (String.equal flag "-m"
          && List.mem words "--generate" ~equal:String.equal)
    in
    match List.filter flags ~f:(fun (flag, _) -> not (used flag)) with
    | [] -> None
    | candidates -> Some (start, candidates))
;;

let committed ~amend (c : P.Git_status.Commit.t) =
  sprintf
    "%s %s %s on %s"
    (if amend then "amended" else "committed")
    (String.prefix c.sha 7)
    c.subject
    (Option.value c.status.branch ~default:"a detached HEAD")
;;

let pushed (p : P.Git_status.Push.t) =
  sprintf "pushed %s to %s" p.branch p.remote
;;
