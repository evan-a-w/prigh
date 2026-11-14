open! Core
open! Import

let long_chars = 2_000
let long_lines = 40
let max_bytes = 2 * 1024 * 1024

let count_lines text =
  String.count text ~f:(Char.equal '\n')
  + if String.is_empty text || String.is_suffix text ~suffix:"\n" then 0 else 1
;;

(* Each scalar value has one byte that is not a continuation byte. *)
let chars text = String.count text ~f:(fun c -> Char.to_int c land 0xC0 <> 0x80)

let is_long text =
  String.length text > long_chars
  && chars text > long_chars
  || count_lines text > long_lines
;;

let size bytes =
  if bytes < 1024
  then sprintf "%d B" bytes
  else if bytes < 1024 * 1024
  then sprintf "%.1f KB" (Float.of_int bytes /. 1024.)
  else sprintf "%.1f MB" (Float.of_int bytes /. 1_048_576.)
;;

let too_large text =
  let bytes = String.length text in
  Option.some_if
    (bytes > max_bytes)
    (sprintf
       "That paste is %s, over the %s a pasted text may be: save it to a file \
        and attach that instead."
       (size bytes)
       (size max_bytes))
;;

let name n = sprintf "Pasted text #%d" n

let summary ~lines ~bytes =
  sprintf "%d line%s · %s" lines (if lines = 1 then "" else "s") (size bytes)
;;

let preview ?(max = 60) text =
  let line =
    List.find (String.split_lines text) ~f:(fun l ->
      not (String.is_empty (String.strip l)))
    |> Option.value_map ~default:"" ~f:String.strip
  in
  let pieces = List.map (Text_width.uchars line) ~f:fst in
  if List.length pieces <= max
  then line
  else String.concat (List.take pieces (max - 1)) ^ "…"
;;

let token ~n ~lines =
  sprintf "[%s +%d line%s]" (name n) lines (if lines = 1 then "" else "s")
;;

let replace_tokens text pairs =
  let found =
    List.filter_map pairs ~f:(fun (old, new_) ->
      Option.map (String.substr_index text ~pattern:old) ~f:(fun at ->
        at, String.length old, new_))
    |> List.sort ~compare:(fun (a, _, _) (b, _, _) -> Int.compare a b)
  in
  let buf = Buffer.create (String.length text) in
  let at =
    List.fold found ~init:0 ~f:(fun at (pos, len, new_) ->
      if pos < at
      then at
      else (
        Buffer.add_string buf (String.sub text ~pos:at ~len:(pos - at));
        Buffer.add_string buf new_;
        pos + len))
  in
  Buffer.add_string buf (String.drop_prefix text at);
  Buffer.contents buf
;;

(* As the backend's [Injected.Pasted.make]. *)
let block ~n text =
  sprintf
    "<pasted_text name=\"%s\" lines=\"%d\">\n%s\n</pasted_text>"
    (name n)
    (count_lines text)
    text
;;

let inline ~text pasted =
  String.concat
    (text :: List.mapi pasted ~f:(fun i p -> "\n\n" ^ block ~n:(i + 1) p))
;;

module Token = struct
  type t =
    { token : string
    ; text : string
    }
  [@@deriving sexp_of]
end

let resolve text tokens =
  let found =
    List.filter_map tokens ~f:(fun (t : Token.t) ->
      Option.map (String.substr_index text ~pattern:t.token) ~f:(fun at -> at, t))
    |> List.sort ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> List.map ~f:snd
  in
  let typed =
    replace_tokens text (List.map found ~f:(fun t -> t.token, ""))
  in
  let text =
    if String.is_empty (String.strip typed)
    then ""
    else
      replace_tokens
        text
        (List.mapi found ~f:(fun i t -> t.token, sprintf "[%s]" (name (i + 1))))
  in
  text, List.map found ~f:(fun t -> t.text)
;;

let params ~backend_takes ~text pasted =
  match pasted with
  | [] -> [ "text", P.Json.str text ]
  | pasted when backend_takes ->
    [ "text", P.Json.str text; "pasted", `Array (List.map pasted ~f:P.Json.str) ]
  | pasted -> [ "text", P.Json.str (inline ~text pasted) ]
;;
