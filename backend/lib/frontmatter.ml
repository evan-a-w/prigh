open! Core

type t = (string * string) list [@@deriving sexp_of]

let find t key = List.Assoc.find t ~equal:String.equal key

let strip_cr line =
  String.chop_suffix line ~suffix:"\r" |> Option.value ~default:line
;;

let is_fence line = String.equal (String.rstrip line) "---"
let is_end line = is_fence line || String.equal (String.rstrip line) "..."
let indented line = String.is_prefix line ~prefix:" " || String.is_prefix line ~prefix:"\t"
let is_blank line = String.is_empty (String.strip line)

let unquote_single s =
  String.substr_replace_all s ~pattern:"''" ~with_:"'"
;;

let unquote_double s =
  let b = Buffer.create (String.length s) in
  let rec go i =
    if i < String.length s
    then (
      match s.[i] with
      | '\\' when i + 1 < String.length s ->
        (match s.[i + 1] with
         | 'n' -> Buffer.add_char b '\n'
         | 't' -> Buffer.add_char b '\t'
         | c -> Buffer.add_char b c);
        go (i + 2)
      | c ->
        Buffer.add_char b c;
        go (i + 1))
  in
  go 0;
  Buffer.contents b
;;

(* Lines of a block scalar lose their common indentation; [>] folds them
   into one paragraph per run of non-blank lines. *)
let block_scalar ~literal lines =
  let indent line = String.length line - String.length (String.lstrip line) in
  let common =
    List.filter lines ~f:(Fn.non is_blank)
    |> List.map ~f:indent
    |> List.min_elt ~compare:Int.compare
    |> Option.value ~default:0
  in
  let lines =
    List.map lines ~f:(fun line ->
      if is_blank line then "" else String.drop_prefix line common)
  in
  if literal
  then String.concat ~sep:"\n" lines |> String.rstrip
  else
    List.group lines ~break:(fun a b -> Bool.(is_blank a <> is_blank b))
    |> List.filter_map ~f:(fun group ->
      if List.for_all group ~f:is_blank
      then None
      else Some (String.concat ~sep:" " (List.map group ~f:String.strip)))
    |> String.concat ~sep:"\n"
;;

let scalar first continuation =
  let first = String.strip first in
  let rest = List.map continuation ~f:String.strip in
  let joined = String.concat ~sep:" " (List.filter (first :: rest) ~f:(Fn.non String.is_empty)) in
  match String.prefix first 1 with
  | "|" -> block_scalar ~literal:true continuation
  | ">" -> block_scalar ~literal:false continuation
  | "'" when String.length joined >= 2 && String.is_suffix joined ~suffix:"'" ->
    unquote_single (String.sub joined ~pos:1 ~len:(String.length joined - 2))
  | "\"" when String.length joined >= 2 && String.is_suffix joined ~suffix:"\"" ->
    unquote_double (String.sub joined ~pos:1 ~len:(String.length joined - 2))
  | _ ->
    if List.is_empty rest
    then first
    else if String.is_empty first
    then (* a nested mapping or list: keep its text *)
      block_scalar ~literal:true continuation
    else joined
;;

let rec fields lines =
  match lines with
  | [] -> []
  | line :: rest when is_blank line || String.is_prefix (String.lstrip line) ~prefix:"#" ->
    fields rest
  | line :: rest ->
    let continuation, rest =
      List.split_while rest ~f:(fun l -> indented l || is_blank l)
    in
    (match String.lsplit2 line ~on:':' with
     | Some (key, value) when not (indented line) ->
       (String.strip key, scalar value continuation) :: fields rest
     | Some _ | None -> fields rest)
;;

let split text =
  match String.split text ~on:'\n' |> List.map ~f:strip_cr with
  | first :: rest when is_fence first ->
    (match List.findi rest ~f:(fun _ line -> is_end line) with
     | None -> [], text
     | Some (i, _) ->
       let header = List.take rest i in
       let body = List.drop rest (i + 1) in
       fields header, String.concat ~sep:"\n" body)
  | _ -> [], text
;;
