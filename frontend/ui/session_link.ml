open! Core
open! Import

let href id = "?session=" ^ id
let id_char c = Char.is_alphanum c || Char.equal c '_' || Char.equal c '-'

let valid id =
  (not (String.is_empty id))
  && String.length id <= 64
  && String.for_all id ~f:id_char
;;

let before_fragment s = List.hd_exn (String.split s ~on:'#')

let of_href href =
  let href = String.strip href in
  let id =
    match String.chop_prefix href ~prefix:"prigh://session/" with
    | Some rest ->
      Some (List.hd_exn (String.split_on_chars rest ~on:[ '?'; '#'; '/' ]))
    | None ->
      let query =
        match String.chop_prefix href ~prefix:"/" with
        | Some rest -> String.chop_prefix rest ~prefix:"?"
        | None -> String.chop_prefix href ~prefix:"?"
      in
      Option.bind query ~f:(fun query ->
        String.split (before_fragment query) ~on:'&'
        |> List.find_map ~f:(String.chop_prefix ~prefix:"session="))
  in
  Option.filter id ~f:valid
;;

let title (s : P.Session_summary.t) =
  match s.name with
  | Some name when not (String.is_empty (String.strip name)) -> name
  | Some _ | None -> P.Session_summary.blurb s
;;

module Known = struct
  type t =
    { sessions : P.Session_summary.t list
    ; by_id : P.Session_summary.t String.Map.t
    ; stamp : int
    }

  let create sessions =
    { sessions
    ; by_id =
        String.Map.of_alist_reduce
          (List.map sessions ~f:(fun (s : P.Session_summary.t) -> s.id, s))
          ~f:(fun first _ -> first)
    ; stamp =
        [%hash: (string * string) list]
          (List.map sessions ~f:(fun s -> s.id, title s))
    }
  ;;

  let empty = create []
  let stamp t = t.stamp

  module Lookup = struct
    type t =
      | Found of P.Session_summary.t
      | Ambiguous of P.Session_summary.t list
      | Missing of { closest : P.Session_summary.t option }
    [@@deriving sexp_of]
  end

  let starting t key =
    Map.to_sequence t.by_id ~keys_greater_or_equal_to:key
    |> Sequence.take_while ~f:(fun (id, _) -> String.is_prefix id ~prefix:key)
    |> Sequence.map ~f:snd
    |> Fn.flip Sequence.take 6
    |> Sequence.to_list
  ;;

  (* A typo of an id: its first characters, one or two edits away. *)
  let closest t key =
    if String.length key < 4
    then None
    else
      List.filter_map t.sessions ~f:(fun (s : P.Session_summary.t) ->
        let d =
          Edit_distance.distance key (String.prefix s.id (String.length key))
        in
        Option.some_if (d <= 2) (d, s))
      |> List.min_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b)
      |> Option.map ~f:snd
  ;;

  let find t key : Lookup.t =
    let key = String.lowercase (String.strip key) in
    if String.is_empty key
    then Missing { closest = None }
    else (
      match Map.find t.by_id key with
      | Some s -> Found s
      | None ->
        (match starting t key with
         | [ s ] -> Found s
         | [] -> Missing { closest = closest t key }
         | many -> Ambiguous many))
  ;;
end

let is_hex c = Char.is_digit c || Char.between c ~low:'a' ~high:'f'
let word_char c = Char.is_alphanum c || Char.equal c '_'

let ids (known : Known.t) text =
  let n = String.length text in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else if is_hex text.[i] && (i = 0 || not (word_char text.[i - 1]))
    then (
      let j = ref i in
      while !j < n && is_hex text.[!j] do
        incr j
      done;
      let len = !j - i in
      let acc =
        if len >= 8 && len <= 16 && (!j = n || not (word_char text.[!j]))
        then (
          match Known.find known (String.sub text ~pos:i ~len) with
          | Found s -> (i, len, s) :: acc
          | Ambiguous _ | Missing _ -> acc)
        else acc
      in
      go !j acc)
    else go (i + 1) acc
  in
  if Map.is_empty known.by_id then [] else go 0 []
;;
