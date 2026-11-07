open! Core
open! Import

let is_word_byte c = Char.is_alphanum c || Char.to_int c >= 0x80

(* (start, stop) of each word of [s], in order. *)
let spans s =
  let n = String.length s in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else if is_word_byte s.[i]
    then (
      let j = ref i in
      while !j < n && is_word_byte s.[!j] do
        incr j
      done;
      go !j ((i, !j) :: acc))
    else go (i + 1) acc
  in
  go 0 []
;;

let word s (start, stop) =
  String.lowercase (String.sub s ~pos:start ~len:(stop - start))
;;

let words s = List.map (spans s) ~f:(word s)

let query_words query =
  List.fold (words query) ~init:[] ~f:(fun acc w ->
    if List.mem acc w ~equal:String.equal then acc else w :: acc)
  |> List.rev
;;

(* Whether [a] and [b] are at most one insertion, deletion, substitution or
   transposition of adjacent letters apart. *)
let within_one_edit a b =
  let la = String.length a
  and lb = String.length b in
  let rec common i =
    if i < la && i < lb && Char.equal a.[i] b.[i] then common (i + 1) else i
  in
  let i = common 0 in
  let rest_equal ia ib =
    la - ia = lb - ib
    && String.equal
         (String.sub a ~pos:ia ~len:(la - ia))
         (String.sub b ~pos:ib ~len:(lb - ib))
  in
  if i = la && i = lb
  then true
  else if la = lb
  then
    rest_equal (i + 1) (i + 1)
    || (i + 1 < la
        && Char.equal a.[i] b.[i + 1]
        && Char.equal a.[i + 1] b.[i]
        && rest_equal (i + 2) (i + 2))
  else if la = lb + 1
  then rest_equal (i + 1) i
  else if lb = la + 1
  then rest_equal i (i + 1)
  else false
;;

(* How [w] (a query word) matches [v] (a word of the text): its score and the
   part of [v] that matched, (offset, length). *)
let kind ~w v =
  let lw = String.length w
  and lv = String.length v in
  if String.equal w v
  then Some (10, (0, lv))
  else if String.is_prefix v ~prefix:w
  then Some (8, (0, lw))
  else (
    let substring =
      if lw >= 3 then String.substr_index v ~pattern:w else None
    in
    match substring with
    | Some i -> Some (6, (i, lw))
    | None ->
      if lw >= 4
      then
        List.find_map
          [ lw; lw - 1; lw + 1 ]
          ~f:(fun len ->
            if len <= lv && within_one_edit w (String.sub v ~pos:0 ~len)
            then Some (4, (0, len))
            else None)
      else None)
;;

module Weight = struct
  let name = 4
  let first_prompt = 3
  let message = 2
end

module Doc = struct
  type t =
    { num : int
    ; id : string
    ; path : string
    ; size : Int64.t
    ; mtime : float
    ; name : string option
    ; texts : (int * string) array (** weight, text; the name first, if any *)
    ; words : int String.Table.t (** each word's best weight *)
    }

  let texts ~name session =
    let messages =
      List.filter_map
        (Session.active_path session)
        ~f:(fun (e : Session.Entry.t) ->
          match e.payload with
          | Message m when Option.is_none (Session.summary_of_message m) ->
            (match m with
             | User u -> Some (`User, Message.User.body u)
             | Assistant a -> Some (`Assistant, Message.Assistant.text a)
             | Tool_result _ -> None)
          | _ -> None)
      |> List.filter ~f:(fun (_, text) ->
        not (String.is_empty (String.strip text)))
    in
    let first_user = ref true in
    let messages =
      List.map messages ~f:(fun (who, text) ->
        match who with
        | `User when !first_user ->
          first_user := false;
          Weight.first_prompt, Skill.as_typed text
        | `User | `Assistant -> Weight.message, text)
    in
    Array.of_list
      (Option.value_map name ~default:[] ~f:(fun n -> [ Weight.name, n ])
       @ messages)
  ;;

  let create ~num ~(summary : Session.Summary.t) ~size ~mtime session =
    let texts = texts ~name:summary.name session in
    let table = String.Table.create () in
    Array.iter texts ~f:(fun (weight, text) ->
      List.iter (words text) ~f:(fun w ->
        Hashtbl.update table w ~f:(function
          | Some best -> Int.max best weight
          | None -> weight)));
    { num
    ; id = summary.id
    ; path = summary.path
    ; size
    ; mtime
    ; name = summary.name
    ; texts
    ; words = table
    }
  ;;
end

type t =
  { docs : Doc.t String.Table.t (** by path *)
  ; postings : int Int.Table.t String.Table.t
    (** each word's sessions (by [Doc.num]) with its best weight in them *)
  ; mutable next : int
  }

let create () =
  { docs = String.Table.create (); postings = String.Table.create (); next = 0 }
;;

let remove t (doc : Doc.t) =
  Hashtbl.remove t.docs doc.path;
  Hashtbl.iter_keys doc.words ~f:(fun w ->
    match Hashtbl.find t.postings w with
    | None -> ()
    | Some docs ->
      Hashtbl.remove docs doc.num;
      if Hashtbl.is_empty docs then Hashtbl.remove t.postings w)
;;

let add t (doc : Doc.t) =
  Hashtbl.set t.docs ~key:doc.path ~data:doc;
  Hashtbl.iteri doc.words ~f:(fun ~key:w ~data:weight ->
    Hashtbl.set
      (Hashtbl.find_or_add t.postings w ~default:Int.Table.create)
      ~key:doc.num
      ~data:weight)
;;

let refresh t ~sessions ~load =
  let listed =
    String.Set.of_list
      (List.map sessions ~f:(fun (s : Session.Summary.t) -> s.path))
  in
  List.iter (Hashtbl.data t.docs) ~f:(fun doc ->
    if not (Set.mem listed doc.path) then remove t doc);
  List.iter sessions ~f:(fun (summary : Session.Summary.t) ->
    let drop () =
      Option.iter (Hashtbl.find t.docs summary.path) ~f:(remove t)
    in
    match Core_unix.stat summary.path with
    | exception _ -> drop ()
    | { st_size = size; st_mtime = mtime; _ } ->
      let fresh =
        match Hashtbl.find t.docs summary.path with
        | Some doc ->
          Int64.equal doc.size size
          && Float.equal doc.mtime mtime
          && Option.equal String.equal doc.name summary.name
        | None -> false
      in
      if not fresh
      then (
        let loaded = load summary in
        (* [load] may let another search index it meanwhile. *)
        drop ();
        Option.iter loaded ~f:(fun session ->
          t.next <- t.next + 1;
          add t (Doc.create ~num:t.next ~summary ~size ~mtime session))))
;;

(* Each session's best score for [w]: one look at every distinct word. *)
let scores t ~w =
  let best = Int.Table.create () in
  Hashtbl.iteri t.postings ~f:(fun ~key:v ~data:docs ->
    match kind ~w v with
    | None -> ()
    | Some (k, _) ->
      Hashtbl.iteri docs ~f:(fun ~key:num ~data:weight ->
        let score = k * weight in
        Hashtbl.update best num ~f:(function
          | Some b -> Int.max b score
          | None -> score)));
  best
;;

module Snippet = struct
  type t =
    { text : string
    ; ranges : (int * int) list
    }
  [@@deriving sexp_of]

  let collapse s =
    String.split_on_chars s ~on:[ ' '; '\n'; '\t'; '\r' ]
    |> List.filter ~f:(Fn.non String.is_empty)
    |> String.concat ~sep:" "
  ;;

  (* Every match of a query word in [text]: (word index, score, start,
     length). *)
  let matches ws text =
    List.concat_map (spans text) ~f:(fun span ->
      let v = word text span in
      List.filter_mapi ws ~f:(fun i w ->
        Option.map (kind ~w v) ~f:(fun (k, (off, len)) ->
          i, k, fst span + off, len)))
  ;;

  let is_continuation c = Char.to_int c land 0xC0 = 0x80
  let width = 80
  let before = 20
  let slack = 15

  let window text ~first =
    let n = String.length text in
    let start = Int.max 0 (first - before) in
    let start =
      if start = 0 || Char.equal text.[start - 1] ' '
      then start
      else (
        match String.index_from text start ' ' with
        | Some i when i < first && i - start <= slack -> i + 1
        | Some _ | None ->
          let s = ref start in
          while !s < first && is_continuation text.[!s] do
            incr s
          done;
          !s)
    in
    let stop = Int.min n (start + width) in
    let stop =
      if stop = n || Char.equal text.[stop] ' '
      then stop
      else (
        match String.rindex_from text stop ' ' with
        | Some i when i > first && stop - i <= slack -> i
        | Some _ | None ->
          let s = ref stop in
          while !s > start && is_continuation text.[!s] do
            decr s
          done;
          !s)
    in
    start, stop
  ;;

  let merge ranges =
    List.sort ranges ~compare:[%compare: int * int]
    |> List.fold ~init:[] ~f:(fun acc (s, l) ->
      match acc with
      | (ps, pl) :: rest when s <= ps + pl ->
        (ps, Int.max pl (s + l - ps)) :: rest
      | _ -> (s, l) :: acc)
    |> List.rev
  ;;

  let create ws (doc : Doc.t) =
    let best =
      Array.fold doc.texts ~init:None ~f:(fun best (weight, text) ->
        if weight = Weight.name
        then best
        else (
          let text = collapse text in
          match matches ws text with
          | [] -> best
          | ms ->
            let per_word =
              List.fold ms ~init:Int.Map.empty ~f:(fun acc (i, k, _, _) ->
                Map.update acc i ~f:(function
                  | Some b -> Int.max b (k * weight)
                  | None -> k * weight))
            in
            let rank =
              ( Map.length per_word
              , List.sum (module Int) (Map.data per_word) ~f:Fn.id )
            in
            (match best with
             | Some (best_rank, _, _)
               when [%compare: int * int] rank best_rank <= 0 -> best
             | _ -> Some (rank, text, ms))))
    in
    Option.map best ~f:(fun (_, text, ms) ->
      let first =
        List.fold ms ~init:Int.max_value ~f:(fun acc (_, _, s, _) ->
          Int.min acc s)
      in
      let start, stop = window text ~first in
      let ellipsis = "…" in
      let shift = if start > 0 then String.length ellipsis else 0 in
      let ranges =
        List.filter_map ms ~f:(fun (_, _, s, l) ->
          if s >= start && s < stop
          then Some (s - start + shift, Int.min l (stop - s))
          else None)
        |> merge
      in
      { text =
          ((if start > 0 then ellipsis else "")
           ^ String.sub text ~pos:start ~len:(stop - start)
           ^ if stop < String.length text then ellipsis else "")
      ; ranges
      })
  ;;
end

module Hit = struct
  type t =
    { id : string
    ; path : string
    ; score : int
    ; snippet : Snippet.t option
    }
  [@@deriving sexp_of]

  let to_json t =
    let int n = `Number (Int.to_string n) in
    `Object
      [ "id", `String t.id
      ; "path", `String t.path
      ; "score", int t.score
      ; ( "snippet"
        , match t.snippet with
          | None -> `Null
          | Some { text; ranges } ->
            `Object
              [ "text", `String text
              ; ( "ranges"
                , `Array
                    (List.map ranges ~f:(fun (s, l) -> `Array [ int s; int l ]))
                )
              ] )
      ]
  ;;
end

let search t ~sessions ~load ~query ~limit =
  match query_words query with
  | [] -> []
  | ws ->
    refresh t ~sessions ~load;
    let tables =
      List.map ws ~f:(fun w -> scores t ~w)
      |> List.sort ~compare:(fun a b ->
        Int.compare (Hashtbl.length a) (Hashtbl.length b))
    in
    let totals =
      match tables with
      | [] -> []
      | smallest :: others ->
        Hashtbl.fold smallest ~init:[] ~f:(fun ~key:num ~data:score acc ->
          match
            List.fold_until
              others
              ~init:score
              ~f:(fun sum table ->
                match Hashtbl.find table num with
                | Some s -> Continue (sum + s)
                | None -> Stop None)
              ~finish:Option.some
          with
          | Some total -> (num, total) :: acc
          | None -> acc)
    in
    let by_num = Int.Table.create () in
    Hashtbl.iter t.docs ~f:(fun (doc : Doc.t) ->
      Hashtbl.set by_num ~key:doc.num ~data:doc);
    let activity = String.Table.create () in
    List.iter sessions ~f:(fun (s : Session.Summary.t) ->
      Hashtbl.set activity ~key:s.path ~data:s.last_activity);
    List.filter_map totals ~f:(fun (num, score) ->
      Option.map (Hashtbl.find by_num num) ~f:(fun (doc : Doc.t) ->
        score, Option.value (Hashtbl.find activity doc.path) ~default:0., doc))
    |> List.sort ~compare:(fun (s1, a1, (d1 : Doc.t)) (s2, a2, (d2 : Doc.t)) ->
      match Int.compare s2 s1 with
      | 0 ->
        (match Float.compare a2 a1 with
         | 0 -> String.compare d1.path d2.path
         | c -> c)
      | c -> c)
    |> Fn.flip List.take limit
    |> List.map ~f:(fun (score, _, (doc : Doc.t)) ->
      { Hit.id = doc.id
      ; path = doc.path
      ; score
      ; snippet = Snippet.create ws doc
      })
;;
