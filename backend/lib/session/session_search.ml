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

(* Whether [a] and the first [lb] bytes of [b] are at most one insertion,
   deletion, substitution or transposition of adjacent letters apart. *)
let within_one_edit a b ~lb =
  let la = String.length a in
  let rec common i =
    if i < la && i < lb && Char.equal a.[i] b.[i] then common (i + 1) else i
  in
  let i = common 0 in
  let rec rest_equal ia ib =
    if ia >= la
    then ib >= lb
    else ib < lb && Char.equal a.[ia] b.[ib] && rest_equal (ia + 1) (ib + 1)
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

let substring_index v ~w =
  let lv = String.length v
  and lw = String.length w in
  let rec matches_at i j =
    j >= lw || (Char.equal v.[i + j] w.[j] && matches_at i (j + 1))
  in
  let rec from i =
    if i + lw > lv then None else if matches_at i 0 then Some i else from (i + 1)
  in
  from 0
;;

(* How [w] (a query word) matches [v] (a word of the text): its score and the
   part of [v] that matched, (offset, length). *)
let kind ~w v =
  let lw = String.length w
  and lv = String.length v in
  if lv < lw - 1
  then None
  else if String.equal w v
  then Some (10, (0, lv))
  else if String.is_prefix v ~prefix:w
  then Some (8, (0, lw))
  else (
    match if lw >= 3 then substring_index v ~w else None with
    | Some i -> Some (6, (i, lw))
    | None ->
      if lw >= 4
      then (
        let typo len = len <= lv && within_one_edit w v ~lb:len in
        if typo lw
        then Some (4, (0, lw))
        else if typo (lw - 1)
        then Some (4, (0, lw - 1))
        else if typo (lw + 1)
        then Some (4, (0, lw + 1))
        else None)
      else None)
;;

module Weight = struct
  let name = 4
  let first_prompt = 3
  let message = 2
end

(* A message's text in the index: where to read it again for a snippet,
   and which of its session's words it has, not the text itself. *)
module Text = struct
  type t =
    { weight : int
    ; offset : int (** of its entry's line in the file; [-1]: unknown *)
    ; words : int array (** indices in [Doc.words] *)
    }
  [@@deriving bin_io]

  let of_message ~weight (m : Message.t) =
    match m with
    | User u ->
      let body = Message.User.body u in
      Some (if weight = Weight.first_prompt then Skill.as_typed body else body)
    | Assistant a -> Some (Message.Assistant.text a)
    | Tool_result _ -> None
  ;;

  (* Its text again, from the file. *)
  let read t ~path =
    if t.offset < 0
    then None
    else (
      match Session.read_entry_at path ~offset:t.offset with
      | Some { payload = Message m; _ } -> of_message ~weight:t.weight m
      | Some _ | None -> None)
  ;;
end

module Doc = struct
  type t =
    { num : int
    ; id : string
    ; path : string
    ; size : Int64.t
    ; mtime : float
    ; name : string option
    ; texts : Text.t array (** the messages' *)
    ; mutable words : string array
      (** its distinct words, the postings' strings (set by [add]) *)
    ; weights : int array (** each word's best weight in it *)
    }

  let texts session =
    let messages =
      List.filter_map
        (Session.active_path session)
        ~f:(fun (e : Session.Entry.t) ->
          match e.payload with
          | Message m when Option.is_none (Session.summary_of_message m) ->
            (match m with
             | User u -> Some (e.id, `User, Message.User.body u)
             | Assistant a -> Some (e.id, `Assistant, Message.Assistant.text a)
             | Tool_result _ -> None)
          | _ -> None)
      |> List.filter ~f:(fun (_, _, text) ->
        not (String.is_empty (String.strip text)))
    in
    let first_user = ref true in
    List.map messages ~f:(fun (id, who, text) ->
      let offset = Option.value (Session.entry_offset session id) ~default:(-1) in
      match who with
      | `User when !first_user ->
        first_user := false;
        Weight.first_prompt, offset, Skill.as_typed text
      | `User | `Assistant -> Weight.message, offset, text)
  ;;

  let create ~num ~(summary : Session.Summary.t) ~size ~mtime session =
    let local = String.Table.create () in
    let distinct = ref [] in
    let weights = ref [] in
    let n = ref 0 in
    let local_word w ~weight =
      match Hashtbl.find local w with
      | Some (i, best) ->
        if weight > !best then best := weight;
        i
      | None ->
        let i = !n in
        incr n;
        let best = ref weight in
        Hashtbl.set local ~key:w ~data:(i, best);
        distinct := w :: !distinct;
        weights := best :: !weights;
        i
    in
    Option.iter summary.name ~f:(fun name ->
      List.iter (words name) ~f:(fun w ->
        ignore (local_word w ~weight:Weight.name : int)));
    let texts =
      List.map (texts session) ~f:(fun (weight, offset, text) ->
        let ids =
          List.map (words text) ~f:(local_word ~weight)
          |> List.dedup_and_sort ~compare:Int.compare
        in
        { Text.weight; offset; words = Array.of_list ids })
    in
    { num
    ; id = summary.id
    ; path = summary.path
    ; size
    ; mtime
    ; name = summary.name
    ; texts = Array.of_list texts
    ; words = Array.of_list (List.rev !distinct)
    ; weights = Array.of_list (List.rev_map !weights ~f:( ! ))
    }
  ;;
end

(* The letters in a word, a bit each, to rule out most words at a glance. *)
let letters w =
  String.fold w ~init:0 ~f:(fun acc c ->
    let bit =
      match c with
      | 'a' .. 'z' -> Char.to_int c - Char.to_int 'a'
      | '0' .. '9' -> 26 + Char.to_int c - Char.to_int '0'
      | _ -> 36 + (Char.to_int c land 15)
    in
    acc lor (1 lsl bit))
;;

(* A word's sessions, packed: a few bytes each, as there are millions. *)
module Posting = struct
  type t =
    { word : string
    ; letters : int
    ; mutable docs : int array
      (** [Doc.num lsl 3 lor weight], the word's best weight there; the first
        [len] are used *)
    ; mutable len : int
    }

  let create word = { word; letters = letters word; docs = [||]; len = 0 }

  let add t ~num ~weight =
    if t.len = Array.length t.docs
    then (
      let docs = Array.create ~len:(Int.max 2 (t.len * 2)) 0 in
      Array.blit ~src:t.docs ~src_pos:0 ~dst:docs ~dst_pos:0 ~len:t.len;
      t.docs <- docs);
    t.docs.(t.len) <- (num lsl 3) lor weight;
    t.len <- t.len + 1
  ;;

  let remove t ~num =
    match Array.findi t.docs ~f:(fun i e -> i < t.len && e lsr 3 = num) with
    | None -> ()
    | Some (i, _) ->
      t.len <- t.len - 1;
      t.docs.(i) <- t.docs.(t.len)
  ;;

  let trim t =
    if Array.length t.docs > t.len
    then t.docs <- Array.sub t.docs ~pos:0 ~len:t.len
  ;;

  let iter t ~f =
    for i = 0 to t.len - 1 do
      let e = t.docs.(i) in
      f ~num:(e lsr 3) ~weight:(e land 7)
    done
  ;;
end

type t =
  { docs : Doc.t String.Table.t (** by path *)
  ; postings : Posting.t String.Table.t
    (** each word's sessions (by [Doc.num]) with its best weight in them *)
  ; mutable next : int (** the highest [Doc.num] given *)
  ; mutable free : int list (** [Doc.num]s given back by [remove] *)
  ; mutable indexing : unit Promise.t option (** [index] under way *)
  ; file : string option (** where it is kept between runs *)
  ; mutable changed : bool (** since it was last read or written *)
  ; mutable saved_at : float
  }

let remove t (doc : Doc.t) =
  Hashtbl.remove t.docs doc.path;
  t.free <- doc.num :: t.free;
  t.changed <- true;
  Array.iter doc.words ~f:(fun w ->
    match Hashtbl.find t.postings w with
    | None -> ()
    | Some posting ->
      Posting.remove posting ~num:doc.num;
      if posting.len = 0 then Hashtbl.remove t.postings w)
;;

let add t (doc : Doc.t) =
  Hashtbl.set t.docs ~key:doc.path ~data:doc;
  t.changed <- true;
  doc.words
  <- Array.mapi doc.words ~f:(fun i w ->
       let posting =
         Hashtbl.find_or_add t.postings w ~default:(fun () -> Posting.create w)
       in
       Posting.add posting ~num:doc.num ~weight:doc.weights.(i);
       posting.word)
;;

let fresh_num t =
  match t.free with
  | num :: rest ->
    t.free <- rest;
    num
  | [] ->
    t.next <- t.next + 1;
    t.next
;;

(* What is kept between runs: the documents without their postings, which
   reading them builds again, their words numbered across documents. *)
type index = t

module Stored = struct
  let magic = "prigh-search-index\n"
  let version = 1

  module Entry = struct
    type t =
      { id : string
      ; path : string
      ; size : Int64.t
      ; mtime : float
      ; name : string option
      ; texts : Text.t array
      ; words : int array (** [word lsl 3 lor weight], [word] in [t.words] *)
      }
    [@@deriving bin_io]
  end

  type t =
    { version : int
    ; words : string array
    ; docs : Entry.t array
    }
  [@@deriving bin_io]

  let file ~dir =
    Filename.concat (Session.cache_dir ~dir) (sprintf "search-v%d" version)
  ;;

  let of_index (index : index) =
    let ids = String.Table.create () in
    let words = ref [] in
    let id w =
      Hashtbl.find_or_add ids w ~default:(fun () ->
        words := w :: !words;
        Hashtbl.length ids)
    in
    let docs =
      Hashtbl.data index.docs
      |> List.sort ~compare:(fun (a : Doc.t) b -> Int.compare a.num b.num)
      |> List.map ~f:(fun (d : Doc.t) : Entry.t ->
        { id = d.id
        ; path = d.path
        ; size = d.size
        ; mtime = d.mtime
        ; name = d.name
        ; texts = d.texts
        ; words = Array.mapi d.words ~f:(fun i w -> (id w lsl 3) lor d.weights.(i))
        })
    in
    { version; words = Array.of_list (List.rev !words); docs = Array.of_list docs }
  ;;

  let write t ~path =
    let size = bin_size_t t in
    let buf = Bin_prot.Common.create_buf (String.length magic + size) in
    Bin_prot.Common.blit_string_buf magic buf ~len:(String.length magic);
    ignore (bin_write_t buf ~pos:(String.length magic) t : int);
    let data = Bigstring.to_string buf in
    Session.write_atomically path ~data
  ;;

  (* [None] unless it is all there, of this version, and consistent. *)
  let read ~path =
    match In_channel.read_all path with
    | exception _ -> None
    | data when not (String.is_prefix data ~prefix:magic) -> None
    | data ->
      (match
         let buf = Bigstring.of_string data in
         let pos_ref = ref (String.length magic) in
         let t = bin_read_t buf ~pos_ref in
         if !pos_ref <> String.length data then failwith "trailing bytes";
         let n = Array.length t.words in
         Array.iter t.docs ~f:(fun d ->
           let local = Array.length d.words in
           Array.iter d.words ~f:(fun w ->
             if w lsr 3 >= n || w land 7 = 0 then failwith "bad word");
           Array.iter d.texts ~f:(fun text ->
             Array.iter text.words ~f:(fun i ->
               if i < 0 || i >= local then failwith "bad text")));
         t
       with
       | { version = v; _ } as t when v = version -> Some t
       | { version = _; _ } | (exception _) -> None)
  ;;
end

let create ?file () =
  let t =
    { docs = String.Table.create ()
    ; postings = String.Table.create ()
    ; next = 0
    ; free = []
    ; indexing = None
    ; file
    ; changed = false
    ; saved_at = 0.
    }
  in
  Option.iter (Option.bind file ~f:(fun path -> Stored.read ~path)) ~f:(fun stored ->
    Array.iter stored.docs ~f:(fun (d : Stored.Entry.t) ->
      add
        t
        { num = fresh_num t
        ; id = d.id
        ; path = d.path
        ; size = d.size
        ; mtime = d.mtime
        ; name = d.name
        ; texts = d.texts
        ; words = Array.map d.words ~f:(fun w -> stored.words.(w lsr 3))
        ; weights = Array.map d.words ~f:(fun w -> w land 7)
        });
    Hashtbl.iter t.postings ~f:Posting.trim;
    t.changed <- false);
  t
;;

let file ~dir = Stored.file ~dir

let save t =
  Option.iter t.file ~f:(fun path ->
    if t.changed
    then (
      t.changed <- false;
      t.saved_at <- Core_unix.gettimeofday ();
      Stored.write (Stored.of_index t) ~path))
;;

(* A search saves what it changed at most this often: the live sessions'
   files change with every message, and the next run only needs to read
   again what changed since the last save. *)
let save_every = 60.

let refresh ?(yield = ignore) t ~sessions ~load =
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
    (match Core_unix.stat summary.path with
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
           add t (Doc.create ~num:(fresh_num t) ~summary ~size ~mtime session))));
    yield ())
;;

let index t ~sessions ~load ~yield =
  match t.indexing with
  | Some indexing -> Promise.await indexing
  | None ->
    let indexing, resolve = Promise.create () in
    t.indexing <- Some indexing;
    Exn.protect
      ~f:(fun () ->
        refresh t ~sessions:(sessions ()) ~load ~yield;
        (* Growing them left up to half of each unused. *)
        Hashtbl.iter t.postings ~f:Posting.trim;
        save t)
      ~finally:(fun () ->
        t.indexing <- None;
        Promise.resolve resolve ())
;;

let indexed t = Hashtbl.length t.docs

(* Each session's best score for [w]: one look at every distinct word. *)
let scores t ~w =
  let best = Array.create ~len:(t.next + 1) 0 in
  let wanted = letters w in
  Hashtbl.iteri t.postings ~f:(fun ~key:v ~data:(posting : Posting.t) ->
    (* Every match but a typo has all of [w]'s letters; a typo all but one. *)
    let missing = wanted land lnot posting.letters in
    if missing land (missing - 1) = 0
    then (
      match kind ~w v with
      | None -> ()
      | Some (k, _) ->
        Posting.iter posting ~f:(fun ~num ~weight ->
          best.(num) <- Int.max best.(num) (k * weight))));
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

  (* The text that matches the most query words, then the best: picked by
     the words the index has of each, then only that one read again. *)
  let create ws (doc : Doc.t) =
    let ws = Array.of_list ws in
    (* For each of the session's words, its best match per query word. *)
    let word_kinds =
      Array.map doc.words ~f:(fun v ->
        Array.map ws ~f:(fun w -> Option.value_map (kind ~w v) ~default:0 ~f:fst))
    in
    let best =
      Array.fold doc.texts ~init:None ~f:(fun best (text : Text.t) ->
        let per_word = Array.create ~len:(Array.length ws) 0 in
        Array.iter text.words ~f:(fun i ->
          Array.iteri word_kinds.(i) ~f:(fun j k ->
            per_word.(j) <- Int.max per_word.(j) (k * text.weight)));
        let matched = Array.count per_word ~f:(fun s -> s > 0) in
        if matched = 0
        then best
        else (
          let rank = matched, Array.sum (module Int) per_word ~f:Fn.id in
          match best with
          | Some (best_rank, _) when [%compare: int * int] rank best_rank <= 0 ->
            best
          | _ -> Some (rank, text)))
    in
    Option.bind best ~f:(fun (_, text) ->
      Option.bind (Text.read text ~path:doc.path) ~f:(fun text ->
        let text = collapse text in
        match matches (Array.to_list ws) text with
        | [] -> None
        | ms ->
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
          Some
            { text =
                ((if start > 0 then ellipsis else "")
                 ^ String.sub text ~pos:start ~len:(stop - start)
                 ^ if stop < String.length text then ellipsis else "")
            ; ranges
            }))
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
    Option.iter t.indexing ~f:Promise.await;
    refresh t ~sessions ~load;
    if Float.(Core_unix.gettimeofday () - t.saved_at >= save_every) then save t;
    let tables = List.map ws ~f:(fun w -> scores t ~w) in
    let totals =
      List.filter_map
        (List.range 1 (t.next + 1))
        ~f:(fun num ->
          if List.for_all tables ~f:(fun best -> best.(num) > 0)
          then
            Some (num, List.sum (module Int) tables ~f:(fun best -> best.(num)))
          else None)
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
