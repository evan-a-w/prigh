open! Core
open! Import

module Field = struct
  type t =
    | Name
    | Prompt
    | Text
    | Place
  [@@deriving sexp_of, equal]

  let weight = function
    | Name -> 4
    | Prompt -> 3
    | Text -> 2
    | Place -> 1
  ;;
end

module Kind = struct
  type t =
    | Exact
    | Prefix
    | Substring
    | Typo
    | Subsequence
  [@@deriving sexp_of, equal]

  let score = function
    | Exact -> 10
    | Prefix -> 8
    | Substring -> 6
    | Typo -> 4
    | Subsequence -> 3
  ;;
end

module Why = struct
  type t =
    { word : string
    ; field : Field.t
    ; kind : Kind.t
    }
  [@@deriving sexp_of, equal]
end

module Match = struct
  type t =
    { score : int
    ; ranges : (int * int) list list
    ; why : Why.t list
    }
  [@@deriving sexp_of, equal]
end

let is_word_char c = Char.is_alphanum c || Char.to_int c >= 128

(* [(start, lowercased word)] *)
let tokens s =
  let n = String.length s in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else if is_word_char s.[i]
    then (
      let j = ref i in
      while !j < n && is_word_char s.[!j] do
        incr j
      done;
      go !j ((i, String.lowercase (String.sub s ~pos:i ~len:(!j - i))) :: acc))
    else go (i + 1) acc
  in
  go 0 []
;;

let words query =
  List.fold (tokens query) ~init:[] ~f:(fun acc (_, w) ->
    if List.mem acc w ~equal:String.equal then acc else w :: acc)
  |> List.rev
;;

(* Optimal string alignment distance at most 1. *)
let within_one_edit a b =
  let la = String.length a
  and lb = String.length b in
  if abs (la - lb) > 1
  then false
  else (
    let i = ref 0 in
    while !i < la && !i < lb && Char.equal a.[!i] b.[!i] do
      incr i
    done;
    let i = !i in
    let rest_equal ~da ~db =
      String.equal
        (String.drop_prefix a (Int.min la (i + da)))
        (String.drop_prefix b (Int.min lb (i + db)))
    in
    if i = la && i = lb
    then true
    else if la = lb
    then
      rest_equal ~da:1 ~db:1
      || (i + 1 < la
          && Char.equal a.[i] b.[i + 1]
          && Char.equal a.[i + 1] b.[i]
          && rest_equal ~da:2 ~db:2)
    else if la > lb
    then rest_equal ~da:1 ~db:0
    else rest_equal ~da:0 ~db:1)
;;

let typo ~w v =
  let n = String.length w in
  List.find_map [ n; n - 1; n + 1 ] ~f:(fun len ->
    if len <= String.length v && within_one_edit w (String.prefix v len)
    then Some len
    else None)
;;

(* The positions of [w]'s letters in [v], from its first letter on. *)
let subsequence ~w v =
  let n = String.length w in
  if n = 0 || String.is_empty v || not (Char.equal w.[0] v.[0])
  then None
  else (
    let rec go qi vi acc =
      if qi >= n
      then Some (List.rev acc)
      else (
        match String.index_from v vi w.[qi] with
        | None -> None
        | Some p -> go (qi + 1) (p + 1) (p :: acc))
    in
    go 1 1 [ 0 ])
;;

(* Adjacent positions as [(start, length)] runs. *)
let runs positions =
  List.fold positions ~init:[] ~f:(fun acc p ->
    match acc with
    | (start, len) :: rest when start + len = p -> (start, len + 1) :: rest
    | _ -> (p, 1) :: acc)
  |> List.rev
;;

(* How [w] matches the text word [v], and where in it. *)
let match_word ~w v =
  let n = String.length w in
  if String.equal w v
  then Some (Kind.Exact, [ 0, n ])
  else if String.is_prefix v ~prefix:w
  then Some (Prefix, [ 0, n ])
  else (
    match
      if n >= 3 then String.substr_index v ~pattern:w else None
    with
    | Some i -> Some (Substring, [ i, n ])
    | None ->
      (match if n >= 4 then typo ~w v else None with
       | Some len -> Some (Typo, [ 0, len ])
       | None ->
         if n >= 3
         then
           Option.map (subsequence ~w v) ~f:(fun ps ->
             Kind.Subsequence, runs ps)
         else None))
;;

let merge_ranges ranges =
  List.sort ranges ~compare:[%compare: int * int]
  |> List.fold ~init:[] ~f:(fun acc (start, len) ->
    match acc with
    | (s, l) :: rest when start <= s + l ->
      (s, Int.max l (start + len - s)) :: rest
    | _ -> (start, len) :: acc)
  |> List.rev
;;

let score ~query fields =
  match words query with
  | [] -> Some { Match.score = 0; ranges = List.map fields ~f:(fun _ -> []); why = [] }
  | query_words ->
    let fields =
      List.map fields ~f:(fun (field, text) -> field, tokens text)
    in
    (* Per field, per token: the matches of any query word, with ranges. *)
    let best =
      List.map query_words ~f:(fun w ->
        List.fold fields ~init:None ~f:(fun best (field, toks) ->
          List.fold toks ~init:best ~f:(fun best (_, v) ->
            match match_word ~w v with
            | None -> best
            | Some (kind, _) ->
              let s = Kind.score kind * Field.weight field in
              (match best with
               | Some (b, _) when b >= s -> best
               | _ -> Some (s, { Why.word = w; field; kind })))))
    in
    if List.exists best ~f:Option.is_none
    then None
    else (
      let best = List.filter_map best ~f:Fn.id in
      let ranges =
        List.map fields ~f:(fun (_, toks) ->
          List.concat_map toks ~f:(fun (start, v) ->
            List.concat_map query_words ~f:(fun w ->
              match match_word ~w v with
              | None -> []
              | Some (_, ranges) ->
                List.map ranges ~f:(fun (s, l) -> start + s, l)))
          |> merge_ranges)
      in
      Some
        { score = List.sum (module Int) best ~f:fst
        ; ranges
        ; why = List.map best ~f:snd
        })
;;

let pieces text ranges =
  let n = String.length text in
  let rec go pos ranges acc =
    match ranges with
    | [] ->
      List.rev
        (if pos < n then (`Plain, String.drop_prefix text pos) :: acc else acc)
    | (start, len) :: rest ->
      let start = Int.clamp_exn start ~min:pos ~max:n in
      let stop = Int.clamp_exn (start + len) ~min:start ~max:n in
      let acc =
        if start > pos
        then (`Plain, String.sub text ~pos ~len:(start - pos)) :: acc
        else acc
      in
      let acc =
        if stop > start
        then (`Match, String.sub text ~pos:start ~len:(stop - start)) :: acc
        else acc
      in
      go stop rest acc
  in
  go 0 ranges []
;;

module Result = struct
  type t =
    { session : P.Session_summary.t
    ; score : int
    ; title : (int * int) list
    ; snippet : P.Session_hits.Snippet.t option
    ; why : Why.t list
    }
  [@@deriving sexp_of, equal]
end

let title (s : P.Session_summary.t) =
  match s.name with
  | Some name when not (String.is_empty (String.strip name)) -> name
  | _ -> P.Session_summary.blurb s
;;

let session_match ~query (s : P.Session_summary.t) =
  let title = title s in
  let named =
    Option.exists s.name ~f:(fun n -> not (String.is_empty (String.strip n)))
  in
  let others =
    [ Field.Prompt, Option.value s.first_prompt ~default:""
    ; Text, Option.value s.description ~default:""
    ; Place, s.cwd
    ; Place, Option.value (Session_worktrees.place s) ~default:""
    ]
    @ List.map s.worktrees ~f:(fun w -> Field.Place, P.Session_worktree.label w)
  in
  (* Unnamed, the title is the blurb, which is searched as what it is. *)
  let fields = if named then (Field.Name, title) :: others else others in
  Option.map (score ~query fields) ~f:(fun (m : Match.t) ->
    let title_ranges =
      if named
      then List.hd m.ranges |> Option.value ~default:[]
      else (
        match
          List.find_map (List.zip_exn fields m.ranges) ~f:(fun ((f, text), r) ->
            if
              (Field.equal f Prompt || Field.equal f Text)
              && String.equal
                   (String.concat ~sep:" " (String.split_lines text))
                   title
              && not (String.is_empty text)
            then Some r
            else None)
        with
        | Some r -> r
        | None -> [])
    in
    m, title_ranges)
;;

let sessions ?content ~query sessions =
  if List.is_empty (words query)
  then
    List.map sessions ~f:(fun session ->
      { Result.session; score = 0; title = []; snippet = None; why = [] })
  else (
    let hits =
      match content with
      | Some (c : P.Session_hits.t)
        when [%equal: string list]
               (List.sort (words c.query) ~compare:String.compare)
               (List.sort (words query) ~compare:String.compare) ->
        String.Map.of_alist_reduce
          (List.map c.hits ~f:(fun (h : P.Session_hits.Hit.t) -> h.path, h))
          ~f:(fun a _ -> a)
      | Some _ | None -> String.Map.empty
    in
    List.filter_map sessions ~f:(fun (session : P.Session_summary.t) ->
      let local = session_match ~query session in
      let hit = Map.find hits session.path in
      match local, hit with
      | None, None -> None
      | _ ->
        let local_score =
          Option.value_map local ~default:0 ~f:(fun ((m : Match.t), _) ->
            m.score)
        in
        let hit_score =
          Option.value_map hit ~default:0 ~f:(fun (h : P.Session_hits.Hit.t) ->
            h.score)
        in
        Some
          { Result.session
          ; score = Int.max local_score hit_score
          ; title = Option.value_map local ~default:[] ~f:snd
          ; snippet = Option.bind hit ~f:(fun h -> h.snippet)
          ; why =
              Option.value_map local ~default:[] ~f:(fun ((m : Match.t), _) ->
                m.why)
          })
    |> List.stable_sort ~compare:(fun (a : Result.t) (b : Result.t) ->
      match Int.compare b.score a.score with
      | 0 ->
        [%compare: string option] b.session.updated_at a.session.updated_at
      | c -> c))
;;
