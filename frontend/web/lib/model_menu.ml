open! Core
open! Import

module Tab = struct
  type t =
    | Favourites
    | Provider of string
  [@@deriving sexp_of, equal]
end

type t =
  { tab : Tab.t
  ; more : bool
  ; access_open : bool
  ; engine_open : bool
  }
[@@deriving sexp_of, equal]

let init =
  { tab = Favourites; more = false; access_open = false; engine_open = false }
;;

let shown = 6

let offered (models : Llm.t list) ~(auth : Auth_status.t list) ~current =
  (* A provider prigh has no login for is a CLI's (Codex, Claude Code), which
     lists only what its own login offers. *)
  let signed_in provider =
    match
      List.find auth ~f:(fun (s : Auth_status.t) ->
        String.equal s.provider provider)
    with
    | None -> true
    | Some s -> Option.is_some s.configured
  in
  List.filter models ~f:(fun model ->
    Option.equal String.equal current (Some model.key)
    || ((not model.legacy) && signed_in model.provider))
;;

(* The first run of digits and dots: "GPT-6.2 Sol" is [6; 2]. *)
let version name =
  let start =
    String.lfindi name ~f:(fun _ c -> Char.is_digit c)
    |> Option.value ~default:(-1)
  in
  if start < 0
  then None
  else (
    let rest = String.drop_prefix name start in
    let run =
      String.take_while rest ~f:(fun c -> Char.is_digit c || Char.equal c '.')
    in
    Some
      (String.split run ~on:'.'
       |> List.filter_map ~f:(fun part ->
         if String.is_empty part then None else Int.of_string_opt part)))
;;

let newest_first models =
  (* A CLI's (it has a [default]) stay in its order: its default first. *)
  let cli =
    List.filter_map models ~f:(fun (m : Llm.t) ->
      Option.some_if (String.equal m.id "default") m.provider)
  in
  let is_cli (m : Llm.t) = List.mem cli m.provider ~equal:String.equal in
  (* The others are sorted among themselves, in the places they held. *)
  let sorted =
    List.filter models ~f:(Fn.non is_cli)
    |> List.stable_sort ~compare:(fun (a : Llm.t) (b : Llm.t) ->
      match version a.name, version b.name with
      | Some a, Some b -> List.compare Int.compare b a
      | Some _, None -> -1
      | None, Some _ -> 1
      | None, None -> 0)
  in
  List.folding_map models ~init:sorted ~f:(fun sorted m ->
    match sorted with
    | next :: rest when not (is_cli m) -> rest, next
    | _ -> sorted, m)
;;

let providers (models : Llm.t list) =
  List.fold models ~init:[] ~f:(fun acc (model : Llm.t) ->
    if List.mem acc model.provider ~equal:String.equal
    then acc
    else model.provider :: acc)
  |> List.rev
;;

let tabs models =
  Tab.Favourites :: List.map (providers models) ~f:(fun p -> Tab.Provider p)
;;

let opening (models : Llm.t list) ~current =
  let tab =
    match
      List.find models ~f:(fun (l : Llm.t) ->
        Option.equal String.equal current (Some l.key))
    with
    | Some l -> Tab.Provider l.provider
    | None ->
      (match providers models with
       | p :: _ -> Provider p
       | [] -> Favourites)
  in
  { init with tab }
;;

let rows ?current t (models : Llm.t list) ~favourites =
  match t.tab with
  | Favourites ->
    ( List.filter_map favourites ~f:(fun key ->
        List.find models ~f:(fun (l : Llm.t) -> String.equal l.key key))
    , 0 )
  | Provider p ->
    let all =
      newest_first
        (List.filter models ~f:(fun (l : Llm.t) -> String.equal l.provider p))
    in
    if t.more
    then all, 0
    else (
      let first = List.take all shown in
      (* The session's model is in the list even when it is an old one. *)
      let first =
        match
          List.find all ~f:(fun (l : Llm.t) ->
            Option.equal String.equal current (Some l.key))
        with
        | Some l
          when not
                 (List.exists first ~f:(fun (f : Llm.t) ->
                    String.equal f.key l.key)) -> first @ [ l ]
        | _ -> first
      in
      first, Int.max 0 (List.length all - List.length first))
;;

let set_tab t tab =
  if Tab.equal t.tab tab then t else { t with tab; more = false }
;;

let step t models delta =
  let tabs = Array.of_list (tabs models) in
  let n = Array.length tabs in
  let i =
    Array.findi tabs ~f:(fun _ tab -> Tab.equal tab t.tab)
    |> Option.value_map ~default:0 ~f:fst
  in
  set_tab t tabs.((i + delta) % n)
;;

let makers = [ "claude"; "gpt"; "gemini"; "deepseek"; "grok" ]

let short_name name =
  let maker word =
    List.mem makers (String.lowercase word) ~equal:String.equal
  in
  match String.split name ~on:' ' with
  | first :: (second :: _ as rest) ->
    (match String.lsplit2 first ~on:'-' with
     | Some (family, suffix)
       when maker family
            && (not (String.is_empty suffix))
            && Char.is_digit suffix.[0] ->
       String.concat ~sep:" " (suffix :: rest)
     | _ ->
       (* Only a maker's name goes, and only before a word: "Claude Opus
          4.6" is "Opus 4.6", "Gemini 3.8 Flash" stays. *)
       if
         maker first
         && List.length rest >= 2
         && (not (String.is_empty second))
         && Char.is_alpha second.[0]
       then String.concat ~sep:" " rest
       else name)
  | _ -> name
;;
