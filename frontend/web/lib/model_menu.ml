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
  }
[@@deriving sexp_of, equal]

let init = { tab = Favourites; more = false; access_open = false }
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
  List.stable_sort models ~compare:(fun (a : Llm.t) (b : Llm.t) ->
    match version a.name, version b.name with
    | _ when is_cli a && is_cli b -> 0
    | Some a, Some b -> List.compare Int.compare b a
    | Some _, None -> -1
    | None, Some _ -> 1
    | None, None -> 0)
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

let rows t (models : Llm.t list) ~favourites =
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
    else List.take all shown, Int.max 0 (List.length all - shown)
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

let short_name name =
  match String.split name ~on:' ' with
  | first :: (_ :: _ as rest) ->
    (match String.lsplit2 first ~on:'-' with
     | Some (_, suffix)
       when (not (String.is_empty suffix)) && Char.is_digit suffix.[0] ->
       String.concat ~sep:" " (suffix :: rest)
     | _ ->
       (match rest with
        | _ :: _ :: _ -> String.concat ~sep:" " rest
        | _ -> name))
  | _ -> name
;;
