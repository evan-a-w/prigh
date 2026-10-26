open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set_at of int
    | Set_model of string option
    | Set_mask of bool
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage =
  "usage: /compaction, /compaction at TOKENS|off, /compaction model MODEL|off \
   or /compaction mask on|off"
;;

(* [200000], [200k], [1.5m] *)
let parse_tokens word =
  let word =
    String.lowercase (String.filter word ~f:(fun c -> Char.( <> ) c '_'))
  in
  let number, scale =
    match String.chop_suffix word ~suffix:"k" with
    | Some n -> n, 1_000.
    | None ->
      (match String.chop_suffix word ~suffix:"m" with
       | Some n -> n, 1_000_000.
       | None -> word, 1.)
  in
  match Float.of_string_opt number with
  | Some n when Float.is_finite n && Float.(n >= 0.) ->
    Some (Float.iround_nearest_exn (n *. scale))
  | Some _ | None -> None
;;

let parse rest =
  match
    String.split (String.strip rest) ~on:' '
    |> List.filter ~f:(Fn.non String.is_empty)
  with
  | [] -> Action.Show
  | [ "at"; ("off" | "0") ] -> Set_at 0
  | [ "at"; word ] ->
    (match parse_tokens word with
     | Some n -> Set_at n
     | None ->
       Error
         (sprintf
            "%S is not a number of tokens (e.g. 150k or 200000; off: at 80%% \
             of the model's window only); %s"
            word
            usage))
  | [ "model"; "off" ] -> Set_model None
  | [ "model"; name ] -> Set_model (Some name)
  | [ "mask"; "on" ] -> Set_mask true
  | [ "mask"; "off" ] -> Set_mask false
  | _ -> Error usage
;;

let fields : Action.t -> _ = function
  | Set_at n -> [ "compact_at_tokens", `Number (Int.to_string n) ]
  | Set_model model ->
    [ ( "compaction_model"
      , Option.value_map model ~default:`Null ~f:(fun s -> `String s) )
    ]
  | Set_mask on -> [ ("mask_observations", if on then `True else `False) ]
  | Show | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set_at n -> { config with compact_at_tokens = n }
  | Set_model compaction_model -> { config with compaction_model }
  | Set_mask mask_observations -> { config with mask_observations }
  | Show | Error _ -> config
;;

let tokens n =
  let scaled divisor suffix =
    let x = Float.of_int n /. divisor in
    if Float.(x = round_down x)
    then sprintf "%d%s" (Float.to_int x) suffix
    else sprintf "%.1f%s" x suffix
  in
  if n >= 1_000_000 && n % 100_000 = 0
  then scaled 1_000_000. "M"
  else if n >= 1_000 && n % 100 = 0
  then scaled 1_000. "k"
  else Int.to_string n
;;

let summary (config : P.Config.t) =
  sprintf
    "compaction: %s; summaries by %s; observation masking %s (/compaction at \
     TOKENS|off, /compaction model MODEL|off, /compaction mask on|off)"
    (if config.compact_at_tokens > 0
     then
       sprintf
         "at %s tokens, or 80%% of the model's window if that is less"
         (tokens config.compact_at_tokens)
     else "at 80% of the model's window")
    (match config.compaction_model with
     | Some model -> model
     | None -> "the session's model")
    (if config.mask_observations then "on" else "off")
;;

let subcommands =
  [ "at", "compact once the context exceeds this many tokens (e.g. 150k)"
  ; "model", "the model that writes the summaries"
  ; "mask", "replace old tool outputs with placeholders first"
  ]
;;

let complete ~models rest =
  match String.lfindi rest ~f:(fun _ c -> Char.is_whitespace c) with
  | None -> Some (0, subcommands)
  | Some i ->
    let word = String.prefix rest i in
    let arg = String.lstrip (String.drop_prefix rest i) in
    let offset = String.length rest - String.length arg in
    if String.exists arg ~f:Char.is_whitespace
    then None
    else (
      match word with
      | "at" -> Some (offset, [ "off", "only at 80% of the model's window" ])
      | "mask" ->
        Some
          ( offset
          , [ "on", "mask old tool outputs before summarising"
            ; "off", "only summarise"
            ] )
      | "model" ->
        Some
          ( offset
          , ("off", "the session's model writes them")
            :: List.map models ~f:(fun (m : P.Model.t) -> m.key, m.name) )
      | _ -> None)
;;
