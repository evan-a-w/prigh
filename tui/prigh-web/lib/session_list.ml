open! Core
open! Import

let title (s : Session_summary.t) =
  match s.name with
  | Some name when not (String.is_empty (String.strip name)) -> name
  | _ -> Session_summary.blurb s
;;

let filter sessions ~query =
  Prigh_ui.Fuzzy.rank ~query sessions ~key:(fun (s : Session_summary.t) ->
    String.concat
      ~sep:" "
      [ title s; Option.value s.first_prompt ~default:""; s.cwd ])
;;
