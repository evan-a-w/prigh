open! Core
open! Import

type t =
  { draft : string option
  ; error : string option
  }
[@@deriving sexp_of, equal]

let empty = { draft = None; error = None }
let draft t = t.draft
let error t = t.error
let input _ text = { draft = Some text; error = None }

let commit t =
  match Option.map t.draft ~f:String.strip with
  | None | Some "" -> empty, None
  | Some text ->
    (match Prigh_ui.Compaction_command.parse ("at " ^ text) with
     | Set_at n -> empty, Some n
     | Show | Set_model _ | Set_mask _ | Error _ ->
       ( { t with
           error =
             Some
               (sprintf
                  "“%s” isn't a number of tokens: type one like 150k or \
                   200000, or off to compact only at 80%% of the window."
                  text)
         }
       , None ))
;;
