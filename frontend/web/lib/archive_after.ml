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
    (match Prigh_ui.Auto_archive_command.parse text with
     | Set days -> empty, Some days
     | Show | Error _ ->
       ( { t with
           error =
             Some
               (sprintf
                  "“%s” isn't a number of days from 0 to %g: type one like 7, \
                   or off to archive only by hand."
                  text
                  Prigh_ui.Auto_archive_command.max_days)
         }
       , None ))
;;
