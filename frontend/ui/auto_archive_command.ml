open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Set of float
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /auto-archive or /auto-archive DAYS|off"
let max_days = 365.

let parse rest =
  let word = String.lowercase (String.strip rest) in
  let number =
    List.find_map [ " days"; "days"; " day"; "day"; "d" ] ~f:(fun suffix ->
      String.chop_suffix word ~suffix)
    |> Option.value ~default:word
    |> String.strip
  in
  match word with
  | "" -> Action.Show
  | "off" | "never" -> Set 0.
  | _ ->
    (match Float.of_string_opt number with
     | Some n when Float.is_finite n && Float.(n >= 0.) && Float.(n <= max_days)
       -> Set n
     | Some _ | None ->
       Error
         (sprintf
            "%S is not a number of days from 0 to %g (off: never archive by \
             age); %s"
            (String.strip rest)
            max_days
            usage))
;;

let fields : Action.t -> _ = function
  | Set n -> [ "archive_after_days", `Number (sprintf "%g" n) ]
  | Show | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set archive_after_days -> { config with archive_after_days }
  | Show | Error _ -> config
;;

let days n = sprintf "%g day%s" n (if Float.equal n 1. then "" else "s")

let summary (config : P.Config.t) =
  if Float.(config.archive_after_days <= 0.)
  then
    "sessions are never archived by age, only by hand (/auto-archive DAYS \
     archives those inactive that long)"
  else
    sprintf
      "sessions are archived after %s without activity (/auto-archive DAYS|off)"
      (days config.archive_after_days)
;;
