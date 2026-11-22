open! Core
open! Import

module Action = struct
  type t =
    | Show
    | Now
    | Cancel
    | Set of bool
    | Error of string
  [@@deriving sexp_of, equal]
end

let usage = "usage: /resume [now|cancel|on|off]"

let parse rest =
  match String.lowercase (String.strip rest) with
  | "" -> Action.Show
  | "now" -> Now
  | "cancel" -> Cancel
  | "on" -> Set true
  | "off" -> Set false
  | _ ->
    Error
      (sprintf
         "unknown /resume argument %S: now retries at once, cancel stays \
          stopped, on|off turns the automatic resume after a usage limit on or \
          off; %s"
         (String.strip rest)
         usage)
;;

let fields : Action.t -> _ = function
  | Set on -> [ ("resume_on_limit_reset", if on then `True else `False) ]
  | Show | Now | Cancel | Error _ -> []
;;

let apply (action : Action.t) (config : P.Config.t) =
  match action with
  | Set resume_on_limit_reset -> { config with resume_on_limit_reset }
  | Show | Now | Cancel | Error _ -> config
;;

let summary (config : P.Config.t) =
  if config.resume_on_limit_reset
  then
    "runs stopped by a usage limit resume by themselves when it resets \
     (/resume off keeps them stopped)"
  else
    "runs stopped by a usage limit stay stopped until you send a message \
     (/resume on resumes them when the limit resets)"
;;
