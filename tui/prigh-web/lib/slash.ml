open! Core

module Argument = struct
  type t =
    | Model
    | Thinking
    | Login
    | Logout
    | Directory
  [@@deriving sexp_of, equal]
end

module Spec = struct
  type t =
    { name : string
    ; args : string
    ; help : string
    ; argument : Argument.t option
    }
  [@@deriving sexp_of, equal]
end

let c ?argument ?(args = "") name help = { Spec.name; args; help; argument }

let all =
  [ c "help" "show commands and keys"
  ; c "new" "start a new session"
  ; c ~argument:Model ~args:"[name]" "model" "pick or switch the model"
  ; c
      ~argument:Thinking
      ~args:"[off|low|on|high|max]"
      "thinking"
      "pick or set the thinking level"
  ; c "compact" "summarise older messages to free context"
  ; c ~args:"[name]" "name" "rename the session"
  ; c "sessions" "search the saved sessions"
  ; c "clone" "copy this session into a new one"
  ; c ~argument:Directory ~args:"<path>" "cd" "change the working directory"
  ; c "abort" "stop the current run"
  ; c "agents" "show background subagents and jobs"
  ; c
      ~argument:Login
      ~args:"[provider]"
      "login"
      "log in to a model provider (or /login custom)"
  ; c ~argument:Logout ~args:"[provider]" "logout" "remove a provider's login"
  ; c "auth" "show which providers are logged in"
  ; c "signout" "sign out of this backend"
  ]
;;

let find name = List.find all ~f:(fun s -> String.equal s.name name)

let closest name =
  if String.is_empty name
  then None
  else
    List.filter_map all ~f:(fun s ->
      let d = Prigh_ui.Edit_distance.distance s.name name in
      Option.some_if (d <= 2 || String.is_prefix s.name ~prefix:name) (d, s))
    |> List.min_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b)
    |> Option.map ~f:snd
;;

module Parsed = struct
  type t =
    { name : string
    ; rest : string
    }
  [@@deriving sexp_of, equal]
end

let parse text =
  let text = String.strip text in
  match String.chop_prefix text ~prefix:"/" with
  | Some body when not (String.mem body '\n') ->
    let name, rest =
      match String.lsplit2 body ~on:' ' with
      | Some (name, rest) -> name, String.strip rest
      | None -> body, ""
    in
    if String.mem name '/' then None else Some { Parsed.name; rest }
  | _ -> None
;;
