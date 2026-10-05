open! Core

module Argument = struct
  type t =
    | Model
    | Thinking
    | Login
    | Logout
    | Directory
    | Verbosity
    | Confirm
    | Session
    | Path
    | Host
    | User
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
  [ c ~args:"[command]" "help" "show commands and keys, or a command's usage"
  ; c "hotkeys" "show the keyboard shortcuts"
  ; c "new" "start a new session"
  ; c ~argument:Model ~args:"[name]" "model" "pick or switch the model"
  ; c "scoped-models" "pick the models Ctrl+P and Alt+P cycle through"
  ; c
      ~argument:Thinking
      ~args:"[off|low|on|high|max]"
      "thinking"
      "pick or set the thinking level"
  ; c
      "change_default"
      "save the model and thinking level as the default for new sessions"
  ; c
      ~argument:Verbosity
      ~args:"[quiet|normal|verbose]"
      "verbosity"
      "how much of tool calls and thinking the transcript shows"
  ; c
      ~argument:Confirm
      ~args:"[on|off]"
      "confirm"
      "ask before bash, write and edit run"
  ; c
      ~args:"[instructions]"
      "compact"
      "summarise older messages to free context"
  ; c ~args:"[name]" "name" "rename the session"
  ; c "session" "show the session's details and statistics"
  ; c "sessions" "search the saved sessions"
  ; c ~argument:Session ~args:"[path]" "switch" "switch to a saved session"
  ; c "clone" "copy this session into a new one"
  ; c "fork" "start a new session from an earlier message"
  ; c "rewind" "go back to an earlier message in this session"
  ; c "tree" "show the session tree and move to any message in it"
  ; c ~argument:Directory ~args:"[path]" "cd" "change the working directory"
  ; c
      ~argument:Host
      ~args:"[name|backend]"
      "host"
      "pick where tools run, and the directory there"
  ; c
      ~argument:Path
      ~args:"[path]"
      "export"
      "export the transcript on the backend (markdown, or .jsonl)"
  ; c
      ~argument:Path
      ~args:"[path]"
      "import"
      "import a session from a JSONL file on the backend"
  ; c "copy" "copy the last reply to the clipboard"
  ; c
      ~args:"<question>"
      "btw"
      "ask a side question without interrupting the run (not added to the \
       conversation)"
  ; c "abort" "stop the current run"
  ; c
      ~args:"[n|id|cancel <n|id>]"
      "agents"
      "follow subagents and background jobs in the agents panel, or cancel one"
  ; c
      ~args:"[id|kill <id>]"
      "jobs"
      "background jobs in the agents panel: list them, show or kill one"
  ; c
      ~argument:Login
      ~args:"[provider]"
      "login"
      "log in to a model provider (or /login custom)"
  ; c ~argument:Logout ~args:"[provider]" "logout" "remove a provider's login"
  ; c "auth" "show which providers are logged in"
  ; c
      ~argument:User
      ~args:"[user]"
      "setusr"
      "act as another user (superusers); without a user, pick one"
  ; c "signout" "sign out of this account"
  ; c "retry-backend-connection" "reconnect to the backend now"
  ; c "state" "show the session state as the backend reports it"
  ; c "clear" "clear the transcript view (the conversation is kept)"
  ; c "quit" "how to leave (close the tab; /signout signs out)"
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
