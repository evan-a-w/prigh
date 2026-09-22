open! Core

module Argument = struct
  type t =
    | Model
    | Thinking
    | Verbosity
    | Confirm
    | Login
    | Logout
    | Sessions
    | Path
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

let c ?argument name args help = { Spec.name; args; help; argument }

(* In cycling order: "on" is the provider's default budget. *)
let thinking_levels = [ "off"; "low"; "on"; "high"; "max" ]

let all =
  [ c "help" "" "show commands and keys"
  ; c "hotkeys" "" "show keyboard shortcuts"
  ; c
      ~argument:Argument.Model
      "model"
      "[name|id|provider/id]"
      "pick or switch the model"
  ; c "scoped-models" "" "pick the models Ctrl+P cycles through"
  ; c
      ~argument:Argument.Login
      "login"
      "[provider] [api_key|oauth]"
      "log in to a provider"
  ; c
      ~argument:Argument.Logout
      "logout"
      "[provider]"
      "remove a provider's stored credential"
  ; c
      ~argument:Argument.Thinking
      "thinking"
      "[off|low|on|high|max]"
      "pick or set the thinking level"
  ; c
      ~argument:Argument.Verbosity
      "verbosity"
      "[quiet|normal|verbose]"
      "set the transcript verbosity"
  ; c
      ~argument:Argument.Confirm
      "confirm"
      "[on|off]"
      "ask before destructive tools"
  ; c "auth" "" "show which providers are configured"
  ; c "compact" "" "summarise older messages to free context"
  ; c "new" "" "start a new session"
  ; c "name" "[text]" "set the session name"
  ; c "session" "" "show session statistics"
  ; c "sessions" "" "pick a saved session (Ctrl+N named, Ctrl+D delete)"
  ; c "agents" "" "focus a subagent"
  ; c
      "host"
      "[name|backend]"
      "pick where tools run (this frontend, another one, or the backend) and \
       the directory there"
  ; c ~argument:Argument.Sessions "switch" "[path]" "switch to a saved session"
  ; c ~argument:Argument.Path "cd" "[path]" "change the working directory"
  ; c "fork" "" "fork at a previous user message"
  ; c "rewind" "" "rewind the head to a previous user message"
  ; c "tree" "" "show the session tree and switch head"
  ; c "clone" "" "clone the current session"
  ; c
      ~argument:Argument.Path
      "export"
      "[path]"
      "export the transcript (markdown or .jsonl)"
  ; c
      ~argument:Argument.Path
      "import"
      "[path]"
      "import a session from a JSONL file"
  ; c "abort" "" "abort the current run"
  ; c
      "retry-backend-connection"
      ""
      "reconnect to the backend now instead of waiting for the next retry"
  ; c "state" "" "show session state"
  ; c "clear" "" "clear the transcript"
  ; c "quit" "" "exit"
  ]
;;

let find name = List.find all ~f:(fun s -> String.equal s.name name)

module Parsed = struct
  type t =
    { name : string
    ; args : string list
    ; rest : string
    }
  [@@deriving sexp_of]
end

let parse input =
  let trimmed = String.strip input in
  match String.chop_prefix trimmed ~prefix:"/" with
  | None -> None
  | Some body ->
    (match
       String.split body ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
     with
     | [] -> Some { Parsed.name = ""; args = []; rest = "" }
     | name :: args ->
       let rest = String.strip (String.drop_prefix body (String.length name)) in
       Some { name; args; rest })
;;

let closest name =
  List.filter_map all ~f:(fun s ->
    let d = Edit_distance.distance s.name name in
    Option.some_if (d <= 2 || String.is_prefix s.name ~prefix:name) (d, s))
  |> List.min_elt ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> Option.map ~f:snd
;;

let help : Content.t =
  let rows =
    List.map all ~f:(fun s ->
      String.strip ("/" ^ s.name ^ " " ^ s.args), s.help)
  in
  let width =
    List.fold rows ~init:0 ~f:(fun acc (k, _) -> Int.max acc (String.length k))
  in
  List.map rows ~f:(fun (k, help) ->
    [ { Content.Span.text = Text_width.pad_right k ~width
      ; style = Style.bold Style.plain
      }
    ; { text = "  " ^ help; style = Style.plain }
    ])
;;
