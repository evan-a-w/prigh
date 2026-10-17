open! Core

module Argument = struct
  type t =
    | Model
    | Models
    | Thinking
    | Verbosity
    | Confirm
    | Mode
    | Plan
    | Notify
    | Login
    | Logout
    | Sessions
    | Session_list (** several, completed word by word *)
    | Path
    | Directory
    | Default_directory
    | Skill
    | Mcp
    | Budget
    | Goal
    | Resume
    | Snippet
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
      "change_default"
      ""
      "save the current model and thinking level as the default for new \
       sessions"
  ; c
      ~argument:Argument.Models
      "fallback"
      "[off|model...]"
      "show or set the models that take over, in order, when a model's usage \
       runs out"
  ; c
      ~argument:Argument.Default_directory
      "default-dir"
      "[off|path]"
      "show or set the directory new sessions start in"
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
      "ask before destructive tools (approval mode ask or auto, also for new \
       sessions)"
  ; c
      ~argument:Argument.Mode
      "mode"
      "[ask|auto-edit|auto|plan]"
      "pick or set the mode: what asks first, or read-only planning (Alt+M \
       cycles)"
  ; c
      ~argument:Argument.Plan
      "plan"
      "[on|off]"
      "toggle plan mode: the model investigates and plans, changing no files, \
       until you approve its plan"
  ; c "todos" "" "show the model's plan (its update_plan steps)"
  ; c
      "permissions"
      ""
      "show the mode and the allow/ask/deny rules (config and this session)"
  ; c
      ~argument:Argument.Notify
      "notify"
      "[off|bell|desktop]"
      "how to tell you that a turn ended or something waits for you while the \
       terminal is not focused"
  ; c "auth" "" "show which providers are configured"
  ; c
      ~argument:Argument.Budget
      "budget"
      "[dollars|off]"
      "show or set the session's cost budget: no turn starts once it is spent"
  ; c
      ~argument:Argument.Goal
      "goal"
      "[text|off|resume]"
      "set a goal the agent keeps working toward until it is met; show, clear \
       or resume it"
  ; c
      ~argument:Argument.Resume
      "resume"
      "[now|cancel]"
      "show, run now or cancel the automatic resume after a usage limit"
  ; c
      "compact"
      "[instructions]"
      "summarise older messages to free context"
  ; c
      ~argument:Argument.Path
      "diff"
      "[path]"
      "show the uncommitted changes where the tools run (git diff, untracked \
       files too)"
  ; c
      "review"
      "[base] [instructions]"
      "review the uncommitted changes, or those against a base branch, for \
       prioritised findings"
  ; c "init" "" "write or improve the project's AGENTS.md"
  ; c "new" "" "start a new session"
  ; c "name" "[text]" "set the session name"
  ; c "session" "" "show session statistics"
  ; c
      "sessions"
      ""
      "pick a saved session (Tab marks, Ctrl+D deletes, Alt+A archives, Alt+D \
       deletes all archived, Ctrl+N named)"
  ; c
      ~argument:Argument.Sessions
      "archive"
      "[session]"
      "archive a session (the current one without an argument)"
  ; c
      ~argument:Argument.Sessions
      "unarchive"
      "[session]"
      "unarchive a session (the current one without an argument)"
  ; c
      ~argument:Argument.Session_list
      "delete"
      "[session...]"
      "delete sessions, after a confirmation (none: pick them in /sessions)"
  ; c "delete-archived" "" "delete every archived session, after a confirmation"
  ; c
      "agents"
      "[cancel <n>]"
      "focus a subagent (Ctrl+D cancels one), or cancel subagent n"
  ; c
      "jobs"
      "[id|kill <id>]"
      "list background jobs (Enter shows output, Ctrl+D kills), or show/kill \
       one"
  ; c
      "queue"
      ""
      "the queued steers and follow-ups (Enter edits one, Ctrl+D removes it)"
  ; c
      "host"
      "[name|backend]"
      "pick where tools run (this frontend, another one, or the backend) and \
       the directory there"
  ; c ~argument:Argument.Sessions "switch" "[path]" "switch to a saved session"
  ; c
      ~argument:Argument.Directory
      "cd"
      "[path]"
      "change the working directory (asks to create a missing one)"
  ; c
      ~argument:Argument.Directory
      "mkdir"
      "[path]"
      "create a directory (and its parents) and change to it"
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
      "btw"
      "<question>"
      "ask a side question without interrupting the turn (not added to the \
       conversation)"
  ; c "skills" "" "pick a skill to run (Enter puts /skill:NAME in the editor)"
  ; c
      ~argument:Argument.Skill
      "skill:"
      "NAME [args]"
      "run a skill, with what follows as its arguments"
  ; c
      ~argument:Argument.Snippet
      "snippet"
      "[NAME|add|rm]"
      "insert a saved text at the cursor (none: pick one, Alt+N); add NAME \
       [TEXT] saves one (no TEXT: your last prompt), rm NAME deletes it"
  ; c
      ~argument:Argument.Mcp
      "mcp"
      "[reconnect]"
      "list MCP servers (Enter approves one or lists its tools); reconnect \
       restarts failed ones"
  ; c
      "retry-backend-connection"
      ""
      "reconnect to the backend now instead of waiting for the next retry"
  ; c "state" "" "show session state"
  ; c "clear" "" "clear the transcript"
  ; c "signout" "" "sign out to log in as another user (browser only)"
  ; c
      "setusr"
      "[user]"
      "act as another user (superusers only); without a user, list them"
  ; c "quit" "" "exit"
  ]
;;

let find name = List.find all ~f:(fun s -> String.equal s.name name)

let usage (s : Spec.t) =
  let sep = if String.is_suffix s.name ~suffix:":" then "" else " " in
  String.strip ("/" ^ s.name ^ sep ^ s.args)
;;

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

let help = Help_table.render (List.map all ~f:(fun s -> usage s, s.help))

let input_help =
  Help_table.render
    [ "!command", "run a shell command; its output joins the context"
    ; "!!command", "run a shell command without adding it to the context"
    ; "!&command", "run a shell command as a background job (/jobs)"
    ; "@path", "attach a file (Ctrl+R completes paths)"
    ; "#name", "insert a snippet: Tab or Enter on its completion (/snippet)"
    ]
;;
