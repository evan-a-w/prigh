open! Core

module Argument = struct
  type t =
    | Model
    | Models
    | Thinking
    | Login
    | Logout
    | Directory
    | Backend_directory
    | Verbosity
    | Theme
    | Accent
    | Confirm
    | Mode
    | Plan
    | Notify
    | Session
    | Path
    | Host
    | User
    | Skill
    | Budget
    | Goal
    | Resume
    | Snippet
    | Remind
    | Pr
    | Worktree
    | Review
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

  let usage t =
    if String.is_suffix t.name ~suffix:":"
    then "/" ^ t.name ^ t.args
    else String.strip ("/" ^ t.name ^ " " ^ t.args)
  ;;
end

let c ?argument ?(args = "") name help = { Spec.name; args; help; argument }

let all =
  [ c ~args:"[command]" "help" "show commands and keys, or a command's usage"
  ; c "hotkeys" "show the keyboard shortcuts"
  ; c
      "settings"
      "theme and accent, notifications, transcript detail, tool confirmation, \
       your account and providers (Ctrl+,)"
  ; c "new" "start a new session"
  ; c ~argument:Model ~args:"[name]" "model" "pick or switch the model"
  ; c "scoped-models" "pick the models Ctrl+P and Alt+P cycle through"
  ; c
      ~argument:Models
      ~args:"[model...|off]"
      "fallback"
      "show or set the models that take over, in order, when one's usage runs \
       out"
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
      ~argument:Theme
      ~args:"[original|dark|light]"
      "theme"
      "pick or set the theme, for you in every browser"
  ; c
      ~argument:Accent
      ~args:"[name|#rrggbb]"
      "accent"
      "pick or set the accent colour: orange, blue, green, purple, pink, teal, \
       yellow or any #rrggbb"
  ; c
      ~argument:Confirm
      ~args:"[on|off]"
      "confirm"
      "ask before bash, job_input, edits and MCP tools run (mode ask or auto, \
       also for new sessions)"
  ; c
      ~argument:Mode
      ~args:"[ask|auto-edit|auto|plan]"
      "mode"
      "pick or set the mode: what asks first, or read-only planning (Alt+M \
       cycles)"
  ; c
      ~argument:Plan
      ~args:"[on|off]"
      "plan"
      "toggle plan mode: the model investigates and plans, changing no files, \
       until you approve its plan"
  ; c "todos" "show the model's plan (its update_plan steps)"
  ; c
      "permissions"
      "show the mode and the allow/ask/deny rules (config and this session)"
  ; c
      ~argument:Notify
      ~args:"[on|off]"
      "notify"
      "tell you when a turn ends or something waits for you while the tab is \
       hidden"
  ; c
      ~args:"[instructions]"
      "compact"
      "summarise older messages to free context"
  ; c
      ~argument:Path
      ~args:"[path]"
      "diff"
      "show the uncommitted changes where the tools run, untracked files too"
  ; c
      ~argument:Review
      ~args:"[worktree] [--base REV]"
      "review"
      "the review page, the changes file by file to approve, comment on and \
       edit (Alt+R): of the session's worktree or another (its branch, path or \
       name), against its base or REV; /review BASE [instructions] (HEAD: the \
       uncommitted changes) has the model review them for prioritised findings"
  ; c "init" "write or improve the project's AGENTS.md"
  ; c ~args:"[name]" "name" "rename the session"
  ; c "session" "show the session's details and statistics"
  ; c "sessions" "search the saved sessions"
  ; c ~argument:Session ~args:"[path]" "switch" "switch to a saved session"
  ; c
      ~argument:Session
      ~args:"[session]"
      "archive"
      "archive a session (this one without an argument): it moves under \
       Archived in the sidebar"
  ; c
      ~argument:Session
      ~args:"[session]"
      "unarchive"
      "unarchive a session (this one without an argument)"
  ; c
      ~argument:Session
      ~args:"[session...]"
      "delete"
      "delete sessions, after a confirmation (none: select them in the sidebar)"
  ; c "delete-archived" "delete every archived session, after a confirmation"
  ; c "clone" "copy this session into a new one"
  ; c "fork" "start a new session from an earlier message"
  ; c "rewind" "go back to an earlier message in this session"
  ; c "tree" "show the session tree and move to any message in it"
  ; c
      ~argument:Directory
      ~args:"[path]"
      "cd"
      "change the working directory (asks to create a missing one)"
  ; c
      ~argument:Directory
      ~args:"[path]"
      "mkdir"
      "create a directory (and its parents) and change to it"
  ; c
      ~argument:Worktree
      ~args:"[list|new|open|rm ...]"
      "worktree"
      "pick a git worktree of the repository to work in (Alt+Enter: in a new \
       session, Alt+R: review its changes, Delete removes it); new BRANCH \
       [BASE] creates one and works there; open [-n] TARGET (-n: in a new \
       session); rm [-f] TARGET (-f: with uncommitted changes)"
  ; c
      ~argument:Backend_directory
      ~args:"[path|off]"
      "default-dir"
      "show or set the directory new sessions start in"
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
  ; c
      ~argument:Snippet
      ~args:"[NAME|add|rm]"
      "snippet"
      "insert a saved text at the caret (none: pick one, Alt+N); add NAME \
       [TEXT] saves one (no TEXT: your last prompt), rm NAME deletes it"
  ; c "skills" "pick a skill to invoke (/skill:name)"
  ; c
      ~argument:Skill
      ~args:"<name> [args]"
      "skill:"
      "send a skill's instructions to the agent, with your arguments"
  ; c
      ~args:"[reconnect]"
      "mcp"
      "MCP servers: their tools, approve a project's, or restart failed ones"
  ; c
      ~argument:Budget
      ~args:"[dollars|off]"
      "budget"
      "show or set the session's cost budget: no turn starts once it is spent"
  ; c
      ~argument:Goal
      ~args:"[text|off|resume]"
      "goal"
      "set a goal the agent keeps working toward until it is met; show, clear \
       or resume it"
  ; c
      ~argument:Remind
      ~args:"[stop|every|rm|off ...]"
      "remind"
      "stop [N] TEXT: remind the agent when it ends its turn (at most N times \
       in a row, default 3); every N TEXT: every N turns; rm NAME, off: remove \
       one or all; none: list the session's (Delete removes one)"
  ; c
      ~argument:Resume
      ~args:"[now|cancel]"
      "resume"
      "show, run now or cancel the automatic resume after a usage limit"
  ; c "copy" "copy the last reply to the clipboard"
  ; c
      ~args:"<question>"
      "btw"
      "ask a side question without interrupting the run (not added to the \
       conversation)"
  ; c "abort" "stop the current run"
  ; c
      ~argument:Directory
      ~args:"[dir]"
      "terminal"
      "open a shell where the tools run, in the session's directory (Ctrl+`), \
       or a shell of its own in DIR (a worktree, say)"
  ; c
      ~argument:Directory
      ~args:"[path]"
      "files"
      "browse, preview, upload and download files where the tools run \
       (Ctrl+Shift+F)"
  ; c
      ~args:"[n|id|cancel <n|id>|steer <n|id> <text>|follow-up <n|id> <text>]"
      "agents"
      "follow subagents and background jobs in the agents panel, cancel one, \
       or send a running subagent a message (steer: at its next step; \
       follow-up: when it ends its turn)"
  ; c
      ~args:"[id|kill <id>]"
      "jobs"
      "background jobs in the agents panel: list them, show or kill one"
  ; c
      ~argument:Login
      ~args:"[provider|github [HOST]|forgejo HOST|HOST] [token]"
      "login"
      "log in to a model provider (or /login custom), or sign in to GitHub or \
       a Forgejo or Gitea host (token: paste one instead of OAuth)"
  ; c
      ~argument:Logout
      ~args:"[provider|HOST]"
      "logout"
      "remove a provider's login, or sign out of a git host (github: \
       github.com)"
  ; c "auth" "show which providers and git hosts are logged in"
  ; c
      ~argument:Pr
      ~args:"[list [open|closed|all]|view N|new [title]|checkout N]"
      "pr"
      "the repository's pull requests: list and filter them, open one with its \
       checks and diff, create one, or check one out in a worktree"
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
  | Some body
    when (not (String.mem body '\n'))
         || String.is_prefix body ~prefix:"snippet add "
         || String.is_prefix body ~prefix:"remind stop "
         || String.is_prefix body ~prefix:"remind every " ->
    let name, rest =
      match String.lsplit2 body ~on:' ' with
      | Some (name, rest) -> name, String.strip rest
      | None -> body, ""
    in
    if String.mem name '/' then None else Some { Parsed.name; rest }
  | _ -> None
;;
