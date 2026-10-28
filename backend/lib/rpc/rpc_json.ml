open! Core
open! Import

let str s = `String s
let int i = `Number (Int.to_string i)
let float f = `Number (sprintf "%.15g" f)
let bool b = if b then `True else `False

let usage (u : Usage.t) =
  `Object
    [ "input", int u.input
    ; "output", int u.output
    ; "cache_read", int u.cache_read
    ]
;;

let stop_reason (r : Stop_reason.t) =
  match r with
  | End_turn -> `Object [ "type", str "end_turn" ]
  | Tool_use -> `Object [ "type", str "tool_use" ]
  | Length -> `Object [ "type", str "length" ]
  | Aborted -> `Object [ "type", str "aborted" ]
  | Error message -> `Object [ "type", str "error"; "message", str message ]
;;

let tool_call (c : Content.Tool_call.t) =
  `Object [ "id", str c.id; "name", str c.name; "arguments", str c.arguments ]
;;

let content (c : Content.t) =
  match c with
  | Text text -> `Object [ "type", str "text"; "text", str text ]
  | Thinking th -> `Object [ "type", str "thinking"; "text", str th.text ]
  | Tool_call call ->
    `Object
      [ "type", str "tool_call"
      ; "id", str call.id
      ; "name", str call.name
      ; "arguments", str call.arguments
      ]
  | Server_tool tool ->
    `Object
      ([ "type", str "server_tool"
       ; "id", str tool.id
       ; "name", str tool.name
       ; "input", str tool.input
       ]
       @ Option.value_map tool.output ~default:[] ~f:(fun output ->
         [ "output", str output ])
       @ [ "is_error", bool tool.is_error ])
;;

(* Only when there are any. *)
let images_field = function
  | [] -> []
  | images -> [ "images", [%jsonaf_of: Image.t list] images ]
;;

let tool_result (r : Message.Tool_result.t) =
  `Object
    ([ "role", str "tool_result"
     ; "tool_call_id", str r.tool_call_id
     ; "tool_name", str r.tool_name
     ; "text", str r.text
     ; "is_error", bool r.is_error
     ]
     @ images_field r.images)
;;

let tool_result_value (r : Tool_result.t) =
  `Object [ "text", str r.text; "is_error", bool r.is_error ]
;;

let assistant (a : Message.Assistant.t) =
  `Object
    [ "role", str "assistant"
    ; "content", `Array (List.map a.content ~f:content)
    ; "stop_reason", stop_reason a.stop_reason
    ; "usage", usage a.usage
    ; "model", str a.model
    ]
;;

let message (m : Message.t) =
  match m with
  | User u ->
    `Object
      ([ "role", str "user"; "text", str u.text ]
       @ images_field u.images
       @
       match Injected.of_message ~origin:u.origin u.text with
       | None -> []
       | Some origin -> [ "origin", Injected.to_json origin ~text:u.text ])
  | Assistant a -> assistant a
  | Tool_result r -> tool_result r
;;

let ms seconds = int (Float.iround_nearest_exn (seconds *. 1000.))

let message_at m ~(at : float option) =
  match message m, at with
  | `Object fields, Some at -> `Object (fields @ [ "at", ms at ])
  | json, _ -> json
;;

let timed_message ({ message; at } : Timed_message.t) = message_at message ~at

let delta (d : Assistant_event.t) =
  match d with
  | Text_delta text -> `Object [ "type", str "text_delta"; "text", str text ]
  | Thinking_delta text ->
    `Object [ "type", str "thinking_delta"; "text", str text ]
  | Thinking_signature _ -> `Object [ "type", str "thinking_signature" ]
  | Tool_call_start { index; id; name } ->
    `Object
      [ "type", str "tool_call_start"
      ; "index", int index
      ; "id", str id
      ; "name", str name
      ]
  | Tool_call_delta { index; arguments } ->
    `Object
      [ "type", str "tool_call_delta"
      ; "index", int index
      ; "arguments", str arguments
      ]
  | Server_tool_use { id; name; input } ->
    `Object
      [ "type", str "server_tool_use"
      ; "id", str id
      ; "name", str name
      ; "input", str input
      ]
  | Server_tool_result { id; output; is_error; raw = _ } ->
    `Object
      [ "type", str "server_tool_result"
      ; "id", str id
      ; "output", str output
      ; "is_error", bool is_error
      ]
;;

let thinking t = str (Thinking.to_string t)
let thinking_of_string = Thinking.of_string

let model (m : Model.t) =
  `Object
    [ "id", str m.id
    ; "provider", str (Provider_id.to_string m.provider)
    ; "key", str (Model.key m)
    ; "name", str m.name
    ; "context_window", int m.context_window
    ; "max_output", int m.max_output
    ; "supports_thinking", bool m.supports_thinking
    ; ( "cost"
      , `Object
          [ "input", float m.cost.input
          ; "output", float m.cost.output
          ; "cache_read", float m.cost.cache_read
          ] )
    ]
;;

let review_comment (c : Review_comment.t) =
  let post (p : Review_comment.Post.t) =
    `Object
      ([ ( "author"
         , str
             (match p.author with
              | User -> "user"
              | Agent -> "agent") )
       ; "text", str p.text
       ; "at_ms", ms p.at
       ; "delivered", bool p.delivered
       ]
       @
       match p.proposal with
       | None -> []
       | Some { edits; state } ->
         [ ( "proposal"
           , `Object
               ([ ( "edits"
                  , `Array
                      (List.map edits ~f:(fun (e : Review_comment.Edit.t) ->
                         `Object
                           [ "old_text", str e.old_text
                           ; "new_text", str e.new_text
                           ])) )
                ; "state", str (Review_comment.Proposal.State.to_string state)
                ]
                @
                match state with
                | Failed e -> [ "error", str e ]
                | Pending | Accepted | Rejected -> []) )
         ])
  in
  `Object
    ([ "id", str c.id
     ; "root", str c.root
     ; "path", str c.path
     ; "first", int c.range.first
     ; "last", int c.range.last
     ; "side", str (Review_comment.Side.to_string c.range.side)
     ]
     @ (match c.range.columns with
        | None -> []
        | Some { start_col; end_col } ->
          [ "start_col", int start_col; "end_col", int end_col ])
     @ [ "anchor", str c.anchor
       ; "status", str (Review_comment.Status.to_string c.status)
       ; "thread", `Array (List.map c.thread ~f:post)
       ])
;;

let snapshot_content (c : Review_snapshot.Content.t) =
  `Object
    (match c with
     | Text { md5 } -> [ "kind", str "text"; "md5", str md5 ]
     | Absent -> [ "kind", str "absent" ]
     | Binary { md5 } -> [ "kind", str "binary"; "md5", str md5 ]
     | Too_large { bytes } -> [ "kind", str "too_large"; "bytes", int bytes ])
;;

let review_snapshot (s : Review_snapshot.t) =
  `Object
    ([ "root", str s.root
     ; "path", str s.path
     ; "action", str (Review_snapshot.Action.to_string s.action)
     ]
     @ Option.value_map s.by ~default:[] ~f:(fun by -> [ "by", str by ])
     @ [ "at_ms", ms s.at; "content", snapshot_content s.content ])
;;

let review (r : Review_state.t) =
  `Object
    [ "comments", `Array (List.map r.comments ~f:review_comment)
    ; ( "approvals"
      , `Array
          (List.map
             (Map.to_alist r.approvals)
             ~f:(fun ({ Review_state.File_key.root; path }, hash) ->
               `Object
                 (Option.value_map root ~default:[] ~f:(fun r ->
                    [ "root", str r ])
                  @ [ "path", str path; "hash", str hash ]))) )
    ; ( "snapshots"
      , `Array
          (Map.data r.snapshots
           |> List.concat_map ~f:List.rev
           |> List.map ~f:review_snapshot) )
    ]
;;

let review_since (s : Review_state.Since.t) =
  `Object
    ([ "path", str s.path
     ; "index", int s.index
     ; "snapshot", review_snapshot s.snapshot
     ; "current", snapshot_content s.current
     ; "changed", bool s.changed
     ; "added", int s.diff.added
     ; "removed", int s.diff.removed
     ; "diff", str s.diff.diff
     ]
     @ Option.value_map s.note ~default:[] ~f:(fun n -> [ "note", str n ]))
;;

let config (c : Config.t) = Config.to_json c

let reminder ~stops (r : Reminder.t) =
  `Object
    ([ "name", str r.name; "text", str r.text ]
     @
     match r.trigger with
     | On_stop { max_repeats } ->
       [ "on", str "stop"
       ; "max_repeats", int max_repeats
       ; "repeats", int (Int.min stops max_repeats)
       ]
     | Every_turns n -> [ "every_turns", int n ])
;;

let state (s : Agent.State.t) =
  `Object
    [ "session_id", str s.session_id
    ; "session_path", str s.session_path
    ; "session_name", Option.value_map s.session_name ~default:`Null ~f:str
    ; ( "session_description"
      , Option.value_map s.session_description ~default:`Null ~f:str )
    ; "cwd", str s.cwd
    ; "git_branch", Option.value_map s.git_branch ~default:`Null ~f:str
    ; "model", model s.model
    ; "thinking", thinking s.thinking
    ; "running", bool s.running
    ; "message_count", int s.message_count
    ; "usage", usage s.usage
    ; "cost_usd", float s.cost_usd
    ; "context_tokens", int s.context_tokens
    ; "active_host", str s.active_host
    ; ( "hosts"
      , `Array
          (List.map s.hosts ~f:(fun (h : Agent.Host.t) ->
             let opt = Option.value_map ~default:`Null ~f:str in
             `Object
               ([ "id", str h.id
                ; "name", str h.name
                ; "cwd", str h.cwd
                ; "session_id", opt h.session_id
                ; "session_name", opt h.session_name
                ]
                @ Option.value_map h.home ~default:[] ~f:(fun home ->
                  [ "home", str home ])))) )
    ; ( "subagents"
      , `Array
          (List.map s.subagents ~f:(fun (a : Background_tasks.Summary.t) ->
             `Object
               [ "id", str a.id
               ; "task", str a.label
               ; "running", bool a.running
               ])) )
    ; ( "jobs"
      , `Array
          (List.map s.jobs ~f:(fun (j : Background_tasks.Summary.t) ->
             `Object
               [ "id", str j.id
               ; "command", str j.label
               ; "running", bool j.running
               ; "exit", Option.value_map j.status ~default:`Null ~f:str
               ])) )
    ; "budget_usd", Option.value_map s.budget_usd ~default:`Null ~f:float
    ; ( "goal"
      , Option.value_map s.goal ~default:`Null ~f:(fun (g : Goal.t) ->
          `Object
            ([ "text", str g.text
             ; "status", str (Goal.Status.to_string g.status)
             ; "continuations", int g.continuations
             ; "max_continuations", int s.goal_max_continuations
             ]
             @
             match g.status with
             | Active -> []
             | Done { summary } -> [ "summary", str summary ]
             | Stopped { reason } -> [ "reason", str reason ])) )
    ; ( "reminders"
      , `Array (List.map s.reminders ~f:(reminder ~stops:s.reminder_stops)) )
    ; ( "resume_at_ms"
      , Option.value_map s.resume_at ~default:`Null ~f:(fun at -> ms at) )
    ; "approval_mode", str (Approval_mode.to_string s.approval_mode)
    ; "plan_mode", bool s.plan_mode
    ; "plan", Option.value_map s.plan ~default:`Null ~f:Plan.to_json
    ; "session_rules", `Array (List.map s.session_rules ~f:str)
    ]
;;

let job ~now task =
  let module Task = Background_tasks.Task in
  let output = Task.output task in
  `Object
    [ "id", str (Task.id task)
    ; "command", str (Task.label task)
    ; "running", bool (Task.running task)
    ; ( "exit"
      , Option.value_map (Task.outcome task) ~default:`Null ~f:(fun o ->
          str o.status) )
    ; "delivered", bool (Task.delivered task)
    ; ( "elapsed"
      , float
          (Option.value (Task.finished_at task) ~default:now
           -. Task.started_at task) )
    ; "bytes", int (Output_tail.total_bytes output)
    ; ( "last_line"
      , Option.value_map (Output_tail.last_line output) ~default:`Null ~f:str )
    ]
;;

let agent_worktree (w : Agent_worktree.t) =
  `Object
    [ "agent", str w.agent
    ; "task", str w.task
    ; "path", str w.path
    ; "branch", str w.branch
    ; "root", str w.root
    ]
;;

let session_worktree (w : Session_worktree.t) =
  `Object
    ([ "path", str w.path
     ; "branch", Option.value_map w.branch ~default:`Null ~f:str
     ; "root", str w.root
     ]
     @
     match w.source with
     | Created -> [ "source", str "created" ]
     | Entered -> [ "source", str "entered" ]
     | Pull_request { number } ->
       [ "source", str "pull_request"; "pr", int number ]
     | Agent { agent; task } ->
       [ "source", str "agent"; "agent", str agent; "task", str task ])
;;

let repo (r : Git_location.t) =
  `Object
    [ "root", str r.root
    ; "worktree", str r.worktree
    ; "branch", Option.value_map r.branch ~default:`Null ~f:str
    ; ("main", if r.main then `True else `False)
    ]
;;

let session_home (h : Session.Home.t) =
  `Object
    [ "cwd", str h.cwd
    ; ( "host"
      , Option.value_map
          h.host
          ~default:`Null
          ~f:(fun (host : Session.Host.t) ->
            `Object [ "id", str host.id; "name", str host.name ]) )
    ; "repo", Option.value_map h.repo ~default:`Null ~f:repo
    ]
;;

(* Only while set: most sessions never have one. *)
let home = Option.value_map ~default:[] ~f:(fun h -> [ "home", session_home h ])

let trim_result (trim : Session.Trim.t) =
  `Object [ "messages", int trim.messages; "tokens", int trim.tokens ]
;;

let trim (trim : Session.Trim.t) =
  `Object
    ([ "before", str trim.before
     ; "messages", int trim.messages
     ; "tokens", int trim.tokens
     ]
     @ Option.value_map trim.at ~default:[] ~f:(fun at -> [ "at", ms at ]))
;;

let hidden (h : Session.Hidden.t) =
  let kind =
    match h.kind with
    | Trim_before { before } ->
      [ "kind", str "trim_before"; "message", str before ]
    | Trim_after { after; through } ->
      [ "kind", str "trim_after"; "message", str after; "through", str through ]
    | Deleted { message } -> [ "kind", str "deleted"; "message", str message ]
  in
  `Object
    ([ "entry", str h.entry ]
     @ kind
     @ [ "messages", int h.messages; "tokens", int h.tokens ]
     @ Option.value_map h.at ~default:[] ~f:(fun at -> [ "at", ms at ])
     @ Option.value_map h.divider ~default:[] ~f:(fun d -> [ "divider", int d ])
    )
;;

let state (s : Agent.State.t) =
  match state s with
  | `Object fields ->
    `Object
      (fields
       @ home s.home
       @ Option.value_map s.trim ~default:[] ~f:(fun t -> [ "trim", trim t ])
       @ Option.value_map s.transcript_filter ~default:[] ~f:(fun f ->
         [ "transcript_filter", str f ])
       @
       if List.is_empty s.hidden
       then []
       else [ "hidden", `Array (List.map s.hidden ~f:hidden) ])
  | json -> json
;;

let session_summary (s : Session.Summary.t) =
  [ "id", str s.id
  ; "path", str s.path
  ; "name", Option.value_map s.name ~default:`Null ~f:str
  ; "description", Option.value_map s.description ~default:`Null ~f:str
  ; "cwd", str s.cwd
  ; "created_at", str s.created_at
  ; "updated_at", str s.updated_at
  ; "first_prompt", Option.value_map s.first_prompt ~default:`Null ~f:str
  ; "message_count", int s.message_count
  ; "parent", Option.value_map s.parent ~default:`Null ~f:str
  ; "host", Option.value_map s.host ~default:`Null ~f:str
  ]
  @ (match s.repo with
     | None -> []
     | Some r -> [ "repo", Option.value_map r ~default:`Null ~f:repo ])
  @ [ "worktrees", `Array (List.map s.worktrees ~f:session_worktree)
    ; "bytes", int s.bytes
    ]
  @ home s.home
  |> fun fields -> `Object fields
;;

let session_stats (s : Agent.Session_stats.t) =
  `Object
    [ "message_count", int s.message_count
    ; "turns", int s.turns
    ; ( "tool_calls"
      , `Object
          (List.map s.tool_calls ~f:(fun (name, count) -> name, int count)) )
    ; "usage", usage s.usage
    ; "cost_usd", float s.cost_usd
    ; "context_percent", float s.context_percent
    ; "model_changes", int s.model_changes
    ; "compactions", int s.compactions
    ; "masks", int s.masks
    ; "duration_seconds", float s.duration_seconds
    ]
;;

let entry (e : Session.Entry.t) =
  let payload =
    match e.payload with
    | Message m -> [ "kind", str "message"; "message", message m ]
    | Model { model; thinking = t } ->
      [ "kind", str "model"; "model", str model; "thinking", thinking t ]
    | Compaction { summary; kept_from; pinned } ->
      [ "kind", str "compaction"
      ; "summary", str summary
      ; "kept_from", str kept_from
      ]
      @
      if List.is_empty pinned
      then []
      else
        [ ( "pinned"
          , `Array
              (List.map pinned ~f:(fun (pin : Pinned_messages.Pin.t) ->
                 str pin.id)) )
        ]
    | Masked { through } -> [ "kind", str "masked"; "through", str through ]
    | Name { name } -> [ "kind", str "name"; "name", str name ]
    | Description { text } -> [ "kind", str "description"; "text", str text ]
    | Cwd { cwd; host = _ } -> [ "kind", str "cwd"; "cwd", str cwd ]
    | System_prompt _ -> [ "kind", str "system_prompt" ]
    | Goal _ -> [ "kind", str "goal" ]
    | Reminders _ -> [ "kind", str "reminders" ]
    | Budget _ -> [ "kind", str "budget" ]
    | Approval { mode; plan } ->
      [ "kind", str "approval"; "mode", str mode; "plan", bool plan ]
    | Allow_rule { rule } -> [ "kind", str "allow_rule"; "rule", str rule ]
    | Archived { archived } ->
      [ "kind", str "archived"
      ; ("archived", if archived then `True else `False)
      ]
    | Spend { usage = u; cost_usd } ->
      [ "kind", str "spend"; "usage", usage u; "cost_usd", float cost_usd ]
    | Resume_at { at } ->
      [ "kind", str "resume_at"
      ; "at", Option.value_map at ~default:`Null ~f:ms
      ]
    | Told_environment _ -> [ "kind", str "told_environment" ]
    | Review _ -> [ "kind", str "review" ]
    | Repo _ -> [ "kind", str "repo" ]
    | Agent_worktree w -> [ "kind", str "agent_worktree"; "path", str w.path ]
    | Worktree w -> [ "kind", str "worktree"; "path", str w.path ]
    | Worktree_removed { path } ->
      [ "kind", str "worktree_removed"; "path", str path ]
    | Home _ -> [ "kind", str "home" ]
    | Transcript_filter { filter } ->
      [ "kind", str "transcript_filter"
      ; "filter", Option.value_map filter ~default:`Null ~f:str
      ]
    | Trim { before } -> [ "kind", str "trim"; "before", str before ]
    | Untrim -> [ "kind", str "untrim" ]
    | Trim_after { after; through } ->
      [ "kind", str "trim_after"; "after", str after; "through", str through ]
    | Delete { message } -> [ "kind", str "delete"; "message", str message ]
    | Restore { entry } -> [ "kind", str "restore"; "entry", str entry ]
  in
  `Object
    ([ "id", str e.id
     ; "parent", Option.value_map e.parent ~default:`Null ~f:str
     ]
     @ Option.value_map e.at ~default:[] ~f:(fun at -> [ "at", ms at ])
     @ payload)
;;

let auth_status (s : Provider_auth.Status.t) =
  `Object
    ([ "provider", str (Provider_id.to_string s.provider)
     ; "name", str (Provider_id.display_name s.provider)
     ; ( "methods"
       , `Array
           (List.map s.methods ~f:(fun m ->
              `Object
                [ "method", str (Provider_auth.Method.to_string m)
                ; "label", str (Provider_auth.Method.label s.provider m)
                ])) )
     ; ( "configured"
       , match s.configured with
         | None -> `Null
         | Some (m, source) ->
           `Object
             [ "method", str (Provider_auth.Method.to_string m)
             ; "source", str source
             ] )
     ; "expires_ms", Option.value_map s.expires_ms ~default:`Null ~f:int
     ]
     @
     match s.custom with
     | None -> []
     | Some c ->
       [ ( "custom"
         , `Object
             [ "base_url", str c.base_url
             ; "api", str (Custom_provider.Api.to_string c.api)
             ; "api_label", str (Custom_provider.Api.label c.api)
             ] )
       ])
;;

let auth_prompt (p : Auth_interaction.Prompt.t) =
  match p with
  | Secret { message; allow_empty } ->
    [ "prompt", str "secret"; "message", str message ]
    @ if allow_empty then [ "allow_empty", bool true ] else []
  | Text { message; placeholder; default } ->
    [ "prompt", str "text"
    ; "message", str message
    ; "placeholder", str placeholder
    ; "default", str default
    ]
  | Manual_code { message; placeholder } ->
    [ "prompt", str "manual_code"
    ; "message", str message
    ; "placeholder", str placeholder
    ]
  | Select { message; options } ->
    [ "prompt", str "select"
    ; "message", str message
    ; ( "options"
      , `Array
          (List.map options ~f:(fun (id, label) ->
             `Object [ "id", str id; "label", str label ])) )
    ]
;;

let login_event (e : Login_manager.Event.t) =
  let fields =
    match e with
    | Auth_url { url; instructions } ->
      [ "kind", str "auth_url"
      ; "url", str url
      ; "instructions", str instructions
      ]
    | Prompt { id; prompt } ->
      [ "kind", str "prompt"; "id", str id ] @ auth_prompt prompt
    | Prompt_cancelled { id } ->
      [ "kind", str "prompt_cancelled"; "id", str id ]
    | Progress message -> [ "kind", str "progress"; "message", str message ]
    | Done { provider; method_ } ->
      [ "kind", str "done"
      ; "provider", str (Provider_id.to_string provider)
      ; "method", str (Provider_auth.Method.to_string method_)
      ]
    | Failed { provider; error } ->
      [ "kind", str "failed"; "provider", str provider; "error", str error ]
    | Logged_out provider ->
      [ "kind", str "logged_out"
      ; "provider", str (Provider_id.to_string provider)
      ]
    | Forge_done { host; user; method_ } ->
      [ "kind", str "forge_done"
      ; "host", str host
      ; "user", Option.value_map user ~default:`Null ~f:str
      ; "method", str (Forge_credential.Method.to_string method_)
      ]
    | Forge_logged_out host ->
      [ "kind", str "forge_logged_out"; "host", str host ]
  in
  `Object ([ "type", str "event"; "event", str "auth" ] @ fields)
;;

let agent_queues agents =
  `Array
    (List.map agents ~f:(fun (q : Subagent_inbox.Listing.t) ->
       `Object
         [ "agent_id", str q.agent_id
         ; "steer_texts", `Array (List.map q.steer ~f:str)
         ; "follow_up_texts", `Array (List.map q.follow_up ~f:str)
         ]))
;;

let rec event ?now (e : Agent.Event.t) =
  let fields =
    match e with
    | Loop Agent_start -> [ "event", str "agent_start" ]
    | Loop (Agent_end added) ->
      [ "event", str "agent_end"
      ; "messages", `Array (List.map added ~f:message)
      ]
    | Loop Turn_start -> [ "event", str "turn_start" ]
    | Loop (Turn_end { assistant = a; tool_results }) ->
      [ "event", str "turn_end"
      ; "assistant", assistant a
      ; "tool_results", `Array (List.map tool_results ~f:tool_result)
      ]
    | Loop (Message_start m) ->
      [ "event", str "message_start"; "message", message_at m ~at:now ]
    | Loop (Message_update { partial; delta = d }) ->
      [ "event", str "message_update"
      ; "partial", assistant partial
      ; "delta", delta d
      ]
    | Loop (Message_end m) ->
      [ "event", str "message_end"; "message", message_at m ~at:now ]
    | Loop (Retry { attempt; max_attempts; error; delay }) ->
      [ "event", str "retry"
      ; "attempt", int attempt
      ; "max_attempts", int max_attempts
      ; "error", str error
      ; "delay_ms", ms delay
      ]
    | Loop (Tool_start call) ->
      [ "event", str "tool_start"; "call", tool_call call ]
    | Loop (Tool_output { call_id; chunk }) ->
      [ "event", str "tool_output"; "call_id", str call_id; "chunk", str chunk ]
    | Loop (Tool_confirm { call_id; agent_id; name; summary; rules }) ->
      [ "event", str "tool_confirm"
      ; "call_id", str call_id
      ; "agent_id", Option.value_map agent_id ~default:`Null ~f:str
      ; "name", str name
      ; "summary", str summary
      ; "rules", `Array (List.map rules ~f:str)
      ]
    | Loop (Tool_confirm_done { call_id; agent_id }) ->
      [ "event", str "tool_confirm_done"
      ; "call_id", str call_id
      ; "agent_id", Option.value_map agent_id ~default:`Null ~f:str
      ]
    | Loop (Ask_user_done { call_id; timed_out }) ->
      [ "event", str "ask_user_done"
      ; "call_id", str call_id
      ; "timed_out", bool timed_out
      ]
    | Loop (Ask_user { call_id; questions; deadline }) ->
      [ "event", str "ask_user"
      ; "call_id", str call_id
      ; "questions", `Array (List.map questions ~f:User_question.to_json)
      ; "deadline_ms", Option.value_map deadline ~default:`Null ~f:ms
      ]
    | Loop (Tool_end { call; result }) ->
      [ "event", str "tool_end"
      ; "call", tool_call call
      ; "result", tool_result result
      ]
    | Loop
        (Subagent_start
           { call_id
           ; agent_id
           ; task
           ; model
           ; tools
           ; worktree
           ; forked
           ; continued
           }) ->
      [ "event", str "subagent_start"
      ; "call_id", str call_id
      ; "agent_id", str agent_id
      ; "task", str task
      ; "model", str model
      ; "tools", `Array (List.map tools ~f:str)
      ]
      @ Option.value_map worktree ~default:[] ~f:(fun w ->
        [ "worktree", agent_worktree w ])
      @ (if forked then [ "forked", `True ] else [])
      @ if continued then [ "continued", `True ] else []
    | Loop (Subagent { call_id; agent_id; event = inner }) ->
      [ "event", str "subagent"
      ; "call_id", str call_id
      ; "agent_id", str agent_id
      ; "inner", event ?now (Loop inner)
      ]
    | Loop
        (Subagent_end { call_id; agent_id; usage = u; turns; cost_usd; result })
      ->
      [ "event", str "subagent_end"
      ; "call_id", str call_id
      ; "agent_id", str agent_id
      ; "usage", usage u
      ; "turns", int turns
      ; "cost_usd", float cost_usd
      ; "result", tool_result_value result
      ]
    | Loop (Worktrees_changed _) ->
      (* [Agent] records it rather than broadcasting it. *)
      [ "event", str "worktrees_changed" ]
    | Loop (Compacted { summary; usage = u; cost_usd }) ->
      [ "event", str "compacted"
      ; "summary", str summary
      ; "usage", usage u
      ; "cost_usd", float cost_usd
      ]
    | Loop (Masked { masked; tokens_saved }) ->
      [ "event", str "masked"
      ; "masked", int masked
      ; "tokens_saved", int tokens_saved
      ]
    | State_changed s -> [ "event", str "state"; "state", state s ]
    | Compacted { summary } ->
      [ "event", str "compacted"; "summary", str summary ]
    | Config_changed c -> [ "event", str "config_changed"; "config", config c ]
    | Review_changed r -> [ "event", str "review"; "review", review r ]
    | Notice text -> [ "event", str "notice"; "text", str text ]
    | Subagent_queues agents ->
      [ "event", str "agent_queues"; "agents", agent_queues agents ]
    | Queue_update { steer; follow_up } ->
      [ "event", str "queue_update"
      ; "steer", int (List.length steer)
      ; "follow_up", int (List.length follow_up)
      ; "steer_texts", `Array (List.map steer ~f:str)
      ; "follow_up_texts", `Array (List.map follow_up ~f:str)
      ]
    | Tool_exec
        { host; session_id; exec_id; call_id; name; arguments; cwd; stdin } ->
      [ "event", str "tool_exec"
      ; "host", str host
      ; "session_id", str session_id
      ; "exec_id", str exec_id
      ; "call_id", str call_id
      ; "name", str name
      ; "arguments", arguments
      ; "cwd", str cwd
      ]
      @ if stdin then [ "stdin", `True ] else []
    | Tool_exec_input { host; exec_id; data; close } ->
      [ "event", str "tool_exec_input"
      ; "host", str host
      ; "exec_id", str exec_id
      ; "data", str data
      ; "close", bool close
      ]
    | Tool_exec_cancel { host; exec_id } ->
      [ "event", str "tool_exec_cancel"
      ; "host", str host
      ; "exec_id", str exec_id
      ]
  in
  `Object (("type", str "event") :: fields)
;;

let opt f = Option.value_map ~default:`Null ~f

let subagent_summary (s : Subagent_log.Summary.t) =
  `Object
    [ "id", str s.id
    ; "call_id", str s.call_id
    ; "parent", opt str s.parent
    ; "task", str s.task
    ; "model", str s.model
    ; "state", str (Subagent_log.State.to_string s.state)
    ; "started_at_ms", ms s.started_at
    ; "updated_at_ms", ms s.updated_at
    ; "ended_at_ms", opt ms s.ended_at
    ; "turns", int s.turns
    ; "tool_calls", int s.tool_calls
    ; "current_tool", opt (fun (name, _) -> str name) s.current_tool
    ; "current_tool_started_at_ms", opt (fun (_, at) -> ms at) s.current_tool
    ; "message_count", int s.message_count
    ; "stale", bool s.stale
    ; "result", opt tool_result_value s.result
    ]
;;

let subagent ((s : Subagent_log.Summary.t), messages) =
  `Object
    [ "subagent", subagent_summary s
    ; "messages", `Array (List.map messages ~f:timed_message)
    ]
;;

let pending ~steer ~follow_up ~agents ~confirms ~questions =
  `Object
    [ "steer_texts", `Array (List.map steer ~f:str)
    ; "follow_up_texts", `Array (List.map follow_up ~f:str)
    ; "agents", agent_queues agents
    ; ( "confirms"
      , `Array
          (List.map confirms ~f:(fun (c : Agent.Pending_confirm.t) ->
             `Object
               [ "call_id", str c.call_id
               ; "agent_id", Option.value_map c.agent_id ~default:`Null ~f:str
               ; "name", str c.name
               ; "summary", str c.summary
               ; "rules", `Array (List.map c.rules ~f:str)
               ])) )
    ; ( "questions"
      , `Array
          (List.map questions ~f:(fun (q : Agent.Pending_question.t) ->
             `Object
               [ "call_id", str q.call_id
               ; ( "questions"
                 , `Array (List.map q.questions ~f:User_question.to_json) )
               ; "deadline_ms", Option.value_map q.deadline ~default:`Null ~f:ms
               ])) )
    ]
;;
