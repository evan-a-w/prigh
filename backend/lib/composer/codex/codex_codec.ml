open! Core
open! Import

module Settings = struct
  type t =
    { cwd : string
    ; model : string option
    ; thinking : Thinking.t
    ; approval : Approval_mode.t
    ; plan : bool
    ; efforts : string list option
    ; sandbox : bool
    }
  [@@deriving sexp_of]
end

module Dynamic_tool = struct
  type t =
    { name : string
    ; description : string
    ; input_schema : Json.t
    }
end

module Thread_options = struct
  type t =
    { developer_instructions : string option
    ; mcp_servers : (string * Json.t) list
    ; dynamic_tools : Dynamic_tool.t list
    ; config : (string * Json.t) list
    }

  let empty =
    { developer_instructions = None
    ; mcp_servers = []
    ; dynamic_tools = []
    ; config = []
    }
  ;;
end

module Input = struct
  type t =
    | Text of string
    | Image of Image.t
    | Local_image of string
end

module Account = struct
  type t =
    | Logged_out
    | Not_needed
    | Api_key
    | Chatgpt of
        { email : string option
        ; plan : string option
        }
    | Other of string
  [@@deriving sexp_of, equal]
end

module Chatgpt_tokens = struct
  type t =
    { access_token : string
    ; account_id : string
    ; plan : string option
    }

  let sexp_of_t t =
    [%sexp
      { access_token = "<redacted>"
      ; account_id : string = t.account_id
      ; plan : string option = t.plan
      }]
  ;;

  let fields t =
    [ "accessToken", `String t.access_token
    ; "chatgptAccountId", `String t.account_id
    ; ( "chatgptPlanType"
      , match t.plan with
        | Some p -> `String p
        | None -> `Null )
    ]
  ;;
end

module Model_info = struct
  type t =
    { id : string
    ; display_name : string
    ; description : string
    ; is_default : bool
    ; hidden : bool
    ; efforts : string list
    ; default_effort : string option
    ; images : bool
    }
  [@@deriving sexp_of]
end

module Request_kind = struct
  type t =
    | Initialize
    | Thread_start
    | Thread_resume
    | Thread_fork
    | Turn_start
    | Turn_steer
    | Turn_interrupt
    | Compact
    | Revert
    | Model_list
    | Account_read
    | Rate_limits_read
    | Login
  [@@deriving sexp_of, equal]
end

module Response = struct
  type t =
    | Initialized of
        { user_agent : string
        ; version : string option
        }
    | Thread of
        { kind : Request_kind.t
        ; thread_id : string
        ; model : string option
        }
    | Turn_accepted of { turn_id : string }
    | Steered of { turn_id : string }
    | Models of
        { models : Model_info.t list
        ; next_cursor : string option
        }
    | Account of Account.t
    | Rate_limits of Subscription_usage.Status.t
    | Logged_in
    | Done of Request_kind.t
    | Failed of
        { kind : Request_kind.t
        ; code : int
        ; message : string
        }
  [@@deriving sexp_of]
end

module Tool_call = struct
  type t =
    { request_id : string
    ; call_id : string
    ; tool : string
    ; arguments : Json.t
    }
  [@@deriving sexp_of]
end

module Output = struct
  type t =
    | Event of Adapter_event.t
    | Send of string
    | Response of Response.t
    | Tool_call of Tool_call.t
    | Token_refresh of
        { request_id : string
        ; previous_account_id : string option
        }
  [@@deriving sexp_of]
end

(* A request Codex made of us and has not had answered. *)
module Server_request = struct
  type t =
    | Command_approval of { decisions : Json.t list option }
    | File_approval
    | Permissions of { requested : Json.t }
    | Mcp_tool_approval of { schema : Json.t option }
    | Question of { question_ids : string list }
    | Tool
    | Token_refresh
end

(* A request of Codex's waiting for our answer, with its JSON-RPC id. *)
module Pending = struct
  type t =
    { id : Json.t
    ; request : Server_request.t
    }
end

module Interrupt = struct
  type t =
    | Not_asked
    | Wanted (* before Codex said the turn started *)
    | Sent
end

module Turn = struct
  type t =
    { mutable id : string option
    ; mutable started : bool
    ; mutable interrupt : Interrupt.t
    }
end

(* A subagent's call: the item under which its thread's calls are shown. *)
module Subagent = struct
  type t =
    { call : string
    ; mutable last_text : string
    ; mutable ended : bool
    }
end

type t =
  { mutable next_id : int
  ; pending : Request_kind.t Int.Table.t
  ; server_requests : Pending.t String.Table.t
  ; mutable thread : string option
  ; mutable thread_model : string option
  ; mutable turn : Turn.t option
  ; finished_turns : String.Hash_set.t
  ; subagents : Subagent.t String.Table.t (* by the subagent's thread id *)
  ; file_changes : Json.t list String.Table.t (* by item id, for approvals *)
  ; reasoning_streamed : String.Hash_set.t
  ; mutable last_total : Usage.t option
  ; mutable turn_error : (Adapter_event.Error_kind.t * string) option
  ; mutable resets_at : float option
  ; mutable limits : Subscription_usage.Status.t option
  }

let create () =
  { next_id = 0
  ; pending = Int.Table.create ()
  ; server_requests = String.Table.create ()
  ; thread = None
  ; thread_model = None
  ; turn = None
  ; finished_turns = String.Hash_set.create ()
  ; subagents = String.Table.create ()
  ; file_changes = String.Table.create ()
  ; reasoning_streamed = String.Hash_set.create ()
  ; last_total = None
  ; turn_error = None
  ; resets_at = None
  ; limits = None
  }
;;

let thread_id t = t.thread
let turn_id t = Option.bind t.turn ~f:(fun turn -> turn.id)

(* JSON access *)

let field name json = Json.member name json
let str name json = Option.bind (field name json) ~f:Json.string
let int name json = Option.bind (field name json) ~f:Json.int
let bool name json = Option.bind (field name json) ~f:Json.bool
let float name json = Option.bind (field name json) ~f:Json.float

let list name json =
  Option.value (Option.bind (field name json) ~f:Json.list) ~default:[]
;;

let num n = `Number (Int.to_string n)
let opt name f = Option.map ~f:(fun v -> name, f v)
let obj fields = `Object (List.filter_opt fields)

let version_of_user_agent ua =
  match String.lsplit2 ua ~on:'/' with
  | None -> None
  | Some (_, rest) ->
    (match String.split rest ~on:' ' with
     | version :: _ when not (String.is_empty version) -> Some version
     | _ -> None)
;;

(* Outgoing *)

let request t kind method_ params =
  let id = t.next_id in
  t.next_id <- id + 1;
  Hashtbl.set t.pending ~key:id ~data:kind;
  Json.to_string
    (`Object [ "id", num id; "method", `String method_; "params", params ])
;;

let notify method_ = Json.to_string (`Object [ "method", `String method_ ])
let reply id result = Json.to_string (`Object [ "id", id; "result", result ])

let reply_error id ~code message =
  Json.to_string
    (`Object
        [ "id", id
        ; "error", `Object [ "code", num code; "message", `String message ]
        ])
;;

(* Codex asks prigh before every command it does not know to be read-only
   and every file change, in every mode: prigh's approval (the session's
   mode, its deny and ask rules, Smart's judge) decides, as for the
   harness's own tools. Modes where Codex decides itself ("on-request"
   inside the sandbox, "never", its own [auto_review] reviewer) would skip
   those rules. *)
let approval_policy : Json.t = `String "untrusted"
let reviewer = "user"

(* Where the sandbox cannot run ([sandbox] false), none: the approval policy
   stays, so Codex still asks before all it does not know to be read-only. *)
let sandbox_mode (s : Settings.t) =
  match s.approval with
  | _ when not s.sandbox -> "danger-full-access"
  | Ask -> "read-only"
  | Auto_edit | Smart -> "workspace-write"
  | Auto -> "danger-full-access"
;;

let sandbox_policy (s : Settings.t) =
  let type_ =
    match s.approval with
    | _ when not s.sandbox -> "dangerFullAccess"
    | Ask -> "readOnly"
    | Auto_edit | Smart -> "workspaceWrite"
    | Auto -> "dangerFullAccess"
  in
  `Object [ "type", `String type_ ]
;;

(* Codex's efforts, lowest first. *)
let effort_order = [ "none"; "minimal"; "low"; "medium"; "high"; "xhigh" ]

let effort_rank e =
  List.findi effort_order ~f:(fun _ x -> String.equal x e) |> Option.map ~f:fst
;;

let effort ?supported (thinking : Thinking.t) =
  let wanted =
    match thinking with
    | Off -> Some "none"
    | On None -> None
    | On (Some Max) -> Some "xhigh"
    | On (Some level) -> Some (Thinking.Level.to_string level)
  in
  match wanted, supported with
  | None, _ | _, (None | Some []) -> wanted
  | Some e, Some supported when List.mem supported e ~equal:String.equal ->
    Some e
  | Some e, Some supported ->
    let rank e = Option.value (effort_rank e) ~default:Int.max_value in
    let target = rank e in
    List.min_elt supported ~compare:(fun a b ->
      Comparable.lift
        [%compare: int * int]
        ~f:(fun x -> abs (rank x - target), rank x)
        a
        b)
;;

let thread_params
      t
      ?thread_id
      ?last_turn_id
      (s : Settings.t)
      (o : Thread_options.t)
  =
  let config =
    ("tools.update_plan.enabled", `True)
    :: (List.map o.mcp_servers ~f:(fun (name, server) ->
          "mcp_servers." ^ name, server)
        @ o.config)
  in
  let dynamic_tools =
    match o.dynamic_tools with
    | [] -> None
    | tools ->
      Some
        ( "dynamicTools"
        , `Array
            (List.map tools ~f:(fun (tool : Dynamic_tool.t) ->
               `Object
                 [ "type", `String "function"
                 ; "name", `String tool.name
                 ; "description", `String tool.description
                 ; "inputSchema", tool.input_schema
                 ])) )
  in
  ignore t;
  obj
    [ opt "threadId" (fun id -> `String id) thread_id
    ; opt "lastTurnId" (fun id -> `String id) last_turn_id
    ; Option.map thread_id ~f:(fun _ -> "excludeTurns", `True)
    ; Some ("cwd", `String s.cwd)
    ; opt "model" (fun m -> `String m) s.model
    ; Some ("approvalPolicy", approval_policy)
    ; Some ("approvalsReviewer", `String reviewer)
    ; Some ("sandbox", `String (sandbox_mode s))
    ; opt "developerInstructions" (fun i -> `String i) o.developer_instructions
    ; Some ("config", `Object config)
    ; dynamic_tools
    ]
;;

let initialize t ~client_name ~client_version =
  request
    t
    Initialize
    "initialize"
    (`Object
        [ ( "clientInfo"
          , `Object
              [ "name", `String client_name
              ; "title", `String client_name
              ; "version", `String client_version
              ] )
        ; ( "capabilities"
          , `Object
              [ "experimentalApi", `True
              ; ( "optOutNotificationMethods"
                , `Array [ `String "turn/diff/updated" ] )
              ] )
        ])
;;

let start_thread t settings options =
  request t Thread_start "thread/start" (thread_params t settings options)
;;

let resume_thread t ~thread_id settings options =
  request
    t
    Thread_resume
    "thread/resume"
    (thread_params t ~thread_id settings options)
;;

let fork_thread t ~thread_id ~last_turn_id settings options =
  request
    t
    Thread_fork
    "thread/fork"
    (thread_params t ~thread_id ?last_turn_id settings options)
;;

let input_json : Input.t -> Json.t = function
  | Text text -> `Object [ "type", `String "text"; "text", `String text ]
  | Image image ->
    `Object
      [ "type", `String "image"
      ; "url", `String [%string "data:%{image.mime_type};base64,%{image.data}"]
      ]
  | Local_image path ->
    `Object [ "type", `String "localImage"; "path", `String path ]
;;

let require_thread t =
  match t.thread with
  | Some id -> Ok id
  | None -> Or_error.error_string "Codex has no thread yet"
;;

let start_turn t (s : Settings.t) inputs =
  let%bind.Or_error thread = require_thread t in
  match t.turn with
  | Some _ -> Or_error.error_string "a Codex turn is already running"
  | None ->
    t.turn <- Some { id = None; started = false; interrupt = Not_asked };
    t.turn_error <- None;
    let effort = effort ?supported:s.efforts s.thinking in
    let model = Option.first_some s.model t.thread_model in
    let collaboration =
      Option.map model ~f:(fun model ->
        ( "collaborationMode"
        , `Object
            [ "mode", `String (if s.plan then "plan" else "default")
            ; ( "settings"
              , `Object
                  [ "model", `String model
                  ; ( "reasoning_effort"
                    , Option.value_map effort ~default:`Null ~f:(fun e ->
                        `String e) )
                  ; "developer_instructions", `Null
                  ] )
            ] ))
    in
    Ok
      (request
         t
         Turn_start
         "turn/start"
         (obj
            [ Some ("threadId", `String thread)
            ; Some ("input", `Array (List.map inputs ~f:input_json))
            ; Some ("cwd", `String s.cwd)
            ; Some ("approvalPolicy", approval_policy)
            ; Some ("approvalsReviewer", `String reviewer)
            ; Some ("sandboxPolicy", sandbox_policy s)
            ; opt "model" (fun m -> `String m) s.model
            ; opt "effort" (fun e -> `String e) effort
            ; Some ("summary", `String "auto")
            ; collaboration
            ]))
;;

let steer t inputs =
  let%bind.Or_error thread = require_thread t in
  match t.turn with
  | Some { id = Some turn; started = true; _ } ->
    Ok
      (request
         t
         Turn_steer
         "turn/steer"
         (`Object
             [ "threadId", `String thread
             ; "input", `Array (List.map inputs ~f:input_json)
             ; "expectedTurnId", `String turn
             ]))
  | Some _ -> Or_error.error_string "the Codex turn has not started yet"
  | None -> Or_error.error_string "no Codex turn is running"
;;

let interrupt_request t ~thread ~turn =
  request
    t
    Turn_interrupt
    "turn/interrupt"
    (`Object [ "threadId", `String thread; "turnId", `String turn ])
;;

let interrupt t =
  match t.turn, t.thread with
  | None, _ | _, None -> `Idle
  | Some ({ interrupt = Sent | Wanted; _ } : Turn.t), _ -> `Deferred
  | Some ({ id = Some turn; started = true; _ } as current), Some thread ->
    current.interrupt <- Sent;
    `Send (interrupt_request t ~thread ~turn)
  | Some current, Some _ ->
    current.interrupt <- Wanted;
    `Deferred
;;

let compact t =
  let%bind.Or_error thread = require_thread t in
  match t.turn with
  | Some _ -> Or_error.error_string "a Codex turn is already running"
  | None ->
    (* Codex compacts in a turn of its own, which it starts. *)
    t.turn <- Some { id = None; started = false; interrupt = Not_asked };
    t.turn_error <- None;
    Ok
      (request
         t
         Compact
         "thread/compact/start"
         (`Object [ "threadId", `String thread ]))
;;

let revert t ~before_turn_id =
  let%map.Or_error thread = require_thread t in
  request
    t
    Revert
    "thread/revert"
    (`Object
        [ "threadId", `String thread; "beforeTurnId", `String before_turn_id ])
;;

let list_models t ~cursor =
  request
    t
    Model_list
    "model/list"
    (obj [ opt "cursor" (fun c -> `String c) cursor ])
;;

let read_account t =
  request t Account_read "account/read" (`Object [ "refreshToken", `False ])
;;

let read_rate_limits t =
  request t Rate_limits_read "account/rateLimits/read" `Null
;;

let login_tokens t tokens =
  request
    t
    Login
    "account/login/start"
    (`Object
        (("type", `String "chatgptAuthTokens") :: Chatgpt_tokens.fields tokens))
;;

let limits t = t.limits

let request_key : Json.t -> string = function
  | `String s -> s
  | `Number n -> n
  | json -> Json.to_string json
;;

let take_server_request t request_id =
  let entry = Hashtbl.find t.server_requests request_id in
  Hashtbl.remove t.server_requests request_id;
  entry
;;

let offered decisions name =
  match decisions with
  | None -> true
  | Some decisions ->
    List.exists decisions ~f:(function
      | `String s -> String.equal s name
      | _ -> false)
;;

let amendment decisions =
  List.find (Option.value decisions ~default:[]) ~f:(fun d ->
    Option.is_some (field "acceptWithExecpolicyAmendment" d))
;;

(* The answer accepting an MCP tool-call approval form ([always]: for the
   session): each of the form's fields gets the option that means "allow
   once" (or "for the session"), a persistence switch [always], else its
   default. A form with a required field left unanswered is cancelled
   rather than guessed. *)
let mcp_accept ~schema ~always =
  let persistent value =
    let v = String.lowercase value in
    List.exists
      [ "session"; "always"; "permanent"; "forever"; "persist" ]
      ~f:(fun word -> String.is_substring v ~substring:word)
  in
  let once value =
    let v = String.lowercase (String.strip value) in
    (not (persistent v))
    && List.exists [ "once"; "accept"; "approve"; "allow" ] ~f:(fun word ->
      String.is_prefix v ~prefix:word)
  in
  let for_session value =
    String.is_substring (String.lowercase value) ~substring:"session"
  in
  let properties =
    match Option.bind schema ~f:(Json.member "properties") with
    | Some (`Object fields) -> fields
    | _ -> []
  in
  let options field =
    match Json.member "oneOf" field, Json.member "enum" field with
    | Some (`Array options), _ ->
      List.filter_map options ~f:(fun o ->
        Option.bind (Json.member "const" o) ~f:Json.string)
    | _, Some (`Array values) -> List.filter_map values ~f:Json.string
    | _ -> []
  in
  let content =
    List.filter_map properties ~f:(fun (key, field) ->
      let choice =
        List.find (options field) ~f:(if always then for_session else once)
      in
      match choice with
      | Some value -> Some (key, `String value)
      | None ->
        let title =
          Option.value
            (Option.bind (Json.member "title" field) ~f:Json.string)
            ~default:""
        in
        (match Option.bind (Json.member "type" field) ~f:Json.string with
         | Some "boolean" when persistent key || persistent title ->
           Some (key, if always then `True else `False)
         | _ ->
           (match Json.member "default" field with
            | None | Some `Null -> None
            | Some default -> Some (key, default))))
  in
  let required =
    match Option.bind schema ~f:(Json.member "required") with
    | Some (`Array keys) -> List.filter_map keys ~f:Json.string
    | _ -> []
  in
  if
    List.exists required ~f:(fun key ->
      not (List.Assoc.mem content key ~equal:String.equal))
  then `Object [ "action", `String "cancel" ]
  else
    `Object
      ([ "action", `String "accept" ]
       @ (if always
          then [ "_meta", `Object [ "persist", `String "session" ] ]
          else [])
       @ if Option.is_some schema then [ "content", `Object content ] else [])
;;

let reply_approval t ~request_id (answer : Adapter_event.Approval_reply.t) =
  match Hashtbl.find t.server_requests request_id with
  | None -> None
  | Some { request = Question _ | Tool | Token_refresh; _ } -> None
  | Some { Pending.id; request } ->
    Hashtbl.remove t.server_requests request_id;
    let decision name = `Object [ "decision", `String name ] in
    let result =
      match request, answer with
      | (Question _ | Tool | Token_refresh), _ -> assert false
      | Command_approval _, Allow | File_approval, Allow -> decision "accept"
      | Command_approval { decisions }, Allow_always ->
        if offered decisions "acceptForSession"
        then decision "acceptForSession"
        else (
          match amendment decisions with
          | Some amendment -> `Object [ "decision", amendment ]
          | None -> decision "accept")
      | File_approval, Allow_always -> decision "acceptForSession"
      | Command_approval { decisions }, Deny _ ->
        decision (if offered decisions "decline" then "decline" else "cancel")
      | File_approval, Deny _ -> decision "decline"
      | (Command_approval _ | File_approval), Cancel -> decision "cancel"
      | Permissions { requested }, (Allow | Allow_always) ->
        let granted =
          match requested with
          | `Object fields ->
            `Object
              (List.filter fields ~f:(fun (_, v) ->
                 match v with
                 | `Null -> false
                 | _ -> true))
          | _ -> `Object []
        in
        `Object
          [ "permissions", granted
          ; ( "scope"
            , `String
                (match answer with
                 | Allow_always -> "session"
                 | _ -> "turn") )
          ]
      | Permissions _, (Deny _ | Cancel) ->
        `Object [ "permissions", `Object []; "scope", `String "turn" ]
      | Mcp_tool_approval { schema }, Allow -> mcp_accept ~schema ~always:false
      | Mcp_tool_approval { schema }, Allow_always ->
        mcp_accept ~schema ~always:true
      | Mcp_tool_approval _, Deny _ -> `Object [ "action", `String "decline" ]
      | Mcp_tool_approval _, Cancel -> `Object [ "action", `String "cancel" ]
    in
    Some (reply id result)
;;

let answer_question t ~request_id (answer : User_question.Reply.t) =
  match Hashtbl.find t.server_requests request_id with
  | Some { Pending.id; request = Question { question_ids } } ->
    Hashtbl.remove t.server_requests request_id;
    let answers =
      match answer with
      | Answered answers ->
        List.zip_with_remainder question_ids answers
        |> fst
        |> List.map ~f:(fun (qid, answer) ->
          ( qid
          , `Object
              [ "answers", `Array (List.map answer ~f:(fun a -> `String a)) ] ))
      | Declined | Cancelled | Timed_out _ | Unavailable _ -> []
    in
    Some (reply id (`Object [ "answers", `Object answers ]))
  | Some _ | None -> None
;;

let reply_tool_call t ~request_id ~output ~success =
  match Hashtbl.find t.server_requests request_id with
  | Some { Pending.id; request = Tool } ->
    Hashtbl.remove t.server_requests request_id;
    Some
      (reply
         id
         (`Object
             [ ( "contentItems"
               , `Array
                   [ `Object
                       [ "type", `String "inputText"; "text", `String output ]
                   ] )
             ; ("success", if success then `True else `False)
             ]))
  | Some _ | None -> None
;;

(* Incoming *)

let event e = Output.Event e

let error_kind t info : Adapter_event.Error_kind.t =
  let usage_limit =
    Adapter_event.Error_kind.Usage_limit { resets_at = t.resets_at }
  in
  match info with
  | Some (`String ("usageLimitExceeded" | "rateLimitExceeded")) -> usage_limit
  | Some (`String "unauthorized") ->
    Auth_required
      { hint = "Codex is not logged in: run `codex login` where it runs" }
  | Some (`Object [ (_, details) ])
    when Option.equal Int.equal (int "httpStatusCode" details) (Some 429) ->
    usage_limit
  | _ -> Other
;;

let turn_error_of t json =
  let message = Option.value (str "message" json) ~default:"Codex failed" in
  let message =
    match str "additionalDetails" json with
    | Some details when not (String.is_empty details) ->
      message ^ "\n" ^ details
    | _ -> message
  in
  error_kind t (field "codexErrorInfo" json), message
;;

let usage_of json =
  { Usage.input = Option.value (int "inputTokens" json) ~default:0
  ; output = Option.value (int "outputTokens" json) ~default:0
  ; cache_read = Option.value (int "cachedInputTokens" json) ~default:0
  }
;;

let sub (a : Usage.t) (b : Usage.t) =
  { Usage.input = a.input - b.input
  ; output = a.output - b.output
  ; cache_read = a.cache_read - b.cache_read
  }
;;

let token_usage t params =
  let usage = Option.value (field "tokenUsage" params) ~default:`Null in
  match field "total" usage, field "last" usage with
  | None, _ | _, None -> []
  | Some total, Some last ->
    let total = usage_of total in
    let delta =
      match t.last_total with
      | Some previous ->
        let delta = sub total previous in
        (* A revert shrinks the totals: count the last call alone. *)
        if delta.input < 0 || delta.output < 0 || delta.cache_read < 0
        then usage_of last
        else delta
      | None -> usage_of last
    in
    t.last_total <- Some total;
    (* Outside a turn (on resume) Codex repeats the thread's totals: not new
       usage. *)
    if Option.is_none t.turn
    then []
    else
      [ event
          (Usage
             { usage = delta
             ; cost_usd = None
             ; context_tokens = int "totalTokens" last
             ; context_window = int "modelContextWindow" usage
             })
      ]
;;

(* A [RateLimitSnapshot] ([primary]/[secondary]: [{usedPercent,
   windowDurationMins, resetsAt}], [planType]) in the shape of ChatGPT's
   usage endpoint, which {!Subscription_usage.parse_codex} reads. *)
let limits_of_snapshot snapshot =
  let window w =
    match w with
    | Some (`Object _ as w) ->
      let n key ~f =
        Option.map (field key w) ~f:(fun v ->
          match v with
          | `Number s ->
            (match Float.of_string_opt s with
             | Some x -> `Number (sprintf "%.17g" (f x))
             | None -> v)
          | v -> v)
      in
      Some
        (`Object
            (List.filter_opt
               [ Option.map (n "usedPercent" ~f:Fn.id) ~f:(fun v ->
                   "used_percent", v)
               ; Option.map
                   (n "windowDurationMins" ~f:(fun m -> m *. 60.))
                   ~f:(fun v -> "limit_window_seconds", v)
               ; Option.map (n "resetsAt" ~f:Fn.id) ~f:(fun v -> "reset_at", v)
               ]))
    | _ -> None
  in
  let rate_limit =
    `Object
      (List.filter_opt
         [ Option.map
             (window (field "primary" snapshot))
             ~f:(fun w -> "primary_window", w)
         ; Option.map
             (window (field "secondary" snapshot))
             ~f:(fun w -> "secondary_window", w)
         ])
  in
  let plan =
    match str "planType" snapshot with
    | Some "unknown" | None -> `Null
    | Some p -> `String p
  in
  Subscription_usage.parse_codex
    ~now:0.
    (Json.to_string (`Object [ "plan_type", plan; "rate_limit", rate_limit ]))
;;

let rate_limits t params =
  let snapshot = Option.value (field "rateLimits" params) ~default:`Null in
  (* A snapshot without windows (some limits have none) keeps what an
     earlier one said. *)
  (match snapshot, limits_of_snapshot snapshot with
   | `Object _, (Limits { windows = _ :: _; _ } as status) ->
     t.limits <- Some status
   | `Object _, status when Option.is_none t.limits -> t.limits <- Some status
   | _ -> ());
  let windows =
    List.filter_map [ "primary"; "secondary" ] ~f:(fun name ->
      match field name snapshot with
      | Some (`Object _ as w) ->
        Option.both (float "usedPercent" w) (float "resetsAt" w)
      | _ -> None)
  in
  match
    List.max_elt windows ~compare:(fun (a, _) (b, _) -> Float.compare a b)
  with
  | Some (_, resets_at) -> t.resets_at <- Some resets_at
  | None -> ()
;;

(* Whose an item is: the main thread's ([None]), a known subagent's
   ([Some call]), or a thread we don't follow. *)
let owner t params =
  match str "threadId" params, t.thread with
  | Some thread, Some main when String.equal thread main -> `Main
  | Some thread, _ ->
    (match Hashtbl.find t.subagents thread with
     | Some sub -> `Subagent sub
     | None -> `Unknown)
  | None, _ -> `Main
;;

let parent = function
  | `Subagent (sub : Subagent.t) -> Some sub.call
  | `Main | `Unknown -> None
;;

let shell_command item =
  match list "commandActions" item with
  | [ action ] -> Option.value (str "command" action) ~default:""
  | _ -> Option.value (str "command" item) ~default:""
;;

let change_kind json : Adapter_event.Change.Kind.t =
  match Option.bind (field "kind" json) ~f:(str "type") with
  | Some "add" -> Add
  | Some "delete" -> Delete
  | _ ->
    Update { move_to = Option.bind (field "kind" json) ~f:(str "move_path") }
;;

(* Codex gives an added or deleted file's content, and an update's hunks
   without the file header. *)
let unified_diff ~path (kind : Adapter_event.Change.Kind.t) diff =
  let side prefix path =
    if String.is_prefix path ~prefix:"/" then path else prefix ^ path
  in
  let a = side "a/" path in
  let b =
    side
      "b/"
      (match kind with
       | Update { move_to = Some target } -> target
       | _ -> path)
  in
  let marked mark =
    let lines = String.split_lines diff in
    ( List.length lines
    , String.concat (List.map lines ~f:(fun line -> mark ^ line ^ "\n")) )
  in
  match kind with
  | Add ->
    let n, body = marked "+" in
    [%string "--- /dev/null\n+++ %{b}\n@@ -0,0 +1,%{n#Int} @@\n%{body}"]
  | Delete ->
    let n, body = marked "-" in
    [%string "--- %{a}\n+++ /dev/null\n@@ -1,%{n#Int} +0,0 @@\n%{body}"]
  | Update _ ->
    if String.is_prefix diff ~prefix:"---" || String.is_empty diff
    then diff
    else [%string "--- %{a}\n+++ %{b}\n%{diff}"]
;;

let changes item =
  List.map (list "changes" item) ~f:(fun change ->
    let path = Option.value (str "path" change) ~default:"" in
    let kind = change_kind change in
    { Adapter_event.Change.path
    ; kind
    ; diff =
        unified_diff ~path kind (Option.value (str "diff" change) ~default:"")
    })
;;

let changes_title changes =
  List.map changes ~f:(fun change ->
    let path = Option.value (str "path" change) ~default:"?" in
    match change_kind change with
    | Add -> "Add " ^ path
    | Delete -> "Delete " ^ path
    | Update { move_to = Some target } -> [%string "Move %{path} to %{target}"]
    | Update { move_to = None } -> "Edit " ^ path)
  |> String.concat ~sep:", "
;;

let tool_start ~owner ~id ~kind ~native_name ~title ~input =
  event
    (Tool_start { id; kind; native_name; title; input; parent = parent owner })
;;

let tool_end ?(changes = []) ?(images = []) ~id ~ok output =
  event (Tool_end { id; ok; output; changes; images })
;;

let mcp_output item =
  let result = Option.value (field "result" item) ~default:`Null in
  let content = list "content" result in
  let texts = List.filter_map content ~f:(fun c -> str "text" c) in
  let images =
    List.filter_map content ~f:(fun c ->
      match str "type" c, str "data" c with
      | Some "image", Some data ->
        Some
          { Image.mime_type =
              Option.value (str "mimeType" c) ~default:"image/png"
          ; data
          }
      | _ -> None)
  in
  let error = Option.bind (field "error" item) ~f:(str "message") in
  let text =
    match error with
    | Some message -> message
    | None -> String.concat texts ~sep:"\n"
  in
  text, images, Option.is_none error
;;

let status_ok item =
  match str "status" item with
  | Some ("completed" | "success") | None -> true
  | Some _ -> false
;;

(* A web search's title and input: opening a page is a fetch of its URL. *)
let web_search_call item =
  let action = Option.value (field "action" item) ~default:`Null in
  let get name = Option.value (str name action) ~default:"" in
  let query q = q, `Object [ "query", `String q ] in
  match str "type" action with
  | Some ("openPage" | "open_page") ->
    let url = get "url" in
    url, `Object [ "url", `String url ]
  | Some ("findInPage" | "find_in_page") ->
    query (get "pattern" ^ " in " ^ get "url")
  | _ ->
    Option.first_some (str "query" action) (str "query" item)
    |> Option.value ~default:""
    |> query
;;

let end_subagent (sub : Subagent.t) ~ok =
  if sub.ended
  then []
  else (
    sub.ended <- true;
    [ tool_end ~id:sub.call ~ok sub.last_text ])
;;

let adopt_subagent t ~thread ~call =
  if not (Hashtbl.mem t.subagents thread)
  then
    Hashtbl.set
      t.subagents
      ~key:thread
      ~data:{ call; last_text = ""; ended = false }
;;

let item_started t params =
  let owner = owner t params in
  let item = Option.value (field "item" params) ~default:`Null in
  let id = Option.value (str "id" item) ~default:"" in
  match owner, str "type" item with
  | `Unknown, _ -> []
  | _, Some "commandExecution" ->
    let command = shell_command item in
    [ tool_start
        ~owner
        ~id
        ~kind:Command
        ~native_name:"commandExecution"
        ~title:command
        ~input:
          (obj
             [ Some ("command", `String command)
             ; opt "cwd" (fun c -> `String c) (str "cwd" item)
             ])
    ]
  | _, Some "fileChange" ->
    let changes = list "changes" item in
    Hashtbl.set t.file_changes ~key:id ~data:changes;
    [ tool_start
        ~owner
        ~id
        ~kind:File_change
        ~native_name:"fileChange"
        ~title:(changes_title changes)
        ~input:
          (`Object
              [ ( "changes"
                , `Array
                    (List.map changes ~f:(fun c ->
                       obj
                         [ opt "path" (fun p -> `String p) (str "path" c)
                         ; opt "kind" Fn.id (field "kind" c)
                         ])) )
              ])
    ]
  | _, Some "mcpToolCall" ->
    let server = Option.value (str "server" item) ~default:"" in
    let tool = Option.value (str "tool" item) ~default:"" in
    [ tool_start
        ~owner
        ~id
        ~kind:(Mcp { server; tool })
        ~native_name:"mcpToolCall"
        ~title:[%string "%{server}: %{tool}"]
        ~input:(Option.value (field "arguments" item) ~default:`Null)
    ]
  | _, Some "dynamicToolCall" ->
    let tool = Option.value (str "tool" item) ~default:"" in
    [ tool_start
        ~owner
        ~id
        ~kind:(Other tool)
        ~native_name:"dynamicToolCall"
        ~title:tool
        ~input:(Option.value (field "arguments" item) ~default:`Null)
    ]
  | _, Some "imageView" ->
    let path = Option.value (str "path" item) ~default:"" in
    [ tool_start
        ~owner
        ~id
        ~kind:Read
        ~native_name:"imageView"
        ~title:path
        ~input:(`Object [ "path", `String path ])
    ]
  | _, Some "collabAgentToolCall" ->
    let tool = Option.value (str "tool" item) ~default:"" in
    let prompt = Option.value (str "prompt" item) ~default:"" in
    (match tool with
     | "spawnAgent" ->
       List.iter (list "receiverThreadIds" item) ~f:(fun thread ->
         Option.iter (Json.string thread) ~f:(fun thread ->
           adopt_subagent t ~thread ~call:id));
       [ tool_start
           ~owner
           ~id
           ~kind:Subagent
           ~native_name:"collabAgentToolCall"
           ~title:prompt
           ~input:
             (obj
                [ Some ("prompt", `String prompt)
                ; opt "model" (fun m -> `String m) (str "model" item)
                ])
       ]
     | tool ->
       [ tool_start
           ~owner
           ~id
           ~kind:(Other tool)
           ~native_name:"collabAgentToolCall"
           ~title:tool
           ~input:(obj [ Some ("tool", `String tool) ])
       ])
  | _, Some "subAgentActivity" ->
    (match str "kind" item, str "agentThreadId" item with
     | Some "started", Some thread when not (Hashtbl.mem t.subagents thread) ->
       adopt_subagent t ~thread ~call:id;
       let path = Option.value (str "agentPath" item) ~default:thread in
       [ tool_start
           ~owner
           ~id
           ~kind:Subagent
           ~native_name:"subAgentActivity"
           ~title:path
           ~input:
             (obj
                [ Some ("agentPath", `String path)
                ; opt "model" (fun m -> `String m) (str "model" item)
                ])
       ]
     | _ -> [])
  | _ -> []
;;

let item_completed t params =
  let owner = owner t params in
  let item = Option.value (field "item" params) ~default:`Null in
  let id = Option.value (str "id" item) ~default:"" in
  match owner, str "type" item with
  | `Unknown, _ -> []
  | `Main, Some ("agentMessage" | "plan") ->
    [ event
        (Text_done
           { item = id; text = Option.value (str "text" item) ~default:"" })
    ]
  | `Subagent sub, Some "agentMessage" ->
    let text = Option.value (str "text" item) ~default:"" in
    sub.last_text <- text;
    if String.is_empty text
    then []
    else [ event (Tool_update { id = sub.call; output = text ^ "\n" }) ]
  | `Main, Some "reasoning" ->
    let parts key = List.filter_map (list key item) ~f:Json.string in
    let text =
      match parts "summary" with
      | [] -> String.concat (parts "content") ~sep:"\n\n"
      | summary -> String.concat summary ~sep:"\n\n"
    in
    if String.is_empty text && not (Hash_set.mem t.reasoning_streamed id)
    then []
    else (
      Hash_set.remove t.reasoning_streamed id;
      [ event (Thinking_done { item = id; text }) ])
  | _, Some "commandExecution" ->
    let output = Option.value (str "aggregatedOutput" item) ~default:"" in
    let exit_ok =
      match int "exitCode" item with
      | None | Some 0 -> true
      | Some _ -> false
    in
    let output =
      match str "status" item, output with
      | Some "declined", "" -> "declined"
      | _ -> output
    in
    [ tool_end ~id ~ok:(status_ok item && exit_ok) output ]
  | _, Some "fileChange" ->
    Hashtbl.remove t.file_changes id;
    let output =
      match str "status" item with
      | Some "declined" -> "declined"
      | Some "failed" -> "failed"
      | _ -> ""
    in
    [ tool_end ~id ~ok:(status_ok item) ~changes:(changes item) output ]
  | _, Some "mcpToolCall" ->
    let output, images, no_error = mcp_output item in
    [ tool_end ~id ~ok:(no_error && status_ok item) ~images output ]
  | _, Some "dynamicToolCall" ->
    let output =
      List.filter_map (list "contentItems" item) ~f:(str "text")
      |> String.concat ~sep:"\n"
    in
    let ok = Option.value (bool "success" item) ~default:(status_ok item) in
    [ tool_end ~id ~ok output ]
  | _, Some "imageView" -> [ tool_end ~id ~ok:true "" ]
  | _, Some "webSearch" ->
    let title, input = web_search_call item in
    let results =
      List.filter_map (list "results" item) ~f:(fun r ->
        match str "title" r, str "url" r with
        | Some title, Some url -> Some [%string "%{title}\n%{url}"]
        | None, Some url -> Some url
        | _ -> None)
      |> String.concat ~sep:"\n\n"
    in
    [ tool_start
        ~owner
        ~id
        ~kind:Web_search
        ~native_name:"webSearch"
        ~title
        ~input
    ; tool_end ~id ~ok:true results
    ]
  | _, Some "collabAgentToolCall" ->
    (match str "tool" item with
     | Some "spawnAgent" ->
       if status_ok item
       then []
       else (
         let subs =
           Hashtbl.data t.subagents
           |> List.filter ~f:(fun (sub : Subagent.t) ->
             String.equal sub.call id)
         in
         match subs with
         | [] -> [ tool_end ~id ~ok:false "failed" ]
         | subs -> List.concat_map subs ~f:(end_subagent ~ok:false))
     | _ -> [ tool_end ~id ~ok:(status_ok item) "" ])
  | _, Some "subAgentActivity" ->
    (match
       ( str "kind" item
       , Option.bind (str "agentThreadId" item) ~f:(Hashtbl.find t.subagents) )
     with
     | Some "completed", Some sub -> end_subagent sub ~ok:true
     | Some "interrupted", Some sub ->
       if String.is_empty sub.last_text then sub.last_text <- "interrupted";
       end_subagent sub ~ok:false
     | _ -> [])
  | `Main, Some "contextCompaction" ->
    [ event (Compacted { tokens_before = None; tokens_after = None }) ]
  | `Main, Some (("enteredReviewMode" | "exitedReviewMode") as kind) ->
    let review = Option.value (str "review" item) ~default:"" in
    [ event
        (Notice
           (if String.equal kind "enteredReviewMode"
            then "Codex is reviewing: " ^ review
            else "Codex finished reviewing: " ^ review))
    ]
  | _ -> []
;;

let delta params = Option.value (str "delta" params) ~default:""
let item_id params = Option.value (str "itemId" params) ~default:""

let turn_started t params =
  match owner t params with
  | `Main ->
    let turn_id = Option.bind (field "turn" params) ~f:(str "id") in
    (match turn_id with
     | None -> []
     | Some id when Hash_set.mem t.finished_turns id -> []
     | Some id ->
       let current =
         match t.turn with
         | Some current -> current
         | None ->
           let current =
             { Turn.id = None; started = false; interrupt = Not_asked }
           in
           t.turn <- Some current;
           t.turn_error <- None;
           current
       in
       current.id <- Some id;
       current.started <- true;
       let interrupt =
         match current.interrupt, t.thread with
         | Wanted, Some thread ->
           current.interrupt <- Sent;
           [ Output.Send (interrupt_request t ~thread ~turn:id) ]
         | _ -> []
       in
       event (Turn_started { turn_id = id }) :: interrupt)
  | `Subagent _ | `Unknown -> []
;;

let turn_completed t params =
  let turn = Option.value (field "turn" params) ~default:`Null in
  let id = str "id" turn in
  match owner t params with
  | `Subagent sub ->
    end_subagent
      sub
      ~ok:(Option.equal String.equal (str "status" turn) (Some "completed"))
  | `Unknown -> []
  | `Main ->
    (match t.turn, id with
     | Some current, Some id
       when Option.value_map current.id ~default:true ~f:(String.equal id) ->
       Hash_set.add t.finished_turns id;
       let interrupted =
         match current.interrupt with
         | Not_asked -> false
         | Wanted | Sent -> true
       in
       t.turn <- None;
       let stop : Adapter_event.Stop.t =
         match str "status" turn with
         | Some "interrupted" -> Interrupted
         | Some "failed" ->
           let kind, message =
             match t.turn_error, field "error" turn with
             | Some error, _ -> error
             | None, Some (`Object _ as error) -> turn_error_of t error
             | None, _ -> Other, "the Codex turn failed"
           in
           Failed { kind; message }
         | _ -> if interrupted then Interrupted else Done
       in
       t.turn_error <- None;
       [ event (Turn_end { turn_id = Some id; stop }) ]
     | _ -> [])
;;

let notification t method_ params =
  match method_ with
  | "turn/started" -> turn_started t params
  | "turn/completed" -> turn_completed t params
  | "item/started" -> item_started t params
  | "item/completed" -> item_completed t params
  | "item/agentMessage/delta" | "item/plan/delta" ->
    (match owner t params with
     | `Main ->
       [ event (Text_delta { item = item_id params; text = delta params }) ]
     | `Subagent _ | `Unknown -> [])
  | "item/reasoning/summaryTextDelta" | "item/reasoning/textDelta" ->
    (match owner t params with
     | `Main ->
       Hash_set.add t.reasoning_streamed (item_id params);
       [ event (Thinking_delta { item = item_id params; text = delta params }) ]
     | `Subagent _ | `Unknown -> [])
  | "item/reasoning/summaryPartAdded" ->
    (match owner t params, int "summaryIndex" params with
     | `Main, Some index when index > 0 ->
       [ event (Thinking_delta { item = item_id params; text = "\n\n" }) ]
     | _ -> [])
  | "item/commandExecution/outputDelta" | "item/fileChange/outputDelta" ->
    (match owner t params with
     | `Unknown -> []
     | `Main | `Subagent _ ->
       [ event (Tool_update { id = item_id params; output = delta params }) ])
  | "item/mcpToolCall/progress" ->
    (match owner t params, str "message" params with
     | (`Main | `Subagent _), Some message ->
       [ event (Tool_update { id = item_id params; output = message ^ "\n" }) ]
     | _ -> [])
  | "turn/plan/updated" ->
    (match owner t params with
     | `Main ->
       let steps =
         List.filter_map (list "plan" params) ~f:(fun step ->
           match str "step" step with
           | None -> None
           | Some text ->
             let status : Plan.Status.t =
               match str "status" step with
               | Some "completed" -> Completed
               | Some "inProgress" -> In_progress
               | _ -> Pending
             in
             Some { Plan.Step.step = text; status })
       in
       (match steps with
        | [] -> []
        | steps ->
          [ event (Plan { explanation = str "explanation" params; steps }) ])
     | `Subagent _ | `Unknown -> [])
  | "thread/tokenUsage/updated" ->
    (match owner t params with
     | `Main -> token_usage t params
     | `Subagent _ | `Unknown -> [])
  | "error" ->
    (match owner t params with
     | `Main ->
       let error = Option.value (field "error" params) ~default:`Null in
       let kind, message = turn_error_of t error in
       if Option.value (bool "willRetry" params) ~default:false
       then [ event (Retrying { attempt = None; message }) ]
       else if Option.is_none t.turn
       then
         (* No turn to fail: the next one would forget it. *)
         [ event (Notice ("Codex: " ^ message)) ]
       else (
         t.turn_error <- Some (kind, message);
         [])
     | `Subagent _ | `Unknown -> [])
  | "account/rateLimits/updated" ->
    rate_limits t params;
    []
  | "model/rerouted" ->
    let from = Option.value (str "fromModel" params) ~default:"?" in
    let to_ = Option.value (str "toModel" params) ~default:"?" in
    let reason =
      Option.value_map (str "reason" params) ~default:"" ~f:(fun r ->
        [%string " (%{r})"])
    in
    [ event (Notice [%string "Codex switched from %{from} to %{to_}%{reason}"])
    ]
  | "warning" ->
    (match owner t params, str "message" params with
     | `Main, Some message -> [ event (Notice ("Codex: " ^ message)) ]
     | _ -> [])
  | "configWarning" ->
    (match str "summary" params with
     | Some summary -> [ event (Notice ("Codex: " ^ summary)) ]
     | None -> [])
  | "serverRequest/resolved" ->
    (match field "requestId" params with
     | None -> []
     | Some id ->
       let key = request_key id in
       (match take_server_request t key with
        | Some _ -> [ event (Request_withdrawn { request_id = key }) ]
        | None -> []))
  | _ -> []
;;

let server_request t ~id method_ params =
  let key = request_key id in
  let register request =
    Hashtbl.set t.server_requests ~key ~data:{ Pending.id; request }
  in
  let tool_id = str "itemId" params in
  let with_reason summary =
    match str "reason" params with
    | Some reason when not (String.is_empty reason) ->
      [%string "%{summary} (%{reason})"]
    | _ -> summary
  in
  match method_ with
  | "item/commandExecution/requestApproval" ->
    let decisions =
      Option.bind (field "availableDecisions" params) ~f:Json.list
    in
    register (Command_approval { decisions });
    let command = shell_command params in
    let network =
      match field "networkApprovalContext" params with
      | None | Some `Null -> None
      | Some n -> Some n
    in
    let network_host =
      Option.map network ~f:(fun n ->
        let host = Option.value (str "host" n) ~default:"a host" in
        match str "protocol" n with
        | Some protocol -> [%string "%{host} (%{protocol})"]
        | None -> host)
    in
    let stdin =
      Option.equal String.equal (str "kind" params) (Some "writeStdin")
    in
    let summary =
      match network_host with
      | Some host -> [%string "Network access to %{host} for: %{command}"]
      | None when stdin -> "Send input to the running command: " ^ command
      | None -> command
    in
    [ event
        (Approval_request
           { request_id =
               key
               (* Network access is asked for as such: the session's allow
                rules for the command do not cover it. *)
           ; tool_id = (if Option.is_some network then None else tool_id)
           ; kind =
               (if Option.is_some network then Other "network" else Command)
           ; summary = with_reason summary
           ; input =
               obj
                 [ Some ("command", `String command)
                 ; opt "cwd" (fun c -> `String c) (str "cwd" params)
                 ; opt "reason" (fun r -> `String r) (str "reason" params)
                 ; opt "network" Fn.id network
                 ; opt
                     "kind"
                     (fun k -> `String k)
                     (Option.filter (str "kind" params) ~f:(fun k ->
                        not (String.equal k "command")))
                 ]
           ; offers_always =
               offered decisions "acceptForSession"
               || Option.is_some (amendment decisions)
           })
    ]
  | "item/fileChange/requestApproval" ->
    register File_approval;
    let changes = Option.bind tool_id ~f:(Hashtbl.find t.file_changes) in
    let summary =
      Option.value_map changes ~default:"Change files" ~f:changes_title
    in
    [ event
        (Approval_request
           { request_id = key
           ; tool_id
           ; kind = File_change
           ; summary = with_reason summary
           ; input =
               obj
                 [ opt "changes" (fun c -> `Array c) changes
                 ; opt "reason" (fun r -> `String r) (str "reason" params)
                 ; opt "grantRoot" (fun r -> `String r) (str "grantRoot" params)
                 ]
           ; offers_always = true
           })
    ]
  | "item/permissions/requestApproval" ->
    register
      (Permissions
         { requested = Option.value (field "permissions" params) ~default:`Null
         });
    [ event
        (Approval_request
           { request_id = key
           ; tool_id
           ; kind = Other "permissions"
           ; summary = with_reason "Codex asks for more permissions"
           ; input = Option.value (field "permissions" params) ~default:`Null
           ; offers_always = true
           })
    ]
  | "item/tool/requestUserInput" ->
    let questions = list "questions" params in
    register
      (Question
         { question_ids = List.filter_map questions ~f:(fun q -> str "id" q) });
    let questions =
      List.map questions ~f:(fun q ->
        { User_question.question = Option.value (str "question" q) ~default:""
        ; header = str "header" q
        ; choices =
            List.filter_map (list "options" q) ~f:(fun o ->
              Option.map (str "label" o) ~f:(fun label ->
                { User_question.Choice.label
                ; description = str "description" o
                }))
        ; multiple = false
        })
    in
    [ event (User_question { request_id = key; questions }) ]
  | "item/tool/call" ->
    register Tool;
    [ Output.Tool_call
        { request_id = key
        ; call_id = Option.value (str "callId" params) ~default:key
        ; tool = Option.value (str "tool" params) ~default:""
        ; arguments = Option.value (field "arguments" params) ~default:`Null
        }
    ]
  | "mcpServer/elicitation/request" ->
    let server =
      Option.value (str "serverName" params) ~default:"an MCP server"
    in
    let meta = Option.value (field "_meta" params) ~default:`Null in
    let tool_approval =
      Option.equal
        String.equal
        (str "codex_approval_kind" meta)
        (Some "mcp_tool_call")
      && List.mem
           [ "form"; "openai/form"; "openaiForm" ]
           (Option.value (str "mode" params) ~default:"")
           ~equal:String.equal
    in
    let schema = field "requestedSchema" params in
    if tool_approval && String.equal server Injected_tools.server_name
    then
      (* prigh's own tools: prigh's approval runs when the call reaches it. *)
      [ Output.Send (reply id (mcp_accept ~schema ~always:false)) ]
    else if tool_approval
    then (
      register (Mcp_tool_approval { schema });
      let tool =
        List.find_map [ "tool_name"; "toolName"; "tool" ] ~f:(fun key ->
          str key meta)
        |> Option.value ~default:"tool"
      in
      let message = Option.value (str "message" params) ~default:"" in
      [ event
          (Approval_request
             { request_id = key
             ; tool_id
             ; kind = Mcp { server; tool }
             ; summary =
                 (if String.is_empty message
                  then [%string "Use %{server}'s %{tool}"]
                  else message)
             ; input = meta
             ; offers_always = true
             })
      ])
    else
      (* Nothing prigh can show: [cancel] leaves no decision behind, where
         [decline] reads as the user's refusal (which Codex may keep for
         the thread). *)
      [ Output.Send (reply id (`Object [ "action", `String "cancel" ]))
      ; event
          (Notice
             [%string
               "Codex's %{server} asked for input, which prigh cannot show \
                yet: cancelled"])
      ]
  | "account/chatgptAuthTokens/refresh" ->
    register Token_refresh;
    [ Output.Token_refresh
        { request_id = key
        ; previous_account_id = str "previousAccountId" params
        }
    ]
  | _ ->
    [ Output.Send
        (reply_error id ~code:(-32601) ("prigh does not handle " ^ method_))
    ]
;;

let reply_token_refresh t ~request_id tokens =
  match Hashtbl.find t.server_requests request_id with
  | Some { Pending.id; request = Token_refresh } ->
    Hashtbl.remove t.server_requests request_id;
    Some
      (match tokens with
       | Ok tokens -> reply id (`Object (Chatgpt_tokens.fields tokens))
       | Error e -> reply_error id ~code:(-32603) (Error.to_string_hum e))
  | Some _ | None -> None
;;

let model_info json =
  { Model_info.id =
      Option.first_some (str "model" json) (str "id" json)
      |> Option.value ~default:""
  ; display_name =
      Option.first_some (str "displayName" json) (str "model" json)
      |> Option.value ~default:""
  ; description = Option.value (str "description" json) ~default:""
  ; is_default = Option.value (bool "isDefault" json) ~default:false
  ; hidden = Option.value (bool "hidden" json) ~default:false
  ; efforts =
      List.filter_map
        (list "supportedReasoningEfforts" json)
        ~f:(str "reasoningEffort")
  ; default_effort = str "defaultReasoningEffort" json
  ; images =
      Option.is_none (field "inputModalities" json)
      || List.exists (list "inputModalities" json) ~f:(fun m ->
        Option.equal String.equal (Json.string m) (Some "image"))
  }
;;

let account result : Account.t =
  match field "account" result with
  | None | Some `Null ->
    if Option.value (bool "requiresOpenaiAuth" result) ~default:true
    then Logged_out
    else Not_needed
  | Some account ->
    (match str "type" account with
     | Some "apiKey" -> Api_key
     | Some "chatgpt" ->
       Chatgpt { email = str "email" account; plan = str "planType" account }
     | Some other -> Other other
     | None -> Other "unknown")
;;

let response t (kind : Request_kind.t) json =
  match field "error" json with
  | Some error ->
    let code = Option.value (int "code" error) ~default:0 in
    let message = Option.value (str "message" error) ~default:"error" in
    let failed = Output.Response (Failed { kind; code; message }) in
    (match kind with
     | Turn_start ->
       (match t.turn with
        | Some { id = None; _ } -> t.turn <- None
        | _ -> ());
       [ failed
       ; event
           (Turn_end
              { turn_id = None
              ; stop =
                  Failed
                    { kind = Other
                    ; message = "Codex refused the turn: " ^ message
                    }
              })
       ]
     | _ -> [ failed ])
  | None ->
    let result = Option.value (field "result" json) ~default:`Null in
    (match kind with
     | Initialize ->
       let user_agent = Option.value (str "userAgent" result) ~default:"" in
       [ Output.Send (notify "initialized")
       ; Response
           (Initialized
              { user_agent; version = version_of_user_agent user_agent })
       ]
     | Thread_start | Thread_resume | Thread_fork ->
       (match Option.bind (field "thread" result) ~f:(str "id") with
        | None ->
          [ Output.Response
              (Failed
                 { kind; code = 0; message = "Codex answered without a thread" })
          ]
        | Some thread_id ->
          let model = str "model" result in
          t.thread <- Some thread_id;
          t.thread_model <- model;
          t.turn <- None;
          t.last_total <- None;
          Hashtbl.clear t.subagents;
          [ event (Thread_started { thread_id; model })
          ; Response (Thread { kind; thread_id; model })
          ])
     | Turn_start ->
       (match Option.bind (field "turn" result) ~f:(str "id") with
        | None -> []
        | Some turn_id ->
          (match t.turn with
           | Some ({ id = None; _ } as current)
             when not (Hash_set.mem t.finished_turns turn_id) ->
             current.id <- Some turn_id
           | _ -> ());
          [ Output.Response (Turn_accepted { turn_id }) ])
     | Turn_steer ->
       [ Output.Response
           (Steered { turn_id = Option.value (str "turnId" result) ~default:"" })
       ]
     | Model_list ->
       [ Output.Response
           (Models
              { models = List.map (list "data" result) ~f:model_info
              ; next_cursor = str "nextCursor" result
              })
       ]
     | Account_read -> [ Output.Response (Account (account result)) ]
     | Rate_limits_read ->
       let snapshot = Option.value (field "rateLimits" result) ~default:`Null in
       let status = limits_of_snapshot snapshot in
       t.limits <- Some status;
       [ Output.Response (Rate_limits status) ]
     | Login -> [ Output.Response Logged_in ]
     | Turn_interrupt | Compact | Revert -> [ Output.Response (Done kind) ])
;;

let handle_line t line =
  match Json.parse line with
  | Error _ ->
    if String.is_empty (String.strip line)
    then []
    else
      [ event
          (Notice
             ("Codex wrote something that is not JSON: "
              ^ String.prefix line 200))
      ]
  | Ok json ->
    let params = Option.value (field "params" json) ~default:(`Object []) in
    (match str "method" json, field "id" json with
     | Some method_, Some id -> server_request t ~id method_ params
     | Some method_, None -> notification t method_ params
     | None, Some id ->
       (match Json.int id with
        | None -> []
        | Some id ->
          (match Hashtbl.find_and_remove t.pending id with
           | None -> []
           | Some kind -> response t kind json))
     | None, None -> [])
;;
