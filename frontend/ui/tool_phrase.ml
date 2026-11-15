open! Core
open! Import

module Step = struct
  type t =
    | Call of P.Tool_call.t
    | Thinking
    | Report
end

module Phrase = struct
  type t =
    { kind : string (** steps of one kind are counted together *)
    ; doing : string
    ; doing_path : string option
      (** with the call's [path]: ["Editing"] reads ["Editing src/foo.ml"] *)
    ; one : string
    ; many : string (** [#] is the count *)
    ; by_path : bool (** counts the files, not the calls *)
    }

  let v ?doing_path ?(by_path = false) kind ~doing ~one ~many =
    { kind; doing; doing_path; one; many; by_path }
  ;;

  let count t n =
    if n = 1
    then t.one
    else String.substr_replace_all t.many ~pattern:"#" ~with_:(Int.to_string n)
  ;;
end

let command =
  Phrase.v "command" ~doing:"Running a command" ~one:"ran a command"
    ~many:"ran # commands"
;;

let search =
  Phrase.v "search" ~doing:"Searching the code" ~one:"searched the code"
    ~many:"searched the code # times"
;;

let edit =
  Phrase.v
    "edit"
    ~by_path:true
    ~doing:"Editing a file"
    ~doing_path:"Editing"
    ~one:"edited a file"
    ~many:"edited # files"
;;

let web_search =
  Phrase.v "web_search" ~doing:"Searching the web" ~one:"searched the web"
    ~many:"searched the web # times"
;;

let table =
  [ [ "bash"; "shell" ], command
  ; ( [ "read" ]
    , Phrase.v "read" ~by_path:true ~doing:"Reading a file" ~one:"read a file"
        ~many:"read # files" )
  ; ( [ "write" ]
    , Phrase.v
        "write"
        ~by_path:true
        ~doing:"Writing a file"
        ~doing_path:"Writing"
        ~one:"wrote a file"
        ~many:"wrote # files" )
  ; [ "edit"; "apply_patch" ], edit
  ; ( [ "ls" ]
    , Phrase.v "ls" ~by_path:true ~doing:"Listing a directory"
        ~one:"listed a directory" ~many:"listed # directories" )
  ; [ "grep"; "find" ], search
  ; ( [ "web_fetch" ]
    , Phrase.v "web_fetch" ~doing:"Fetching a page" ~one:"fetched a page"
        ~many:"fetched # pages" )
  ; [ "web_search" ], web_search
  ; ( [ "update_plan" ]
    , Phrase.v "plan" ~doing:"Updating the plan" ~one:"updated the plan"
        ~many:"updated the plan # times" )
  ; ( [ "ask_user" ]
    , Phrase.v "ask_user" ~doing:"Asking you" ~one:"asked you a question"
        ~many:"asked you # questions" )
  ; ( [ "render_ui" ]
    , Phrase.v "render_ui" ~doing:"Showing you a view" ~one:"showed a view"
        ~many:"showed # views" )
  ; ( [ "subagent" ]
    , Phrase.v "subagent" ~doing:"Starting a subagent"
        ~one:"started a subagent" ~many:"started # subagents" )
  ; ( [ "subagent_continue" ]
    , Phrase.v "subagent_continue" ~doing:"Continuing a subagent"
        ~one:"continued a subagent" ~many:"continued # subagents" )
  ; ( [ "subagent_status" ]
    , Phrase.v "subagent_status" ~doing:"Checking on subagents"
        ~one:"checked on subagents" ~many:"checked on subagents # times" )
  ; ( [ "subagent_cancel" ]
    , Phrase.v "subagent_cancel" ~doing:"Cancelling a subagent"
        ~one:"cancelled a subagent" ~many:"cancelled # subagents" )
  ; ( [ "subagent_message" ]
    , Phrase.v "subagent_message" ~doing:"Messaging a subagent"
        ~one:"messaged a subagent" ~many:"messaged subagents # times" )
  ; ( [ "job_status" ]
    , Phrase.v "job_status" ~doing:"Checking on jobs" ~one:"checked on jobs"
        ~many:"checked on jobs # times" )
  ; ( [ "job_output" ]
    , Phrase.v "job_output" ~doing:"Reading a job's output"
        ~one:"read a job's output" ~many:"read jobs' output # times" )
  ; ( [ "job_input" ]
    , Phrase.v "job_input" ~doing:"Sending input to a job"
        ~one:"sent input to a job" ~many:"sent input to jobs # times" )
  ; ( [ "job_kill" ]
    , Phrase.v "job_kill" ~doing:"Stopping a job" ~one:"stopped a job"
        ~many:"stopped # jobs" )
  ; ( [ "goal_complete" ]
    , Phrase.v "goal" ~doing:"Completing the goal" ~one:"completed the goal"
        ~many:"completed the goal # times" )
  ; ( [ "memory" ]
    , Phrase.v "memory" ~doing:"Using its memory" ~one:"used its memory"
        ~many:"used its memory # times" )
  ; ( [ "custom_instructions" ]
    , Phrase.v "custom_instructions" ~doing:"Updating your instructions"
        ~one:"updated your instructions"
        ~many:"updated your instructions # times" )
  ; ( [ "reply_to_comment" ]
    , Phrase.v "reply_to_comment" ~doing:"Replying to a comment"
        ~one:"replied to a comment" ~many:"replied to # comments" )
  ; ( [ "sessions" ]
    , Phrase.v "sessions" ~doing:"Looking through sessions"
        ~one:"looked through sessions"
        ~many:"looked through sessions # times" )
  ; ( [ "ask_session" ]
    , Phrase.v "ask_session" ~doing:"Asking another session"
        ~one:"asked another session" ~many:"asked other sessions # times" )
  ; ( [ "answer_session" ]
    , Phrase.v "answer_session" ~doing:"Answering another session"
        ~one:"answered another session" ~many:"answered # questions" )
  ; ( [ "worktree" ]
    , Phrase.v "worktree" ~doing:"Managing worktrees"
        ~one:"managed worktrees" ~many:"managed worktrees # times" )
  ; ( [ "pull_request" ]
    , Phrase.v "pull_request" ~doing:"Working on a pull request"
        ~one:"worked on a pull request"
        ~many:"worked on pull requests # times" )
  ; ( [ "issue" ]
    , Phrase.v "issue" ~doing:"Working on an issue" ~one:"worked on an issue"
        ~many:"worked on issues # times" )
  ; ( [ "repo" ]
    , Phrase.v "repo" ~doing:"Checking the repository"
        ~one:"checked the repository" ~many:"checked the repository # times"
    )
  ; ( [ "on_host" ]
    , Phrase.v "on_host" ~doing:"Listing the machines"
        ~one:"listed the machines" ~many:"listed the machines # times" )
  ; ( [ "computer" ]
    , Phrase.v "computer" ~doing:"Using the computer"
        ~one:"used the computer" ~many:"used the computer # times" )
  ]
;;

let by_name =
  String.Map.of_alist_exn
    (List.concat_map table ~f:(fun (names, phrase) ->
       List.map names ~f:(fun name -> name, phrase)))
;;

let known = Map.keys by_name

let thinking =
  Phrase.v "thinking" ~doing:"Thinking" ~one:"thought" ~many:"thought"
;;

let report =
  Phrase.v "report" ~doing:"Reading a report" ~one:"got a report"
    ~many:"got # reports"
;;

let generic name =
  Phrase.v
    ("tool:" ^ name)
    ~doing:("Using " ^ name)
    ~one:("used " ^ name)
    ~many:(sprintf "used %s # times" name)
;;

(* An [on_host] call is the call it runs. *)
let call (c : P.Tool_call.t) =
  let c =
    match Tool_summary.on_host c with
    | Some (_, inner) -> inner
    | None -> c
  in
  ( Option.value (Map.find by_name c.name) ~default:(generic c.name)
  , Tool_args.string (Tool_args.of_call c) "path" )
;;

let phrase : Step.t -> Phrase.t * string option = function
  | Call c -> call c
  | Thinking -> thinking, None
  | Report -> report, None
;;

let doing step =
  match phrase step with
  | { doing_path = Some verb; _ }, Some path
    when not (String.is_empty (String.strip path)) -> verb ^ " " ^ path
  | p, _ -> p.doing
;;

let did steps =
  let phrases = List.map steps ~f:phrase in
  let worked =
    List.filter phrases ~f:(fun ((p : Phrase.t), _) ->
      not (String.equal p.kind thinking.kind))
  in
  let phrases = if List.is_empty worked then phrases else worked in
  (* Kinds in the order they first appear, with what each touched. *)
  let kinds =
    List.fold phrases ~init:[] ~f:(fun kinds ((p : Phrase.t), path) ->
      let key = if p.by_path then path else None in
      match List.Assoc.find kinds ~equal:String.equal p.kind with
      | Some _ ->
        List.map kinds ~f:(fun (kind, (p', keys)) ->
          if String.equal kind p.kind then kind, (p', key :: keys) else kind, (p', keys))
      | None -> kinds @ [ p.kind, (p, [ key ]) ])
  in
  let parts =
    List.map kinds ~f:(fun (_, ((p : Phrase.t), keys)) ->
      let n =
        if p.by_path
        then (
          let paths = List.filter_opt keys in
          List.length (List.dedup_and_sort paths ~compare:String.compare)
          + List.count keys ~f:Option.is_none)
        else List.length keys
      in
      Phrase.count p n)
  in
  String.capitalize (String.concat ~sep:", " parts)
;;

let duration span =
  let s = Float.iround_down_exn (Time_ns.Span.to_sec span) in
  if s < 1
  then "<1s"
  else if s < 60
  then sprintf "%ds" s
  else if s < 3600
  then if s % 60 = 0 then sprintf "%dm" (s / 60) else sprintf "%dm %ds" (s / 60) (s % 60)
  else if s / 60 % 60 = 0
  then sprintf "%dh" (s / 3600)
  else sprintf "%dh %dm" (s / 3600) (s / 60 % 60)
;;
