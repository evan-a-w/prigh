open! Core
open! Import

module Role = struct
  type t =
    | Explore
    | Worker
  [@@deriving sexp_of, enumerate]

  let to_string = function
    | Explore -> "explore"
    | Worker -> "worker"
  ;;

  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

  let tools = function
    | Explore ->
      [ Tool_read.tool; Tool_ls.tool; Tool_grep.tool; Tool_find.tool ]
    | Worker -> Tools.all
  ;;

  let instructions = function
    | Explore ->
      "You are a read-only research subagent. Investigate the task using the \
       tools, then reply with a concise, factual report: relevant files (with \
       paths), how things work, and anything surprising. Do not propose large \
       plans; report what you found."
    | Worker ->
      "You are a worker subagent. Complete the task fully, verify your work \
       (build/tests where possible), then reply with a concise report of what \
       you changed (files and a summary) and anything the caller must know."
  ;;
end

let max_turns = 40

let spec =
  { Tool_spec.name = "subagent"
  ; description =
      "Delegate a self-contained task to a subagent with its own context. Use \
       'explore' for read-only investigation of the codebase and 'worker' for \
       tasks that change files or run commands. The subagent cannot see this \
       conversation: include all necessary context in the task. Returns the \
       subagent's final report."
  ; parameters =
      Tool_args.schema
        ~required:[ "role"; "task" ]
        [ "role", `String, "explore | worker"
        ; ( "task"
          , `String
          , "Complete description of the task, including relevant paths and \
             constraints" )
        ]
  }
;;

let create ~provider ~current_model ~current_thinking ~home =
  let run (context : Tool.Context.t) args =
    let role =
      match Role.of_string (Tool_args.string args "role") with
      | Some r -> r
      | None -> raise (Tool_args.Invalid "role must be explore or worker")
    in
    let task = Tool_args.string args "task" in
    let tools = Role.tools role in
    let config =
      { Agent_loop.Config.model = current_model ()
      ; thinking = current_thinking ()
      ; system =
          Some
            (Role.instructions role
             ^ "\n\n"
             ^ System_prompt.build
                 ~cwd:context.cwd
                 ~home
                 ~tools:(Tools.specs tools)
                 ())
      ; tools
      ; max_turns = Some max_turns
      ; max_tokens = None
      ; retries = Agent_loop.Config.default_retries
      }
    in
    let turns = ref 0 in
    let added =
      Agent_loop.run
        ~env:context.env
        ~provider
        ~config
        ~cwd:context.cwd
        ~cancel:context.cancel
        ~emit:(function
          | Turn_start -> incr turns
          | Tool_start call ->
            context.on_output
              (sprintf
                 "[%s] %s %s\n"
                 (Role.to_string role)
                 call.name
                 (String.prefix call.arguments 120))
          | _ -> ())
        ~context:[]
        ~prompts:[ Message.user task ]
        ()
    in
    let last_assistant =
      List.rev added
      |> List.find_map ~f:(function
        | Message.Assistant a -> Some a
        | User _ | Tool_result _ -> None)
    in
    match last_assistant with
    | None -> Tool.Result.error "subagent produced no reply"
    | Some a ->
      let report = Message.Assistant.text a in
      (match a.stop_reason with
       | Error e ->
         Tool.Result.error (sprintf "subagent failed: %s\n%s" e report)
       | Aborted -> Tool.Result.error ("subagent aborted\n" ^ report)
       | End_turn | Tool_use | Length ->
         if
           !turns >= max_turns
           && not (List.is_empty (Message.Assistant.tool_calls a))
         then
           Tool.Result.error
             (sprintf
                "subagent hit the %d-turn limit without finishing\n%s"
                max_turns
                report)
         else
           Tool.Result.ok (sprintf "%s\n[subagent used %d turns]" report !turns))
  in
  { Tool.spec; run }
;;
