open! Core
open! Import

module Command = struct
  type t =
    { name : string
    ; description : string
    ; hint : string option
    }

  let to_json t =
    `Object
      ([ "name", `String t.name; "description", `String t.description ]
       @ Option.value_map t.hint ~default:[] ~f:(fun hint ->
         [ "input", `Object [ "hint", `String hint ] ]))
  ;;
end

let c ?hint name description = { Command.name; description; hint }

let builtin =
  [ c "compact" ~hint:"[instructions]" "summarise older messages to free context"
  ; c
      "goal"
      ~hint:"[text|off|resume]"
      "set a goal the agent keeps working toward until it is met; show, clear \
       or resume it"
  ; c
      "remind"
      ~hint:"[stop [N] TEXT | every N TEXT | rm NAME | off]"
      "remind the agent when it ends its turn (stop) or every N turns; list or \
       remove reminders"
  ; c
      "budget"
      ~hint:"[dollars|off]"
      "show or set the session's cost budget: no turn starts once it is spent"
  ; c "model" ~hint:"[model]" "show or set the model (id, name or prefix)"
  ; c "thinking" ~hint:"[off|on|low|high|max]" "show or set the thinking level"
  ; c
      "plan"
      ~hint:"[on|off]"
      "toggle plan mode: the model investigates and plans, changing no files"
  ; c
      "review"
      ~hint:"[base] [instructions]"
      "review the uncommitted changes, or those against a base branch"
  ; c "init" "write or improve the project's AGENTS.md"
  ; c "btw" ~hint:"question" "ask a side question without adding it to the session"
  ; c "name" ~hint:"text" "set the session name"
  ; c "diff" ~hint:"[path]" "show the uncommitted changes (git diff)"
  ]
;;

module Action = struct
  type t =
    | Call of
        { method_ : string
        ; params : (string * Json.t) list
        ; turn : bool
        ; ack : (Json.t -> string) [@sexp.opaque]
        }
    | Reply of string
    | Prompt
  [@@deriving sexp_of]
end

let str s = `String s
let field name json = Option.value (Json.member name json) ~default:`Null
let string_field name json = Option.value (Json.string (field name json)) ~default:""

let call ?(params = []) ?(turn = false) method_ ack =
  Action.Call { method_; params; turn; ack }
;;

let const text _ = text

let usd_to_string usd = sprintf "$%.2f" usd

let describe_goal state =
  match field "goal" state with
  | `Object _ as goal ->
    sprintf
      "goal (%s, %s/%s continuations): %s%s"
      (string_field "status" goal)
      (Json.to_string (field "continuations" goal))
      (Json.to_string (field "max_continuations" goal))
      (string_field "text" goal)
      (match Json.string (field "summary" goal), Json.string (field "reason" goal) with
       | Some s, _ | None, Some s -> "\n" ^ s
       | None, None -> "")
  | _ -> "no goal; /goal TEXT sets one"
;;

let describe_budget state =
  let spent = Option.value (Json.float (field "cost_usd" state)) ~default:0. in
  match Json.float (field "budget_usd" state) with
  | Some budget ->
    sprintf "budget %s, %s spent" (usd_to_string budget) (usd_to_string spent)
  | None ->
    sprintf "no budget (%s spent); /budget DOLLARS sets one" (usd_to_string spent)
;;

let describe_reminder r =
  let trigger =
    match Json.string (field "on" r), Json.int (field "every_turns" r) with
    | Some "stop", _ ->
      sprintf
        "on stop (%s/%s)"
        (Json.to_string (field "repeats" r))
        (Json.to_string (field "max_repeats" r))
    | _, Some n -> sprintf "every %d turns" n
    | _ -> "?"
  in
  sprintf "%s · %s · %s" (string_field "name" r) trigger (string_field "text" r)
;;

let remind_usage =
  "usage: /remind stop [N] TEXT | /remind every N TEXT | /remind rm NAME | \
   /remind off"
;;

let remind ~state rest =
  let words = String.split rest ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
  let text_after n =
    String.strip (String.concat ~sep:" " (List.drop words n))
  in
  let add params =
    call "add_reminder" ~params (fun r -> "reminder added: " ^ describe_reminder r)
  in
  match words with
  | [] ->
    (match Json.list (field "reminders" state) with
     | Some (_ :: _ as reminders) ->
       Action.Reply (String.concat ~sep:"\n" (List.map reminders ~f:describe_reminder))
     | Some [] | None -> Reply ("no reminders; " ^ remind_usage))
  | [ "off" ] -> call "clear_reminders" (const "reminders removed")
  | [ "rm"; name ] ->
    call "remove_reminder" ~params:[ "name", str name ] (const ("removed " ^ name))
  | "stop" :: n :: _ when Option.is_some (Int.of_string_opt n) && List.length words > 2 ->
    add
      [ "text", str (text_after 2)
      ; "on", str "stop"
      ; "max_repeats", `Number n
      ]
  | "stop" :: _ :: _ -> add [ "text", str (text_after 1); "on", str "stop" ]
  | "every" :: n :: _ :: _ when Option.is_some (Int.of_string_opt n) ->
    add [ "text", str (text_after 2); "every_turns", `Number n ]
  | _ -> Reply remind_usage
;;

let parse ~state text =
  let text = String.strip text in
  match String.chop_prefix text ~prefix:"/" with
  | None -> Action.Prompt
  | Some command ->
    let name, rest =
      match String.lsplit2 command ~on:' ' with
      | Some (name, rest) -> name, String.strip rest
      | None -> command, ""
    in
    let opt_param key = if String.is_empty rest then [] else [ key, str rest ] in
    (match name, rest with
     | "compact", _ ->
       call "compact" ~params:(opt_param "instructions") (const "context compacted")
     | "goal", "" -> Reply (describe_goal state)
     | "goal", "off" -> call "clear_goal" (const "goal cleared")
     | "goal", "resume" -> call "resume_goal" ~turn:true (const "")
     | "goal", text -> call "set_goal" ~params:[ "text", str text ] ~turn:true (const "")
     | "remind", rest -> remind ~state rest
     | "budget", "" -> Reply (describe_budget state)
     | "budget", ("off" | "none") ->
       call "set_budget" ~params:[ "usd", `Null ] (const "budget removed")
     | "budget", amount ->
       (match Float.of_string_opt (String.chop_prefix_if_exists amount ~prefix:"$") with
        | Some usd when Float.( > ) usd 0. ->
          call
            "set_budget"
            ~params:[ "usd", `Number (Float.to_string usd) ]
            (const ("budget set to " ^ usd_to_string usd))
        | _ -> Reply "usage: /budget DOLLARS (a positive number) or /budget off")
     | "model", "" ->
       let model = field "model" state in
       Reply
         (sprintf
            "model: %s (%s)"
            (string_field "key" model)
            (string_field "name" model))
     | "model", model ->
       call "set_model" ~params:[ "model", str model ] (const ("model set to " ^ model))
     | "thinking", "" -> Reply ("thinking: " ^ string_field "thinking" state)
     | "thinking", level ->
       call
         "set_thinking"
         ~params:[ "thinking", str level ]
         (const ("thinking set to " ^ level))
     | "plan", ("" | "on" | "off") ->
       let on =
         match rest with
         | "on" -> true
         | "off" -> false
         | _ -> not (Json.exactly_equal (field "plan_mode" state) `True)
       in
       call
         "set_mode"
         ~params:[ "plan_mode", if on then `True else `False ]
         (const (if on then "plan mode on" else "plan mode off"))
     | "plan", _ -> Reply "usage: /plan [on|off]"
     | "review", _ -> call "review" ~params:(opt_param "args") ~turn:true (const "")
     | "init", "" -> call "init" ~turn:true (const "")
     | "btw", "" -> Reply "usage: /btw QUESTION"
     | "btw", question ->
       call "btw" ~params:[ "question", str question ] (string_field "text")
     | "name", "" -> Reply "usage: /name TEXT"
     | "name", name ->
       call "set_session_name" ~params:[ "name", str name ] (const ("session named " ^ name))
     | "diff", _ ->
       call "git_diff" ~params:(opt_param "path") (fun result ->
         match string_field "diff" result with
         | "" -> "no uncommitted changes"
         | diff ->
           sprintf
             "```diff\n%s%s```%s"
             diff
             (if String.is_suffix diff ~suffix:"\n" then "" else "\n")
             (if Json.exactly_equal (field "truncated" result) `True
              then "\n(truncated)"
              else ""))
     | _ -> Prompt)
;;
