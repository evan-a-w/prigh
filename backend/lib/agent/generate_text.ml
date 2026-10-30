open! Core
open! Import

module Kind = struct
  type t =
    | Commit_message
    | Pr
    | Branch_name
    | Session_title
  [@@deriving sexp_of, equal, enumerate]

  let to_string = function
    | Commit_message -> "commit_message"
    | Pr -> "pr"
    | Branch_name -> "branch_name"
    | Session_title -> "session_title"
  ;;

  let of_string s =
    match List.find all ~f:(fun k -> String.equal (to_string k) s) with
    | Some k -> Ok k
    | None ->
      Or_error.errorf
        "unknown kind %S: generate_text drafts %s"
        s
        (String.concat ~sep:", " (List.map all ~f:to_string))
  ;;
end

module Target = struct
  type t =
    { dir : string option
    ; base : string option
    ; task : string option
    }
  [@@deriving sexp_of]

  let empty = { dir = None; base = None; task = None }
end

module Prompt = struct
  type t =
    { system : string
    ; user : string
    ; max_tokens : int
    }
  [@@deriving sexp_of]
end

module Result = struct
  type t =
    { text : string
    ; title : string option
    ; body : string option
    ; model : string
    ; cost_usd : float
    }
  [@@deriving sexp_of]

  let to_json t =
    `Object
      ([ "text", `String t.text ]
       @ Option.value_map t.title ~default:[] ~f:(fun s ->
         [ "title", `String s ])
       @ Option.value_map t.body ~default:[] ~f:(fun s -> [ "body", `String s ])
       @ [ "model", `String t.model
         ; "cost_usd", `Number (sprintf "%.15g" t.cost_usd)
         ])
  ;;
end

(* ---- cleaning -------------------------------------------------------- *)

(* A reply wrapped in one code fence is its inside. *)
let unfence text =
  let text = String.strip text in
  match String.split_lines text with
  | first :: (_ :: _ as rest) when String.is_prefix first ~prefix:"```" ->
    (match List.rev rest with
     | last :: inner when String.equal (String.strip last) "```" ->
       String.strip (String.concat ~sep:"\n" (List.rev inner))
     | _ -> text)
  | _ -> text
;;

let unquote s =
  String.strip s ~drop:(fun c ->
    Char.is_whitespace c || List.mem [ '"'; '\''; '`' ] c ~equal:Char.equal)
;;

let split_pr text =
  let text = unfence text in
  let lines = String.split_lines text in
  let lines =
    List.drop_while lines ~f:(fun l -> String.is_empty (String.strip l))
  in
  match lines with
  | [] -> "", ""
  | title :: body ->
    let title = String.strip title in
    let title = String.lstrip title ~drop:(Char.equal '#') |> String.strip in
    let title =
      List.fold
        [ "Title:"; "title:"; "**Title:**" ]
        ~init:title
        ~f:(fun t prefix ->
          Option.value_map
            (String.chop_prefix t ~prefix)
            ~default:t
            ~f:String.strip)
    in
    unquote title, String.strip (String.concat ~sep:"\n" body)
;;

let branch_name text =
  let line =
    unfence text
    |> String.split_lines
    |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
    |> Option.value ~default:""
    |> unquote
  in
  (* "Branch: feat/x" *)
  let line =
    match String.rsplit2 line ~on:':' with
    | Some (_, name) -> unquote name
    | None -> line
  in
  let line = String.lowercase line in
  let mapped =
    String.map line ~f:(fun c ->
      if
        Char.is_alphanum c
        || List.mem [ '/'; '-'; '.'; '_' ] c ~equal:Char.equal
      then c
      else '-')
  in
  let rec collapse s =
    let s' =
      String.substr_replace_all s ~pattern:"--" ~with_:"-"
      |> String.substr_replace_all ~pattern:"//" ~with_:"/"
      |> String.substr_replace_all ~pattern:".." ~with_:"."
      |> String.substr_replace_all ~pattern:"/-" ~with_:"/"
      |> String.substr_replace_all ~pattern:"-/" ~with_:"/"
    in
    if String.equal s s' then s else collapse s'
  in
  collapse mapped
  |> String.strip ~drop:(fun c ->
    List.mem [ '-'; '/'; '.' ] c ~equal:Char.equal)
  |> Fn.flip String.prefix 60
  |> String.rstrip ~drop:(fun c ->
    List.mem [ '-'; '/'; '.' ] c ~equal:Char.equal)
;;

let clean (kind : Kind.t) text =
  match kind with
  | Commit_message -> unfence text
  | Pr ->
    let title, body = split_pr text in
    if String.is_empty body then title else title ^ "\n\n" ^ body
  | Branch_name -> branch_name text
  | Session_title -> Naming.clean (unfence text)
;;

(* ---- context --------------------------------------------------------- *)

let host agent ~cancel ?dir request ~of_json =
  let result =
    Agent.host_call
      agent
      ~cancel
      ~name:Git_actions.op
      (Git_actions.Request.to_json ?dir request)
  in
  if result.is_error
  then Or_error.error_string result.text
  else
    Or_error.try_with (fun () ->
      of_json (Json.parse result.text |> Or_error.ok_exn))
;;

let section title body =
  if String.is_empty (String.strip body)
  then None
  else Some (sprintf "<%s>\n%s\n</%s>" title (String.strip body) title)
;;

let sections parts = String.concat ~sep:"\n\n" (List.filter_opt parts)

let conventions list =
  List.map list ~f:(fun (path, text) -> sprintf "From %s:\n%s" path text)
  |> String.concat ~sep:"\n\n"
;;

(* What the session is about: its goal, plan and name. *)
let session_context agent =
  let state = Agent.state agent in
  let session = Agent.session agent in
  [ Option.bind state.goal ~f:(fun g -> section "session_goal" g.text)
  ; Option.bind state.plan ~f:(fun (p : Plan.t) ->
      section
        "session_plan"
        (String.concat
           ~sep:"\n"
           (Option.to_list p.explanation
            @ List.map p.steps ~f:(fun s ->
              sprintf "- [%s] %s" (Plan.Status.to_string s.status) s.step))))
  ; Option.bind
      (Option.first_some (Session.name session) (Session.description session))
      ~f:(section "session_title")
  ]
;;

let last_request agent =
  List.rev (Agent.messages agent)
  |> List.find_map ~f:(function
    | Message.User u ->
      let text = String.strip (Message.User.body u) in
      Option.some_if (not (String.is_empty text)) text
    | Assistant _ | Tool_result _ -> None)
;;

module Spec = struct
  type t =
    { instructions : string
    ; max_tokens : int
    ; gather : Agent.t -> Target.t -> cancel:Cancellation.t -> string Or_error.t
    }
end

let commit_spec =
  { Spec.instructions =
      "Write a git commit message for the staged changes. Follow the style of \
       the repository's recent commits (their subjects are listed): their \
       tense, capitalisation, prefixes such as \"fix:\" or \"web:\", and \
       length, and its commit guidance if given. Format: a subject line of at \
       most 72 characters; then, when the change needs explaining, a blank \
       line and a body wrapped at 72 columns saying what changed and why. \
       Reply with the message only: no code fences, no preamble."
  ; max_tokens = 1_000
  ; gather =
      (fun agent target ~cancel ->
        Or_error.map
          (host
             agent
             ~cancel
             ?dir:target.dir
             Commit_context
             ~of_json:[%of_jsonaf: Git_actions.Commit_context.t])
          ~f:(fun (c : Git_actions.Commit_context.t) ->
            sections
              ([ Option.bind c.branch ~f:(section "branch")
               ; section "recent_commits" (String.concat ~sep:"\n" c.recent)
               ; section "commit_guidance" (conventions c.conventions)
               ]
               @ session_context agent
               @ [ section "staged_diff" c.diff ])))
  }
;;

let pr_spec =
  { Spec.instructions =
      "Write a pull request for the branch: its title on the first line (at \
       most 72 characters, no \"Title:\"), a blank line, then the body in \
       Markdown saying what changed and why, how it was tested when the \
       commits say, and anything left for later. Follow the repository's pull \
       request guidance, and fill in its template when one is given. Reply \
       with the title and body only: no code fence around them, no preamble."
  ; max_tokens = 2_500
  ; gather =
      (fun agent target ~cancel ->
        Or_error.map
          (host
             agent
             ~cancel
             ?dir:target.dir
             (Pr_context { base = target.base })
             ~of_json:[%of_jsonaf: Git_actions.Pr_context.t])
          ~f:(fun (c : Git_actions.Pr_context.t) ->
            sections
              ([ section
                   "branch"
                   (sprintf
                      "%s, into %s"
                      (Option.value c.branch ~default:"HEAD")
                      c.base)
               ; section
                   "commits"
                   (List.map c.commits ~f:(fun (subject, body) ->
                      if String.is_empty body
                      then "- " ^ subject
                      else sprintf "- %s\n\n%s\n" subject body)
                    |> String.concat ~sep:"\n")
               ; section "pull_request_guidance" (conventions c.conventions)
               ; Option.bind c.template ~f:(fun (path, text) ->
                   section "pull_request_template" (sprintf "%s:\n%s" path text))
               ]
               @ session_context agent
               @ [ section "diff" c.diff ])))
  }
;;

let branch_spec =
  { Spec.instructions =
      "Name a git branch for the task: lowercase words joined by hyphens, at \
       most 40 characters, with a prefix such as feat/, fix/ or docs/ when one \
       fits. Reply with the name only."
  ; max_tokens = 60
  ; gather =
      (fun agent target ~cancel:_ ->
        let task =
          List.find_map
            [ target.task
            ; Option.map (Agent.state agent).goal ~f:(fun g -> g.text)
            ; last_request agent
            ]
            ~f:
              (Option.filter ~f:(fun s ->
                 not (String.is_empty (String.strip s))))
        in
        match task with
        | Some task ->
          Ok (sections [ section "task" (String.prefix task 4_000) ])
        | None ->
          Or_error.error_string
            "say what the branch is for first: type the task, then Generate")
  }
;;

let title_spec =
  { Spec.instructions = Session_description.instructions
  ; max_tokens = 60
  ; gather =
      (fun agent _ ~cancel:_ ->
        match Agent.messages agent with
        | [] ->
          Or_error.error_string
            "the session has no conversation to name yet: send a message first"
        | messages -> Ok (Session_description.conversation messages))
  }
;;

let spec : Kind.t -> Spec.t = function
  | Commit_message -> commit_spec
  | Pr -> pr_spec
  | Branch_name -> branch_spec
  | Session_title -> title_spec
;;

let prompt_with agent kind target ~cancel =
  let spec = spec kind in
  Or_error.map (spec.gather agent target ~cancel) ~f:(fun user ->
    { Prompt.system = spec.instructions; user; max_tokens = spec.max_tokens })
;;

let prompt agent kind target =
  prompt_with agent kind target ~cancel:Cancellation.never
;;

let run agent kind target ~cancel ~on_delta =
  let open Or_error.Let_syntax in
  let%bind model =
    match kind with
    | Kind.Session_title -> Agent.naming_model agent
    | Commit_message | Pr | Branch_name -> Agent.generate_model agent
  in
  let%bind prompt = prompt_with agent kind target ~cancel in
  let%map reply, cost_usd =
    Agent.complete
      agent
      ~model
      ~system:prompt.system
      ~user:prompt.user
      ~max_tokens:prompt.max_tokens
      ~cancel
      ~on_delta
  in
  let text = clean kind reply in
  let title, body =
    match kind with
    | Pr ->
      let title, body = split_pr text in
      Some title, Some body
    | Commit_message | Branch_name | Session_title -> None, None
  in
  { Result.text; title; body; model = Model.key model; cost_usd }
;;
