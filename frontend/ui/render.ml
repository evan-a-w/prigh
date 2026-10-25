open! Core
open! Import
open App.Model

let gray = Style.fg Gray
let dim = Style.dim Style.plain
let span ?(style = Style.plain) text = { Content.Span.text; style }

let spinner_frames = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let format_tokens = App.format_tokens

let agent_symbol (m : App.Model.t) (a : Agent_view.t) =
  match a.status with
  | Agent_view.Status.Running ->
    spinner_frames.(m.spinner % Array.length spinner_frames)
  | Agent_view.Status.Done _ -> "✓"
  | Agent_view.Status.Failed _ -> "✗"
;;

let doing m =
  Option.value (Activity.brief (App.Model.activity m)) ~default:"working"
;;

(* The status line is quiet gray; yellow marks what wants attention (the mode
   you are in, running work, warm context) and red what is wrong. *)
let attention = Style.fg Yellow

let level_style : Loop_status.Level.t -> Style.t = function
  | Normal -> gray
  | Warn -> attention
  | Over -> Style.fg Red
;;

let context_style percent =
  if percent >= 80
  then Style.fg Red
  else if percent >= 50
  then attention
  else gray
;;

let agent_marker text ~active =
  let text = if active then "[" ^ text ^ "]" else text in
  if active
  then [ span ~style:(Style.invert (Style.bold Style.plain)) text ]
  else [ span ~style:gray text ]
;;

(** The M3 agent strip, folded into the status line as [agents:[main] 1⠋ 2✓]. *)
let agents_part (m : App.Model.t) : Content.Line.t option =
  let background = (not (App.Model.running m)) && App.Model.agents_running m in
  if List.is_empty m.agents
  then (
    (* Running agents whose events this client missed (e.g. it reconnected). *)
    let running =
      Option.value_map m.state ~default:0 ~f:(fun s ->
        List.count s.subagents ~f:(fun a -> a.running))
    in
    if running = 0
    then None
    else
      Some
        [ span ~style:(Style.bold Style.plain) "agents:"
        ; span (sprintf "%d running" running)
        ])
  else (
    let main =
      agent_marker
        "main"
        ~active:
          (match m.focus with
           | `Main -> true
           | `Agent _ -> false)
    in
    let is_focused id =
      match m.focus with
      | `Agent fid -> String.equal id fid
      | `Main -> false
    in
    let agents =
      List.mapi m.agents ~f:(fun i (a : Agent_view.t) ->
        agent_marker
          (sprintf "%d%s" (i + 1) (agent_symbol m a))
          ~active:(is_focused a.id))
    in
    Some
      ([ span ~style:(Style.bold Style.plain) "agents:" ]
       @ main
       @ List.concat_map agents ~f:(fun a -> span " " :: a)
       @ if background then [ span ~style:(Style.fg Yellow) " bg" ] else []))
;;

let jobs_part (m : App.Model.t) : Content.Line.t option =
  let running =
    Option.value_map m.state ~default:0 ~f:(fun s ->
      List.count s.jobs ~f:(fun (j : P.State.Job.t) -> j.running))
  in
  if running = 0
  then None
  else (
    let idle = not (App.Model.running m) in
    Some
      ([ span ~style:(Style.bold Style.plain) "jobs:"
       ; span (Int.to_string running)
       ]
       @
       if idle
       then
         [ span
             ~style:(Style.fg Yellow)
             (" " ^ spinner_frames.(m.spinner % Array.length spinner_frames))
         ]
       else []))
;;

(* A hint of keys ("A · B · C") cut to [width] by dropping whole keys from
   the end, the least important; cut short only when the first does not
   fit. *)
let fit_keys text ~width =
  let keys =
    String.Search_pattern.split_on (String.Search_pattern.create " · ") text
  in
  let rec take acc = function
    | [] -> acc
    | key :: rest ->
      let next = if String.is_empty acc then key else acc ^ " · " ^ key in
      if Text_width.string next <= width then take next rest else acc
  in
  match take "" keys with
  | "" -> Text_width.truncate text ~width
  | fitted -> fitted
;;

(* Picker keys beyond the status line's, on the title row. *)
let picker_keys (kind : Mode.Picker_kind.t) =
  match kind with
  | Sessions _ ->
    Some "Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named"
  | _ -> None
;;

let sign_in_keys = "Tab next field · Enter signs in · Esc closes"

let pr_form_keys = "Enter creates · Esc cancels"

let disconnected_hint (m : App.Model.t) =
  match m.mode with
  | Sign_in _ -> sign_in_keys
  | _ ->
    sprintf
      "not signed in%s · /signin signs in · /connect switches server"
      (match Option.bind m.server ~f:(fun s -> s.address) with
       | Some address -> " to " ^ address
       | None -> "")
;;

let mode_hint (m : App.Model.t) : Content.Line.t option =
  match m.connection with
  | Disconnected _ -> Some [ span ~style:attention (disconnected_hint m) ]
  | Switching _ -> Some [ span ~style:attention "connecting…" ]
  | Reconnecting { attempt; delay_ms; _ } ->
    Some
      [ span
          ~style:(Style.fg Red)
          (sprintf
             "backend gone · retry %d in %gs"
             attempt
             (Float.of_int delay_ms /. 1000.))
      ]
  | Connected ->
    (match m.state with
     | None -> None
     | Some s ->
       (match m.mode with
        | Picker { kind = Scoped_models; _ } ->
          Some
            [ span
                ~style:attention
                "Space toggle · Ctrl+A all · Ctrl+X none · Enter save"
            ]
        | Picker { kind = Models _; _ } ->
          Some
            [ span
                ~style:attention
                "Enter selects · Esc closes · Ctrl+N logged-in only"
            ]
        | Picker { kind = Sessions selection; picker } ->
          let marked =
            match Session_selection.marked_summary selection picker with
            | Some marked -> marked ^ " · "
            | None -> ""
          in
          Some
            [ span
                ~style:attention
                (marked
                 ^ "Enter switches · Tab marks · Ctrl+D deletes · Esc closes")
            ]
        | Picker { kind = Agents; _ } ->
          Some
            [ span
                ~style:attention
                "Enter focuses · Esc closes · Ctrl+D cancels"
            ]
        | Picker { kind = Jobs; _ } ->
          Some
            [ span
                ~style:attention
                "Enter shows output · Esc closes · Ctrl+D kills"
            ]
        | Picker { kind = Queue; _ } ->
          Some
            [ span ~style:attention "Enter edits · Esc closes · Ctrl+D removes"
            ]
        | Picker { kind = Skills; _ } ->
          Some
            [ span
                ~style:attention
                "Enter puts /skill:NAME in the editor · Esc closes"
            ]
        | Picker { kind = Reminders; _ } ->
          Some
            [ span
                ~style:attention
                "Enter shows it · Esc closes · Ctrl+D removes"
            ]
        | Picker { kind = Worktrees _; _ } ->
          Some
            [ span
                ~style:attention
                "Enter opens · Ctrl+D removes · Ctrl+N in a new session · Esc \
                 closes"
            ]
        | Picker { kind = Snippets; _ } ->
          Some
            [ span
                ~style:attention
                "Enter inserts the text at the cursor · Esc closes"
            ]
        | Picker { kind = Pull_requests; _ } ->
          Some
            [ span
                ~style:attention
                "Enter shows it, its checks and its diff · Esc closes"
            ]
        | Picker { kind = Mcp _; _ } ->
          Some
            [ span
                ~style:attention
                "Enter approves a server or lists its tools · Esc closes"
            ]
        | Picker _ ->
          Some [ span ~style:attention "Enter selects · Esc closes" ]
        | Login_prompt _ ->
          Some [ span ~style:attention "login: Enter answers · Esc cancels" ]
        | Text_prompt { action = Deny_reason _; _ } ->
          Some [ span ~style:attention "Enter denies · Esc goes back" ]
        | Text_prompt _ ->
          (match m.autocomplete with
           | Some ac when Autocomplete.accepts_on_enter ac ->
             Some [ span ~style:attention "Tab/Enter accept · Alt+S as typed" ]
           | Some _ ->
             Some
               [ span
                   ~style:attention
                   "Tab completes · Enter submits · Esc hides"
               ]
           | None ->
             Some [ span ~style:attention "Enter submits · Esc cancels" ])
        | Confirm { action = Tool_confirm c; _ } ->
          Some
            [ span
                ~style:attention
                ("confirm: "
                 ^ String.concat
                     ~sep:"/"
                     (List.map (Confirm_prompt.answers c) ~f:(fun (_, key, _) ->
                        key)))
            ]
        | Confirm _ -> Some [ span ~style:attention "confirm: y / n" ]
        | Question _ ->
          Some [ span ~style:attention "Enter accepts · Esc declines" ]
        | Sign_in _ -> Some [ span ~style:attention sign_in_keys ]
        | Pr_form _ -> Some [ span ~style:attention pr_form_keys ]
        | Search { query = _; matches; current } ->
          if List.is_empty matches
          then
            Some [ span ~style:attention "search: type to find · Esc closes" ]
          else
            Some
              [ span
                  ~style:attention
                  (sprintf
                     "search %d/%d · ↓↑ next/prev · Esc closes"
                     (current + 1)
                     (List.length matches))
              ]
        | Editing ->
          (match m.autocomplete with
           | Some ac when Autocomplete.accepts_on_enter ac ->
             Some [ span ~style:attention "Tab/Enter accept · Alt+S as typed" ]
           | Some _ ->
             Some
               [ span
                   ~style:attention
                   "Tab accepts · Enter as typed · Esc hides"
               ]
           | None ->
             if m.pending_quit
             then Some [ span ~style:attention "Ctrl+C again quits" ]
             else if Option.is_some m.btw
             then
               Some
                 [ span
                     ~style:attention
                     (if s.running
                      then
                        spinner_frames.(m.spinner % Array.length spinner_frames)
                        ^ sprintf
                            " %s · Esc closes btw · Enter steers"
                            (doing m)
                      else "Esc dismisses btw")
                 ]
             else if s.running
             then
               Some
                 [ span
                     ~style:attention
                     (spinner_frames.(m.spinner % Array.length spinner_frames)
                      ^ sprintf " %s · Esc aborts · Enter steers" (doing m))
                 ]
             else None)
        | Output _ ->
          Some
            [ span
                ~style:attention
                "↑↓ scroll · Esc closes · typing goes to the editor"
            ]))
;;

let tools_part (m : App.Model.t) (s : P.State.t) =
  let is_me id =
    Option.value_map m.client_id ~default:false ~f:(String.equal id)
  in
  match List.find s.hosts ~f:(fun h -> String.equal h.id s.active_host) with
  | None when List.is_empty s.hosts -> []
  | None -> [ 2, [ span ~style:(Style.fg Red) "tools:offline" ] ]
  | Some h when String.equal h.id P.Host.backend_id || is_me h.id -> []
  | Some h -> [ 4, [ span ~style:attention ("tools:" ^ h.name) ] ]
;;

(* Always shown, like the thinking level; plan mode in yellow. A mode that
   asks first or refuses outlasts [auto] at narrow widths. *)
let mode_part (s : P.State.t) =
  let mode = Session_mode.of_state s in
  let style = if mode.plan then attention else gray in
  let priority =
    if (not mode.plan) && P.Approval_mode.equal mode.approval Auto then 4 else 3
  in
  [ priority, [ span ~style ("mode:" ^ Session_mode.label mode) ] ]
;;

let plan_part (s : P.State.t) =
  match Option.bind s.plan ~f:Plan_view.status with
  | None -> []
  | Some text -> [ 3, [ span ~style:gray text ] ]
;;

let confirm_dialog ~width (c : P.Pending.Confirm.t) =
  let inner = Boxed.inner_width ~width in
  let question =
    span ~style:(Style.bold (Style.fg Yellow)) (Confirm_prompt.title c)
  in
  let plan =
    if Confirm_prompt.is_plan c
    then
      Content.wrap
        (List.map (String.split_lines c.summary) ~f:Content.Line.of_string)
        ~width:inner
      @ [ [] ]
    else []
  in
  let answers =
    List.map (Confirm_prompt.answers c) ~f:(fun (_, key, label) ->
      [ span ~style:(Style.bold Style.plain) key
      ; span ~style:gray ("  " ^ label)
      ])
  in
  Boxed.render
    ~title:(if Confirm_prompt.is_plan c then "Plan" else "Confirm")
    ~width
    (Content.wrap [ [ question ] ] ~width:inner @ plan @ answers)
;;

(* The keys of a question, most important first, wrapped in the box. *)
let question_keys ~(q : P.Question.t) ~total =
  let has_choices = not (List.is_empty q.choices) in
  List.filter_opt
    [ Option.some_if has_choices "↑↓ choose"
    ; Option.some_if q.multiple "Space toggles"
    ; Some (if has_choices then "type your own" else "type an answer")
    ; Some
        (if q.multiple
         then "Enter accepts"
         else if has_choices
         then "Enter picks"
         else "Enter answers")
    ; Option.some_if (total > 1) "←→ questions"
    ; Some "Esc declines (the model is told)"
    ]
;;

(* The dialog and where the cursor goes in it (the own answer's end). *)
let question_dialog ~width ~utc_offset form =
  let inner = Boxed.inner_width ~width in
  let questions = Question_form.questions form in
  let index = Question_form.current form in
  let q : P.Question.t = List.nth_exn questions index in
  let total = List.length questions in
  let title =
    match q.header with
    | Some header when total > 1 ->
      sprintf "%s (%d/%d)" header (index + 1) total
    | Some header -> header
    | None when total > 1 -> sprintf "Question %d/%d" (index + 1) total
    | None -> "Question"
  in
  let wrapped =
    Content.wrap
      [ [ span ~style:(Style.bold (Style.fg Yellow)) q.question ] ]
      ~width:inner
  in
  let pointer row = if Question_form.cursor form = row then "› " else "  " in
  (* Long descriptions and answers wrap, indented under the label. *)
  let chunks text ~width =
    let rec go text acc =
      let head, rest = Text_width.take text ~width:(Int.max 1 width) in
      if String.is_empty rest
      then List.rev (head :: acc)
      else go rest (head :: acc)
    in
    go text []
  in
  let indent = "      " in
  let choices =
    List.mapi q.choices ~f:(fun i (c : P.Question.Choice.t) ->
      let picked = Question_form.is_picked form ~question:index ~choice:i in
      let mark =
        match q.multiple, picked with
        | true, true -> "[x] "
        | true, false -> "[ ] "
        | false, true -> "(•) "
        | false, false -> "( ) "
      in
      let line =
        [ span ~style:attention (pointer i)
        ; span mark
        ; span
            ~style:(if picked then Style.bold Style.plain else Style.plain)
            c.label
        ]
      in
      match c.description with
      | None -> [ line ]
      | Some d when Content.Line.width line + 2 + Text_width.string d <= inner
        -> [ line @ [ span ~style:gray ("  " ^ d) ] ]
      | Some d ->
        line
        :: List.map
             (Content.wrap
                [ [ span ~style:gray d ] ]
                ~width:(inner - String.length indent))
             ~f:(fun row -> span indent :: row))
  in
  let choice_rows = List.concat choices in
  let own_row = List.length q.choices in
  let own_prefix =
    pointer own_row ^ if List.is_empty q.choices then "" else "Other: "
  in
  let own = Question_form.own_answer form ~question:index in
  let own_indent = String.make (Text_width.string own_prefix) ' ' in
  let own_lines =
    List.mapi
      (chunks own ~width:(inner - Text_width.string own_prefix))
      ~f:(fun i chunk ->
        [ span ~style:attention (if i = 0 then own_prefix else own_indent)
        ; span chunk
        ])
  in
  let hint =
    (* Whole keys to a row. *)
    List.fold (question_keys ~q ~total) ~init:[] ~f:(fun rows key ->
      match rows with
      | row :: rest when Text_width.string (row ^ " · " ^ key) <= inner ->
        (row ^ " · " ^ key) :: rest
      | rows -> key :: rows)
    |> List.rev_map ~f:(fun row -> [ span ~style:gray row ])
  in
  let deadline =
    match Question_form.deadline_note form ~utc_offset with
    | None -> []
    | Some note -> Content.wrap [ [ span ~style:gray note ] ] ~width:inner
  in
  let body = wrapped @ choice_rows @ own_lines @ hint @ deadline in
  let cursor =
    if Question_form.on_own_answer form
    then (
      let last = List.last_exn own_lines in
      ( List.length wrapped + List.length choice_rows + List.length own_lines
      , 2 + Content.Line.width last ))
    else (
      (* after the label, not over the pointer *)
      let row = Question_form.cursor form in
      let label = (List.nth_exn q.choices row).label in
      let above =
        List.sum (module Int) (List.take choices row) ~f:List.length
      in
      1 + List.length wrapped + above, 2 + 2 + 4 + Text_width.string label)
  in
  Boxed.render ~title ~width body, cursor
;;

(* The [/signin] dialog: the cursor ends the focused field. *)
let sign_in_dialog ~width form =
  let inner = Boxed.inner_width ~width in
  let field (which : Sign_in_form.Field.t) label value =
    let focused = Sign_in_form.Field.equal (Sign_in_form.focus form) which in
    [ span
        ~style:(if focused then Style.bold attention else gray)
        (sprintf "%-7s" label)
    ; span value
    ]
  in
  let user = field User "user" (Sign_in_form.user form) in
  let token = field Token "token" (Sign_in_form.masked_token form) in
  let note =
    match Sign_in_form.submitting form, Sign_in_form.error form with
    | true, _ -> [ [ span ~style:gray "signing in…" ] ]
    | false, Some error ->
      Content.wrap [ [ span ~style:(Style.fg Red) error ] ] ~width:inner
    | false, None -> []
  in
  let hint =
    [ [ span
          ~style:gray
          "prigh serve -tokens: your user name and password; -token: no user"
      ]
    ]
  in
  let rows = [ user; token ] in
  let cursor =
    let row, line =
      match Sign_in_form.focus form with
      | User -> 0, user
      | Token -> 1, token
    in
    1 + row, 2 + Int.min (Content.Line.width line) (inner - 1)
  in
  ( Boxed.render
      ~title:("Sign in to " ^ Sign_in_form.address form)
      ~width
      (rows @ note @ Content.wrap hint ~width:inner)
  , cursor )
;;

let pr_form_body_rows = 6

(* [/pr new]'s dialog: the cursor ends the focused text field, or sits in
   the focused checkbox. *)
let pr_form_dialog ~width form =
  let inner = Boxed.inner_width ~width in
  let focus = Pr_form.focus form in
  let defaults = Pr_form.defaults form in
  let label (which : Pr_form.Field.t) text =
    span
      ~style:
        (if Pr_form.Field.equal focus which then Style.bold attention else gray)
      text
  in
  let indent = "       " in
  let text_rows which name value ~max_rows =
    let room = Int.max 1 (inner - String.length indent) in
    let rows =
      List.concat_map (String.split value ~on:'\n') ~f:(fun line ->
        let rec go text acc =
          let head, rest = Text_width.take text ~width:room in
          if String.is_empty rest
          then List.rev (head :: acc)
          else go rest (head :: acc)
        in
        go line [])
    in
    let focused = Pr_form.Field.equal focus which in
    let shown, hidden =
      if List.length rows <= max_rows
      then rows, 0
      else if focused
      then (
        let hidden = List.length rows - max_rows in
        List.drop rows hidden, hidden)
      else List.take rows max_rows, List.length rows - max_rows
    in
    let lines =
      List.mapi shown ~f:(fun i row ->
        [ (if i = 0 then label which (sprintf "%-7s" name) else span indent)
        ; span row
        ])
    in
    let note =
      if hidden = 0
      then []
      else
        [ [ span indent
          ; span
              ~style:dim
              (sprintf
                 "(%d more line%s %s)"
                 hidden
                 (if hidden = 1 then "" else "s")
                 (if focused then "above" else "below"))
          ]
        ]
    in
    if focused then note @ lines else lines @ note
  in
  let checkbox which on text detail =
    [ label which (if on then "[x] " else "[ ] ")
    ; label which text
    ; span ~style:gray detail
    ]
  in
  let title = text_rows Title "title" (Pr_form.title form) ~max_rows:2 in
  let body =
    text_rows Body "body" (Pr_form.body form) ~max_rows:pr_form_body_rows
  in
  let base = text_rows Base "base" (Pr_form.base form) ~max_rows:1 in
  let draft = [ checkbox Draft (Pr_form.draft form) "draft" "" ] in
  let push =
    [ checkbox
        Push
        (Pr_form.push form)
        (sprintf "push %s first" defaults.head)
        (if defaults.pushed
         then "  (it is on the remote)"
         else "  (not on the remote yet)")
    ]
  in
  let commits = List.length defaults.commits in
  let summary =
    [ [ span
          ~style:gray
          (sprintf
             "%s → %s · %d commit%s"
             defaults.head
             (Pr_form.base form)
             commits
             (if commits = 1 then "" else "s"))
      ]
    ]
  in
  let note =
    match Pr_form.submitting form, Pr_form.error form with
    | true, _ -> [ [ span ~style:gray "creating…" ] ]
    | false, Some error ->
      Content.wrap [ [ span ~style:(Style.fg Red) error ] ] ~width:inner
    | false, None -> []
  in
  let keys =
    Content.wrap
      [ [ span
            ~style:gray
            "Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · \
             Enter creates · Esc cancels"
        ]
      ]
      ~width:inner
  in
  let sections = [ title; body; base; draft; push ] in
  let rows = List.concat sections in
  let cursor =
    let index =
      match focus with
      | Title -> 0
      | Body -> 1
      | Base -> 2
      | Draft -> 3
      | Push -> 4
    in
    let above =
      List.sum (module Int) (List.take sections index) ~f:List.length
    in
    let own = List.nth_exn sections index in
    match focus with
    | Draft | Push -> 1 + above, 2 + 3
    | Title | Body | Base ->
      let last = List.length own - 1 in
      let line = List.nth_exn own last in
      1 + above + last, 2 + Int.min (Content.Line.width line) (inner - 1)
  in
  ( Boxed.render
      ~title:("New pull request in " ^ P.Forge_repo.slug defaults.repo)
      ~width
      (rows @ summary @ note @ keys)
  , cursor )
;;

(* Editor lines hard-wrapped after a two-column marker, returning the rows and
   the cursor position relative to the first row. Collapsed paste chips render
   as a single dim line while the cursor is elsewhere. *)
let editor_rows ?(width = 0) (m : App.Model.t) ~marker ~marker_style ~mask
  : Content.t * (int * int)
  =
  let width = if width > 0 then width else m.width in
  let inner = Int.max 1 (width - 2) in
  let pos = Editor.position m.editor in
  let lines = Editor.lines m.editor in
  let chips = Editor.chips m.editor in
  let rows = ref [] in
  let cursor = ref (0, 2) in
  let emitted = ref 0 in
  let prefix i j =
    if i = 0 && j = 0 then span ~style:marker_style marker else span "  "
  in
  let render_line i line =
    let line =
      if mask
      then String.make (List.length (Text_width.uchars line)) '*'
      else line
    in
    let chunks =
      let rec go text acc =
        let head, rest = Text_width.take text ~width:inner in
        if String.is_empty rest
        then List.rev (head :: acc)
        else go rest (head :: acc)
      in
      go line []
    in
    let first_row = !emitted in
    List.iteri chunks ~f:(fun j chunk ->
      rows := (prefix i j :: Content.Line.of_string chunk) :: !rows;
      incr emitted);
    if i = pos.line
    then (
      let before =
        String.concat
          (List.take (List.map (Text_width.uchars line) ~f:fst) pos.col)
      in
      let w = Text_width.string before in
      cursor := first_row + (w / inner), 2 + (w % inner))
  in
  let rec go i =
    if i >= List.length lines
    then ()
    else (
      let collapsed =
        List.find chips ~f:(fun (c : Editor.Chip.t) ->
          c.start.line = i && not (Editor.Chip.contains c pos))
      in
      match collapsed with
      | Some chip ->
        let line = List.nth_exn lines i in
        let before =
          String.concat
            (List.take
               (List.map (Text_width.uchars line) ~f:fst)
               chip.start.col)
        in
        let label = sprintf "[%d lines pasted]" (Editor.Chip.lines chip) in
        let shortened =
          if Text_width.string (before ^ label) <= inner
          then before ^ label
          else Text_width.truncate (before ^ label) ~width:inner
        in
        let row = List.length !rows in
        rows
        := (prefix i 0 :: Content.Line.of_string ~style:dim shortened) :: !rows;
        incr emitted;
        (* The cursor sits after the chip when it is past the pasted block. *)
        if pos.line = chip.stop.line && pos.col >= chip.stop.col
        then cursor := row, 2 + Text_width.string shortened;
        go (chip.stop.line + 1)
      | None ->
        render_line i (List.nth_exn lines i);
        go (i + 1))
  in
  go 0;
  List.rev !rows, !cursor
;;

(* The reason for denying a tool call, typed in the box under the call it
   is about; the cursor is in the box. *)
let deny_reason_dialog (m : App.Model.t) ~width ~question c =
  let inner = Boxed.inner_width ~width in
  let call =
    Content.wrap
      [ [ span ~style:(Style.bold (Style.fg Yellow)) (Confirm_prompt.title c) ]
      ]
      ~width:inner
  in
  let rows, (row, col) =
    editor_rows
      m
      ~width:inner
      ~marker:"› "
      ~marker_style:(Style.bold (Style.fg Yellow))
      ~mask:false
  in
  ( Boxed.render ~title:question ~width (call @ rows)
  , (1 + List.length call + row, 2 + col) )
;;

let subagent_header (m : App.Model.t) (a : Agent_view.t) : Content.Line.t =
  let total = List.length m.agents in
  let index =
    match List.findi m.agents ~f:(fun _ x -> String.equal x.id a.id) with
    | Some (i, _) -> i + 1
    | None -> 0
  in
  let status =
    match a.status with
    | Agent_view.Status.Running ->
      sprintf
        "%s running %d turns"
        spinner_frames.(m.spinner % Array.length spinner_frames)
        a.turns
    | Agent_view.Status.Done { turns; cost_usd } ->
      sprintf "✓ done %d turns $%.2f" turns cost_usd
    | Agent_view.Status.Failed _ -> "✗ failed"
  in
  let task =
    Text_width.truncate
      (String.concat ~sep:" " (String.split_lines a.task))
      ~width:40
  in
  [ span
      ~style:(Style.bold (Style.fg Cyan))
      (sprintf "◆ subagent %d/%d" index total)
  ; span ~style:dim (sprintf "  %s" a.model)
  ; span ~style:gray (sprintf "  %s" status)
  ; span (sprintf "  \"%s\"" task)
  ]
;;

(* ---- the status line --------------------------------------------------- *)

let format_cwd (m : App.Model.t) (s : P.State.t) =
  let cwd =
    match s.git_branch with
    | Some branch -> sprintf "%s (%s)" (App.tilde m s.cwd) branch
    | None -> App.tilde m s.cwd
  in
  match s.session_name with
  | Some name -> sprintf "%s %S" cwd name
  | None -> cwd
;;

(* [ctx:0.2%/1.0M] like pi: a tenth of a percent shows early growth in a
   large window. *)
let context_text (s : P.State.t) =
  let window = s.model.context_window in
  if window > 0
  then (
    let percent =
      100. *. Float.of_int s.context_tokens /. Float.of_int window
    in
    sprintf
      "ctx:%s%%/%s"
      (if Float.( < ) percent 10.
       then sprintf "%.1f" percent
       else sprintf "%.0f" (Float.round_down percent))
      (format_tokens window))
  else "ctx:" ^ format_tokens s.context_tokens
;;

let server_part (m : App.Model.t) =
  match Option.bind m.server ~f:(fun s -> s.address) with
  | Some address -> [ 6, [ span ~style:gray ("server:" ^ address) ] ]
  | None -> []
;;

let status (m : App.Model.t) : Content.Line.t =
  match m.state, m.connection with
  | _, Disconnected _ -> [ span ~style:attention (disconnected_hint m) ]
  | None, Switching _ -> [ span ~style:gray "connecting…" ]
  | None, _ ->
    (match m.mode with
     | Sign_in _ -> [ span ~style:attention sign_in_keys ]
     | _ -> [ span ~style:gray "connecting…" ])
  | Some s, _ ->
    let max_width = Int.max 1 m.width in
    let base = gray in
    let context =
      if s.model.context_window > 0
      then 100 * s.context_tokens / s.model.context_window
      else 0
    in
    (* Each part after the model carries a priority: at narrow widths the mode
       hint and context survive before think, the mode and the cost, those
       before view. Display order is fixed. *)
    let cwd = [ span ~style:base (format_cwd m s) ] in
    let model = [ span ~style:base s.model.id ] in
    let after_model =
      [ ( 4
        , [ span
              ~style:base
              (sprintf
                 "think:%s"
                 (if s.model.supports_thinking then s.thinking else "n/a"))
          ] )
      ]
      @ mode_part s
      @ [ 7, [ span ~style:base ("view:" ^ Verbosity.name m.verbosity) ]
        ; 2, [ span ~style:(context_style context) (context_text s) ]
        ; ( 4
          , match Loop_status.budget_part s with
            | None -> [ span ~style:base (sprintf "$%.2f" s.cost_usd) ]
            | Some (text, level) -> [ span ~style:(level_style level) text ] )
        ]
      @ Option.value_map s.goal ~default:[] ~f:(fun goal ->
        let text, level = Loop_status.goal_part goal in
        [ 3, [ span ~style:(level_style level) text ] ])
      @ Option.value_map
          (Remind_command.status_part s.reminders)
          ~default:[]
          ~f:(fun text -> [ 3, [ span ~style:base text ] ])
      @ Option.value_map s.resume_at ~default:[] ~f:(fun at ->
        [ ( 2
          , [ span
                ~style:attention
                (Loop_status.resume_part at ~utc_offset:m.utc_offset)
            ] )
        ])
      @ server_part m
      @ Option.value_map m.namespace ~default:[] ~f:(fun namespace ->
        let text =
          match App.Model.acting_as m, m.user with
          | Some _, Some user -> sprintf "user:%s as %s" user namespace
          | _ -> "user:" ^ namespace
        in
        [ 6, [ span ~style:base text ] ])
      @ tools_part m s
      @ plan_part s
      (* A model that keeps failing: shown even with a dialog over the
         hint. *)
      @ (match Activity.retry (App.Model.activity m) with
         | Some { attempt; max_attempts; _ } ->
           [ ( 1
             , [ span
                   ~style:attention
                   (sprintf "retry:%d/%d" attempt max_attempts)
               ] )
           ]
         | None -> [])
      @ (let queued = List.length m.queued_messages in
         if queued > 0
         then [ 3, [ span ~style:base (sprintf "queued:%d" queued) ] ]
         else [])
      (* Confirmations and questions waiting behind a dialog: before
         anything but the model. *)
      @ (let waiting =
           List.length m.pending_confirms + List.length m.pending_questions
         in
         if waiting > 0
         then [ 0, [ span ~style:attention (sprintf "to answer:%d" waiting) ] ]
         else [])
      @ Option.to_list (Option.map (agents_part m) ~f:(fun p -> 3, p))
      @ Option.to_list (Option.map (jobs_part m) ~f:(fun p -> 3, p))
      @ (match m.viewport with
         | Viewport.Anchored { new_lines; _ } when new_lines > 0 ->
           [ 1, [ span ~style:attention (sprintf "↓ %d new" new_lines) ] ]
         | Viewport.Anchored _ -> [ 1, [ span ~style:attention "↑ scrolled" ] ]
         | Viewport.Follow -> [])
      @ Option.to_list (Option.map (mode_hint m) ~f:(fun p -> 0, p))
    in
    let hint_index =
      match List.last after_model with
      | Some (0, _) -> List.length after_model - 1
      | _ -> -1
    in
    let cwd_width = Content.Line.width cwd in
    let model_width = Content.Line.width model in
    (* Keep whole parts by priority until the width is exhausted, then
       restore the display order. *)
    let fill ?(max_prio = Int.max_value) used0 =
      (* A mode hint too wide to fit whole is cut short after the context
         (priority 2) has its place, rather than hidden. *)
      let indexed =
        List.mapi after_model ~f:(fun i (prio, part) ->
          let whole = used0 + 2 + Content.Line.width part <= max_width in
          if prio = 0 && not whole then 2, i, part else prio, i, part)
        |> List.filter ~f:(fun (prio, _, _) -> prio <= max_prio)
      in
      let by_priority =
        List.sort indexed ~compare:(fun (p1, i1, _) (p2, i2, _) ->
          match Int.compare p1 p2 with
          | 0 -> Int.compare i1 i2
          | c -> c)
      in
      let kept, used =
        List.fold
          by_priority
          ~init:([], used0)
          ~f:(fun (kept, used) (_, i, part) ->
            let w = Content.Line.width part in
            let room = max_width - used - 2 in
            if w <= room
            then (i, part) :: kept, used + 2 + w
            else if i = hint_index && room >= 12
            then (
              let part =
                match part with
                | [ (hint : Content.Span.t) ] ->
                  [ { hint with text = fit_keys hint.text ~width:room } ]
                | part -> Content.Line.truncate part ~width:room
              in
              (i, part) :: kept, max_width)
            else kept, used)
      in
      ( List.map
          (List.sort kept ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2))
          ~f:snd
      , used )
    in
    let fits (_, used) = used + 2 + cwd_width <= max_width in
    (* The view (priority 7) gives way to the directory, the directory to
       everything else; the model stays. *)
    let right, cwd_fits =
      match
        List.find [ fill model_width; fill ~max_prio:6 model_width ] ~f:fits
      with
      | Some (right, _) -> right, true
      | None -> fst (fill model_width), false
    in
    let line =
      (if cwd_fits then cwd @ [ span "  " ] else [])
      @ model
      @ List.concat_map right ~f:(fun part -> span "  " :: part)
    in
    Content.Line.truncate line ~width:max_width
;;

let first_line text =
  match String.split_lines (String.strip text) with
  | line :: _ -> line
  | [] -> ""
;;

(* ---- the composer's rule ----------------------------------------------- *)

let notice_style (severity : Severity.t) =
  match severity with
  | Error -> Style.fg Red
  | Warn -> attention
  | Info | Debug -> Style.plain
;;

(* Whether the notice fits in the rule; otherwise a box shows it. *)
let notice_inline (n : Notice.t) ~width =
  (not (String.mem n.text '\n')) && Text_width.string n.text + 6 <= width
;;

let rule_glyphs n = String.concat (List.init (Int.max 0 n) ~f:(fun _ -> "─"))

(* A plain rule; the latest notice, when short, sits on its right. *)
let composer_rule (m : App.Model.t) ~width : Content.Line.t =
  match m.notice with
  | Some n when notice_inline n ~width ->
    let notice = Text_width.string n.text in
    [ span ~style:gray (rule_glyphs (width - notice - 5) ^ " ")
    ; span ~style:(notice_style n.severity) n.text
    ; span ~style:gray " ───"
    ]
  | _ -> [ span ~style:gray (rule_glyphs width) ]
;;

(* ---- overlays: drawn over the transcript's bottom rows ------------------ *)

module Overlay = struct
  type t =
    { lines : Content.t
    ; cursor : (int * int) option (** relative to its first row *)
    }

  let create ?cursor lines = { lines; cursor }
end

let max_queued_shown = 3

(** The queued steers and follow-ups, oldest first. *)
let queued_block (m : App.Model.t) ~width : Overlay.t option =
  let total = List.length m.queued_messages in
  if total = 0
  then None
  else (
    let header =
      [ span
          ~style:dim
          (sprintf
             "queued (%d) · Alt+Up: edit the last · /queue: edit or remove"
             total)
      ]
    in
    let shown = List.take m.queued_messages max_queued_shown in
    let rows =
      List.map shown ~f:(fun (q : Queued_message.t) ->
        [ span
            ~style:attention
            (Text_width.pad_right
               ("  " ^ Queued_message.Kind.name q.kind)
               ~width:13)
        ; span ~style:dim (Queued_message.one_line q)
        ])
    in
    let more = total - List.length shown in
    let more =
      if more > 0 && not (List.is_empty shown)
      then [ [ span ~style:dim (sprintf "  +%d more" more) ] ]
      else []
    in
    Some
      (Overlay.create
         (List.map
            ((header :: rows) @ more)
            ~f:(Content.Line.truncate ~width))))
;;

let notice_box (m : App.Model.t) ~width : Overlay.t option =
  match m.notice with
  | Some n when not (notice_inline n ~width:m.width) ->
    let inner = Boxed.inner_width ~width in
    let body =
      Content.wrap
        (List.map (String.split_lines n.text) ~f:(fun line ->
           [ span ~style:(notice_style n.severity) line ]))
        ~width:inner
    in
    let body =
      if List.length body > 6
      then List.take body 5 @ [ [ span ~style:dim "…" ] ]
      else body
    in
    let title, border =
      match n.severity with
      | Error -> "Error", Style.fg Red
      | Warn -> "Warning", attention
      | Info | Debug -> "Notice", gray
    in
    Some
      (Overlay.create
         (Boxed.render ~border ~title ~footer:"Esc dismisses" ~width body))
  | _ -> None
;;

let picker_block ?keys (p : Picker.t) ~width ~rows : Overlay.t =
  let visible = Picker.visible p in
  let selected = Picker.selected p in
  let start =
    Int.max 0 (Int.min (selected - (rows / 2)) (List.length visible - rows))
  in
  let shown = List.take (List.drop visible start) rows in
  let total = List.length visible in
  let title =
    [ span ~style:(Style.bold Style.plain) (Picker.title p)
    ; span
        ~style:dim
        (if total <= rows
         then sprintf "  (%d)" total
         else
           sprintf
             "  (%d–%d of %d)"
             (start + 1)
             (start + List.length shown)
             total)
    ]
  in
  let title =
    match keys with
    | None -> title
    | Some keys ->
      let room = width - Content.Line.width title - 2 in
      if room < 12
      then title
      else title @ [ span ~style:dim ("  " ^ fit_keys keys ~width:room) ]
  in
  let filter =
    [ span ~style:(Style.bold (Style.fg Yellow)) "/ "; span (Picker.query p) ]
  in
  let label_width =
    List.fold shown ~init:0 ~f:(fun acc (item : Picker.Item.t) ->
      Int.max acc (Text_width.string item.label))
    |> Int.min (width / 2)
  in
  let lines =
    List.mapi shown ~f:(fun i (item : Picker.Item.t) ->
      let is_selected = start + i = selected in
      let mark =
        (if is_selected then "▸" else " ")
        ^
        if Picker.multi p
        then if Set.mem (Picker.checked p) item.id then "[x] " else "[ ] "
        else if Set.mem (Picker.checked p) item.id
        then "✓ "
        else if item.marked
        then "* "
        else "  "
      in
      let label = Text_width.pad_right item.label ~width:label_width in
      let label_style =
        let s = if item.dimmed then dim else Style.plain in
        if is_selected then Style.invert (Style.bold s) else s
      in
      let line =
        [ span ~style:label_style (mark ^ label)
        ; span
            ~style:(if is_selected then Style.invert dim else dim)
            (if String.is_empty item.detail then "" else "  " ^ item.detail)
        ]
      in
      let divider =
        let previous =
          if i = 0
          then if start = 0 then None else List.nth visible (start - 1)
          else List.nth shown (i - 1)
        in
        match previous with
        | _ when String.is_empty item.section -> []
        | Some (previous : Picker.Item.t)
          when String.equal previous.section item.section -> []
        | _ -> [ [ span ~style:dim ("  ── " ^ item.section ^ " ──") ] ]
      in
      divider @ [ Content.Line.truncate line ~width ])
    |> List.concat
  in
  let lines =
    if List.is_empty lines
    then [ [ span ~style:dim "  no matches" ] ]
    else lines
  in
  Overlay.create
    ~cursor:(1, 2 + Text_width.string (Picker.query p))
    (title :: filter :: lines)
;;

let autocomplete_block (ac : Autocomplete.t) ~width ~rows : Overlay.t =
  let items = Autocomplete.items ac in
  let selected = Autocomplete.selected ac in
  let total = List.length items in
  let rows = Int.max 1 (Int.min 8 rows) in
  let start = Int.max 0 (Int.min (selected - (rows / 2)) (total - rows)) in
  let shown = List.take (List.drop items start) rows in
  let overflow =
    if total <= rows
    then []
    else
      [ [ span
            ~style:dim
            (sprintf
               "  ↕ %d–%d of %d"
               (start + 1)
               (start + List.length shown)
               total)
        ]
      ]
  in
  let lines =
    match Autocomplete.source ac with
    | Autocomplete.Source.Command ->
      let usage (item : Picker.Item.t) =
        Option.value_map
          (Commands.find item.id)
          ~default:("/" ^ item.label)
          ~f:Commands.usage
      in
      let usage_width =
        List.fold shown ~init:0 ~f:(fun acc item ->
          Int.max acc (Text_width.string (usage item)))
        |> Int.min (width / 2)
      in
      List.mapi shown ~f:(fun i (item : Picker.Item.t) ->
        let is_selected = start + i = selected in
        let style s = if is_selected then Style.invert s else s in
        let help =
          Option.value_map (Commands.find item.id) ~default:"" ~f:(fun s ->
            s.help)
        in
        let name = "/" ^ item.label in
        let args =
          Text_width.pad_right
            (String.drop_prefix (usage item) (String.length name))
            ~width:(usage_width - Text_width.string name)
        in
        Content.Line.truncate
          [ span
              ~style:(style Style.plain)
              (if is_selected then "▸ " else "  ")
          ; span ~style:(style (Style.bold Style.plain)) name
          ; span ~style:(style dim) args
          ; span ~style:(style dim) ("  " ^ help)
          ]
          ~width)
    | Autocomplete.Source.Argument _ | Path | Directory _ | Skill | Snippet ->
      let label_width =
        List.fold shown ~init:0 ~f:(fun acc (item : Picker.Item.t) ->
          Int.max acc (Text_width.string item.label))
        |> Int.min (width / 2)
      in
      List.mapi shown ~f:(fun i (item : Picker.Item.t) ->
        let is_selected = start + i = selected in
        let style s = if is_selected then Style.invert s else s in
        Content.Line.truncate
          [ span
              ~style:(style Style.plain)
              (if is_selected then "▸ " else "  ")
          ; span
              ~style:(style (Style.bold Style.plain))
              (Text_width.pad_right item.label ~width:label_width)
          ; span
              ~style:(style dim)
              (if String.is_empty item.detail then "" else "  " ^ item.detail)
          ]
          ~width)
  in
  Overlay.create (lines @ overflow)
;;

let output_box (box : Output_box.t) ~width ~rows : Overlay.t =
  let lines, footer =
    Output_box.window box ~width:(Boxed.inner_width ~width) ~rows:(rows - 2)
  in
  Overlay.create (Boxed.render ~footer ~title:box.title ~width lines)
;;

(* The mode's own rows, top to bottom, nearest the composer last. *)
let mode_overlays (m : App.Model.t) ~width ~rows : Overlay.t list =
  let autocomplete () =
    match m.autocomplete with
    | Some ac -> [ autocomplete_block ac ~width ~rows:(rows - 1) ]
    | None -> []
  in
  match m.mode with
  | Editing -> autocomplete ()
  | Output box -> [ output_box box ~width ~rows ]
  | Picker { picker; kind } ->
    [ picker_block
        ?keys:(picker_keys kind)
        picker
        ~width
        ~rows:(Int.min (App.picker_rows m) (rows - 2))
    ]
  | Confirm { action = Tool_confirm c; _ } ->
    [ Overlay.create (confirm_dialog ~width c) ]
  | Confirm { question; _ } ->
    [ Overlay.create
        (Boxed.render
           ~title:"Confirm"
           ~width
           (Content.wrap
              [ [ span ~style:(Style.bold (Style.fg Yellow)) question ] ]
              ~width:(Boxed.inner_width ~width)))
    ]
  | Question { form; _ } ->
    let lines, cursor = question_dialog ~width ~utc_offset:m.utc_offset form in
    [ Overlay.create ~cursor lines ]
  | Sign_in form ->
    let lines, cursor = sign_in_dialog ~width form in
    [ Overlay.create ~cursor lines ]
  | Pr_form form ->
    let lines, cursor = pr_form_dialog ~width form in
    [ Overlay.create ~cursor lines ]
  | Login_prompt _ ->
    [ Overlay.create
        (Boxed.render
           ~title:"Log in"
           ~width
           (Content.wrap
              (List.map m.login_lines ~f:Content.Line.of_string)
              ~width:(Boxed.inner_width ~width)))
    ]
  | Text_prompt { question; action = Deny_reason { confirm; _ } } ->
    let lines, cursor = deny_reason_dialog m ~width ~question confirm in
    [ Overlay.create ~cursor lines ]
  | Text_prompt { question; _ } ->
    Overlay.create (Boxed.render ~title:question ~width []) :: autocomplete ()
  | Search { query; _ } ->
    [ Overlay.create
        ~cursor:(0, 2 + Text_width.string query)
        [ [ span ~style:(Style.bold (Style.fg Yellow)) "/ "; span query ] ]
    ]
;;

(* What comes and goes with Editing (btw, queued messages) gives way first
   when the area is short, then the notice; the mode's own rows are cut to
   fit, the top first. *)
let overlays (m : App.Model.t) ~width ~rows : Overlay.t list =
  let optional =
    (match m.mode, m.btw with
     | Editing, Some box ->
       [ Overlay.create
           (Btw_box.render
              box
              ~width
              ~max_rows:(Int.min rows (Btw_box.max_rows ~height:m.height)))
       ]
     | _ -> [])
    @ (match m.mode with
       | Editing -> Option.to_list (queued_block m ~width)
       | _ -> [])
    @ Option.to_list (notice_box m ~width)
  in
  let primary = mode_overlays m ~width ~rows in
  let height (o : Overlay.t list) =
    List.sum (module Int) o ~f:(fun o -> List.length o.lines)
  in
  let rec fit optional =
    match optional with
    | _ :: rest when height optional + height primary > rows -> fit rest
    | optional -> optional
  in
  let optional = fit optional in
  let room = rows - height optional in
  let rec cut boxes room =
    match boxes with
    | [] -> []
    | (box : Overlay.t) :: rest ->
      let below = height rest in
      let keep = Int.max 0 (room - below) in
      if keep >= List.length box.lines
      then box :: cut rest (room - List.length box.lines)
      else if keep = 0
      then cut rest room
      else
        { box with lines = List.take box.lines keep } :: cut rest (room - keep)
  in
  optional @ cut primary room
;;

(* ---- the sidebar -------------------------------------------------------- *)

let dot style = span ~style "●"

let waiting_for_you (m : App.Model.t) ~agent_id =
  List.exists m.pending_confirms ~f:(fun (c : P.Pending.Confirm.t) ->
    Option.equal String.equal c.agent_id agent_id)
  ||
  match m.mode with
  | Confirm { action = Tool_confirm c; _ } ->
    Option.equal String.equal c.agent_id agent_id
  | Text_prompt { action = Deny_reason { confirm; _ }; _ } ->
    Option.equal String.equal confirm.agent_id agent_id
  | Question _ -> Option.is_none agent_id
  | _ -> false
;;

(* One entry: the dot and name, the state on the right; a dim line of
   meta under it. *)
let sidebar_entry ~width ~focused ~dot:d ~name ~state ~meta : Content.t =
  let marker = if focused then span ~style:attention "▌" else span " " in
  let state_w = Text_width.string state in
  let name_room = Int.max 1 (width - 3 - state_w - 2) in
  let name = Text_width.truncate name ~width:name_room in
  let gap = width - 3 - Text_width.string name - state_w in
  [ [ marker
    ; d
    ; span " "
    ; span ~style:(if focused then Style.bold Style.plain else Style.plain) name
    ; span (String.make (Int.max 1 gap) ' ')
    ; span ~style:dim state
    ]
  ; [ span "   "
    ; span ~style:dim (Text_width.truncate meta ~width:(width - 3))
    ]
  ]
;;

let turns n = if n = 1 then "1 turn" else sprintf "%d turns" n
let heading text = [ span ~style:(Style.bold gray) (" " ^ text) ]

let sidebar (m : App.Model.t) ~width ~rows : Content.t =
  let running_dot = dot (Style.fg Green) in
  let main =
    let running = App.Model.running m in
    let d, state, meta =
      if waiting_for_you m ~agent_id:None
      then dot attention, "waiting", "waiting for you"
      else if running
      then running_dot, "running", doing m
      else
        ( span ~style:gray "○"
        , "idle"
        , Option.value_map m.state ~default:"" ~f:(fun s -> s.model.id) )
    in
    sidebar_entry
      ~width
      ~focused:
        (match m.focus with
         | `Main -> true
         | `Agent _ -> false)
      ~dot:d
      ~name:"main"
      ~state
      ~meta
  in
  let agents =
    List.concat_mapi m.agents ~f:(fun i (a : Agent_view.t) ->
      let d, state, meta =
        match a.status with
        | _ when waiting_for_you m ~agent_id:(Some a.id) ->
          dot attention, "waiting", "waiting for you"
        | Agent_view.Status.Running ->
          running_dot, "running", sprintf "%s · %s" (turns a.turns) a.model
        | Done { turns = n; cost_usd } ->
          dot gray, "done", sprintf "%s · $%.2f" (turns n) cost_usd
        | Failed error -> dot (Style.fg Red), "failed", first_line error
      in
      sidebar_entry
        ~width
        ~focused:
          (match m.focus with
           | `Agent id -> String.equal id a.id
           | `Main -> false)
        ~dot:d
        ~name:(sprintf "%d %s" (i + 1) (first_line a.task))
        ~state
        ~meta)
  in
  (* Running subagents whose events this client missed (it reconnected). *)
  let missed =
    Option.value_map m.state ~default:[] ~f:(fun s ->
      List.filter s.subagents ~f:(fun (a : P.State.Subagent.t) ->
        not (List.exists m.agents ~f:(fun v -> String.equal v.id a.id))))
    |> List.concat_map ~f:(fun (a : P.State.Subagent.t) ->
      sidebar_entry
        ~width
        ~focused:false
        ~dot:(if a.running then running_dot else dot gray)
        ~name:(first_line a.task)
        ~state:(if a.running then "running" else "done")
        ~meta:a.id)
  in
  let jobs =
    Option.value_map m.state ~default:[] ~f:(fun s -> s.jobs)
    |> List.concat_map ~f:(fun (j : P.State.Job.t) ->
      sidebar_entry
        ~width
        ~focused:false
        ~dot:(if j.running then running_dot else dot gray)
        ~name:j.id
        ~state:
          (if j.running then "running" else Option.value j.exit ~default:"done")
        ~meta:(first_line j.command))
  in
  let body =
    (heading "Agents" :: main)
    @ agents
    @ missed
    @ if List.is_empty jobs then [] else [] :: heading "Jobs" :: jobs
  in
  let footer =
    [ span ~style:dim (Text_width.truncate " Alt+L hides · ⇧Tab focuses" ~width)
    ]
  in
  let room = Int.max 0 (rows - 1) in
  let body =
    if List.length body <= room
    then body
    else
      List.take body (room - 1)
      @ [ [ span
              ~style:dim
              (sprintf " +%d more rows" (List.length body - room + 1))
          ]
        ]
  in
  let padding =
    List.init (Int.max 0 (room - List.length body)) ~f:(fun _ -> [])
  in
  if rows <= 0 then [] else body @ padding @ [ footer ]
;;

(* ---- the frame ---------------------------------------------------------- *)

let pad line ~width =
  let line = Content.Line.truncate line ~width in
  line
  @ [ span (String.make (Int.max 0 (width - Content.Line.width line)) ' ') ]
;;

let exactly rows (lines : Content.t) =
  List.take lines rows
  @ List.init (Int.max 0 (rows - List.length lines)) ~f:(fun _ -> [])
;;

(* The editor's rows for the mode, and the cursor in them: the typed text
   (or the draft kept while a dialog asks something else). *)
let composer (m : App.Model.t) : Content.t * (int * int) =
  let marker_style = Style.bold (Style.fg Yellow) in
  let elsewhere = Style.fg Gray in
  match m.mode with
  | Login_prompt { prompt; _ } ->
    let mask =
      match prompt with
      | Secret _ -> true
      | _ -> false
    in
    editor_rows m ~marker:"? " ~marker_style ~mask
  | Text_prompt { action = Deny_reason { draft; _ }; _ } ->
    editor_rows
      { m with editor = Editor.set_text Editor.empty draft }
      ~marker:"> "
      ~marker_style:elsewhere
      ~mask:false
  | Text_prompt _ | Confirm _ ->
    editor_rows m ~marker:"? " ~marker_style ~mask:false
  | Editing | Output _ -> editor_rows m ~marker:"> " ~marker_style ~mask:false
  | Picker _ | Question _ | Sign_in _ | Pr_form _ | Search _ ->
    editor_rows m ~marker:"> " ~marker_style:elsewhere ~mask:false
;;

let screen (m : App.Model.t) : Screen.t =
  let editor_all, (cursor_row, cursor_col) = composer m in
  let layout =
    Layout.compute
      ~width:m.width
      ~height:m.height
      ~sidebar:m.sidebar
      ~editor_rows:(List.length editor_all)
  in
  let width = layout.width in
  let area = layout.main_width in
  let rows = layout.main_rows in
  (* The editor scrolls once it reaches its cap, keeping the cursor shown. *)
  let editor_start =
    Int.max
      0
      (Int.min
         (List.length editor_all - layout.editor_rows)
         (cursor_row - layout.editor_rows + 1))
  in
  let editor =
    List.take (List.drop editor_all editor_start) layout.editor_rows
  in
  let focused =
    match m.focus with
    | `Main -> None
    | `Agent id -> Agent_view.find m.agents id
  in
  let agent_header, body_rows =
    match focused with
    | Some agent when rows > 0 ->
      ( Some (Content.Line.truncate (subagent_header m agent) ~width:area)
      , rows - 1 )
    | _ -> None, rows
  in
  let source =
    match focused with
    | Some agent -> agent.transcript
    | None -> m.transcript
  in
  let overlays = overlays m ~width:area ~rows in
  let overlay_lines = List.concat_map overlays ~f:(fun o -> o.lines) in
  let overlay_top = rows - List.length overlay_lines in
  (* Following, the transcript's tail ends above the overlays, as it did
     above the old dialogs; scrolled back, they cover its bottom rows. *)
  let body_rows =
    match m.viewport with
    | Viewport.Follow -> Int.max 0 (body_rows - List.length overlay_lines)
    | Viewport.Anchored _ -> body_rows
  in
  let transcript =
    match m.viewport with
    | Viewport.Follow ->
      Transcript.render_tail
        source
        ~width:area
        ~rows:body_rows
        ~skip:0
        ~verbosity:m.verbosity
    | Viewport.Anchored { top; _ } ->
      Transcript.render_window
        source
        ~width:area
        ~rows:body_rows
        ~top
        ~verbosity:m.verbosity
  in
  let transcript =
    match m.mode with
    | Search { query; _ } when not (String.is_empty query) ->
      List.map transcript ~f:(Content.Line.highlight ~needle:query)
    | _ -> transcript
  in
  let padding =
    List.init (Int.max 0 (body_rows - List.length transcript)) ~f:(fun _ -> [])
  in
  let transcript = Option.to_list agent_header @ padding @ transcript in
  let transcript = exactly rows transcript in
  let main =
    List.take transcript overlay_top
    @ List.map overlay_lines ~f:(Content.Line.truncate ~width:area)
  in
  let main =
    if layout.sidebar = 0
    then main
    else
      List.map2_exn
        (sidebar m ~width:(layout.sidebar - 2) ~rows)
        main
        ~f:(fun side line ->
          pad side ~width:(layout.sidebar - 2)
          @ [ span ~style:gray "│"; span " " ]
          @ line)
  in
  let lines =
    main
    @ [ composer_rule m ~width ]
    @ List.map editor ~f:(Content.Line.truncate ~width)
    @ [ status m ]
  in
  let lines = List.take lines layout.height in
  let overlay_cursor =
    List.fold_until
      overlays
      ~init:overlay_top
      ~f:(fun top (o : Overlay.t) ->
        match o.cursor with
        | Some (r, c) -> Stop (Some (top + r, c))
        | None -> Continue (top + List.length o.lines))
      ~finish:(fun _ -> None)
  in
  let cursor =
    match overlay_cursor with
    | Some (r, c) -> r, layout.sidebar + c
    | None -> layout.rule_row + 1 + (cursor_row - editor_start), cursor_col
  in
  let row, col = cursor in
  { lines
  ; cursor = Some (Int.min row (layout.height - 1), Int.min col (width - 1))
  ; width
  ; height = layout.height
  }
;;
